################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 01-calculate-outcomes.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 7/15/26 >
##
################################################################################

#Goal: Calculate the four College-Going Outcomes measures (Technical Guide
#Sec 8) from a PSD snapshot: Postsecondary Plans, Immediate College
#Enrollment, 1st to 2nd Year Persistence, and 6-Year College Completion.
#Produces one combined outcome-snapshot table (one row per HS graduating
#cohort) plus a detailed breakdown table for each of the four measures.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(data.table)

#NOTE: unlike 00-update-master-student-list.R / the missing-data cleaning
#scripts, this script reads an already-standardized PSD snapshot (no messy
#external column names to fix), so janitor::clean_names() isn't needed here.
#readxl isn't needed either since the PSD is a .csv. Add them back in if this
#script ever reads directly from a Box Excel file.

## ---------------------------
## directory paths
## ---------------------------

#see current directory
getwd()

#set current directory
code_file_dir<-file.path(".")

data_file_dir<-file.path("..","..")

# Detect OS and set Box path accordingly
if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  # Box Drive syncs via CloudStorage on Mac
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

output_dir<-"/mnt/user-data/outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_dir<-file.path(box_file_dir,
                      "College and Career RPP",
                      "1. NSC Dataset",
                      "RFK",
                      "RFK PSD",
                      # ⚠️ UPDATE: change to most recent PSD file name
                      "20250905-rfk-psd-yo.csv")

## ---------------------------
## helper functions
## ---------------------------

#Sentinel strings the PSD uses in place of a normal value.
NA_LIKE<-c("MISSING DATA", "NA", "N/A", "")
NO_ENROLL<-"NO ENROLLMENT"

#Parse PSD date strings. NSC/staff dates show up as M/D/YYYY, sometimes with
#a trailing time stamp ("6/7/2017 7:52"), and occasionally as YYYYMMDD or
#YYYY-MM-DD (per Technical Guide Sec 7.5's note that NSC has used three
#different date formats over time). lubridate::parse_date_time tries each
#order in turn and returns NA where none match.
parse_psd_date<-function(x){
  x<-str_squish(as.character(x))
  x[x %in% NA_LIKE]<-NA
  x[str_to_upper(x) == NO_ENROLL]<-NA
  as_date(parse_date_time(x, orders = c("mdY HM", "mdY", "Ymd", "ymd")))
}

#Treat PSD sentinel strings ("MISSING DATA", blank) as true NA.
clean_sentinel<-function(x){
  x<-str_squish(as.character(x))
  x[x %in% NA_LIKE]<-NA
  x
}

#Most frequent non-NA value in a vector -- used to resolve the small number
#of students with conflicting hs_diploma / hs_grad_year values across their
#repeated PSD rows (see Data Quality Flags in Part 7).
mode_value<-function(x){
  x<-x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

#TRUE if a college_name value represents an actual, confirmed college
#enrollment (i.e. not blank, not "MISSING DATA", and not the explicit
#"NO ENROLLMENT" sentinel).
is_real_enrollment<-function(college_name){
  cn<-clean_sentinel(college_name)
  !is.na(cn) & str_to_upper(cn) != NO_ENROLL
}

#TRUE for rows that count toward academic "year_num" (1 or 2) relative to a
#student's hs_grad_year, per Sec 8.3: Fall[gy + year_num - 1], or
#Winter/Spring/Summer/"enrolled anytime after fall"[gy + year_num].
in_academic_year<-function(record_term, record_year, hs_grad_year, year_num){
  fall_year<-hs_grad_year + (year_num - 1)
  rest_year<-hs_grad_year + year_num
  is_fall<-record_term == "fall" & record_year == fall_year
  is_rest<-record_term %in% c("winter", "spring", "summer", "enrolled anytime after fall") &
    record_year == rest_year
  is_fall | is_rest
}

#Rough within-year anchor date used only to place a completion record on the
#correct side of an Aug 15 cutoff when coll_grad_date is missing (Sec 8.4
#Unique Cases: "use the record year and term...since this reflects the
#date/term the graduation information was received").
term_approx_date<-function(record_term, record_year){
  month_day<-case_when(
    record_term == "fall" ~ "09-15",
    record_term == "winter" ~ "01-15",
    record_term == "spring" ~ "04-01",
    record_term == "summer" ~ "07-01",
    record_term == "enrolled anytime after fall" ~ "01-01",
    TRUE ~ NA_character_
  )
  if_else(is.na(month_day) | is.na(record_year), as.Date(NA),
          as_date(str_c(record_year, "-", month_day)))
}

#Assigns a completion-milestone year (4-8) to a single completion date, using
#the fixed Aug 15 cutoffs from Sec 8.4:
#  4-year window: [Jan 1 of grad_yr+1,         Aug 15 of grad_yr+4]
#  k-year window (k=5..8): [Aug 16 of grad_yr+k-1, Aug 15 of grad_yr+k]
#Verified against the guide's own worked example: HS grad 2019, Associate
#conferred Dec 2023 -> after the 4-yr window's Aug 15 2023 cutoff -> falls in
#the 5-yr window (Aug 16 2023-Aug 15 2024) -> milestone = 5, matching exactly.
completion_milestone<-function(completion_date, hs_grad_year){
  four_start<-as_date(str_c(hs_grad_year + 1, "-01-01"))
  four_end<-as_date(str_c(hs_grad_year + 4, "-08-15"))
  case_when(
    is.na(completion_date) ~ NA_integer_,
    completion_date < four_start ~ NA_integer_, #implausibly early
    completion_date <= four_end ~ 4L,
    completion_date <= as_date(str_c(hs_grad_year + 5, "-08-15")) ~ 5L,
    completion_date <= as_date(str_c(hs_grad_year + 6, "-08-15")) ~ 6L,
    completion_date <= as_date(str_c(hs_grad_year + 7, "-08-15")) ~ 7L,
    completion_date <= as_date(str_c(hs_grad_year + 8, "-08-15")) ~ 8L,
    TRUE ~ NA_integer_
  )
}

## ---------------------------
## load & inspect data
## ---------------------------

psd_rfk<-file.path(box_file_dir,
                   "College and Career RPP",
                   "1. NSC Dataset",
                   "RFK","RFK PSD",
                   # UPDATE: change to most recent PSD file name
                   "20250905-rfk-psd-yo.csv")

psd_mann<-file.path(box_file_dir,
                    "College and Career RPP",
                    "1. NSC Dataset",
                    #⚠️ UPDATE: change to school site
                    "Mann",
                    "Mann PSD",
                    # ⚠️ UPDATE: change to most recent PSD file name
                    "20260721-mann-psd-sanchez.csv")

psd_demo<-file.path(box_file_dir,
                    "College and Career RPP",
                    "1. NSC Dataset",
                    #⚠️ UPDATE: change to school site
                    "Demo",
                    # ⚠️ UPDATE: change to most recent PSD file name
                    "demo_psd_synthetic.csv")

psd<- fread(psd_demo)

#cat(sprintf("Loaded %s rows, %s columns from %s\n", nrow(psd), ncol(psd), psd_path))

record_term_v<-psd %>% count(record_term)
hs_diploma_v<-psd %>% count(hs_diploma)

## -----------------------------------------------------------------------------
## Part 1 - Clean PSD & build the cohort (denominator) table
## -----------------------------------------------------------------------------
#Each cohort's N is the number of students who graduated with a Regular
#Diploma at the school's June graduation ceremony (Sec 8); Post-June
#Graduation and Certificate of Completion students are explicitly excluded
#(Sec 7.1 / Sec 8).

psd<-psd %>%
  mutate(
    hs_diploma_std = str_to_upper(clean_sentinel(hs_diploma)),
    college_name_std = clean_sentinel(college_name),
    cc_4year_std = str_to_upper(clean_sentinel(cc_4year)),
    record_term_std = str_to_lower(clean_sentinel(record_term)),
    he_graduated_std = str_to_upper(clean_sentinel(he_graduated)),
    hs_grad_date_p = parse_psd_date(hs_grad_date),
    enrollment_begin_p = parse_psd_date(enrollment_begin),
    enrollment_end_p = parse_psd_date(enrollment_end),
    coll_grad_date_p = parse_psd_date(coll_grad_date)
  )

#DATA QUALITY NOTE: a handful of rows in this file have values that clearly
#belong in a different column (an email address in cc_4year, a spreadsheet-
#formula fragment in program_code, "UCC+[@Gender]" in race_ethnicity) --
#copy/paste errors from the source workbook. These are not silently fixed;
#they're flagged in Part 7 below so they get reviewed rather than guessed at.

#hs_diploma / hs_grad_year repeat across a student's rows and are consistent
#for ~99% of students; mode_value() resolves the rare conflicting cases.
student_level<-psd %>%
  group_by(psd_id) %>%
  summarize(
    hs_diploma = mode_value(hs_diploma_std),
    hs_grad_year = as.numeric(mode_value(as.character(hs_grad_year))),
    gender = mode_value(gender),
    race_ethnicity = mode_value(race_ethnicity),
    poverty_indicator = mode_value(poverty_indicator),
    .groups = "drop"
  )

cohort<-student_level %>%
  filter(hs_diploma == "REGULAR DIPLOMA", !is.na(hs_grad_year))

cohort_n<-cohort %>%
  count(hs_grad_year, name = "N") %>%
  arrange(hs_grad_year)

cat("\nCohort N by HS graduating class (Regular Diploma only):\n")
print(cohort_n)

#hs_grad_year has already been captured (cleaned/resolved) in student_level
#above; drop the raw row-level copy now so later inner_join(cohort, ...)
#calls don't produce a colliding hs_grad_year.x/.y column.
psd<-psd %>% select(-hs_grad_year)

## -----------------------------------------------------------------------------
## Part 2 - Outcome 8.1: Postsecondary Plans
## -----------------------------------------------------------------------------
#record_term == "plans" rows capture each student's stated postsecondary plan
#(from the school's college application tracker). Category taken from
#cc_4year. There is no distinct "vocational school" category in this PSD
#extract, so that column is reported as 0 until it's captured upstream.
#"NO ENROLLMENT" plans records are counted as a known plan ("Other / No
#College Plan"), per team direction.

plans_student<-psd %>%
  filter(record_term_std == "plans") %>%
  mutate(
    plan_category = case_when(
      cc_4year_std == "4-YEAR" ~ "FOUR_YEAR",
      cc_4year_std == "FOR-PROFIT" ~ "FOUR_YEAR", #only 1 row in this file; folded into 4-yr, flag if volume grows
      cc_4year_std == "2-YEAR" ~ "TWO_YEAR",
      cc_4year_std == NO_ENROLL ~ "OTHER_NO_PLAN",
      TRUE ~ "MISSING"
    )
  ) %>%
  group_by(psd_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(psd_id, plan_category) %>%
  inner_join(cohort, by = "psd_id")

postsecondary_plans<-plans_student %>%
  group_by(hs_grad_year) %>%
  summarize(
    four_year_college = sum(plan_category == "FOUR_YEAR"),
    two_year_college = sum(plan_category == "TWO_YEAR"),
    vocational_school = 0L, #not currently captured in this PSD extract
    other_postsecondary_pathway = sum(plan_category == "OTHER_NO_PLAN"),
    .groups = "drop"
  ) %>%
  right_join(cohort_n, by = "hs_grad_year") %>%
  replace_na(list(four_year_college = 0L, two_year_college = 0L,
                   vocational_school = 0L, other_postsecondary_pathway = 0L)) %>%
  mutate(
    total_with_known_plan = four_year_college + two_year_college +
      vocational_school + other_postsecondary_pathway,
    pct_four_year = round(four_year_college / N, 4),
    pct_two_year = round(two_year_college / N, 4),
    pct_other = round(other_postsecondary_pathway / N, 4),
    postsecondary_plans_rate = round(total_with_known_plan / N, 4)
  ) %>%
  arrange(hs_grad_year) %>%
  relocate(hs_grad_year, N)

cat("\n--- 8.1 Postsecondary Plans ---\n")
print(postsecondary_plans)
cat("\nNOTE: Per Sec 9.2 (Known Gaps), postsecondary-plans data from the",
    "school's college application tracker has not yet been fully merged",
    "into the unified PSD process, so counts above may run lower than",
    "tracker-based figures reported elsewhere.\n")

## -----------------------------------------------------------------------------
## Part 3 - Outcome 8.2: Immediate College Enrollment
## -----------------------------------------------------------------------------
#Count Regular-Diploma students with a real college enrollment whose
#enrollment_begin date falls within Sept 15-Dec 24 of the graduation year
#(Sec 8.2, applied via literal dates per team direction). A term-based
#comparison (record_term=="fall" & record_year==grad_year, the approach
#Persistence uses in Sec 8.3) is included alongside it as a coverage check,
#plus a combined/union measure that counts a student if EITHER method alone
#would catch them (i.e. not double-counting students both methods agree on,
#but not requiring agreement either) -- see printed note below.

enroll_dates<-psd %>%
  filter(is_real_enrollment(college_name_std), !is.na(enrollment_begin_p)) %>%
  inner_join(cohort, by = "psd_id") %>%
  mutate(
    window_start = as_date(str_c(hs_grad_year, "-09-15")),
    window_end = as_date(str_c(hs_grad_year, "-12-24")),
    in_window = enrollment_begin_p >= window_start & enrollment_begin_p <= window_end
  )

enroll_terms<-psd %>%
  filter(is_real_enrollment(college_name_std), record_term_std == "fall") %>%
  inner_join(cohort, by = "psd_id") %>%
  filter(record_year == hs_grad_year)

immediate_enrollment<-cohort_n %>%
  rowwise() %>%
  mutate(
    date_ids = list(unique(enroll_dates$psd_id[
      enroll_dates$hs_grad_year == hs_grad_year & enroll_dates$in_window])),
    term_ids = list(unique(enroll_terms$psd_id[
      enroll_terms$hs_grad_year == hs_grad_year]))
  ) %>%
  ungroup() %>%
  mutate(
    immediate_enrollment_count = map_int(date_ids, length),
    term_based_comparison_count = map_int(term_ids, length),
    combined_count = map2_int(date_ids, term_ids, ~length(union(.x, .y)))
  ) %>%
  select(-date_ids, -term_ids) %>%
  mutate(
    immediate_enrollment_rate = round(immediate_enrollment_count / N, 4),
    term_based_comparison_rate = round(term_based_comparison_count / N, 4),
    combined_rate = round(combined_count / N, 4)
  ) %>%
  arrange(hs_grad_year)

cat("\n--- 8.2 Immediate College Enrollment ---\n")
print(immediate_enrollment)
cat("\nNOTE: 'immediate_enrollment_*' uses literal enrollment_begin dates",
    "(team direction). Roughly half of all fall-term rows in this file",
    "(staff/old_psd/missing-data sourced) have no literal enrollment_begin",
    "date -- only NSC-sourced records reliably carry one -- so this",
    "under-counts relative to historical tracker-based figures.",
    "'term_based_comparison_*' (fall/grad-year term match, as Sec 8.3 uses",
    "for Persistence) is included purely as a coverage check.",
    "'combined_*' is the UNION of the two -- a student counts if EITHER the",
    "literal-date window OR the fall-term match catches them -- so it's the",
    "closest single measure to historical tracker-based figures without",
    "requiring both sources to agree. Swap which column feeds the final",
    "report as date coverage improves.\n")

## -----------------------------------------------------------------------------
## Part 4 - Outcome 8.3: 1st to 2nd Year Persistence
## -----------------------------------------------------------------------------
#Per Sec 8.3, matching is done on record_term/record_year categories (not
#literal dates) so staff- and NSC-sourced records compare consistently.
#Denominator = students enrolled anywhere in Year 1 (not Cohort N).

enroll_term_rows<-psd %>%
  filter(is_real_enrollment(college_name_std), !is.na(record_term_std), !is.na(record_year)) %>%
  inner_join(cohort, by = "psd_id")

persistence<-cohort_n %>%
  rowwise() %>%
  mutate(
    year1_ids = list(unique(enroll_term_rows$psd_id[
      enroll_term_rows$hs_grad_year == hs_grad_year &
        in_academic_year(enroll_term_rows$record_term_std, enroll_term_rows$record_year, hs_grad_year, 1)])),
    year2_ids = list(unique(enroll_term_rows$psd_id[
      enroll_term_rows$hs_grad_year == hs_grad_year &
        in_academic_year(enroll_term_rows$record_term_std, enroll_term_rows$record_year, hs_grad_year, 2)]))
  ) %>%
  ungroup() %>%
  mutate(
    enrolled_year1 = map_int(year1_ids, length),
    enrolled_year2 = map_int(year2_ids, length),
    persisted_year1_to_year2 = map2_int(year1_ids, year2_ids, ~length(intersect(.x, .y))),
    persistence_rate = round(if_else(enrolled_year1 == 0, NA_real_,
                                      persisted_year1_to_year2 / enrolled_year1), 4)
  ) %>%
  select(-year1_ids, -year2_ids) %>%
  arrange(hs_grad_year)

cat("\n--- 8.3 1st to 2nd Year Persistence ---\n")
print(persistence)
cat("\nNOTE: Denominator is students enrolled anywhere in Year 1 (not Cohort",
    "N), per Sec 8.3. The most recent 1-2 cohorts will show artificially low",
    "persistence simply because Year 2 hasn't happened yet as of this PSD",
    "snapshot -- check record_year coverage before reporting recent cohorts.\n")

## -----------------------------------------------------------------------------
## Part 5 - Outcome 8.4: College Completion (4/5/6/7/8-year)
## -----------------------------------------------------------------------------
#Filter he_graduated == "Y"; assign each student's EARLIEST completion event
#to a 4-8 year milestone bucket using fixed Aug 15 cutoffs (see
#completion_milestone() above). 6-year completion (officially reported) =
#cumulative 4+5+6-year counts / Cohort N, verified against the guide's own
#Class of 2017 example (14+12+2 = 28, matching its separately reported
#"6-year Completion Count"). 8-year is included since it's "under
#consideration" per Sec 8.5/9.1.

grad_student<-psd %>%
  filter(he_graduated_std == "Y") %>%
  mutate(completion_date_est = coalesce(
    coll_grad_date_p, term_approx_date(record_term_std, record_year))) %>%
  inner_join(cohort, by = "psd_id") %>%
  group_by(psd_id) %>%
  slice_min(completion_date_est, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(milestone = map2_int(completion_date_est, hs_grad_year,
                               ~completion_milestone(.x, .y) %||% NA_integer_))

completion<-grad_student %>%
  group_by(hs_grad_year) %>%
  summarize(
    grad_4yr = sum(milestone == 4, na.rm = TRUE),
    grad_5yr = sum(milestone == 5, na.rm = TRUE),
    grad_6yr = sum(milestone == 6, na.rm = TRUE),
    grad_7yr = sum(milestone == 7, na.rm = TRUE),
    grad_8yr = sum(milestone == 8, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(cohort_n, by = "hs_grad_year") %>%
  replace_na(list(grad_4yr = 0L, grad_5yr = 0L, grad_6yr = 0L, grad_7yr = 0L, grad_8yr = 0L)) %>%
  mutate(
    completion_6yr_count = grad_4yr + grad_5yr + grad_6yr,
    completion_6yr_rate = round(completion_6yr_count / N, 4),
    completion_8yr_count = completion_6yr_count + grad_7yr + grad_8yr,
    completion_8yr_rate = round(completion_8yr_count / N, 4)
  ) %>%
  arrange(hs_grad_year) %>%
  relocate(hs_grad_year, N)

cat("\n--- 8.4 College Completion ---\n")
print(completion)
cat("\nNOTE: Recent cohorts (< 6 years post-graduation as of this snapshot)",
    "will show artificially low completion since their full completion",
    "window hasn't elapsed yet. Where coll_grad_date is missing, the",
    "completion year is approximated from record_term/record_year (Sec 8.4",
    "Unique Cases) -- an approximation the guide itself acknowledges, so",
    "counts near a 4/5-year boundary may shift by a student or two versus a",
    "manually-reviewed figure.\n")

## -----------------------------------------------------------------------------
## Part 6 - Combined outcome snapshot (one row per cohort, all 4 outcomes)
## -----------------------------------------------------------------------------

outcome_snapshot<-cohort_n %>%
  left_join(postsecondary_plans %>% select(hs_grad_year, postsecondary_plans_rate), by = "hs_grad_year") %>%
  left_join(immediate_enrollment %>% select(hs_grad_year, immediate_enrollment_rate, term_based_comparison_rate), by = "hs_grad_year") %>%
  left_join(persistence %>% select(hs_grad_year, persistence_rate), by = "hs_grad_year") %>%
  left_join(completion %>% select(hs_grad_year, completion_6yr_rate), by = "hs_grad_year") %>%
  arrange(hs_grad_year) %>%
  rename(
    Graduate_Class = hs_grad_year,
    Postsecondary_Plans = postsecondary_plans_rate,
    Immediate_Enrollment_DateBased = immediate_enrollment_rate,
    Immediate_Enrollment_TermBased = term_based_comparison_rate,
    Persistence_1st_to_2nd_Year = persistence_rate,
    College_Completion_6yr = completion_6yr_rate
  )

cat("\n=== COMBINED OUTCOME SNAPSHOT ===\n")
print(outcome_snapshot)

## -----------------------------------------------------------------------------
## Part 7 - Data quality flags
## -----------------------------------------------------------------------------
#Surfaces rows where a field holds a value outside its documented category
#set -- e.g. an email address in cc_4year, an unrecognized token in
#race_ethnicity -- for manual review rather than silent dropping/guessing.

valid_cc_4year<-c("4-YEAR", "2-YEAR", "MISSING DATA", "NO ENROLLMENT", "FOR-PROFIT", NA)
valid_race<-c("PI","HI","AS","AM","BL","MU","WH","AI","FI","WI","TWO","AA", NA)

dq_report<-bind_rows(
  psd %>% filter(!cc_4year_std %in% valid_cc_4year) %>%
    transmute(psd_id, field = "cc_4year", value = cc_4year),
  psd %>% filter(!str_to_upper(clean_sentinel(race_ethnicity)) %in% valid_race) %>%
    transmute(psd_id, field = "race_ethnicity", value = race_ethnicity)
) %>%
  count(psd_id, field, value, name = "n_rows_affected") %>%
  mutate(row_note = "unrecognized category value")

if (nrow(dq_report) > 0) {
  cat("\n=== DATA QUALITY FLAGS (review before finalizing reports) ===\n")
  print(dq_report)
}

## -----------------------------------------------------------------------------
## Part 8 - Save and Export Files
## -----------------------------------------------------------------------------

# fwrite(outcome_snapshot, file.path(output_dir, "outcome_snapshot_summary.csv"))
# fwrite(postsecondary_plans, file.path(output_dir, "postsecondary_plans_detail.csv"))
# fwrite(immediate_enrollment, file.path(output_dir, "immediate_enrollment_detail.csv"))
# fwrite(persistence, file.path(output_dir, "persistence_detail.csv"))
# fwrite(completion, file.path(output_dir, "completion_detail.csv"))
# fwrite(dq_report, file.path(output_dir, "data_quality_flags.csv"))
# 
# cat("\nAll outputs written to: ", output_dir, "\n", sep = "")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
