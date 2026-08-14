################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 02-sankey-pathways.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 7/29/26 >
##
################################################################################

#Goal: Build two interactive Sankey diagrams of student pathways from HS
#graduation to college outcome, in the style of the CollegeGoingOutcomes.com
#"Student Pathways" chart:
#   (1) GENERAL   : HS Grads -> {Never Enrolled / 2-Year / 4-Year} ->
#                    {Attended-Not-Complete / Completed 2-Yr / Completed 4-Yr}
#   (2) DETAILED  : same, but the 4-Year entry node is split into
#                    UC / CSU / Out-of-State / Other 4-Year
#
#This is a standalone companion script to 01-calculate-outcomes.R (does NOT
#source or modify it) -- it re-derives what it needs directly from the PSD
#snapshot, following the same template conventions.
#
#IMPORTANT - THIS SCRIPT IS DIAGNOSTIC-FIRST: Part 1 prints your actual PSD
#column names and sample values so you can confirm/adjust the CONFIG blocks
#in Part 3 (UC/CSU/Out-of-State patterns) and Part 4 (degree-type column)
#before trusting the final diagrams. Unmatched college names are written to
#a review CSV rather than silently guessed at.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(data.table)
library(plotly)
library(htmlwidgets)

#NOTE: plotly's `type = "sankey"` trace is used for both diagrams. It's a
#single self-contained htmlwidget (no extra JS wiring needed) and natively
#supports the interactive behavior requested: hovering a NODE highlights all
#flows connected to it, hovering a LINK highlights just that flow and shows
#its tooltip. `htmlwidgets::saveWidget()` exports each as a standalone .html
#that opens in any browser.

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

#Same PSD snapshot as 01-calculate-outcomes.R -- update to match whichever
#dated file you're reporting on.

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

output_dir<-"/mnt/user-data/outputs"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

## ---------------------------
## CONFIG - review/edit after checking Part 1 diagnostics
## ---------------------------

#Only include students who earned a Regular Diploma, matching the cohort
#definition used for the four outcome measures in 01-calculate-outcomes.R.
#Set to FALSE to include all diploma types in the Sankey.
RESTRICT_TO_REGULAR_DIPLOMA<-TRUE

#--- UC / CSU pattern matching on college_name (case-insensitive) ---
#Edit these regexes if Part 1's "unmatched 4-year college names" list shows
#campuses these patterns miss.
uc_pattern<-regex(
  paste0(
    "UNIVERSITY OF CALIFORNIA",
    "|\\bUCLA\\b|\\bUC[\\s\\-]?BERKELEY\\b|\\bUC[\\s\\-]?SAN DIEGO\\b",
    "|\\bUC[\\s\\-]?DAVIS\\b|\\bUC[\\s\\-]?IRVINE\\b|\\bUC[\\s\\-]?SANTA BARBARA\\b",
    "|\\bUC[\\s\\-]?SANTA CRUZ\\b|\\bUC[\\s\\-]?RIVERSIDE\\b|\\bUC[\\s\\-]?MERCED\\b",
    "|\\bUCB\\b|\\bUCSD\\b|\\bUCI\\b|\\bUCD\\b|\\bUCSB\\b|\\bUCSC\\b|\\bUCR\\b"
  ),
  ignore_case = TRUE
)

csu_pattern<-regex(
  paste0(
    "CALIFORNIA STATE UNIVERSITY|CAL STATE|CALIFORNIA POLYTECHNIC",
    "|CAL POLY|\\bCSU[\\s\\-]|POLYTECHNIC STATE UNIVERSITY"
  ),
  ignore_case = TRUE
)

#Candidate column names that might hold a pre-coded institution system/sector
#(e.g. "UC","CSU","CCC","OUT_4YR","INP_NP" as in NSC-derived PSD extracts).
#When present, THIS is the primary source for the UC/CSU/Out-of-State split
#-- it's authoritative (comes from NSC's own school code table) rather than
#inferred from free-text college_name. Name-pattern matching below is only a
#fallback for rows where this value is missing/blank.
system_type_col_candidates<-c("system_type")

#Candidate column names that might hold a college's state (checked in Part 1;
#used as a secondary fallback -- after system_type -- to detect Out-of-State).
state_col_candidates<-c("college_state", "state", "institution_state",
                        "school_state", "coll_state")

#Candidate column names that might hold degree/credential type -- if one of
#these exists in your PSD, Part 4 will use it to split completions into
#Associate / Certificate / Bachelor's. If none exist, completions fall back
#to a 2-category split (2-Year Credential vs. Bachelor's Degree).
degree_col_candidates<-c("degree_type", "credential_type", "degree_title",
                         "award_level", "credential_level", "degree_level")

## ---------------------------
## helper functions (self-contained -- not sourced from 01-calculate-outcomes.R)
## ---------------------------

NA_LIKE<-c("MISSING DATA", "NA", "N/A", "")
NO_ENROLL<-"NO ENROLLMENT"

clean_sentinel<-function(x){
  x<-str_squish(as.character(x))
  x[x %in% NA_LIKE]<-NA
  x
}

parse_psd_date<-function(x){
  x<-str_squish(as.character(x))
  x[x %in% NA_LIKE]<-NA
  x[str_to_upper(x) == NO_ENROLL]<-NA
  as_date(parse_date_time(x, orders = c("mdY HM", "mdY", "Ymd", "ymd")))
}

mode_value<-function(x){
  x<-x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

is_real_enrollment<-function(college_name){
  cn<-clean_sentinel(college_name)
  !is.na(cn) & str_to_upper(cn) != NO_ENROLL
}

#Ordering used to find each student's EARLIEST enrollment record when
#literal enrollment_begin dates are missing (common per 01's coverage note).
term_rank<-c(fall = 1, winter = 2, spring = 3, summer = 4,
             `enrolled anytime after fall` = 5)

## -----------------------------------------------------------------------------
## Part 1 - Load PSD & run diagnostics (READ THIS OUTPUT BEFORE TRUSTING RESULTS)
## -----------------------------------------------------------------------------

psd<-fread(psd_demo)

cat("\n=== PSD COLUMN NAMES ===\n")
print(colnames(psd))

detected_system_type_col<-intersect(system_type_col_candidates, colnames(psd))
detected_state_col<-intersect(state_col_candidates, colnames(psd))
detected_degree_col<-intersect(degree_col_candidates, colnames(psd))

cat("\n=== SYSTEM TYPE COLUMN DETECTION (primary UC/CSU/OOS source) ===\n")
if (length(detected_system_type_col) > 0) {
  cat("Found:", detected_system_type_col[1],
      "-- will use this as the AUTHORITATIVE UC/CSU/Out-of-State classifier.\n")
  cat("Unique values:\n")
  print(psd %>% count(.data[[detected_system_type_col[1]]]) %>% arrange(desc(n)))
} else {
  cat("No system_type-style column found among:", paste(system_type_col_candidates, collapse = ", "),
      "-- falling back to college_name regex + state column (less reliable).\n")
}

cat("\n=== STATE COLUMN DETECTION (fallback / cross-check) ===\n")
if (length(detected_state_col) > 0) {
  cat("Found candidate state column(s):", paste(detected_state_col, collapse = ", "),
      "-- will use the first match for Out-of-State detection.\n")
} else {
  cat("No state column found among:", paste(state_col_candidates, collapse = ", "),
      "\nOut-of-State will be inferred ONLY when a 4-year college_name clearly",
      "fails to match UC/CSU/CA patterns AND doesn't look like a CA private",
      "school -- everything else lands in 'Other/Unclassified 4-Year' for your",
      "manual review (see data_quality flags CSV). Add a state column pattern",
      "above, or supply a lookup table, for a cleaner split.\n")
}

cat("\n=== DEGREE-TYPE COLUMN DETECTION ===\n")
if (length(detected_degree_col) > 0) {
  cat("Found candidate degree-type column(s):", paste(detected_degree_col, collapse = ", "),
      "-- will use the first match to split completions into",
      "Associate / Certificate / Bachelor's.\n")
  cat("Unique values:\n")
  print(psd %>% count(.data[[detected_degree_col[1]]]))
} else {
  cat("No degree-type column found among:", paste(degree_col_candidates, collapse = ", "),
      "\nCompletions will fall back to a 2-category split (2-Year Credential vs.",
      "Bachelor's Degree) based on cc_4year of the graduation record. Add the",
      "real column name above if your PSD has one, to unlock the full 4-category",
      "Associate/Certificate/Bachelor's split like the reference chart.\n")
}

cat("\n=== cc_4year VALUES ===\n")
print(psd %>% count(cc_4year))

cat("\n=== he_graduated VALUES ===\n")
print(psd %>% count(he_graduated))

## -----------------------------------------------------------------------------
## Part 2 - Clean PSD & build the cohort (same definition as 01-calculate-outcomes.R)
## -----------------------------------------------------------------------------

psd<-psd %>%
  mutate(
    hs_diploma_std = str_to_upper(clean_sentinel(hs_diploma)),
    college_name_std = clean_sentinel(college_name),
    cc_4year_std = str_to_upper(clean_sentinel(cc_4year)),
    record_term_std = str_to_lower(clean_sentinel(record_term)),
    he_graduated_std = str_to_upper(clean_sentinel(he_graduated)),
    enrollment_begin_p = parse_psd_date(enrollment_begin),
    coll_grad_date_p = parse_psd_date(coll_grad_date),
    term_rank_v = unname(term_rank[record_term_std])
  )

#Standardized system_type / state columns, built from whichever candidate
#column Part 1 detected (kept as plain assignment rather than inside the
#mutate() above so this works whether or not those columns exist in your PSD).
psd$system_type_std<-if (length(detected_system_type_col) > 0) {
  str_to_upper(clean_sentinel(psd[[detected_system_type_col[1]]]))
} else {
  NA_character_
}

psd$state_std<-if (length(detected_state_col) > 0) {
  str_to_upper(clean_sentinel(psd[[detected_state_col[1]]]))
} else {
  NA_character_
}

student_level<-psd %>%
  group_by(psd_id) %>%
  summarize(
    hs_diploma = mode_value(hs_diploma_std),
    hs_grad_year = as.numeric(mode_value(as.character(hs_grad_year))),
    .groups = "drop"
  )

cohort<-if (RESTRICT_TO_REGULAR_DIPLOMA) {
  student_level %>% filter(hs_diploma == "REGULAR DIPLOMA", !is.na(hs_grad_year))
} else {
  student_level %>% filter(!is.na(hs_grad_year))
}

cohort_n<-nrow(cohort)
cat("\nCohort N for Sankey (", if (RESTRICT_TO_REGULAR_DIPLOMA) "Regular Diploma only" else "all diploma types",
    "):", cohort_n, "\n")

#Per-graduating-class N, used to self-normalize each cohort-specific Sankey
#(Part 8B) so its node percentages are relative to that class, not the whole
#sample.
cohort_n_by_year<-cohort %>% count(hs_grad_year, name = "N") %>% arrange(hs_grad_year)
cat("\nCohort N by graduating class:\n")
print(cohort_n_by_year)

psd<-psd %>% select(-hs_grad_year) #avoid .x/.y collisions on join below

## -----------------------------------------------------------------------------
## Part 3 - First enrollment: entry sector (2-Year / 4-Year) + UC/CSU/OOS detail
## -----------------------------------------------------------------------------

enroll_rows<-psd %>%
  filter(is_real_enrollment(college_name_std)) %>%
  inner_join(cohort, by = "psd_id")

first_enroll<-enroll_rows %>%
  arrange(psd_id, record_year, term_rank_v, enrollment_begin_p) %>%
  group_by(psd_id) %>%
  slice(1) %>%
  ungroup()

#PRIMARY: system_type is an authoritative NSC-derived code (UC / CSU / CCC /
#OUT_4YR / OUT_4YRP / OUT_FP / OUT_CC / INP_NP / INP_FP) -- use it directly
#when present. FALLBACK (only used when system_type is missing/blank for a
#given row): college_name regex, then college_state vs. "CA".
classify_four_year_detail<-function(system_type_val, college_name_std, state_val = NA_character_){
  case_when(
    system_type_val == "UC" ~ "UC",
    system_type_val == "CSU" ~ "CSU",
    system_type_val %in% c("OUT_4YR", "OUT_4YRP", "OUT_FP") ~ "Out-of-State",
    system_type_val %in% c("INP_NP", "INP_FP") ~ "Other 4-Year (In-State)",
    #--- fallback path below only fires when system_type_val is NA ---
    str_detect(college_name_std, uc_pattern) ~ "UC",
    str_detect(college_name_std, csu_pattern) ~ "CSU",
    !is.na(state_val) & state_val %in% c("CA", "CALIFORNIA") ~ "Other 4-Year (In-State)",
    !is.na(state_val) & !state_val %in% c("CA", "CALIFORNIA") ~ "Out-of-State",
    TRUE ~ "Other/Unclassified 4-Year" #no system_type or state info -- needs manual review
  )
}

#TRUE for system_type codes that represent a 2-year (community college)
#enrollment, used below only as a rescue path for the small number of rows
#where cc_4year itself is malformed (e.g. this file's one row with a stray
#email address in that column -- see Part 1 data-quality note) but
#system_type is still intact.
two_year_system_codes<-c("CCC", "OUT_CC")
four_year_system_codes<-c("UC", "CSU", "OUT_4YR", "OUT_4YRP", "OUT_FP", "INP_NP", "INP_FP")

first_enroll<-first_enroll %>%
  mutate(
    entry_sector = case_when(
      cc_4year_std == "2-YEAR" ~ "2-Year",
      cc_4year_std %in% c("4-YEAR", "FOR-PROFIT") ~ "4-Year",
      system_type_std %in% two_year_system_codes ~ "2-Year",  #rescues malformed cc_4year rows
      system_type_std %in% four_year_system_codes ~ "4-Year", #rescues malformed cc_4year rows
      TRUE ~ NA_character_
    ),
    four_year_detail = if_else(
      entry_sector == "4-Year",
      classify_four_year_detail(system_type_std, college_name_std, state_std),
      NA_character_
    )
  )

#Rows rescued by system_type despite a malformed cc_4year value -- surfaced
#for awareness, same spirit as the dq_report in 01-calculate-outcomes.R.
cc_4year_rescued<-first_enroll %>%
  filter(!cc_4year_std %in% c("2-YEAR", "4-YEAR", "FOR-PROFIT"), !is.na(entry_sector)) %>%
  select(psd_id, cc_4year, system_type_std, entry_sector, college_name_std)

if (nrow(cc_4year_rescued) > 0) {
  cat("\n=== ROWS WITH MALFORMED cc_4year, RESCUED VIA system_type (review) ===\n")
  print(cc_4year_rescued)
  fwrite(cc_4year_rescued, file.path(output_dir, "sankey_cc_4year_rescued_rows.csv"))
}

#Flag unmatched 4-year names for manual review rather than guessing.
four_year_unmatched<-first_enroll %>%
  filter(four_year_detail == "Other/Unclassified 4-Year") %>%
  count(college_name_std, system_type_std, name = "n_students") %>%
  arrange(desc(n_students))

if (nrow(four_year_unmatched) > 0) {
  cat("\n=== UNMATCHED 4-YEAR COLLEGE NAMES (review / refine uc_pattern, csu_pattern, or check system_type coverage) ===\n")
  print(head(four_year_unmatched, 30))
  fwrite(four_year_unmatched, file.path(output_dir, "sankey_unmatched_4yr_colleges.csv"))
}

## -----------------------------------------------------------------------------
## Part 4 - Completion outcome per student
## -----------------------------------------------------------------------------

grad_rows<-psd %>%
  filter(he_graduated_std == "Y") %>%
  inner_join(cohort, by = "psd_id")

if (length(detected_degree_col) > 0) {
  #HYBRID: prefer an actual degree_title value where one exists (~60% of
  #graduation rows in the test file have one); fall back to the cc_4year-
  #based 2-category approximation, per row, where degree_title is blank.
  #Picking, per student, the graduation row with a non-missing degree_title
  #(if any) avoids losing real detail to slice(1) picking a blank row first.
  degree_col<-detected_degree_col[1]
  grad_first<-grad_rows %>%
    mutate(degree_val_std = str_to_upper(clean_sentinel(.data[[degree_col]]))) %>%
    arrange(psd_id, is.na(degree_val_std)) %>% #non-missing degree_title rows sort first
    group_by(psd_id) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      credential = case_when(
        !is.na(degree_val_std) & str_detect(degree_val_std, "ASSOCIATE|^AA |^AS ") ~ "Completed Associate Degree",
        !is.na(degree_val_std) & str_detect(degree_val_std, "CERTIFICATE") ~ "Completed Certificate",
        !is.na(degree_val_std) & str_detect(degree_val_std, "BACHELOR|BACCALAUREATE|^BS\\b|^BA\\b") ~ "Completed Bachelor's Degree",
        !is.na(degree_val_std) & str_detect(degree_val_std, "MASTER|DOCTOR|\\bPHD\\b|\\bJD\\b|\\bMD\\b|LAW|CREDENTIAL") ~ "Completed Graduate Degree or Higher",
        #degree_title blank on this row -- approximate from cc_4year instead
        cc_4year_std == "2-YEAR" ~ "Completed 2-Year Credential (Type Unspecified)",
        cc_4year_std %in% c("4-YEAR", "FOR-PROFIT") ~ "Completed Bachelor's Degree (Approximated)",
        TRUE ~ "Completed Degree (Type Unspecified)"
      )
    ) %>%
    select(psd_id, credential)
} else {
  #Fallback: approximate credential level from the graduation record's
  #cc_4year value (2-category split -- see Part 1 diagnostic note).
  grad_first<-grad_rows %>%
    group_by(psd_id) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      credential = case_when(
        cc_4year_std == "2-YEAR" ~ "Completed 2-Year Credential",
        cc_4year_std %in% c("4-YEAR", "FOR-PROFIT") ~ "Completed Bachelor's Degree",
        TRUE ~ "Completed Degree (Type Unspecified)"
      )
    ) %>%
    select(psd_id, credential)
}

## -----------------------------------------------------------------------------
## Part 5 - Assemble one row per cohort student: entry sector -> outcome
## -----------------------------------------------------------------------------

student_flows<-cohort %>%
  select(psd_id, hs_grad_year) %>%
  left_join(first_enroll %>% select(psd_id, entry_sector, four_year_detail), by = "psd_id") %>%
  left_join(grad_first, by = "psd_id") %>%
  mutate(
    entry_sector = replace_na(entry_sector, "Never Enrolled"),
    outcome_category = case_when(
      !is.na(credential) ~ credential,
      entry_sector == "Never Enrolled" ~ NA_character_, #terminal node, no downstream flow
      TRUE ~ "Attended, Not Yet Completed"
    )
  )

cat("\n=== STUDENT FLOW SUMMARY (entry sector) ===\n")
print(student_flows %>% count(entry_sector))
cat("\n=== STUDENT FLOW SUMMARY (outcome) ===\n")
print(student_flows %>% count(outcome_category))

## -----------------------------------------------------------------------------
## Part 6 - Build link tables (as reusable functions -- called once for the
## whole sample, then again per graduating class in Part 8B)
## -----------------------------------------------------------------------------

#--- Diagram 1: GENERAL (Never Enrolled / 2-Year / 4-Year) ---
build_general_links<-function(flows_df){
  stage1<-flows_df %>%
    count(entry_sector, name = "value") %>%
    transmute(source_label = "HS Graduates", target_label = entry_sector, value)
  
  stage2<-flows_df %>%
    filter(!is.na(outcome_category)) %>%
    count(entry_sector, outcome_category, name = "value") %>%
    transmute(source_label = entry_sector, target_label = outcome_category, value)
  
  bind_rows(stage1, stage2)
}

#--- Diagram 2: DETAILED (2-Year / UC / CSU / Out-of-State / Other 4-Year) ---
build_detail_links<-function(flows_detail_df){
  stage1<-flows_detail_df %>%
    count(entry_detail, name = "value") %>%
    transmute(source_label = "HS Graduates", target_label = entry_detail, value)
  
  stage2<-flows_detail_df %>%
    filter(!is.na(outcome_category)) %>%
    count(entry_detail, outcome_category, name = "value") %>%
    transmute(source_label = entry_detail, target_label = outcome_category, value)
  
  bind_rows(stage1, stage2)
}

student_flows_detail<-student_flows %>%
  mutate(
    entry_detail = case_when(
      entry_sector == "4-Year" ~ four_year_detail,
      TRUE ~ entry_sector
    )
  )

#Whole-sample link tables (used by the "All Cohorts" diagrams in Part 8A).
links_general<-build_general_links(student_flows)
links_detail<-build_detail_links(student_flows_detail)

fwrite(links_general, file.path(output_dir, "sankey_general_links.csv"))
fwrite(links_detail, file.path(output_dir, "sankey_detail_links.csv"))

## -----------------------------------------------------------------------------
## Part 7 - Generic Sankey builders (plotly) w/ hover-highlight of connected flows
## -----------------------------------------------------------------------------
#Three layers, reused for the whole sample, each graduating class, and the
#combined cohort-switcher versions:
#   build_sankey_trace()    - node/link data for ONE diagram (no plot wrapper)
#   build_sankey_plot()     - wraps one trace into a standalone plot
#   build_sankey_dropdown() - combines several traces + a dropdown to switch
#                             between them (e.g. "All Cohorts" vs. each class)

build_sankey_trace<-function(links_df, node_order, node_colors, total_n){
  
  #Node display labels include % of THIS diagram's own total (whole sample,
  #or a single cohort -- whichever total_n was passed in), matching the
  #reference chart's "label: NN%" style.
  node_totals<-links_df %>%
    group_by(node = target_label) %>%
    summarize(total = sum(value), .groups = "drop")
  #HS Graduates node total = full total_n (it only ever appears as a source).
  node_totals<-bind_rows(node_totals, tibble(node = "HS Graduates", total = total_n))
  
  node_labels_display<-map_chr(node_order, function(lbl){
    tot<-node_totals$total[node_totals$node == lbl]
    tot<-if (length(tot) == 0) NA_real_ else tot[1]
    if (is.na(tot)) lbl else sprintf("%s: %.0f%% (n=%d)", lbl, 100 * tot / total_n, tot)
  })
  
  node_index<-setNames(seq_along(node_order) - 1, node_order)
  
  link_source<-unname(node_index[links_df$source_label])
  link_target<-unname(node_index[links_df$target_label])
  link_base_color<-node_colors[link_source + 1]
  link_color_alpha<-vapply(link_base_color, function(cl) grDevices::adjustcolor(cl, alpha.f = 0.45), character(1))
  
  #ASCII arrow, not a literal Unicode arrow character -- under a non-UTF-8
  #locale (the OS/R install's locale, not anything Sankey-specific), literal
  #multi-byte Unicode characters embedded in R source can get corrupted on
  #output (shows up as garbled bytes like "<e2><86><92>" instead of an
  #arrow). ASCII is guaranteed correct everywhere regardless of locale.
  link_labels<-sprintf("%s -> %s<br>%d students (%.1f%% of this cohort)",
                       links_df$source_label, links_df$target_label,
                       links_df$value, 100 * links_df$value / total_n)
  
  list(
    node = list(
      label = node_labels_display,
      color = node_colors,
      pad = 20,
      thickness = 22,
      line = list(color = "black", width = 0.5),
      hoverlabel = list(bgcolor = "white")
    ),
    link = list(
      source = link_source,
      target = link_target,
      value = links_df$value,
      color = link_color_alpha,
      label = link_labels,
      hoverlabel = list(bgcolor = "white")
    )
  )
}

build_sankey_plot<-function(links_df, node_order, node_colors, title, total_n){
  trace<-build_sankey_trace(links_df, node_order, node_colors, total_n)
  plot_ly(
    type = "sankey", orientation = "h", arrangement = "snap", valueformat = ",",
    node = trace$node, link = trace$link
  ) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      font = list(size = 12),
      margin = list(t = 60, b = 20)
    )
}

#scenarios: a list of list(links_df=, node_order=, node_colors=, total_n=, label=)
#-- one element per dropdown option (e.g. "All Cohorts (Combined)", "Class of
#2019", "Class of 2020", ...). Only the first scenario's trace is visible at
#load; the dropdown swaps visibility + title via a plotly "update" button.
build_sankey_dropdown<-function(scenarios, base_title){
  p<-plot_ly()
  for (i in seq_along(scenarios)) {
    sc<-scenarios[[i]]
    trace<-build_sankey_trace(sc$links_df, sc$node_order, sc$node_colors, sc$total_n)
    p<-p %>% add_trace(
      type = "sankey", orientation = "h", arrangement = "snap", valueformat = ",",
      node = trace$node, link = trace$link, visible = (i == 1)
    )
  }
  
  buttons<-lapply(seq_along(scenarios), function(i){
    vis<-rep(FALSE, length(scenarios))
    vis[i]<-TRUE
    list(
      method = "update",
      args = list(
        list(visible = as.list(vis)),
        list(title = list(text = paste0(base_title, " - ", scenarios[[i]]$label)))
      ),
      label = scenarios[[i]]$label
    )
  })
  
  p %>% layout(
    title = list(text = paste0(base_title, " - ", scenarios[[1]]$label), font = list(size = 16)),
    font = list(size = 12),
    margin = list(t = 90, b = 20),
    updatemenus = list(list(
      type = "dropdown", active = 0, x = 0, y = 1.18, xanchor = "left", yanchor = "top",
      buttons = buttons
    ))
  )
}

#Node display order + colors -- pulled out as functions (rather than one-off
#code) so the exact same category set/order logic applies whether building
#the whole-sample diagram or a single graduating class, and automatically
#drops any node that has zero students in a given cohort.
general_node_order<-function(links_df){
  base_order<-c("HS Graduates", "Never Enrolled", "2-Year", "4-Year", "Attended, Not Yet Completed")
  present<-unique(c(links_df$source_label, links_df$target_label))
  order<-c(base_order, setdiff(present, base_order))
  order[order %in% present]
}

general_node_colors<-function(node_order){
  case_when(
    node_order == "HS Graduates" ~ "#4A5568",
    node_order == "Never Enrolled" ~ "#A0AEC0",
    node_order == "2-Year" ~ "#ED8936",
    node_order == "4-Year" ~ "#4299E1",
    node_order == "Attended, Not Yet Completed" ~ "#CBD5E0",
    TRUE ~ "#38A169" #completion categories
  )
}

detail_node_order<-function(links_df){
  base_order<-c("HS Graduates", "Never Enrolled", "2-Year",
                "UC", "CSU", "Out-of-State", "Other 4-Year (In-State)",
                "Other/Unclassified 4-Year", "Attended, Not Yet Completed")
  present<-unique(c(links_df$source_label, links_df$target_label))
  order<-c(base_order, setdiff(present, base_order))
  order[order %in% present]
}

detail_node_colors<-function(node_order){
  case_when(
    node_order == "HS Graduates" ~ "#4A5568",
    node_order == "Never Enrolled" ~ "#A0AEC0",
    node_order == "2-Year" ~ "#ED8936",
    node_order == "UC" ~ "#2B6CB0",
    node_order == "CSU" ~ "#4299E1",
    node_order == "Out-of-State" ~ "#805AD5",
    node_order == "Other 4-Year (In-State)" ~ "#63B3ED",
    node_order == "Other/Unclassified 4-Year" ~ "#B794F4",
    node_order == "Attended, Not Yet Completed" ~ "#CBD5E0",
    TRUE ~ "#38A169" #completion categories
  )
}

## -----------------------------------------------------------------------------
## Part 8A - Whole-sample diagrams (all graduating classes combined)
## -----------------------------------------------------------------------------

node_order_general<-general_node_order(links_general)
node_colors_general<-general_node_colors(node_order_general)

sankey_general<-build_sankey_plot(
  links_df = links_general,
  node_order = node_order_general,
  node_colors = node_colors_general,
  title = "Student Pathways: High School Graduation to College Outcome (All Cohorts)",
  total_n = cohort_n
)

node_order_detail<-detail_node_order(links_detail)
node_colors_detail<-detail_node_colors(node_order_detail)

sankey_detail<-build_sankey_plot(
  links_df = links_detail,
  node_order = node_order_detail,
  node_colors = node_colors_detail,
  title = "Student Pathways (Detail): UC / CSU / Out-of-State / 2-Year (All Cohorts)",
  total_n = cohort_n
)

## -----------------------------------------------------------------------------
## Part 8B - Per-graduating-class diagrams, organized as named lists
## -----------------------------------------------------------------------------
#Percentages on each cohort's chart are relative to THAT cohort's own N (per
#team direction), so e.g. "UC: 20%" always means 20% of that specific
#graduating class -- not 20% of the whole multi-year sample.
#
#Everything is kept in two named lists, keyed by graduating class year, so
#a given cohort's diagram/links are easy to grab later, e.g.:
#   sankey_general_list[["2019"]]        <- the plotly widget for Class of 2019
#   links_general_by_year[["2019"]]      <- its underlying link table

cohort_years<-cohort_n_by_year$hs_grad_year

sankey_general_list<-list()
sankey_detail_list<-list()
links_general_by_year<-list()
links_detail_by_year<-list()

for (yr in cohort_years) {
  
  yr_key<-as.character(yr)
  yr_n<-cohort_n_by_year$N[cohort_n_by_year$hs_grad_year == yr]
  
  flows_yr<-student_flows %>% filter(hs_grad_year == yr)
  flows_detail_yr<-student_flows_detail %>% filter(hs_grad_year == yr)
  
  links_gen_yr<-build_general_links(flows_yr)
  links_det_yr<-build_detail_links(flows_detail_yr)
  
  links_general_by_year[[yr_key]]<-links_gen_yr
  links_detail_by_year[[yr_key]]<-links_det_yr
  
  node_order_gen_yr<-general_node_order(links_gen_yr)
  sankey_general_list[[yr_key]]<-build_sankey_plot(
    links_df = links_gen_yr,
    node_order = node_order_gen_yr,
    node_colors = general_node_colors(node_order_gen_yr),
    title = sprintf("Student Pathways: Class of %s", yr_key),
    total_n = yr_n
  )
  
  node_order_det_yr<-detail_node_order(links_det_yr)
  sankey_detail_list[[yr_key]]<-build_sankey_plot(
    links_df = links_det_yr,
    node_order = node_order_det_yr,
    node_colors = detail_node_colors(node_order_det_yr),
    title = sprintf("Student Pathways (Detail): Class of %s", yr_key),
    total_n = yr_n
  )
}

cat("\nBuilt", length(sankey_general_list), "per-cohort GENERAL Sankeys and",
    length(sankey_detail_list), "per-cohort DETAIL Sankeys.\n",
    "Access them via sankey_general_list[[\"<year>\"]] /",
    "sankey_detail_list[[\"<year>\"]], e.g. sankey_general_list[[\"",
    as.character(cohort_years[1]), "\"]]\n", sep = "")

## -----------------------------------------------------------------------------
## Part 8C - Combined cohort-switcher diagrams (All Cohorts + each class,
## one file each with a dropdown selector)
## -----------------------------------------------------------------------------

general_scenarios<-c(
  list(list(links_df = links_general, node_order = node_order_general,
            node_colors = node_colors_general, total_n = cohort_n,
            label = "All Cohorts (Combined)")),
  lapply(cohort_years, function(yr){
    yr_key<-as.character(yr)
    ord<-general_node_order(links_general_by_year[[yr_key]])
    list(
      links_df = links_general_by_year[[yr_key]],
      node_order = ord,
      node_colors = general_node_colors(ord),
      total_n = cohort_n_by_year$N[cohort_n_by_year$hs_grad_year == yr],
      label = paste("Class of", yr_key)
    )
  })
)

sankey_general_dropdown<-build_sankey_dropdown(
  general_scenarios, "Student Pathways: HS Graduation to College Outcome"
)

detail_scenarios<-c(
  list(list(links_df = links_detail, node_order = node_order_detail,
            node_colors = node_colors_detail, total_n = cohort_n,
            label = "All Cohorts (Combined)")),
  lapply(cohort_years, function(yr){
    yr_key<-as.character(yr)
    ord<-detail_node_order(links_detail_by_year[[yr_key]])
    list(
      links_df = links_detail_by_year[[yr_key]],
      node_order = ord,
      node_colors = detail_node_colors(ord),
      total_n = cohort_n_by_year$N[cohort_n_by_year$hs_grad_year == yr],
      label = paste("Class of", yr_key)
    )
  })
)

sankey_detail_dropdown<-build_sankey_dropdown(
  detail_scenarios, "Student Pathways (Detail): UC / CSU / Out-of-State / 2-Year"
)

## -----------------------------------------------------------------------------
## Part 8D - Export consolidated per-cohort flow data (for reuse downstream,
## e.g. by a combined outcomes+pathways report -- avoids re-touching the PSD)
## -----------------------------------------------------------------------------
# #One long CSV per diagram type: every cohort's link table stacked together
# #with a cohort_label column ("All Cohorts" + each graduating class), plus a
# #small N lookup. Together these three files are enough to fully reconstruct
# #every Sankey (labels, %, hover text) without needing the PSD again.
# 
# links_general_export<-bind_rows(
#   links_general %>% mutate(cohort_label = "All Cohorts", .before = 1),
#   bind_rows(lapply(names(links_general_by_year), function(yr_key){
#     links_general_by_year[[yr_key]] %>% mutate(cohort_label = yr_key, .before = 1)
#   }))
# )
# 
# links_detail_export<-bind_rows(
#   links_detail %>% mutate(cohort_label = "All Cohorts", .before = 1),
#   bind_rows(lapply(names(links_detail_by_year), function(yr_key){
#     links_detail_by_year[[yr_key]] %>% mutate(cohort_label = yr_key, .before = 1)
#   }))
# )
# 
# cohort_n_export<-bind_rows(
#   tibble(cohort_label = "All Cohorts", total_n = cohort_n),
#   cohort_n_by_year %>% transmute(cohort_label = as.character(hs_grad_year), total_n = N)
# )
# 
# fwrite(links_general_export, file.path(output_dir, "sankey_general_links_by_cohort.csv"))
# fwrite(links_detail_export, file.path(output_dir, "sankey_detail_links_by_cohort.csv"))
# fwrite(cohort_n_export, file.path(output_dir, "sankey_cohort_n.csv"))

## -----------------------------------------------------------------------------
## Part 9 - Save interactive HTML outputs
## -----------------------------------------------------------------------------

# #saveWidget(..., selfcontained = TRUE) embeds everything needed into a single
# #.html, but still drops a "*_files" scratch folder next to it -- clean that
# #up after each save so the output folder doesn't fill up with clutter across
# #dozens of per-cohort files.
# save_sankey_html<-function(widget, path){
#   saveWidget(as_widget(widget), path, selfcontained = TRUE)
#   files_dir<-file.path(dirname(path), paste0(tools::file_path_sans_ext(basename(path)), "_files"))
#   if (dir.exists(files_dir)) unlink(files_dir, recursive = TRUE)
# }
# 
# #--- Whole-sample diagrams ---
# save_sankey_html(sankey_general, file.path(output_dir, "sankey_general.html"))
# save_sankey_html(sankey_detail, file.path(output_dir, "sankey_detail_uc_csu_oos.html"))
# 
# #--- One file per graduating class per diagram ---
# for (yr_key in names(sankey_general_list)) {
#   save_sankey_html(sankey_general_list[[yr_key]],
#                    file.path(output_dir, sprintf("sankey_general_%s.html", yr_key)))
# }
# for (yr_key in names(sankey_detail_list)) {
#   save_sankey_html(sankey_detail_list[[yr_key]],
#                    file.path(output_dir, sprintf("sankey_detail_uc_csu_oos_%s.html", yr_key)))
# }
# 
# #--- Combined cohort-switcher diagrams (All Cohorts + dropdown to pick a class) ---
# save_sankey_html(sankey_general_dropdown, file.path(output_dir, "sankey_general_by_cohort.html"))
# save_sankey_html(sankey_detail_dropdown, file.path(output_dir, "sankey_detail_by_cohort.html"))
# 
# cat("\nSaved whole-sample diagrams to:\n -",
#     file.path(output_dir, "sankey_general.html"), "\n -",
#     file.path(output_dir, "sankey_detail_uc_csu_oos.html"), "\n")
# cat("\nSaved", length(sankey_general_list), "per-cohort GENERAL diagrams (sankey_general_<year>.html) and",
#     length(sankey_detail_list), "per-cohort DETAIL diagrams (sankey_detail_uc_csu_oos_<year>.html) to:\n -",
#     output_dir, "\n")
# cat("\nSaved combined cohort-switcher diagrams (dropdown to pick 'All Cohorts' or a",
#     "single class) to:\n -", file.path(output_dir, "sankey_general_by_cohort.html"),
#     "\n -", file.path(output_dir, "sankey_detail_by_cohort.html"), "\n")
# cat("\nReview files written for manual QA:\n -",
#     file.path(output_dir, "sankey_general_links.csv"), "\n -",
#     file.path(output_dir, "sankey_detail_links.csv"), "\n -",
#     file.path(output_dir, "sankey_unmatched_4yr_colleges.csv"),
#     "(only if unmatched 4-year names were found)\n")
# cat("\nConsolidated per-cohort export (for downstream reuse, e.g. a combined",
#     "report -- reconstructs every Sankey without re-reading the PSD):\n -",
#     file.path(output_dir, "sankey_general_links_by_cohort.csv"), "\n -",
#     file.path(output_dir, "sankey_detail_links_by_cohort.csv"), "\n -",
#     file.path(output_dir, "sankey_cohort_n.csv"), "\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
