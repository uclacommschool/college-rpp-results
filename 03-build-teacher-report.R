################################################################################
##
## [ PROJ ] < Community School Postsecondary Database >
## [ FILE ] < 03-build-teacher-report.R >
## [ AUTH ] < Jeffrey Yo >
## [ INIT ] < 7/30/26 >
##
################################################################################

#Goal: Assemble ONE self-contained, shareable HTML report for teachers and
#educators, combining:
#   - the four outcome measures from 01-calculate-outcomes.R (Postsecondary
#     Plans, Immediate Enrollment, Persistence, 6-Year Completion) as simple
#     "at a glance" KPI tiles, and
#   - both Sankey pathway diagrams from 02-sankey-pathways.R,
#all synced to a single cohort dropdown ("All Cohorts" or one graduating
#class at a time).
#
#This script does NOT read or write any intermediate CSVs. It runs
#01-calculate-outcomes.R and 02-sankey-pathways.R in full (via source()) and
#builds the report directly from their in-memory results -- 01 and 02 are
#left completely unmodified and still produce all of their own usual outputs
#(the outcome CSVs, the standalone Sankey HTMLs, etc.); this script just ALSO
#grabs the finished R objects along the way instead of re-reading files back
#off disk.
#
#Output is a single .html file with no external dependencies besides the
#Plotly.js CDN script tag -- share it as a link from Box/Drive, or attach it
#directly; it opens in any browser with no login, install, or server needed.
#
#Theme: UCLA Center for Community Schooling (communityschooling.gseis.ucla.edu)
#for the page chrome -- colors verified against UCLA's official brand
#guidelines (brand.ucla.edu/identity/colors). NOTE: that reference page's
#exact web font couldn't be read from a plain page fetch (only page text,
#not live CSS, was available) -- Roboto / Roboto Slab were chosen as a safe,
#legible, university-appropriate substitute. Swap the Google Fonts link +
#font-family values in Part 4 below if you confirm the Center's site
#actually uses something else.

################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(jsonlite)

#NOTE: no janitor/readxl/data.table needed directly by this script -- 01 and
#02 already load what they need when sourced below.

## ---------------------------
## directory paths
## ---------------------------

getwd()
code_file_dir<-file.path(".","calculate-outcomes")

if (.Platform$OS.type == "windows") {
  box_file_dir <- file.path(Sys.getenv("USERPROFILE"), "Box")
} else {
  box_file_dir <- file.path(Sys.getenv("HOME"), "Library", "CloudStorage", "Box-Box")
}

## ---------------------------
## CONFIG
## ---------------------------

#Paths to the actual 01/02 script files to run. Default assumes all three
#scripts live in the same folder as this one.
outcomes_script_path<-file.path(code_file_dir, "01-calculate-outcomes.R") #UPDATE if these live elsewhere
sankey_script_path<-file.path(code_file_dir, "02-sankey-pathways.R")      #UPDATE if these live elsewhere

#Where THIS script's report gets saved (01/02 still control their own
#output_dir internally, for their own separate outputs).
#output_dir<-"/mnt/user-data/outputs"

output_dir<-file.path(".", "calculate-outcomes")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

REPORT_TITLE<-"Mann UCLA Community School: College-Going Outcomes"

#Used only to grey/flag KPI tiles for cohorts whose outcome window hasn't
#fully elapsed as of this snapshot (e.g. a Class of 2024 student can't show
#a real 6-year completion figure in a 2025 snapshot). Purely a display note
#-- doesn't change any calculation. Set to the year of the PSD snapshot.
SNAPSHOT_YEAR<-2025 #UPDATE to match the PSD snapshot being reported

output_file<-file.path(output_dir, "college_going_outcomes_report.html")

## -----------------------------------------------------------------------------
## Part 1 - Run 01 and 02, each into its own environment, and pull out just
## the objects this report needs
## -----------------------------------------------------------------------------
#Sourced into SEPARATE environments (rather than straight into this script)
#so the two scripts' internal variables never collide. This matters for real:
#01 uses `cohort_n` for a per-graduating-year N table, while 02 reuses the
#exact same name `cohort_n` for a single overall-N scalar -- sourcing both
#into one shared environment would let 02's value silently overwrite 01's.
#Separate environments mean 01 and 02 each run exactly as written/tested
#standalone, with zero risk of one clobbering the other's same-named object.
#
#NOTE: each script re-reads and re-cleans the PSD from scratch, so this does
#duplicate that work (a few extra seconds per report build) -- kept this way
#deliberately so 01 and 02 remain fully independent, runnable on their own.

cat("\n=== Running 01-calculate-outcomes.R ===\n")
env_outcomes<-new.env()
source(outcomes_script_path, local = env_outcomes)

cat("\n=== Running 02-sankey-pathways.R ===\n")
env_sankey<-new.env()
source(sankey_script_path, local = env_sankey)

cat("\nBoth scripts finished running. Building the report from their",
    "in-memory results (no intermediate files read).\n")

#--- Outcome-measure objects (01) ---
outcome_summary<-env_outcomes$outcome_snapshot
plans_detail<-env_outcomes$postsecondary_plans
enroll_detail<-env_outcomes$immediate_enrollment
persist_detail<-env_outcomes$persistence
complete_detail<-env_outcomes$completion

#--- Sankey flow objects (02) ---
links_general_overall<-env_sankey$links_general        #"All Cohorts" table
links_general_by_year<-env_sankey$links_general_by_year #named list, keyed by year
links_detail_overall<-env_sankey$links_detail
links_detail_by_year<-env_sankey$links_detail_by_year

#--- Reuse 02's node-order, node-COLOR, and trace-building logic directly
#(the same functions its own standalone Sankey HTML files use) -- per team
#direction, the Sankey diagrams keep their original navy/blue/orange/gray/
#green palette rather than the UCLA gold/blue theme applied to the rest of
#this report (KPI cards, header, etc.).
general_node_order<-env_sankey$general_node_order
general_node_colors<-env_sankey$general_node_colors
detail_node_order<-env_sankey$detail_node_order
detail_node_colors<-env_sankey$detail_node_colors
build_sankey_trace<-env_sankey$build_sankey_trace

#--- cohort_label -> N lookup, built directly from 02's live objects (the
#per-year table plus the overall scalar) rather than a lookup CSV.
year_labels<-as.character(sort(env_sankey$cohort_n_by_year$hs_grad_year))
cohort_n_map<-setNames(
  c(env_sankey$cohort_n, env_sankey$cohort_n_by_year$N),
  c("All Cohorts", as.character(env_sankey$cohort_n_by_year$hs_grad_year))
)

## -----------------------------------------------------------------------------
## Part 2 - "All Cohorts" outcome KPI = a true weighted rate, calculated from
## 01's in-memory detail tables
## -----------------------------------------------------------------------------
#Sum of numerators / sum of denominators across every graduating class --
#NOT an average of each cohort's rate, which would over-weight small early
#cohorts against large recent ones.

all_cohorts_kpi<-tibble(
  Graduate_Class = "All Cohorts",
  N = sum(outcome_summary$N),
  Postsecondary_Plans = sum(plans_detail$total_with_known_plan) / sum(plans_detail$N),
  #Term-based comparison used as the headline Immediate Enrollment number --
  #per 01's Part 3 note, roughly half of literal enrollment_begin dates are
  #missing, so the date-based column under-counts; term-based is the more
  #complete measure currently available.
  Immediate_Enrollment = sum(enroll_detail$term_based_comparison_count) / sum(enroll_detail$N),
  Persistence_1st_to_2nd_Year = sum(persist_detail$persisted_year1_to_year2) / sum(persist_detail$enrolled_year1),
  College_Completion_6yr = sum(complete_detail$completion_6yr_count) / sum(complete_detail$N)
)

per_cohort_kpi<-outcome_summary %>%
  transmute(
    Graduate_Class = as.character(Graduate_Class),
    N,
    Postsecondary_Plans,
    Immediate_Enrollment = Immediate_Enrollment_TermBased, #headline column, see note above
    Persistence_1st_to_2nd_Year,
    College_Completion_6yr
  )

kpi_table<-bind_rows(all_cohorts_kpi, per_cohort_kpi) %>%
  mutate(
    grad_year_num = suppressWarnings(as.numeric(Graduate_Class)),
    #"All Cohorts" blends immature cohorts in with mature ones -- flagged via
    #the static footnote in the report rather than a per-tile badge, since a
    #single badge can't summarize 13 different cohort windows at once.
    persistence_mature = is.na(grad_year_num) | (SNAPSHOT_YEAR - grad_year_num) >= 2,
    completion_mature = is.na(grad_year_num) | (SNAPSHOT_YEAR - grad_year_num) >= 6
  )

## -----------------------------------------------------------------------------
## Part 3 - Assemble the combined per-cohort payload (KPIs + both Sankeys)
## -----------------------------------------------------------------------------

cohort_labels<-c("All Cohorts", year_labels)

get_links<-function(label, overall_df, by_year_list){
  if (label == "All Cohorts") overall_df else by_year_list[[label]]
}

build_cohort_payload<-function(label){
  n_total<-cohort_n_map[[label]]
  
  lg<-get_links(label, links_general_overall, links_general_by_year)
  ld<-get_links(label, links_detail_overall, links_detail_by_year)
  
  ord_g<-general_node_order(lg); col_g<-general_node_colors(ord_g)
  ord_d<-detail_node_order(ld); col_d<-detail_node_colors(ord_d)
  
  kpi_row<-kpi_table %>% filter(Graduate_Class == label)
  
  list(
    label = label,
    n = n_total,
    kpis = list(
      postsecondaryPlans = round(kpi_row$Postsecondary_Plans[1] * 100, 1),
      immediateEnrollment = round(kpi_row$Immediate_Enrollment[1] * 100, 1),
      persistence = round(kpi_row$Persistence_1st_to_2nd_Year[1] * 100, 1),
      completion6yr = round(kpi_row$College_Completion_6yr[1] * 100, 1),
      persistenceMature = isTRUE(kpi_row$persistence_mature[1]),
      completionMature = isTRUE(kpi_row$completion_mature[1])
    ),
    sankeyGeneral = build_sankey_trace(lg, ord_g, col_g, n_total),
    sankeyDetail = build_sankey_trace(ld, ord_d, col_d, n_total)
  )
}

report_payload<-lapply(cohort_labels, build_cohort_payload)
names(report_payload)<-cohort_labels

report_json<-toJSON(report_payload, auto_unbox = TRUE, null = "null", digits = NA)

cat("\nBuilt combined report payload for", length(cohort_labels), "cohort views",
    "(All Cohorts +", length(year_labels), "graduating classes).\n")

## -----------------------------------------------------------------------------
## Part 4 - Build the HTML/CSS/JS report template
## -----------------------------------------------------------------------------
#Design notes: UCLA Center for Community Schooling theme applies to the page
#chrome (header, KPI cards, typography) -- UCLA Darkest Blue header, UCLA
#Blue for headings/numerals, UCLA Gold reserved for graphic accents only
#(KPI underline, dropdown caret, badges), never as text color, per UCLA's
#own brand guidance on gold/tertiary color use and plain accessibility
#(gold-on-white text fails contrast). The two Sankey diagrams intentionally
#keep 02-sankey-pathways.R's original navy/blue/orange/gray/green palette
#(see Part 1 above) rather than the UCLA theme, per team direction. Roboto
#Slab for headlines, Roboto for body/labels/numerals -- font choice
#rationale is in the file header comment at the top of this script (search
#for "Theme:"). No chart toolbar clutter, no animation beyond simple hover
#states.

year_range_label<-if (length(year_labels) > 0) {
  sprintf("Class of %s - %s", min(year_labels), max(year_labels))
} else {
  "All Graduating Classes"
}

option_tags<-paste0(
  vapply(cohort_labels, function(lbl){
    display<-if (lbl == "All Cohorts") sprintf("All Cohorts (%s)", year_range_label) else paste("Class of", lbl)
    sprintf('<option value="%s">%s</option>', lbl, display)
  }, character(1)),
  collapse = "\n        "
)

generated_date_label<-format(Sys.Date(), "%B %d, %Y")

html_template<-r"---(<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>{{REPORT_TITLE}}</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link href="https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@500;600;700&family=Roboto:wght@400;500;700;900&display=swap" rel="stylesheet">
<script src="https://cdn.plot.ly/plotly-2.32.0.min.js"></script>
<style>
  * { box-sizing: border-box; }
  :root {
    /* UCLA official brand colors (brand.ucla.edu/identity/colors) */
    --ucla-blue:#2774AE; --ucla-darkest-blue:#003B5C; --ucla-darker-blue:#005587;
    --ucla-lighter-blue:#8BB8E8; --ucla-gold:#FFD100; --ucla-gold-deep:#FFC72C;
    /* Applied roles */
    --ink:#0F2A3D; --ink-soft:#3F5A6E; --paper:#EAF3FC; --card:#FFFFFF;
    --border:#C9DDF0; --green:#2F7A4F; --amber:#B4691E; --radius:12px;
  }
  html, body { margin:0; padding:0; background:var(--paper); color:var(--ink);
    font-family:'Roboto', system-ui, sans-serif; }
  h1, h2 { font-family:'Roboto Slab', Georgia, serif; margin:0; }
  a { color:var(--ucla-blue); }

  .site-header { background:var(--ucla-darkest-blue); color:#fff; padding:2rem 1.5rem 1.75rem; }
  .header-inner { max-width:1080px; margin:0 auto; display:flex; align-items:flex-start;
    gap:1rem; flex-wrap:wrap; justify-content:space-between; }
  .title-block { display:flex; gap:.9rem; align-items:flex-start; }
  .title-block h1 { font-size:1.55rem; font-weight:600; letter-spacing:.005em; }
  .title-block p { margin:.3rem 0 0; color:#B9D6EF; font-size:.92rem; }

  .cohort-picker { display:flex; flex-direction:column; gap:.35rem; align-items:flex-end; }
  .cohort-picker label { font-size:.7rem; text-transform:uppercase; letter-spacing:.09em; color:#8FB6D9; }
  select#cohortSelect {
    appearance:none; background:var(--ucla-darker-blue); border:1px solid #1F6690; color:#fff;
    font-family:'Roboto', sans-serif; font-weight:500; font-size:.92rem; padding:.5rem 2.3rem .5rem .9rem;
    border-radius:999px; cursor:pointer; min-width:230px;
    background-image:linear-gradient(45deg,transparent 50%,var(--ucla-gold) 50%),linear-gradient(135deg,var(--ucla-gold) 50%,transparent 50%);
    background-position:calc(100% - 18px) 55%, calc(100% - 12px) 55%;
    background-size:6px 6px, 6px 6px; background-repeat:no-repeat;
  }
  select#cohortSelect:focus-visible { outline:2px solid var(--ucla-gold); outline-offset:2px; }

  main { max-width:1080px; margin:0 auto; padding:1.75rem 1.5rem 3rem; }
  .n-note { font-size:.82rem; color:var(--ink-soft); margin:0 0 1.1rem; }

  .kpi-row { display:grid; grid-template-columns:repeat(4, 1fr); gap:1rem; margin-bottom:.5rem; }
  @media (max-width:820px) { .kpi-row { grid-template-columns:repeat(2, 1fr); } }
  @media (max-width:480px) { .kpi-row { grid-template-columns:1fr; } }

  .kpi-card { background:var(--card); border:1px solid var(--border); border-radius:var(--radius);
    padding:1.1rem 1.2rem 1rem; }
  .kpi-eyebrow { font-size:.68rem; text-transform:uppercase; letter-spacing:.08em; color:var(--ucla-darkest-blue); font-weight:700; }
  .kpi-number { font-family:'Roboto', sans-serif; font-size:2.3rem; font-weight:900; line-height:1; color:var(--ucla-darkest-blue);
    margin:.4rem 0 .1rem; border-bottom:4px solid var(--ucla-gold); display:inline-block; padding-bottom:.15rem; }
  .kpi-label { font-size:.85rem; color:var(--ink-soft); margin:.4rem 0 0; }
  .kpi-badge { display:inline-block; margin-top:.5rem; font-size:.7rem; background:#FFF6D6; color:var(--amber);
    border:1px solid #F0DE94; border-radius:999px; padding:.15rem .55rem; }
  .kpi-badge[hidden] { display:none; }

  .sankey-section { background:var(--card); border:1px solid var(--border); border-radius:var(--radius);
    padding:1.25rem 1.25rem .5rem; margin-top:1.25rem; }
  .sankey-section h2 { font-size:1.1rem; font-weight:600; color:var(--ucla-darkest-blue); }
  .sankey-section .section-note { font-size:.82rem; color:var(--ink-soft); margin:.3rem 0 .8rem; }
  .sankey-plot { width:100%; height:460px; }

  footer { font-size:.78rem; color:var(--ink-soft); line-height:1.65; border-top:1px solid var(--border);
    padding-top:1.25rem; margin-top:1.75rem; }
  footer p { margin:0 0 .6rem; }
  footer strong { color:var(--ink); }

  @media (prefers-reduced-motion: reduce) { * { transition:none !important; } }
</style>
</head>
<body>

<header class="site-header">
  <div class="header-inner">
    <div class="title-block">
      <div>
        <h1>{{REPORT_TITLE}}</h1>
        <p>{{YEAR_RANGE_LABEL}} &middot; Regular Diploma graduates</p>
      </div>
    </div>
    <div class="cohort-picker">
      <label for="cohortSelect">Viewing</label>
      <select id="cohortSelect">
        {{OPTION_TAGS}}
      </select>
    </div>
  </div>
</header>

<main>
  <p class="n-note" id="cohortN"></p>

  <section class="kpi-row">
    <div class="kpi-card">
      <div class="kpi-eyebrow">Postsecondary Plans</div>
      <div class="kpi-number" id="kpiPlans">&mdash;</div>
      <p class="kpi-label">Had a known college or career plan on file by graduation</p>
    </div>
    <div class="kpi-card">
      <div class="kpi-eyebrow">Immediate Enrollment</div>
      <div class="kpi-number" id="kpiEnroll">&mdash;</div>
      <p class="kpi-label">Enrolled in college the fall after graduating</p>
    </div>
    <div class="kpi-card">
      <div class="kpi-eyebrow">1st-to-2nd-Year Persistence</div>
      <div class="kpi-number" id="kpiPersist">&mdash;</div>
      <p class="kpi-label">Of students enrolled in Year 1, returned for Year 2</p>
      <span class="kpi-badge" id="badgePersist" hidden>Still within outcome window</span>
    </div>
    <div class="kpi-card">
      <div class="kpi-eyebrow">6-Year Completion</div>
      <div class="kpi-number" id="kpiComplete">&mdash;</div>
      <p class="kpi-label">Earned a degree within 6 years of graduating</p>
      <span class="kpi-badge" id="badgeComplete" hidden>Still within outcome window</span>
    </div>
  </section>

  <section class="sankey-section">
    <h2>Pathways Overview</h2>
    <p class="section-note">Where students went after graduation, and what they had completed as of this snapshot. Hover any flow or node for details.</p>
    <div id="sankeyGeneral" class="sankey-plot"></div>
  </section>

  <section class="sankey-section">
    <h2>Four-Year Pathway Detail: UC / CSU / Out-of-State</h2>
    <p class="section-note">Same pathways, with four-year enrollment broken out by UC, CSU, Out-of-State, and other four-year institutions.</p>
    <div id="sankeyDetail" class="sankey-plot"></div>
  </section>

  <footer>
    <p><strong>Cohort:</strong> Regular Diploma graduates only (Post-June Graduation and Certificate of Completion students are excluded). <strong>Postsecondary Plans:</strong> known plan on file from the senior-year college application tracker, including an explicit "no plan" as a known outcome. <strong>Immediate Enrollment:</strong> enrolled at any college in the fall term matching the student's graduation year. <strong>Persistence:</strong> of students enrolled anywhere in Year 1, the share also enrolled in Year 2. <strong>6-Year Completion:</strong> earned any degree (associate, certificate, bachelor's, or higher) within 6 years of high school graduation.</p>
    <p>Recent graduating classes haven't reached their full Persistence (2 years) or Completion (6 years) outcome windows yet, so those figures will keep rising as more data comes in &mdash; flagged above where relevant. The "All Cohorts" view blends mature and still-developing cohorts together.</p>
    <p>Source: Postsecondary Dataset (PSD) snapshot, per the Postsecondary Pathways Dataset Management technical guide. Report generated {{GENERATED_DATE}}.</p>
  </footer>
</main>

<script>
const REPORT_DATA = {{REPORT_JSON}};

const plotConfig = { responsive: true, displayModeBar: false };

function fmtPct(x) {
  return (x === null || x === undefined || isNaN(x)) ? "\u2014" : x.toFixed(1) + "%";
}

function renderSankey(divId, sankeyData) {
  const trace = [{
    type: "sankey", orientation: "h", arrangement: "snap", valueformat: ",",
    node: sankeyData.node, link: sankeyData.link
  }];
  const layout = { font: { size: 12, family: "Roboto, sans-serif" },
    margin: { t: 10, l: 10, r: 10, b: 10 }, height: 460 };
  const el = document.getElementById(divId);
  if (el.data) { Plotly.react(divId, trace, layout, plotConfig); }
  else { Plotly.newPlot(divId, trace, layout, plotConfig); }
}

function updateReport(label) {
  const d = REPORT_DATA[label];
  if (!d) return;

  document.getElementById("cohortN").textContent = "n = " + d.n.toLocaleString() + " Regular Diploma graduates";

  document.getElementById("kpiPlans").textContent = fmtPct(d.kpis.postsecondaryPlans);
  document.getElementById("kpiEnroll").textContent = fmtPct(d.kpis.immediateEnrollment);
  document.getElementById("kpiPersist").textContent = fmtPct(d.kpis.persistence);
  document.getElementById("kpiComplete").textContent = fmtPct(d.kpis.completion6yr);

  document.getElementById("badgePersist").hidden = d.kpis.persistenceMature !== false;
  document.getElementById("badgeComplete").hidden = d.kpis.completionMature !== false;

  renderSankey("sankeyGeneral", d.sankeyGeneral);
  renderSankey("sankeyDetail", d.sankeyDetail);
}

document.getElementById("cohortSelect").addEventListener("change", e => updateReport(e.target.value));
window.addEventListener("resize", () => {
  Plotly.Plots.resize("sankeyGeneral");
  Plotly.Plots.resize("sankeyDetail");
});

updateReport(document.getElementById("cohortSelect").value);
</script>

</body>
</html>
)---"

final_html<-html_template
final_html<-gsub("{{REPORT_TITLE}}", REPORT_TITLE, final_html, fixed = TRUE)
final_html<-gsub("{{YEAR_RANGE_LABEL}}", year_range_label, final_html, fixed = TRUE)
final_html<-gsub("{{OPTION_TAGS}}", option_tags, final_html, fixed = TRUE)
final_html<-gsub("{{GENERATED_DATE}}", generated_date_label, final_html, fixed = TRUE)
final_html<-gsub("{{REPORT_JSON}}", report_json, final_html, fixed = TRUE)

writeLines(final_html, output_file, useBytes = TRUE)

cat("\nSaved combined teacher report to:\n -", output_file,
    "\n\nThis is a single self-contained file (besides the Plotly.js CDN script",
    "tag) -- share the link from Box/Drive, or attach it directly. It opens",
    "in any browser with no login, install, or server required.\n")

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------
