# College RPP Results

This repo holds the analysis pipeline for the **UCLA Center for Community Schooling's College and Career Research-Practice Partnership (RPP)**. It turns a school's Postsecondary Database (PSD) snapshot — a student-level export tracking each graduate's college plans, enrollment, and completion — into the four official College-Going Outcomes measures, a pair of interactive Sankey "pathway" diagrams, and a single shareable HTML report that teachers and school staff can open in any browser.

All of the outcome definitions and calculation rules implemented here follow the partnership's **Postsecondary Pathways Dataset Management Technical Guide** (referenced throughout the code comments as "the Technical Guide," Sections 7–9).

## What it produces

- **Four outcome measures**, one row per high school graduating cohort:
  - Postsecondary Plans (known college/career plan on file by graduation)
  - Immediate College Enrollment (enrolled the fall after graduating)
  - 1st-to-2nd-Year Persistence (of students enrolled in Year 1, returned for Year 2)
  - 6-Year College Completion (earned a degree — associate, certificate, bachelor's, or higher — within 6 years)
- **Two interactive Sankey diagrams** showing student pathways from HS graduation through entry sector (2-year / 4-year, with UC/CSU/out-of-state detail) to eventual completion status, viewable "All Cohorts" or filtered to a single graduating class.
- **One self-contained HTML report** combining both of the above behind a single cohort dropdown, styled with the UCLA Center for Community Schooling's branding, ready to email or post as a link.

## Data source (not included in this repo)

The pipeline reads a school's PSD export — a `.csv` synced via Box Drive — from a path like:

```
Box/College and Career RPP/1. NSC Dataset/<School>/<School> PSD/<date>-<school>-psd-<name>.csv
```

**No student data lives in this repo.** Each script points at a local Box-synced file path (see `psd_rfk`, `psd_mann`, `psd_demo` in the scripts) that you edit to match your machine and the school/snapshot you're reporting on. A synthetic demo file (`demo_psd_synthetic.csv`) is used as the active default so the scripts run out of the box without real student data.

## How the pieces fit together

```
01-calculate-outcomes.R  →  the 4 outcome measures (tables, printed to console)
02-sankey-pathways.R     →  the 2 Sankey diagrams (standalone, re-derives its own data)
                                │
                                ▼
03-build-teacher-report.R   or   04-teacher-report.qmd
        → sources 01 and 02, pulls their in-memory results,
          and assembles the combined HTML report
```

`01` and `02` are independent — `02` does not source or depend on `01`; it re-reads and re-cleans the PSD itself so it can be run and tested on its own. `03` (R script) and `04` (Quarto document) are two different ways of building the *same* combined report: `03` assembles the HTML as one big string in R, while `04` is a Quarto file that's easier to open and hand-edit (prose, labels, colors) before re-rendering. Both source `01` and `02` into separate environments to avoid variable-name collisions, then read the resulting R objects directly rather than round-tripping through intermediate CSVs.

## File-by-file breakdown
<details>

### `01-calculate-outcomes.R`
<details>
Reads a PSD snapshot and calculates all four outcome measures (Technical Guide Sec. 8):

- Builds the cohort ("denominator") table: one row per student who earned a **Regular Diploma** at the school's June ceremony. Post-June Graduation and Certificate of Completion students are excluded, per the Guide.
- **Postsecondary Plans** — from `record_term == "plans"` rows, categorized by `cc_4year`.
- **Immediate College Enrollment** — computed two ways (literal enrollment date vs. term/year match) plus a combined union measure, since roughly half of enrollment records lack a literal `enrollment_begin` date.
- **1st-to-2nd-Year Persistence** — matched on record term/year categories (not literal dates) so staff- and NSC-sourced records compare consistently; denominator is students enrolled anywhere in Year 1.
- **College Completion** — assigns each graduate's earliest completion event to a 4–8 year milestone bucket using fixed Aug 15 cutoffs; 6-year completion is the headline figure (cumulative 4+5+6-year counts).
- A **data quality flags** section surfaces rows with values that don't match any documented category (e.g. stray emails or formula fragments from copy/paste errors in the source PSD) for manual review rather than silently dropping or guessing at them.
- Exports (currently commented out — uncomment to write files): `outcome_snapshot_summary.csv`, `postsecondary_plans_detail.csv`, `immediate_enrollment_detail.csv`, `persistence_detail.csv`, `completion_detail.csv`, `data_quality_flags.csv`.
</details>

### `02-sankey-pathways.R`
<details>
Builds two Plotly Sankey diagrams of student pathways, styled after the CollegeGoingOutcomes.com "Student Pathways" chart:

- **General diagram**: HS Graduates → {Never Enrolled / 2-Year / 4-Year} → {Attended-Not-Complete / Completed 2-Yr / Completed 4-Yr}.
- **Detailed diagram**: same, but the 4-Year entry node is split into UC / CSU / Out-of-State / Other 4-Year.
- **Diagnostic-first design**: Part 1 of the script prints the PSD's actual column names and sample values so you can confirm the CONFIG block (UC/CSU name-matching regexes, which column holds degree type, etc.) before trusting the output. It prefers an authoritative `system_type` code column when present, falling back to name-pattern matching or a state column only when that's missing.
- Unmatched 4-year college names and rows "rescued" via `system_type` despite a malformed `cc_4year` value are written to review CSVs instead of being silently guessed at.
- Produces both a whole-sample version and one diagram per graduating class (plus a combined dropdown-selector version) for both the general and detailed layouts.
- This script is standalone — it does not source or modify `01`; it re-derives everything it needs directly from the PSD.
</details>

### `03-build-teacher-report.R`
<details>
Assembles the single combined HTML report:

- Runs `01` and `02` in full (via `source()`, each into its own R environment) and pulls their finished tables/diagrams directly out of memory — no intermediate CSVs are read.
- Calculates an **"All Cohorts" KPI** as a true weighted rate (sum of numerators ÷ sum of denominators across all cohorts), not an average of each cohort's rate, so small early cohorts don't get over-weighted against large recent ones.
- Flags KPI tiles for cohorts whose outcome window hasn't fully elapsed yet (e.g. a very recent graduating class can't have a real 6-year completion figure) via the `SNAPSHOT_YEAR` config value.
- Outputs one self-contained `college_going_outcomes_report.html` — no dependencies besides the Plotly.js CDN script tag — themed with UCLA's official brand colors for the page chrome, while the Sankey diagrams keep their own navy/blue/orange/gray/green palette per team direction.
</details>

### `04-teacher-report.qmd`
<details>
A Quarto version of `03` that produces the identical report but is meant to be hand-edited directly (prose, labels, colors) rather than modifying an R script that assembles a big HTML string. Render it from a terminal in this folder with:

```bash
quarto render 04-teacher-report.qmd
```

This produces `college_going_outcomes_report.html` in the same folder.
</details>

### `docs/04-teacher-report.html`
<details>
A pre-rendered, static copy of the teacher report output — published via GitHub Pages (see below) so it can be previewed without installing R or Quarto.
</details>

### `docs/index.html`
<details>
A tiny redirect page so the site's root URL lands on the report without needing to know its filename (see "Publishing to GitHub Pages" below).
</details>
### `.nojekyll`
<details>
An empty marker file at the repo root that tells GitHub Pages to skip Jekyll processing and serve the `docs/` folder's files as-is. Standard practice for publishing Quarto/static HTML output (recommended in [Quarto's own GitHub Pages guide](https://quarto.org/docs/publishing/github-pages.html)) — without it, GitHub's Jekyll build can silently ignore or mishandle generated files.
</details>

</details>

## Quick start
<details>
**Requirements:**
- [R](https://www.r-project.org/) (any recent version)
- R packages: `tidyverse`, `data.table`, `plotly`, `htmlwidgets`, `jsonlite`
- To render `04-teacher-report.qmd`: [Quarto](https://quarto.org/)
- Box Drive installed and synced, with access to the `College and Career RPP` folder (or point the scripts at a local copy of a PSD `.csv`)

**Install the R packages:**

```r
install.packages(c("tidyverse", "data.table", "plotly", "htmlwidgets", "jsonlite"))
```

**Run the pipeline:**

1. Open `01-calculate-outcomes.R` and `02-sankey-pathways.R`. Each has a small block of `psd_rfk` / `psd_mann` / `psd_demo` path definitions near the top — edit these (or add a new one for your school) to point at the correct dated PSD `.csv` file, and update the `fread(...)` call to use it. The synthetic demo file is used by default.
2. Run `01-calculate-outcomes.R` on its own to sanity-check the four outcome measures print correctly for your data (look at the console output and the "Data Quality Flags" section).
3. Run `02-sankey-pathways.R` on its own and read the Part 1 diagnostic output — it tells you whether your PSD has a `system_type` column (preferred) and whether the UC/CSU regex patterns need adjusting for your data.
4. Once both run cleanly, build the combined report:
   - Either run `03-build-teacher-report.R` directly in R, **or**
   - Run `quarto render 04-teacher-report.qmd` from a terminal in this folder.
5. Both approaches write `college_going_outcomes_report.html` — open it in any browser, or share it as a link/attachment. Update `REPORT_TITLE` and `SNAPSHOT_YEAR` at the top of `03` or `04` first to match the school and PSD snapshot you're reporting on.

Several `fwrite(...)` export lines in `01` and `02` are commented out by default (the scripts print results to the console instead); uncomment them if you want the detail tables and diagnostic CSVs saved to disk.
</details>

## Publishing to GitHub Pages
<details>
The rendered report is published straight out of the `docs/` folder. To view it live, or to fix a Pages deployment that isn't showing the report, confirm these three things:

1. **Pages is pointed at the `docs/` folder on `main`.** In the repo, go to **Settings → Pages → Build and deployment → Source**, and set it to **"Deploy from a branch"** with **Branch: `main`**, **Folder: `/docs`**. If Pages was never turned on for this repo, this is almost always the missing step.
2. **`docs/index.html` exists.** GitHub Pages only auto-serves a file named `index.html` at a given URL; there was previously no file by that name in `docs/`, so the site's root URL (`https://uclacommschool.github.io/college-rpp-results/`) would 404 even with Pages correctly configured. `docs/index.html` now redirects to `docs/04-teacher-report.html`, so the root URL works.
3. **`.nojekyll` exists at the repo root.** GitHub Pages runs the Jekyll static-site generator by default, which can ignore or mangle files it doesn't expect. This repo now includes an empty `.nojekyll` file at the root, which tells GitHub Pages to skip Jekyll entirely and serve the raw HTML — the standard recommendation for publishing Quarto output.

Once those are in place, the report is available at:

- `https://uclacommschool.github.io/college-rpp-results/` (redirects to the report)
- `https://uclacommschool.github.io/college-rpp-results/04-teacher-report.html` (direct link)

A deployment can take a minute or two after pushing; check the **Actions** tab (or **Settings → Pages**) for the "pages build and deployment" run status and any error it reports.

**Updating the published report:** re-render `04-teacher-report.qmd` (or run `03-build-teacher-report.R`) and copy/move the resulting `college_going_outcomes_report.html` into `docs/04-teacher-report.html`, then commit and push. `docs/index.html` does not need to change.
</details>

## Known limitations
<details>
These are called out in code comments and carried through into the report's footnotes:

- **Postsecondary Plans** data from the school's college application tracker hasn't yet been fully merged into the unified PSD process, so counts may run lower than tracker-based figures reported elsewhere.
- **Immediate Enrollment**: roughly half of fall-term PSD rows lack a literal `enrollment_begin` date (only NSC-sourced records reliably carry one), so the date-based measure under-counts; the term-based measure is used as the more complete headline figure.
- **Persistence and Completion** figures for recent graduating classes are artificially low simply because their full outcome window (2 years for persistence, 6 years for completion) hasn't elapsed yet as of the PSD snapshot being used.
- Where `coll_grad_date` is missing, completion year is approximated from the record's term/year — an approximation the Technical Guide itself acknowledges, so counts near a 4/5-year boundary may shift by a student or two versus manual review.
</details>