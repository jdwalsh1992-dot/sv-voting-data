# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

# Sierra Vista Area Election Explorer

R Shiny app showing election results for Arizona's 6th congressional district
(Sierra Vista / Cochise County) and its neighbors AZ-7 and AZ-2, at district,
county and precinct level, 2016–2026. All data is static and built ahead of
time. See README.md for the user-facing description and caveats.

## Commands

```sh
Rscript -e 'shiny::runApp(port = 8765, launch.browser = TRUE)'   # run the app
Rscript data-raw/build_data.R                                     # rebuild data/ (needs internet, ~1–2 min with cache)
Rscript build_site.R                                              # rebuild the GitHub Pages site in docs/ (Shinylive)
Rscript -e 'httpuv::runStaticServer("docs", port = 8008)'          # preview docs/ locally
```

**Live site:** https://jdwalsh1992-dot.github.io/sv-voting-data/, served by GitHub Pages from `docs/` on `main`
(repo `jdwalsh1992-dot/sv-voting-data`, public). `docs/` is generated, so never edit it by hand. Rebuild and commit it
after any change to `app.R`, `R/` or `data/`, or the live site goes stale. The app runs in webR, so every
package it uses must exist as a webR binary, and the first load is slow (~170 MB).

There is no test suite. To check the server logic, run `shiny::testServer(shiny::shinyAppDir("."), {...})`.
Set inputs for every race × mode × level and read `map_data()`, `swing_df()`,
`table_data()` and the `output$...` values. Shape counts must equal the number
of geographies: 3 districts, 14 counties, about 670–690 precincts.

## Layout

- `app.R`: UI and server. Tabs: Map (leaflet), Trends (plotly), Results, Compare districts, Data.
- **Startup data assembly (`app.R`, top of file):** `results` is not just `results.rds`. The app drops
  district-level House rows from it, then adds `house_results.csv` (official canvass) and
  `preliminary_2026.csv`. `PRELIM_YEARS`, `is_prelim()` and `year_label()` come from the 2026 file
  and drive the "(preliminary)" labels and the district-only notices. To change district-level House
  numbers, edit those CSVs, not the build script (county and precinct House rows still come from the build).
- `R/helpers.R` (auto-sourced): `margin_summary()`, `swing()`, `fmt_margin()`, `fmt_swing()`,
  `margin_palette()` + `squish()`, `popup_html()`.
- `data-raw/build_data.R`: one-time download, clean, validate and write to `data/`. Downloads are cached in `data-raw/cache/` (~400 MB, safe to delete).
- `data/`
  - `results.rds`: long table with `level, geo_id, year, race, party, candidate, votes, estimated`.
    `level` ∈ district / county / precinct / precinct_current.
  - `districts.rds`, `counties.rds`, `precincts.rds` (sf, EPSG:4326). `precincts.rds` holds four
    vintages via `geo_set`: vest2016 / vest2018 / vest2020 / current (2024 shapes).
  - `context.rds`: Arizona outline + the six other districts, drawn faintly on the map.
  - `house_results.csv`: official 2022/2024 House canvass. Used as truth for validation and for district-level House in the app.
  - `preliminary_2026.csv`: unofficial 2026 House results (district level, D/R only).
  - `districts.csv`: district descriptions and current representatives.
  - `precinct_match_report.csv`: 2022/2024 precinct name-match rate by county.

## Conventions

- **Margin** = (R − D) / all votes, in points. Positive = Republican lead. Show it with `fmt_margin()` ("R+2.5" / "D+8.3").
- **Swing** = margin_to − margin_from. Positive = shift toward Republicans.
- **District lines** are always the current 2022 map (119th Congress).
- **Estimated rows** (`estimated = TRUE`): 2016–2020 values reallocated by area onto today's districts or 2024 precinct shapes (`st_interpolate_aw`).
- **`precinct_current`**: every year's precinct votes on the 2024 shapes. Used only for precinct swing maps.
- **Small precincts**: a precinct whose smaller election total is under 25 votes gets an NA swing (grey).

## Data sources

| Years | Results | Precinct shapes |
|---|---|---|
| 2016/2018/2020 | VEST shapefiles, Harvard Dataverse file ids 4773516 / 4773519 / 4864722 (president, senate, governor; no House) | VEST |
| 2022 / 2024 | MIT MEDSL precinct returns, Dataverse file ids 14249794 / 13731136 | NYT 2024 precinct map |
| 2026 | Wikipedia results tables, Nov 9–10 2026 (unofficial) | none yet |

Free 2022/2024 VEST shapefiles don't exist: the UF Election Lab versions need a login or payment.

## Gotchas found while building

- **Mixed vote modes:** MEDSL counties report either a `TOTAL` row or a breakdown by vote mode, and a few report both (e.g. Graham 2022). Per precinct, use TOTAL if present, else sum the modes.
- **Blank parties:** MEDSL 2024 leaves `party_simplified` blank for some counties. Fill it from the same candidate's other rows.
- **Precinct-name matching** between MEDSL and NYT is a cascade within each county: exact normalized
  name → letters only → first number → edit distance ≤ 2, with a key accepted only when it is unique.
  The match rate is about 91%. Unmatched precincts are blank on the map but still counted in county and district totals.
- **Validation** (in the build script; it stops on failure):
  - District House totals must be within 2% of `house_results.csv`. AZ-6 matches exactly. 2022 AZ-7 is about 1.5% short in MEDSL.
  - Statewide totals must be within 1% of the canvass. 2016–2020 match exactly.
- **`bind_rows()` on named lists:** `bind_rows(list_a, list_b)` treats each list's elements as columns. Use `bind_rows(c(list_a, list_b))`.
- **Shape simplification:** simplify each precinct vintage separately. Simplifying the overlapping sets together keeps every vertex and produced a 14 MB file; done separately it's about 2 MB.
- **Basemap tiles:** CARTO tiles now need an API key and show watermarks. Use keyless tiles: `Esri.WorldGrayCanvas`, `OpenStreetMap`, `Esri.WorldImagery`.
- **Leaflet proxy timing:** proxy updates wait for `input$map_zoom` (the `map_ready` flag) so they aren't lost before the widget exists.
- **The map looks too big but is correct:** AZ-2, AZ-6 and AZ-7 together cover about 77% of Arizona's area (AZ-2 alone is 51%).

## Machine setup (this Mac)

- **R:** installed via the Homebrew *formula* (`brew install r`), along with gdal, geos, proj and udunits. The cask needs sudo.
- **`png`:** must be installed with Homebrew's libpng first on the PATH, because Anaconda's `libpng-config` shadows it:
  `PATH=/opt/homebrew/opt/libpng/bin:/opt/homebrew/bin:$PATH`.
- **`chromote` / `shinytest2`:** don't compile against OpenSSL 4. Headless Chrome also crashes inside Claude Code's sandbox, so check the UI in a real browser.

## Open items

- **2026 certified results:** replace `data/preliminary_2026.csv` once the state certifies (late Nov / early Dec 2026). Add 2026 county, precinct and Governor results once MEDSL or the Secretary of State publish precinct returns.
- **Secretary of State live-results API:** the site `app.enhancedvoting.com/results/public/Arizona/elections/110326GeneralElection` has a JSON API at
  `/results/public/api/elections/Arizona/110326GeneralElection`. Querying it was blocked by the permission settings, so ask the user before using it.
- **Possible addition:** 2018/2020 House results under the old district lines (left out because the boundaries changed).
