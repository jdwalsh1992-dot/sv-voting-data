# Sierra Vista Area Election Explorer

An R Shiny app for election results in Arizona's 6th congressional district
(Sierra Vista, Cochise County) and its neighbors, AZ-7 and AZ-2. It covers
2016–2024 at district, county and precinct level. All data is static: it's
built once into `data/`, and the app makes no data requests at runtime. Only
the map's background tiles load from the internet.

## Run

```r
install.packages(c("shiny", "bslib", "dplyr", "tidyr", "ggplot2", "DT",
                   "leaflet", "sf", "plotly"))
shiny::runApp()
```

## Rebuild the data (optional, needs internet)

```r
install.packages(c("tigris", "rmapshaper", "data.table", "stringr"))
```
```sh
Rscript data-raw/build_data.R
```

The script downloads about 400 MB into `data-raw/cache/`. It stops if the
rebuilt totals don't match the official results: House totals by district must
be within 2% of `data/house_results.csv`, and statewide totals must be within 1%
of the Secretary of State canvass.

## Features

- **Map:** Leaflet map with zoom and pan. It shows each district, county or
  precinct shaded by margin in a single election, or by the swing between two
  election years. Hover a shape for its margin and click it for full results.
  Click a district to focus it, or click a county to add it to the Trends chart.
  You can switch the base map between Light, Streets and Satellite.
- **Trends:** interactive line chart of the R − D margin over time for each
  district and up to five counties. A bar chart and a table show the +/- swing
  for every district and county.
- **Results / Compare districts:** candidate results for the focus district,
  and the vote share by party for all three districts.
- **Data:** a filterable table you can download as CSV, at any level.

## Data sources and caveats

| Years | Results | Precinct shapes |
|---|---|---|
| 2016, 2018, 2020 | Voting and Election Science Team (Harvard Dataverse) | VEST |
| 2022, 2024 | MIT Election Data + Science Lab precinct returns | NYT 2024 precinct map |
| Districts, counties | U.S. Census TIGER (119th Congress lines, drawn in 2022) | |

- **Before 2022:** the district lines were different, so the 2016–2020
  district values are estimated by reallocating precinct votes onto today's
  lines by area. These rows are flagged as estimated.
- **Precinct swing maps:** older precinct votes are re-estimated by area onto
  the 2024 precinct shapes. Precincts with fewer than 25 votes are shown grey.
- **Precinct name matching:** 2022 and 2024 precinct results are joined to the
  2024 shapes by name. About 91% of precincts in the three districts match; see
  `data/precinct_match_report.csv`. Unmatched precincts appear blank on the map
  but are still counted in the county and district totals.
- **County-level House results** combine every House race on that county's
  ballot.
- **District-level House results** use the official canvass in
  `data/house_results.csv`.
- **2026 (preliminary):** `data/preliminary_2026.csv` holds unofficial U.S. House
  results for AZ-2, 6 and 7 as reported Nov 9–10, 2026 (98–99% reporting),
  district level only and major-party candidates only. Replace it with the
  certified canvass once it's published. County and precinct results, and 2026
  Governor results, will need the official precinct returns.
