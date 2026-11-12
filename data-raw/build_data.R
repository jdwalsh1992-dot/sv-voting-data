# Build the static data used by app.R.
#
# Run once from the project root (needs internet):
#   Rscript data-raw/build_data.R
#
# Downloads are cached in data-raw/cache/. Outputs go to data/:
#   districts.rds      sf  AZ-2, AZ-6, AZ-7 (119th Congress lines, drawn in 2022)
#   counties.rds       sf  counties that overlap those districts
#   precincts.rds      sf  precinct shapes, one set per vintage (geo_set column)
#   results.rds        long table of votes by level / geography / year / race
#
# Sources
#   2016, 2018, 2020  Voting and Election Science Team (VEST) precinct shapefiles,
#                     Harvard Dataverse (president, U.S. Senate, governor)
#   2022, 2024        MIT Election Data + Science Lab precinct returns, Harvard
#                     Dataverse (all races incl. U.S. House)
#   2024 shapes       NYT "presidential-precinct-map-2024" (official county shapes)
#   boundaries        U.S. Census TIGER cartographic boundaries via tigris

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(data.table)
})
sf_use_s2(FALSE)
options(timeout = 1200, tigris_use_cache = TRUE)

cache <- "data-raw/cache"
dir.create(cache, showWarnings = FALSE, recursive = TRUE)
dir.create("data", showWarnings = FALSE)

DISTRICTS <- c("AZ-2", "AZ-6", "AZ-7")
UTM12 <- 26912 # NAD83 / UTM zone 12N, for area calculations

fetch <- function(url, file) {
  path <- file.path(cache, file)
  if (!file.exists(path)) {
    message("Downloading ", file)
    download.file(url, path, mode = "wb", quiet = TRUE)
  }
  path
}

dataverse <- function(id, file) {
  fetch(sprintf("https://dataverse.harvard.edu/api/access/datafile/%s?format=original", id), file)
}

# ---- Boundaries ---------------------------------------------------------------

message("Boundaries")
cd <- tigris::congressional_districts(state = "AZ", cb = TRUE, year = 2024, progress_bar = FALSE) |>
  st_transform(4326) |>
  transmute(district = paste0("AZ-", as.integer(CD119FP))) |>
  filter(district %in% DISTRICTS)

# Map context: the state outline and Arizona's other six districts, drawn
# faintly so the three study districts can be seen in proportion to the state
build_context <- function() {
  others <- tigris::congressional_districts(state = "AZ", cb = TRUE, year = 2024, progress_bar = FALSE) |>
    st_transform(4326) |>
    transmute(name = paste0("AZ-", as.integer(CD119FP)), type = "district") |>
    filter(!name %in% DISTRICTS)
  state <- tigris::states(cb = TRUE, year = 2024, progress_bar = FALSE) |>
    filter(STUSPS == "AZ") |>
    st_transform(4326) |>
    transmute(name = "Arizona", type = "state")
  rmapshaper::ms_simplify(bind_rows(state, others), keep = 0.2, keep_shapes = TRUE)
}

counties_all <- tigris::counties(state = "AZ", cb = TRUE, year = 2024, progress_bar = FALSE) |>
  st_transform(4326) |>
  transmute(county = toupper(NAME), county_fips = GEOID)

# Keep counties where at least 1% of the county lies inside the three districts
study_area <- st_union(st_transform(cd, UTM12))
cty_utm <- st_transform(counties_all, UTM12)
overlap <- as.numeric(st_area(st_intersection(cty_utm, study_area))) /
  as.numeric(st_area(cty_utm[lengths(st_intersects(cty_utm, study_area)) > 0, ]))
counties <- counties_all[lengths(st_intersects(cty_utm, study_area)) > 0, ][overlap > 0.01, ]
message("  counties: ", paste(sort(counties$county), collapse = ", "))

# ---- VEST 2016 / 2018 / 2020 ------------------------------------------------------

# VEST column -> race, party, candidate. Columns not listed here (minor parties,
# write-ins) are summed into "Other".
vest_lookup <- tribble(
  ~column,      ~race,       ~party,       ~candidate,
  "G16PRERTRU", "President", "Republican", "Donald Trump",
  "G16PREDCLI", "President", "Democratic", "Hillary Clinton",
  "G16USSRMCC", "Senate",    "Republican", "John McCain",
  "G16USSDKIR", "Senate",    "Democratic", "Ann Kirkpatrick",
  "G18USSRMCS", "Senate",    "Republican", "Martha McSally",
  "G18USSDSIN", "Senate",    "Democratic", "Kyrsten Sinema",
  "G18GOVRDUC", "Governor",  "Republican", "Doug Ducey",
  "G18GOVDGAR", "Governor",  "Democratic", "David Garcia",
  "G20PRERTRU", "President", "Republican", "Donald Trump",
  "G20PREDBID", "President", "Democratic", "Joe Biden",
  "G20USSRMCS", "Senate",    "Republican", "Martha McSally",
  "G20USSDKEL", "Senate",    "Democratic", "Mark Kelly"
)
vest_offices <- c(PRE = "President", USS = "Senate", GOV = "Governor")

vest_files <- list(
  `2016` = list(id = 4773516, shp = "az_2016.shp"),
  `2018` = list(id = 4773519, shp = "az_2018.shp"),
  `2020` = list(id = 4864722, shp = "az_2020.shp")
)

read_vest <- function(year) {
  info <- vest_files[[year]]
  zip <- dataverse(info$id, paste0("vest_az_", year, ".zip"))
  dir <- file.path(cache, paste0("vest_az_", year))
  if (!dir.exists(dir)) unzip(zip, exdir = dir)
  st_read(file.path(dir, info$shp), quiet = TRUE) |>
    st_make_valid() |>
    mutate(precinct_id = sprintf("vest%s:%s-%s", year, CDE_COUNTY, PCTNUM),
           precinct_name = str_to_title(PRECINCTNA))
}

vest_long <- function(shapes, year) {
  yy <- substr(year, 3, 4)
  cols <- grep(paste0("^G", yy, "(PRE|USS|GOV)"), names(shapes), value = TRUE)
  st_drop_geometry(shapes) |>
    select(precinct_id, all_of(cols)) |>
    pivot_longer(-precinct_id, names_to = "column", values_to = "votes") |>
    left_join(vest_lookup, by = "column") |>
    mutate(
      race      = coalesce(race, unname(vest_offices[substr(column, 4, 6)])),
      party     = coalesce(party, "Other"),
      candidate = coalesce(candidate, "Other"),
      year      = as.integer(year)
    ) |>
    group_by(precinct_id, year, race, party, candidate) |>
    summarise(votes = sum(votes), .groups = "drop")
}

# ---- MEDSL 2022 / 2024 --------------------------------------------------------------

medsl_files <- list(
  `2022` = list(id = 14249794, file = "medsl_2022_az.tab"),
  `2024` = list(id = 13731136, file = "medsl_2024_az.csv")
)
medsl_offices <- c("US HOUSE" = "House", "US SENATE" = "Senate",
                   "GOVERNOR" = "Governor", "US PRESIDENT" = "President")
non_candidates <- c("OVER VOTES", "UNDER VOTES", "OVERVOTES", "UNDERVOTES",
                    "TOTAL VOTES CAST", "REGISTERED VOTERS", "BALLOTS CAST", "")

read_medsl <- function(year) {
  info <- medsl_files[[year]]
  path <- dataverse(info$id, info$file)
  dt <- fread(path, colClasses = "character",
              select = c("precinct", "office", "district", "party_simplified",
                         "candidate", "mode", "votes", "county_name"))
  dt <- dt[office %in% names(medsl_offices) & !candidate %in% non_candidates]
  dt[, votes := as.numeric(votes)]
  # Some counties leave the party blank; take it from the candidate's other rows
  dt[, party_simplified := {
    known <- party_simplified[!is.na(party_simplified) & party_simplified != ""]
    if (length(known)) names(which.max(table(known))) else ""
  }, by = .(office, candidate)]

  # Counties report either a TOTAL row or a breakdown by vote mode (and a few
  # report both). Use TOTAL when present, otherwise sum the modes.
  dt <- dt[, .(votes = if ("TOTAL" %in% mode) sum(votes[mode == "TOTAL"]) else sum(votes)),
           by = .(county_name, precinct, office, district, party_simplified, candidate)]

  as_tibble(dt) |>
    mutate(
      year      = as.integer(year),
      race      = unname(medsl_offices[office]),
      district  = ifelse(race == "House", paste0("AZ-", as.integer(district)), NA),
      party     = case_when(party_simplified == "DEMOCRAT"   ~ "Democratic",
                            party_simplified == "REPUBLICAN" ~ "Republican",
                            TRUE ~ "Other"),
      candidate = ifelse(party == "Other", "Other", str_to_title(candidate)),
      county    = county_name
    ) |>
    group_by(year, county, precinct, race, district, party, candidate) |>
    summarise(votes = sum(votes, na.rm = TRUE), .groups = "drop")
}

# Precinct names differ between MEDSL and the NYT shapes ("02 BE J_SIX" vs
# "BE J-SIX", "Precinct 001" vs "1"). Normalise both sides to a common key.
name_key <- function(x) {
  x <- toupper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_replace_all(x, "\\b(PRECINCT|PCT)\\b", " ")
  x <- str_squish(x)
  num_only <- str_detect(x, "^[0-9]+$")
  x[num_only] <- as.character(as.integer(x[num_only]))
  # Drop a leading sequence number when a name follows ("02 BE J SIX" -> "BE J SIX")
  x[!num_only] <- str_replace(x[!num_only], "^[0-9]+ (?=[A-Z])", "")
  # Drop leading zeros inside numbers ("NOGALES 01" -> "NOGALES 1")
  str_replace_all(x, "\\b0+([0-9])", "\\1")
}

alpha_key <- function(key) str_squish(str_replace_all(key, "\\b[0-9]+\\b", " "))
num_key   <- function(key) str_extract(key, "\\b[0-9]+\\b")

# Match MEDSL (county, precinct) pairs to NYT precinct_ids. Tries, in order:
# the full normalised name, the name without numbers ("PAGE WEST 75" ->
# "PAGE WEST"), the first precinct number ("BACA 19 19 1" -> "19"), then a
# close spelling ("CAVALRY WEB" -> "CALVARY WEBB"). Each step only accepts keys
# that are unique among the county's NYT precincts. Several MEDSL rows may map
# to one shape (split precincts), never the reverse.
match_precincts <- function(medsl, shapes) {
  medsl <- mutate(medsl, key = name_key(precinct))
  out <- list()
  remaining <- medsl
  for (step in c("key", "alpha", "num")) {
    f <- switch(step, key = identity, alpha = alpha_key, num = num_key)
    lut <- shapes |>
      mutate(k = f(key)) |>
      filter(!is.na(k), k != "") |>
      group_by(county, k) |> filter(n() == 1) |> ungroup() |>
      select(county, k, precinct_id)
    hit <- remaining |> mutate(k = f(key)) |> inner_join(lut, by = c("county", "k"))
    out[[step]] <- select(hit, county, precinct, precinct_id)
    remaining <- anti_join(remaining, hit, by = c("county", "precinct"))
  }
  # Fuzzy: edit distance <= 2 and a single closest candidate
  used <- bind_rows(out)$precinct_id
  fuzzy <- remaining |>
    rowwise() |>
    mutate(precinct_id = {
      cand <- shapes[shapes$county == county & !shapes$precinct_id %in% used, ]
      d <- if (nrow(cand)) adist(key, cand$key)[1, ] else integer()
      if (length(d) && min(d) <= 2 && sum(d == min(d)) == 1) cand$precinct_id[which.min(d)] else NA_character_
    }) |>
    ungroup() |>
    filter(!is.na(precinct_id))
  bind_rows(c(out, list(select(fuzzy, county, precinct, precinct_id))))
}

# ---- NYT 2024 precinct shapes -----------------------------------------------------

message("NYT 2024 precinct shapes")
nyt_path <- fetch("https://int.nyt.com/newsgraphics/elections/map-data/2024/national/AZ-precincts-with-results.geojson.gz",
                  "nyt_az_2024.geojson.gz")
nyt_geojson <- file.path(cache, "nyt_az_2024.geojson")
if (!file.exists(nyt_geojson)) {
  con <- gzfile(nyt_path, "rb"); writeBin(readBin(con, "raw", 2e8), nyt_geojson); close(con)
}
nyt <- st_read(nyt_geojson, quiet = TRUE) |>
  st_make_valid() |>
  mutate(
    county_fips = substr(GEOID, 1, 5),
    nyt_name    = substring(GEOID, 7)
  ) |>
  left_join(st_drop_geometry(counties_all), by = "county_fips") |>
  mutate(precinct_id = paste0("cur:", GEOID),
         precinct_name = nyt_name,
         key = name_key(nyt_name))

# ---- Study-area precinct shapes ------------------------------------------------------

in_study_area <- function(shapes) {
  pts <- st_point_on_surface(st_transform(shapes, UTM12))
  lengths(st_intersects(pts, study_area)) > 0
}

simplify <- function(shapes, keep = 0.12) {
  rmapshaper::ms_simplify(shapes, keep = keep, keep_shapes = TRUE)
}

# Areal-weighted interpolation of vote counts from `long` (one row per
# id x key columns) on `src` shapes onto `target` shapes. Returns long rows
# with `target_id` and the key columns.
interp_votes <- function(long, src, target, target_ids, key_cols) {
  keys <- distinct(long, across(all_of(key_cols))) |> mutate(k = paste0("k", row_number()))
  wide <- long |>
    inner_join(keys, by = key_cols) |>
    group_by(id, k) |> summarise(votes = sum(votes), .groups = "drop") |>
    pivot_wider(names_from = k, values_from = votes, values_fill = 0)
  src <- src |> select(id) |> inner_join(wide, by = "id")
  est <- st_interpolate_aw(st_transform(select(src, -id), UTM12),
                           st_transform(target, UTM12), extensive = TRUE)
  st_drop_geometry(est) |>
    mutate(target_id = target_ids) |>
    pivot_longer(-target_id, names_to = "k", values_to = "votes") |>
    inner_join(keys, by = "k") |>
    select(-k)
}

assign_points <- function(shapes, polys, col) {
  pts <- st_point_on_surface(st_transform(shapes, UTM12))
  idx <- st_nearest_feature(pts, st_transform(polys, UTM12))
  polys[[col]][idx]
}

# ---- Assemble results ---------------------------------------------------------------

message("VEST 2016-2020")
vest_shapes <- list()
vest_results <- list()
for (yr in names(vest_files)) {
  shp <- read_vest(yr) |> st_transform(4326)
  shp$county <- assign_points(shp, counties_all, "county")
  long <- vest_long(shp, yr)

  # District totals: areal-weighted interpolation of each precinct's votes onto
  # the current district lines (older precincts can straddle the new lines).
  district_long <- interp_votes(
    rename(long, id = precinct_id), rename(shp, id = precinct_id),
    st_geometry(cd), cd$district, c("race", "party", "candidate")
  ) |>
    rename(geo_id = target_id) |>
    mutate(level = "district", year = as.integer(yr), votes = round(votes), estimated = TRUE)

  county_long <- long |>
    left_join(st_drop_geometry(shp) |> select(precinct_id, county), by = "precinct_id") |>
    filter(county %in% counties$county) |>
    group_by(geo_id = county, year, race, party, candidate) |>
    summarise(votes = sum(votes), .groups = "drop") |>
    mutate(level = "county", estimated = FALSE)

  keep <- in_study_area(shp)
  precinct_long <- long |>
    filter(precinct_id %in% shp$precinct_id[keep]) |>
    rename(geo_id = precinct_id) |>
    mutate(level = "precinct", estimated = FALSE)

  vest_shapes[[yr]] <- shp[keep, ] |>
    transmute(geo_id = precinct_id, geo_set = paste0("vest", yr), precinct_name, county)
  vest_results[[yr]] <- bind_rows(district_long, county_long, precinct_long)
  statewide <- long |> group_by(year, race, candidate) |> summarise(votes = sum(votes), .groups = "drop")
  vest_results[[paste0(yr, "_state")]] <- statewide |> mutate(level = "state")
}

message("MEDSL 2022 / 2024")
nyt_study <- nyt[in_study_area(nyt), ]
medsl_results <- list()
match_report <- list()
for (yr in names(medsl_files)) {
  m <- read_medsl(yr) |> mutate(key = name_key(precinct))

  # Which current district each precinct votes in, from its House race. A
  # handful of precincts are split; use the district with the most House votes.
  precinct_cd <- m |>
    filter(race == "House") |>
    group_by(county, precinct, district) |>
    summarise(v = sum(votes), .groups = "drop_last") |>
    slice_max(v, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(county, precinct, cd = district)
  m <- m |> left_join(precinct_cd, by = c("county", "precinct"))

  # House district totals come from each race's own district; statewide races are
  # attributed to the precinct's district.
  district_long <- m |>
    mutate(geo_id = ifelse(race == "House", district, cd)) |>
    filter(geo_id %in% DISTRICTS) |>
    group_by(geo_id, year, race, party, candidate) |>
    summarise(votes = sum(votes), .groups = "drop") |>
    mutate(level = "district", estimated = FALSE)

  # County House results combine whichever House races the county votes in.
  county_long <- m |>
    filter(county %in% counties$county) |>
    mutate(candidate = ifelse(race == "House", party, candidate)) |>
    group_by(geo_id = county, year, race, party, candidate) |>
    summarise(votes = sum(votes), .groups = "drop") |>
    mutate(level = "county", estimated = FALSE)

  # Precinct results: attach to NYT shapes by county + precinct name
  crosswalk <- match_precincts(distinct(m, county, precinct), st_drop_geometry(nyt_study))
  matched <- m |> inner_join(crosswalk, by = c("county", "precinct"))
  study_precincts <- m |> filter(cd %in% DISTRICTS) |> distinct(county, precinct)
  match_report[[yr]] <- study_precincts |>
    left_join(distinct(matched, county, precinct, matched = TRUE), by = c("county", "precinct")) |>
    group_by(county) |>
    summarise(precincts = n(), matched = sum(!is.na(matched)), .groups = "drop") |>
    mutate(year = as.integer(yr))

  precinct_long <- matched |>
    mutate(candidate = ifelse(race == "House", paste0(candidate, " (", district, ")"), candidate)) |>
    group_by(geo_id = precinct_id, year, race, party, candidate) |>
    summarise(votes = sum(votes), .groups = "drop") |>
    mutate(level = "precinct", estimated = FALSE)

  statewide <- m |> filter(race != "House") |>
    group_by(year, race, candidate) |> summarise(votes = sum(votes), .groups = "drop") |>
    mutate(level = "state")
  medsl_results[[yr]] <- bind_rows(district_long, county_long, precinct_long, statewide)
}
match_report <- bind_rows(match_report)
message("Precinct name match rate (precincts in AZ-2/6/7):")
print(match_report |> group_by(year) |> summarise(precincts = sum(precincts), matched = sum(matched)))

results_all <- bind_rows(c(vest_results, medsl_results))

# ---- Validation ------------------------------------------------------------------------

message("Validating")
problems <- character()
check <- function(label, got, want, tol) {
  diff <- abs(got - want) / want
  status <- if (is.na(diff) || diff > tol) "FAIL" else if (diff > 0) "close" else "exact"
  message(sprintf("  %-44s got %10s  want %10s  %s", label,
                  format(got, big.mark = ","), format(want, big.mark = ","), status))
  if (status == "FAIL") problems <<- c(problems, label)
}

# Official U.S. House results (data/house_results.csv), rebuilt from precincts
official_house <- read.csv("data/house_results.csv", stringsAsFactors = FALSE) |>
  filter(party %in% c("Democratic", "Republican"))
for (i in seq_len(nrow(official_house))) {
  o <- official_house[i, ]
  got <- results_all |>
    filter(level == "district", race == "House", year == o$year,
           geo_id == o$district, party == o$party) |>
    pull(votes) |> sum()
  check(sprintf("%s %s House %s", o$year, o$district, o$party), got, o$votes, tol = 0.02)
}

# Official statewide canvass totals (Arizona Secretary of State)
canvass <- tribble(
  ~year, ~race,       ~candidate,         ~votes,
  2016,  "President", "Donald Trump",     1252401,
  2016,  "President", "Hillary Clinton",  1161167,
  2016,  "Senate",    "John McCain",      1359267,
  2016,  "Senate",    "Ann Kirkpatrick",  1031245,
  2018,  "Senate",    "Kyrsten Sinema",   1191100,
  2018,  "Senate",    "Martha McSally",   1135200,
  2018,  "Governor",  "Doug Ducey",       1330863,
  2018,  "Governor",  "David Garcia",      994341,
  2020,  "President", "Joe Biden",        1672143,
  2020,  "President", "Donald Trump",     1661686,
  2020,  "Senate",    "Mark Kelly",       1716467,
  2020,  "Senate",    "Martha McSally",   1637661,
  2022,  "Governor",  "Katie Hobbs",      1287891,
  2022,  "Governor",  "Kari Lake",        1270774,
  2022,  "Senate",    "Mark Kelly",       1322027,
  2022,  "Senate",    "Blake Masters",    1196308,
  2024,  "President", "Donald J Trump",   1770242,
  2024,  "President", "Kamala D Harris",  1582860,
  2024,  "Senate",    "Ruben Gallego",    1676335,
  2024,  "Senate",    "Kari Lake",        1595761
)
for (i in seq_len(nrow(canvass))) {
  o <- canvass[i, ]
  got <- results_all |>
    filter(level == "state", year == o$year, race == o$race, candidate == o$candidate) |>
    pull(votes) |> sum()
  check(sprintf("%s %s %s (statewide)", o$year, o$race, o$candidate), got, o$votes, tol = 0.01)
}
if (length(problems)) stop("Validation failed: ", paste(problems, collapse = "; "))

# ---- Precinct swing on current shapes ---------------------------------------------------

# Swing maps compare two elections on the same polygons. Re-estimate each VEST
# year's two-party vote onto the current (2024) precinct shapes by area.
message("Interpolating 2016-2020 precinct votes onto current precinct shapes")
cur_shapes <- nyt_study |> select(precinct_id)
interp_current <- list()
for (yr in names(vest_files)) {
  src_long <- vest_results[[yr]] |>
    filter(level == "precinct") |>
    rename(id = geo_id)
  interp_current[[yr]] <- interp_votes(
    src_long, rename(vest_shapes[[yr]], id = geo_id),
    st_geometry(cur_shapes), cur_shapes$precinct_id, c("race", "party")
  ) |>
    rename(geo_id = target_id) |>
    mutate(year = as.integer(yr), level = "precinct_current", candidate = party,
           votes = round(votes, 1), estimated = TRUE)
}
# 2022/2024 precinct results already sit on the current shapes
current_actual <- bind_rows(medsl_results) |>
  filter(level == "precinct") |>
  mutate(level = "precinct_current", estimated = FALSE)

results <- bind_rows(results_all, bind_rows(interp_current), current_actual) |>
  filter(level != "state") |>
  select(level, geo_id, year, race, party, candidate, votes, estimated)

# ---- Write -----------------------------------------------------------------------------

# Simplify each vintage on its own: the sets overlap, and simplifying them
# together makes rmapshaper keep every shared vertex.
precincts <- c(
  vest_shapes,
  list(nyt_study |> transmute(geo_id = precinct_id, geo_set = "current", precinct_name, county))
) |>
  lapply(function(x) simplify(st_cast(x, "MULTIPOLYGON"), keep = 0.08)) |>
  bind_rows() |>
  st_set_precision(1e5) |>
  st_make_valid()

saveRDS(simplify(cd, keep = 0.2), "data/districts.rds")
saveRDS(build_context(), "data/context.rds")
saveRDS(simplify(counties, keep = 0.2), "data/counties.rds")
saveRDS(precincts, "data/precincts.rds")
saveRDS(results, "data/results.rds")
write.csv(match_report, "data/precinct_match_report.csv", row.names = FALSE)

message("Done. Wrote ", nrow(results), " result rows and ", nrow(precincts), " precinct shapes.")
