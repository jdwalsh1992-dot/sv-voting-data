# Download candidate photos from Wikipedia / Wikimedia Commons into
# www/candidates/ and write their credits to data/candidate_photos.csv.
# Only freely licensed images (public domain, CC0, CC BY, CC BY-SA) are kept;
# candidates without one get an initials avatar in the app.
#
# Run from the project root (needs internet):  Rscript data-raw/fetch_photos.R

library(jsonlite)

UA <- "sv-voting-data/1.0 (https://github.com/jdwalsh1992-dot/sv-voting-data)"

# Candidate name as it appears in the app's results -> Wikipedia article title
# (NA = no article). Name variants in the source data share one photo.
candidates <- c(
  "Doug Ducey"         = "Doug Ducey",
  "David Garcia"       = "David Garcia (Arizona politician)",
  "Katie Hobbs"        = "Katie Hobbs",
  "Kari Lake"          = "Kari Lake",
  "Juan Ciscomani"     = "Juan Ciscomani",
  "Eli Crane"          = "Eli Crane",
  "Kirsten Engel"      = "Kirsten Engel",
  "Tom O'Halleran"     = "Tom O'Halleran",
  "Luis Pozzolo"       = NA,
  "Raúl Grijalva"      = "Raúl Grijalva",
  "Jonathan Nez"       = "Jonathan Nez",
  "Daniel Butierez"    = NA,
  "Athena Eastwood"    = NA,
  "JoAnna Mendoza"     = "JoAnna Mendoza",
  "Adelita Grijalva"   = "Adelita Grijalva",
  "Hillary Clinton"    = "Hillary Clinton",
  "Donald Trump"       = "Donald Trump",
  "Donald J Trump"     = "Donald Trump",
  "Joe Biden"          = "Joe Biden",
  "Kamala D Harris"    = "Kamala Harris",
  "John McCain"        = "John McCain",
  "Ann Kirkpatrick"    = "Ann Kirkpatrick",
  "Kyrsten Sinema"     = "Kyrsten Sinema",
  "Martha McSally"     = "Martha McSally",
  "Mark Kelly"         = "Mark Kelly",
  "Blake Masters"      = "Blake Masters",
  "Ruben Gallego"      = "Ruben Gallego"
)

get_json <- function(url) {
  h <- curl::new_handle(useragent = UA)
  fromJSON(rawToChar(curl::curl_fetch_memory(url, handle = h)$content), simplifyVector = FALSE)
}
strip_html <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", x %||% "")))
`%||%` <- function(a, b) if (is.null(a)) b else a
slug <- function(x) gsub("[^a-z0-9]+", "-", gsub("['`^~\"]", "", tolower(iconv(x, to = "ASCII//TRANSLIT"))))

dir.create("www/candidates", recursive = TRUE, showWarnings = FALSE)
FREE <- "public domain|^pd|cc0|cc[ -]by"

rows <- list()
for (title in unique(na.omit(candidates))) {
  summ <- get_json(paste0("https://en.wikipedia.org/api/rest_v1/page/summary/",
                          curl::curl_escape(gsub(" ", "_", title))))
  src <- summ$originalimage$source
  if (is.null(src)) { message("no image: ", title); next }
  # .../commons/a/ab/File.jpg, or .../commons/thumb/a/ab/File.jpg/3840px-File.jpg
  parts <- strsplit(sub("\\?.*$", "", src), "/")[[1]]
  file <- URLdecode(if ("thumb" %in% parts) parts[length(parts) - 1] else parts[length(parts)])

  info <- get_json(paste0(
    "https://commons.wikimedia.org/w/api.php?action=query&format=json&prop=imageinfo",
    "&iiprop=url|extmetadata&iiurlwidth=240&titles=", curl::curl_escape(paste0("File:", file))))
  ii <- info$query$pages[[1]]$imageinfo[[1]]
  if (is.null(ii)) { message("not on Commons: ", title); next }
  meta <- ii$extmetadata
  license <- strip_html(meta$LicenseShortName$value)
  if (!grepl(FREE, license, ignore.case = TRUE)) { message("skipped (", license, "): ", title); next }

  out <- file.path("www/candidates", paste0(slug(title), ".jpg"))
  curl::curl_download(ii$thumburl, out, handle = curl::new_handle(useragent = UA))
  rows[[title]] <- data.frame(title = title, photo = sub("^www/", "", out), license = license,
                              artist = strip_html(meta$Artist$value),
                              source = ii$descriptionurl)
  message("ok: ", title, " (", license, ")")
  Sys.sleep(0.5)
}

photos <- do.call(rbind, rows)
map <- data.frame(candidate = names(candidates), title = unname(candidates))
out <- merge(map, photos, by = "title")[, c("candidate", "photo", "license", "artist", "source")]
write.csv(out[order(out$candidate), ], "data/candidate_photos.csv", row.names = FALSE)
message(nrow(out), " candidate names have photos")
