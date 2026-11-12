# Build the static GitHub Pages site in docs/ with Shinylive (R runs in the
# visitor's browser through webR, so no server is needed). The GitHub Action in
# .github/workflows/pages.yml runs this on every push to main and deploys docs/;
# docs/ itself is not committed.
#
# Run from the project root:  Rscript build_site.R
# Preview locally:            Rscript -e 'httpuv::runStaticServer("docs", port = 8008)'

# Copy only what the app needs: shinylive bundles every file in the app
# directory, and data-raw/cache/ alone is ~400 MB.
stage <- file.path(tempdir(), "sv-app")
unlink(stage, recursive = TRUE)
dir.create(stage)
file.copy(c("app.R", "R", "data"), stage, recursive = TRUE)

unlink("docs", recursive = TRUE)
shinylive::export(stage, "docs")

# Tell GitHub Pages to serve the files as-is (no Jekyll processing)
file.create(file.path("docs", ".nojekyll"))
