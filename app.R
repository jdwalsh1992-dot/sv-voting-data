# Sierra Vista Area Election Explorer
# Static election results for Arizona's 6th congressional district (which
# contains Sierra Vista) and its neighbors, AZ-7 and AZ-2, at district, county
# and precinct level, 2016-2024.
#
# Build the data once with: Rscript data-raw/build_data.R
# Then run with:            shiny::runApp()

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(leaflet)
library(sf)
library(plotly)

# ---- Data -------------------------------------------------------------------

districts_sf <- readRDS("data/districts.rds") |> rename(geo_id = district)
counties_sf  <- readRDS("data/counties.rds")  |> rename(geo_id = county)
precincts_sf <- readRDS("data/precincts.rds")
context_sf   <- readRDS("data/context.rds") # state outline + AZ's other districts
district_info <- read.csv("data/districts.csv", stringsAsFactors = FALSE)
DISTRICTS <- district_info$district

# District-level House results come from the official canvass file; everything
# else is rebuilt from precinct returns by data-raw/build_data.R.
house_official <- read.csv("data/house_results.csv", stringsAsFactors = FALSE) |>
  transmute(level = "district", geo_id = district, year, race = "House",
            party, candidate, votes, estimated = FALSE)

# Unofficial 2026 House results (district level only) until the canvass is
# certified and precinct returns are published
prelim_2026 <- read.csv("data/preliminary_2026.csv", stringsAsFactors = FALSE)
PRELIM_YEARS <- unique(prelim_2026$year)

results <- readRDS("data/results.rds") |>
  filter(!(level == "district" & race == "House")) |>
  bind_rows(house_official) |>
  bind_rows(transmute(prelim_2026, level = "district", geo_id = district, year, race = "House",
                      party, candidate, votes, estimated = FALSE))

race_years <- results |>
  filter(level == "district") |>
  distinct(race, year) |>
  arrange(year)
years_for <- function(race) race_years$year[race_years$race == race]
is_prelim <- function(year) year %in% PRELIM_YEARS
year_label <- function(year) ifelse(is_prelim(year), paste(year, "(preliminary)"), as.character(year))
year_choices <- function(years) setNames(years, year_label(years))

RACES <- c("U.S. House" = "House", "President" = "President",
           "U.S. Senate" = "Senate", "Governor" = "Governor")

# Display names for every geography, keyed by geo_id
geo_titles <- c(
  setNames(DISTRICTS, DISTRICTS),
  setNames(paste(tools::toTitleCase(tolower(counties_sf$geo_id)), "County"), counties_sf$geo_id),
  setNames(paste0(precincts_sf$precinct_name, " (",
                  tools::toTitleCase(tolower(precincts_sf$county)), " Co.)"),
           precincts_sf$geo_id)
)

district_colors <- c("AZ-6" = "#6A3D9A", "AZ-7" = "#1B9E77", "AZ-2" = "#D95F02")
county_colors   <- c("#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")

SIERRA_VISTA <- c(lng = -110.3037, lat = 31.5545)
study_bbox <- unname(st_bbox(districts_sf))

# ---- UI ---------------------------------------------------------------------

ui <- page_sidebar(
  title = "Sierra Vista Area Election Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  sidebar = sidebar(
    width = 300,
    selectInput("race", "Race", choices = RACES),
    radioButtons("mode", "View",
                 choices = c("Single election" = "single", "Change between years" = "change")),
    conditionalPanel(
      "input.mode == 'single'",
      selectInput("year", "Election year", choices = NULL)
    ),
    conditionalPanel(
      "input.mode == 'change'",
      layout_columns(
        selectInput("from", "From", choices = NULL),
        selectInput("to", "To", choices = NULL)
      )
    ),
    radioButtons("level", "Map level", inline = TRUE,
                 choices = c("District" = "district", "County" = "county", "Precinct" = "precinct")),
    selectInput("focus", "Focus district",
                choices = setNames(DISTRICTS, paste0(DISTRICTS, " — ", district_info$relation)),
                selected = "AZ-6"),
    actionButton("zoom_focus", "Zoom map to district", icon = icon("magnifying-glass-location"),
                 class = "btn-sm btn-outline-secondary"),
    hr(),
    uiOutput("district_info"),
    p(class = "small text-muted",
      "Sources: AZ Secretary of State canvass; MIT Election Data + Science Lab (2022, 2024",
      "precinct returns); Voting and Election Science Team (2016–2020 precinct shapes);",
      "NYT 2024 precinct shapes; U.S. Census TIGER boundaries.",
      "2026 House results are unofficial (as reported Nov 9–10, 2026).",
      "District lines are the current (2022) map.")
  ),

  layout_columns(
    fill = FALSE,
    value_box(textOutput("vb1_title"), textOutput("vb1"), showcase = icon("trophy")),
    value_box(textOutput("vb2_title"), textOutput("vb2"), showcase = icon("scale-balanced")),
    value_box(textOutput("vb3_title"), textOutput("vb3"), showcase = icon("check-to-slot"))
  ),

  navset_card_tab(
    id = "tabs",
    nav_panel(
      "Map",
      card_body(
        padding = 0,
        leafletOutput("map", height = "100%")
      ),
      card_footer(class = "small text-muted", uiOutput("map_note"))
    ),
    nav_panel(
      "Trends",
      layout_columns(
        col_widths = c(7, 5),
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex gap-3 align-items-center flex-wrap",
            "Margin over time",
            div(style = "width: 190px; margin-bottom: -1rem;",
                selectInput("trend_series", NULL,
                            choices = c("Top of ticket" = "top", RACES[-1], "U.S. House" = "House"))),
            div(style = "min-width: 220px; flex: 1; margin-bottom: -1rem;",
                selectizeInput("trend_counties", NULL, multiple = TRUE,
                               choices = setNames(counties_sf$geo_id, geo_titles[counties_sf$geo_id]),
                               selected = "COCHISE",
                               options = list(maxItems = 5, placeholder = "Add counties…")))
          ),
          plotlyOutput("trend_plot"),
          card_footer(class = "small text-muted",
                      "R − D share of all votes. Above zero = Republican lead.",
                      "“Top of ticket” uses president in 2016/2020/2024 and governor in 2018/2022.",
                      "District values for 2016–2020 are estimated onto today’s lines.",
                      "2026 House values are preliminary.")
        ),
        card(
          full_screen = TRUE,
          card_header(textOutput("swing_title", inline = TRUE)),
          plotlyOutput("swing_plot"),
          card_footer(class = "small text-muted",
                      "Bars right = shift toward Republicans; left = toward Democrats.")
        )
      ),
      card(
        card_header("Change by district and county"),
        DTOutput("swing_table")
      )
    ),
    nav_panel(
      "Results",
      plotlyOutput("result_plot")
    ),
    nav_panel(
      "Compare districts",
      plotlyOutput("compare_plot")
    ),
    nav_panel(
      "Data",
      layout_columns(
        fill = FALSE,
        radioButtons("data_level", NULL, inline = TRUE,
                     choices = c("District" = "district", "County" = "county", "Precinct" = "precinct")),
        div(class = "text-end", downloadButton("download", "Download CSV", class = "btn-sm"))
      ),
      DTOutput("table")
    )
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  # -- Year pickers follow the race -----------------------------------------
  observeEvent(input$race, {
    yrs <- years_for(input$race)
    updateSelectInput(session, "year", choices = year_choices(rev(yrs)), selected = max(yrs))
    updateSelectInput(session, "from", choices = year_choices(yrs), selected = yrs[length(yrs) - 1])
    updateSelectInput(session, "to",   choices = year_choices(yrs), selected = max(yrs))
  })

  sel_year <- reactive({ req(input$year); as.integer(input$year) })
  sel_from <- reactive({ req(input$from); as.integer(input$from) })
  sel_to   <- reactive({ req(input$to);   as.integer(input$to) })
  # Year shown in single-election views (Results, Compare)
  shown_year <- reactive(if (input$mode == "change") sel_to() else sel_year())

  valid_change <- reactive({
    validate(need(sel_from() != sel_to(), "Pick two different years to compare."))
    TRUE
  })

  # -- Sidebar info ------------------------------------------------------------
  output$district_info <- renderUI({
    d <- district_info[district_info$district == input$focus, ]
    tagList(
      h6(d$name),
      p(class = "small mb-1", strong("Representative: "), d$representative),
      p(class = "small", d$area)
    )
  })

  # -- Value boxes ---------------------------------------------------------------
  focus_margins <- reactive({
    results |>
      filter(level == "district", geo_id == input$focus, race == input$race) |>
      margin_summary()
  })

  output$vb1_title <- renderText(
    if (input$mode == "single") paste(input$focus, if (is_prelim(sel_year())) "leader (preliminary)" else "winner")
    else paste(input$focus, "margin,", year_label(sel_from()))
  )
  output$vb2_title <- renderText(
    if (input$mode == "single") "Margin" else paste(input$focus, "margin,", year_label(sel_to()))
  )
  output$vb3_title <- renderText(
    if (input$mode == "single") "Total votes" else "Swing"
  )

  output$vb1 <- renderText({
    m <- focus_margins()
    if (input$mode == "single") {
      top <- results |>
        filter(level == "district", geo_id == input$focus, race == input$race, year == sel_year()) |>
        slice_max(votes, n = 1)
      sprintf("%s (%s)", top$candidate, substr(top$party, 1, 1))
    } else {
      fmt_margin(m$margin[m$year == sel_from()])
    }
  })

  output$vb2 <- renderText({
    m <- focus_margins()
    if (input$mode == "single") fmt_margin(m$margin[m$year == sel_year()])
    else fmt_margin(m$margin[m$year == sel_to()])
  })

  output$vb3 <- renderText({
    m <- focus_margins()
    if (input$mode == "single") {
      format(round(m$total[m$year == sel_year()]), big.mark = ",")
    } else {
      valid_change()
      fmt_swing(m$margin[m$year == sel_to()] - m$margin[m$year == sel_from()])
    }
  })

  # -- Map -----------------------------------------------------------------------
  map_data <- reactive({
    lvl <- input$level
    race <- input$race

    if (input$mode == "single") {
      yr <- sel_year()
      shapes <- switch(
        lvl,
        district = districts_sf,
        county   = counties_sf,
        precinct = filter(precincts_sf, geo_set == if (yr >= 2022) "current" else paste0("vest", yr))
      )
      res <- filter(results, level == lvl, race == !!race, year == yr)
      vals <- margin_summary(res) |> transmute(geo_id, value = margin)
      pops <- popup_html(res, geo_titles) |>
        left_join(vals, by = "geo_id") |>
        mutate(html = paste0(html, "<div style='margin-top:4px'><b>Margin: ", fmt_margin(value), "</b></div>"))
      list(shapes = shapes, values = pops, limit = 50, kind = "margin",
           estimated = FALSE)
    } else {
      valid_change()
      from <- sel_from(); to <- sel_to()
      res_level <- if (lvl == "precinct") "precinct_current" else lvl
      shapes <- switch(lvl,
                       district = districts_sf,
                       county   = counties_sf,
                       precinct = filter(precincts_sf, geo_set == "current"))
      s <- swing(filter(results, level == res_level, race == !!race), from, to) |>
        # A handful of votes makes the margin meaningless; leave those grey
        mutate(swing = ifelse(pmin(total_from, total_to) < 25, NA_real_, swing))
      vals <- s |> transmute(
        geo_id, value = swing,
        html = sprintf(
          "<b>%s</b><table style='font-size:12px;margin-top:4px'>
             <tr><td>%d margin</td><td align='right'>%s</td></tr>
             <tr><td>%d margin</td><td align='right'>%s</td></tr>
             <tr><td><b>Swing</b></td><td align='right'><b>%s</b></td></tr>
             <tr><td>Votes</td><td align='right'>%s → %s</td></tr></table>%s",
          htmltools::htmlEscape(geo_titles[geo_id]), from, fmt_margin(margin_from),
          to, fmt_margin(margin_to), fmt_swing(swing),
          format(round(total_from), big.mark = ","), format(round(total_to), big.mark = ","),
          ifelse(estimated | (lvl == "precinct" & from < 2022) | (lvl == "district" & from < 2022),
                 "<div class='text-muted' style='font-size:11px'>Includes estimated values (votes re-mapped onto current lines).</div>", "")
        )
      )
      list(shapes = shapes, values = vals, limit = 20, kind = "swing",
           estimated = from < 2022 && lvl != "county")
    }
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5)) |>
      addMapPane("fill", zIndex = 410) |>
      addMapPane("outline", zIndex = 420) |>
      # Esri gray canvas: keyless (CARTO basemaps now require an API key)
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
      addProviderTiles(providers$OpenStreetMap, group = "Streets") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addPolygons(data = filter(context_sf, type == "district"), fill = FALSE, weight = 1,
                  color = "#888", opacity = 0.8, dashArray = "4 4", group = "Rest of Arizona",
                  options = pathOptions(pane = "outline", interactive = FALSE)) |>
      addLabelOnlyMarkers(data = st_point_on_surface(filter(context_sf, type == "district", name == "AZ-9")),
                          label = ~name, group = "Rest of Arizona",
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                                      style = list("color" = "#777", "font-size" = "13px"))) |>
      addLabelOnlyMarkers(lng = -112.07, lat = 33.45, label = "Phoenix metro (AZ-1, 3, 4, 5, 8)",
                          group = "Rest of Arizona",
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "left",
                                                      style = list("color" = "#777", "font-size" = "11px"))) |>
      addPolygons(data = filter(context_sf, type == "state"), fill = FALSE, weight = 2.5,
                  color = "#555", opacity = 0.9, group = "Rest of Arizona",
                  options = pathOptions(pane = "outline", interactive = FALSE)) |>
      addPolygons(data = districts_sf, fill = FALSE, weight = 3, color = "#222", opacity = 0.9,
                  group = "District lines",
                  options = pathOptions(pane = "outline", interactive = FALSE)) |>
      addLabelOnlyMarkers(data = st_point_on_surface(districts_sf), label = ~geo_id,
                          group = "District lines",
                          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                                      style = list("font-weight" = "bold",
                                                                   "font-size" = "14px",
                                                                   "text-shadow" = "0 0 3px #fff"))) |>
      addCircleMarkers(lng = SIERRA_VISTA[["lng"]], lat = SIERRA_VISTA[["lat"]], radius = 7,
                       color = "#000", weight = 2, fillColor = "#FFD200", fillOpacity = 1,
                       label = "Sierra Vista", group = "Sierra Vista",
                       options = pathOptions(pane = "outline")) |>
      addLayersControl(baseGroups = c("Light", "Streets", "Satellite"),
                       overlayGroups = c("District lines", "Rest of Arizona", "Sierra Vista"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      addScaleBar(position = "bottomleft") |>
      addEasyButton(easyButton(icon = "fa-globe", title = "Show all three districts",
                               onClick = JS(sprintf("function(btn, map){ map.fitBounds([[%f,%f],[%f,%f]]); }",
                                                    study_bbox[2], study_bbox[1], study_bbox[4], study_bbox[3])))) |>
      fitBounds(study_bbox[1], study_bbox[2], study_bbox[3], study_bbox[4])
  })

  # Proxy updates must wait until the map widget exists in the browser
  map_ready <- reactiveVal(FALSE)
  observeEvent(input$map_zoom, map_ready(TRUE), once = TRUE)

  observe({
    req(map_ready())
    d <- map_data()
    shapes <- inner_join(d$shapes, d$values, by = "geo_id")
    if (nrow(shapes) == 0) {
      # e.g. 2026, which only has preliminary district-level results so far
      leafletProxy("map") |> clearGroup("fill") |> removeControl("legend")
      return()
    }
    pal <- margin_palette(d$limit)
    is_precinct <- input$level == "precinct"
    labels <- lapply(sprintf("<b>%s</b><br>%s", htmltools::htmlEscape(geo_titles[shapes$geo_id]),
                             if (d$kind == "margin") fmt_margin(shapes$value) else fmt_swing(shapes$value)),
                     htmltools::HTML)

    leafletProxy("map") |>
      clearGroup("fill") |>
      removeControl("legend") |>
      addPolygons(data = shapes, group = "fill", layerId = ~geo_id,
                  fillColor = pal(squish(shapes$value, d$limit)), fillOpacity = 0.75,
                  color = "#ffffff", weight = if (is_precinct) 0.4 else 1.2, opacity = 1,
                  label = labels, popup = ~html,
                  highlightOptions = highlightOptions(weight = 3, color = "#333", bringToFront = TRUE),
                  options = pathOptions(pane = "fill")) |>
      addLegend("bottomright", pal = pal, values = c(-d$limit, d$limit), layerId = "legend",
                bins = 5, opacity = 0.85,
                title = if (d$kind == "margin") paste("Margin,", year_label(sel_year()))
                        else paste0("Swing ", sel_from(), "→", year_label(sel_to())),
                labFormat = if (d$kind == "margin") margin_legend_format else swing_legend_format)
  })

  zoom_to <- function(id) {
    bb <- unname(st_bbox(filter(districts_sf, geo_id == id)))
    leafletProxy("map") |> flyToBounds(bb[1], bb[2], bb[3], bb[4])
  }
  observeEvent(input$zoom_focus, zoom_to(input$focus))

  # Clicking a district focuses it; clicking a county adds it to the trend chart
  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (is.null(id)) return()
    if (id %in% DISTRICTS) {
      updateSelectInput(session, "focus", selected = id)
    } else if (id %in% counties_sf$geo_id && !id %in% input$trend_counties) {
      updateSelectizeInput(session, "trend_counties",
                           selected = tail(c(input$trend_counties, id), 5))
    }
  })

  output$map_note <- renderUI({
    d <- map_data()
    years_shown <- if (input$mode == "single") sel_year() else c(sel_from(), sel_to())
    msgs <- c(
      if (any(is_prelim(years_shown)) && input$level != "district")
        "<b>2026 results are only available at district level so far.</b> County and precinct results will be added after the canvass is certified.",
      if (any(is_prelim(years_shown)))
        "2026 figures are unofficial (98–99% reporting, Nov 9–10, 2026) and only include the two major-party candidates.",
      if (input$level == "precinct" && input$mode == "single" && sel_year() >= 2022)
        "2022/2024 precinct results are matched to 2024 precinct shapes by name; unmatched precincts appear blank.",
      if (input$level == "county" && input$race == "House")
        "County House results combine every House race on the county's ballot.",
      if (d$estimated)
        "2016–2020 results are estimated onto today's district lines / precinct shapes by area.",
      "Click a shape for details. Click a district to focus it, or a county to add it to Trends."
    )
    HTML(paste(msgs, collapse = " "))
  })

  # -- Trends ---------------------------------------------------------------------
  trend_df <- reactive({
    geos <- c(DISTRICTS, input$trend_counties)
    df <- results |> filter(level %in% c("district", "county"), geo_id %in% geos)
    df <- if (input$trend_series == "top") {
      filter(df, (race == "President" & year %in% c(2016, 2020, 2024)) |
                 (race == "Governor"  & year %in% c(2018, 2022)))
    } else {
      filter(df, race == input$trend_series)
    }
    margin_summary(df) |>
      mutate(name = geo_titles[geo_id],
             name = factor(name, levels = unique(geo_titles[geos]))) |>
      arrange(name, year)
  })

  output$trend_plot <- renderPlotly({
    df <- trend_df()
    validate(need(nrow(df) > 0, "No data for this selection."))
    cols <- c(district_colors,
              setNames(county_colors[seq_along(input$trend_counties)],
                       geo_titles[input$trend_counties]))
    lim <- max(abs(df$margin), 5, na.rm = TRUE) * 1.1

    plot_ly(df, x = ~year, y = ~margin, color = ~name, colors = cols,
            type = "scatter", mode = "lines+markers",
            line = list(width = 3), marker = list(size = 9),
            hovertext = ~sprintf("<b>%s</b><br>%s %s: %s<br>D %s · R %s",
                                 name, year_label(year), race, fmt_margin(margin),
                                 format(round(dem), big.mark = ","), format(round(rep), big.mark = ",")),
            hoverinfo = "text") |>
      layout(
        xaxis = list(title = "", tickvals = sort(unique(df$year)), fixedrange = FALSE),
        yaxis = list(title = "R − D margin (pts)", zeroline = TRUE, zerolinewidth = 2,
                     zerolinecolor = "#555", range = c(-lim, lim)),
        shapes = list(
          list(type = "rect", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = lim,
               fillcolor = "#C8102E", opacity = 0.05, line = list(width = 0)),
          list(type = "rect", xref = "paper", x0 = 0, x1 = 1, y0 = -lim, y1 = 0,
               fillcolor = "#1F4E9C", opacity = 0.05, line = list(width = 0))
        ),
        legend = list(orientation = "h", y = -0.15),
        hovermode = "closest"
      ) |>
      config(displaylogo = FALSE)
  })

  swing_years <- reactive({
    if (input$mode == "change") {
      valid_change()
      c(sel_from(), sel_to())
    } else {
      yrs <- years_for(input$race)
      c(min(yrs), max(yrs))
    }
  })

  output$swing_title <- renderText({
    y <- swing_years()
    sprintf("%s swing, %d → %d", names(RACES)[RACES == input$race], y[1], y[2])
  })

  swing_df <- reactive({
    y <- swing_years()
    results |>
      filter(level %in% c("district", "county"), race == input$race) |>
      swing(y[1], y[2]) |>
      mutate(name = geo_titles[geo_id]) |>
      arrange(level != "district", swing)
  })

  output$swing_plot <- renderPlotly({
    df <- swing_df()
    validate(need(nrow(df) > 0, "No data for this selection."))
    df$name <- factor(df$name, levels = rev(df$name))
    plot_ly(df, x = ~swing, y = ~name, type = "bar", orientation = "h",
            marker = list(color = ifelse(df$swing > 0, "#C8102E", "#1F4E9C")),
            hovertext = ~sprintf("<b>%s</b><br>%s → %s<br>%s",
                                 name, fmt_margin(margin_from), fmt_margin(margin_to), fmt_swing(swing)),
            hoverinfo = "text") |>
      layout(xaxis = list(title = "Change in R − D margin (pts)", zeroline = TRUE,
                          zerolinewidth = 2, zerolinecolor = "#555"),
             yaxis = list(title = ""),
             bargap = 0.3) |>
      config(displaylogo = FALSE)
  })

  output$swing_table <- renderDT({
    y <- swing_years()
    df <- swing_df() |>
      transmute(Geography = name,
                Level = tools::toTitleCase(level),
                !!paste("Margin", y[1]) := fmt_margin(margin_from),
                !!paste("Margin", y[2]) := fmt_margin(margin_to),
                `Change (+R / −D)` = round(swing, 1),
                `Votes change` = round(turnout_change),
                Estimated = ifelse(estimated, "yes", ""))
    datatable(df, rownames = FALSE, options = list(dom = "t", pageLength = 50, ordering = TRUE)) |>
      formatStyle("Change (+R / −D)",
                  color = styleInterval(0, c("#1F4E9C", "#C8102E")), fontWeight = "bold") |>
      formatRound("Votes change", digits = 0, mark = ",")
  })

  # -- Results / Compare -------------------------------------------------------------
  output$result_plot <- renderPlotly({
    df <- results |>
      filter(level == "district", geo_id == input$focus, race == input$race, year == shown_year()) |>
      group_by(candidate, party) |> summarise(votes = sum(votes), .groups = "drop") |>
      mutate(share = votes / sum(votes)) |>
      arrange(votes) |>
      mutate(candidate = factor(candidate, levels = candidate))
    validate(need(nrow(df) > 0, "No data for this selection."))

    p <- ggplot(df, aes(x = share, y = candidate, fill = party,
                        text = sprintf("%s<br>%s votes (%.1f%%)", candidate,
                                       format(round(votes), big.mark = ","), 100 * share))) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = party_colors) +
      scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
      labs(x = NULL, y = NULL, fill = NULL,
           title = sprintf("%s %s, %s", input$focus, names(RACES)[RACES == input$race], shown_year())) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = "text") |> config(displaylogo = FALSE)
  })

  output$compare_plot <- renderPlotly({
    df <- results |>
      filter(level == "district", race == input$race, year == shown_year()) |>
      group_by(geo_id, party) |> summarise(votes = sum(votes), .groups = "drop_last") |>
      mutate(share = votes / sum(votes)) |> ungroup() |>
      mutate(party = factor(party, levels = rev(names(party_colors))),
             geo_id = factor(geo_id, levels = rev(DISTRICTS)))
    validate(need(nrow(df) > 0, "No data for this selection."))

    p <- ggplot(df, aes(x = share, y = geo_id, fill = party,
                        text = sprintf("%s %s: %.1f%%", geo_id, party, 100 * share))) +
      geom_col(width = 0.6) +
      geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey30") +
      scale_fill_manual(values = party_colors, breaks = names(party_colors)) +
      scale_x_continuous(labels = scales::percent, expand = c(0, 0)) +
      labs(x = "Vote share", y = NULL, fill = NULL,
           title = sprintf("%s vote share by party, %s", names(RACES)[RACES == input$race], shown_year())) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.major.y = element_blank())
    ggplotly(p, tooltip = "text") |> config(displaylogo = FALSE)
  })

  # -- Data ------------------------------------------------------------------------------
  table_data <- reactive({
    lvl <- input$data_level
    if (input$mode == "single") {
      res_level <- lvl
      results |>
        filter(level == res_level, race == input$race, year == sel_year()) |>
        margin_summary() |>
        transmute(Geography = geo_titles[geo_id], Year = year, Race = race,
                  Democratic = round(dem), Republican = round(rep), Total = round(total),
                  Margin = fmt_margin(margin), `Margin (R−D pts)` = round(margin, 2),
                  Estimated = estimated)
    } else {
      valid_change()
      res_level <- if (lvl == "precinct") "precinct_current" else lvl
      results |>
        filter(level == res_level, race == input$race) |>
        swing(sel_from(), sel_to()) |>
        transmute(Geography = geo_titles[geo_id], Race = race,
                  !!paste("Margin", sel_from()) := round(margin_from, 2),
                  !!paste("Margin", sel_to()) := round(margin_to, 2),
                  `Swing (+R / −D)` = round(swing, 2),
                  `Votes change` = round(turnout_change),
                  Estimated = estimated)
    }
  })

  output$table <- renderDT({
    datatable(table_data(), rownames = FALSE, filter = "top",
              options = list(pageLength = 25, dom = "tip", scrollX = TRUE))
  })

  output$download <- downloadHandler(
    filename = function() {
      yrs <- if (input$mode == "single") sel_year() else paste(sel_from(), sel_to(), sep = "-")
      sprintf("sierra_vista_area_%s_%s_%s.csv", tolower(input$race), input$data_level, yrs)
    },
    content = function(file) write.csv(table_data(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
