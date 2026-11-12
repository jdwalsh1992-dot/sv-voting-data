# Shared helpers for app.R (Shiny sources this folder automatically).

party_colors <- c(
  Republican = "#C8102E",
  Democratic = "#1F4E9C",
  Green      = "#3A8A3F",
  Other      = "#8C8C8C"
)

# Two-party margin per geography. `margin` is (R - D) / all votes, in points:
# positive = Republican lead, negative = Democratic lead.
margin_summary <- function(df) {
  df |>
    group_by(level, geo_id, year, race) |>
    summarise(
      dem   = sum(votes[party == "Democratic"]),
      rep   = sum(votes[party == "Republican"]),
      total = sum(votes),
      estimated = any(estimated),
      .groups = "drop"
    ) |>
    mutate(
      margin = ifelse(total > 0, 100 * (rep - dem) / total, NA_real_),
      winner = case_when(rep > dem ~ "Republican", dem > rep ~ "Democratic", TRUE ~ "Tie")
    )
}

# Change in R - D margin (points) between two elections, per geography.
# Positive = shift toward Republicans.
swing <- function(df, from, to) {
  m <- margin_summary(filter(df, year %in% c(from, to)))
  inner_join(
    filter(m, year == from) |> select(level, geo_id, race, margin_from = margin, total_from = total),
    filter(m, year == to)   |> select(level, geo_id, race, margin_to = margin, total_to = total,
                                      estimated),
    by = c("level", "geo_id", "race")
  ) |>
    mutate(swing = margin_to - margin_from,
           turnout_change = total_to - total_from)
}

# "R+4.2" / "D+1.0" / "Even"
fmt_margin <- function(x, digits = 1) {
  ifelse(is.na(x), "–",
         ifelse(round(x, digits) == 0, "Even",
                paste0(ifelse(x > 0, "R+", "D+"), formatC(abs(x), format = "f", digits = digits))))
}

# "+3.1 toward R" / "+2.0 toward D"
fmt_swing <- function(x, digits = 1) {
  ifelse(is.na(x), "–",
         ifelse(round(x, digits) == 0, "No change",
                paste0(formatC(abs(x), format = "f", digits = digits),
                       ifelse(x > 0, " pts toward R", " pts toward D"))))
}

# Diverging blue <-> red scale centred at 0, symmetric around +/- limit.
# Pass values through squish() first so anything past the limit takes the end colour.
margin_palette <- function(limit) {
  leaflet::colorNumeric(c("#1F4E9C", "#9DB4DD", "#F7F7F7", "#E8A2A9", "#C8102E"),
                        domain = c(-limit, limit), na.color = "#d9d9d9")
}

squish <- function(x, limit) pmax(pmin(x, limit), -limit)

margin_legend_format <- function(type, cuts, p) fmt_margin(cuts, 0)
swing_legend_format  <- function(type, cuts, p) {
  ifelse(cuts == 0, "0", paste0(ifelse(cuts > 0, "R +", "D +"), abs(round(cuts))))
}

# Popup HTML per geography: one line per candidate plus the margin.
popup_html <- function(df, titles) {
  df |>
    group_by(geo_id, candidate, party) |>
    summarise(votes = sum(votes), .groups = "drop") |>
    group_by(geo_id) |>
    mutate(share = votes / sum(votes)) |>
    arrange(desc(votes), .by_group = TRUE) |>
    summarise(
      rows = paste0(
        sprintf("<tr><td><span style='color:%s'>&#9632;</span> %s</td><td align='right'>%s</td><td align='right'>%.1f%%</td></tr>",
                party_colors[ifelse(party %in% names(party_colors), party, "Other")],
                htmltools::htmlEscape(candidate),
                format(round(votes), big.mark = ","), 100 * share),
        collapse = ""),
      total = sum(votes),
      .groups = "drop"
    ) |>
    mutate(html = sprintf(
      "<b>%s</b><table style='font-size:12px;margin-top:4px'>%s<tr><td><i>Total</i></td><td align='right'><i>%s</i></td><td></td></tr></table>",
      htmltools::htmlEscape(titles[geo_id]), rows, format(round(total), big.mark = ",")
    )) |>
    select(geo_id, html)
}
