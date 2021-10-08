
# This function formats gt tables into an apa-7 format -----------------------
# x needs to be a gt object (created based on gt and gtsummary packages)
apa_table_formatter <- function(x, title = " ", table_width = 80) {
  x %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 1,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(table_width),
      table.background.color = "white"
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    #title setup
    tab_header(
      title = html("<i>", title, "</i>")
    ) %>%
    opt_align_table_header(align = "left") %>%
    opt_table_font(
      font = list(
        google_font(name = "Times New Roman")))
}
# ----------------------------------------------------------------------------
# This formats a correlation table
apa_cor_formatter <- function(x, title = " ", table_width = 80) {
  x %>%
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      
      table_body.border.top.color = "black",
      table_body.border.top.width = 0.5,
      
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 0.5,
      
      heading.border.bottom.color = "black",
      heading.border.bottom.width = 0.5,
      
    
   #   table_body.hlines.color = "black",
  #    table_body.hlines.width = 0.5,
      
      table.width = pct(table_width),
      table.background.color = "white"
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    ) %>%
    #title setup
    tab_header(
      title = html("<i>", title, "</i>")
    ) %>%
    opt_align_table_header(align = "left") %>%
    opt_table_font(
      font = list(
        google_font(name = "Times New Roman")))
}
# ----------------------------------------------------------------------------