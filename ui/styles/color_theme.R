# --- Shiny theme --------------------------------------------------------------
mytheme <- create_theme(
  theme = "flatly",
  adminlte_color(
    light_blue = "#301014"
  ),
  adminlte_sidebar(
    dark_bg = "#121111",
    dark_hover_bg = "#140708",
    dark_color = "white",
    width = "300px"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "white", 
    info_box_bg = "black"
  )
)

# --- colors -------------------------------------------------------------------
cols <- c(
  "#a6cee3",
  "#1f78b4",
  "#b2df8a",
  "#33a02c",
  "#fb9a99",
  "#e31a1c",
  "#fdbf6f",
  "#ff7f00",
  "#cab2d6",
  "#6a3d9a",
  "#b15928"
)
