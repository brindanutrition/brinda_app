dashboardSidebar(
    tags$style(".left-side, .main-sidebar {padding-top: 100px;}"),
    use_theme(mytheme),
    width = 300,
    sidebarMenuOutput("sidebarmenu")
  )