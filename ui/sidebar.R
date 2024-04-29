source("ui/styles/sidebar.R")
SidebarStyle <- SidebarStyle


dashboardSidebar(
    tags$style(SidebarStyle),
    use_theme(mytheme),
    width = 300,
    sidebarMenuOutput("sidebarmenu")
)