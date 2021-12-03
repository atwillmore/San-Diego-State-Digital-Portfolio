Homepage_UI <- function(id){
  tabItem(tabName = "homepage",
          h1(strong(paste("Hello, happy ", format(Sys.time(), '%A',tz = "America/Los_Angeles"), '! Welcome to the Test Shiny LIMS.', sep = "")), align = "center")

  )
}
