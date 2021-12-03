# UI ----------------------------------------------------------------------
sample_management_UI <- function(id,studies){
  tabItem(tabName = "sample_management",

          #Define position for the upload success notification - middle of the page
          tags$head(tags$style(
            HTML(
              ".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
            )
          )
          ),
          #Bar at top for listing different sample management tasks within this module
          navbarPage(
            "Choose a task",
            #Change storage locations
            change_storage_location_UI("change_storage_locations",studies),
            #Change sample status
            change_sample_status_UI("change_sample_status",studies)

          )

  )
}


# Server ------------------------------------------------------------------
sample_management_Server <- function(id,studies,rcon_lims,token,redcap_url,container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code) {
  #Change storage locations
  change_storage_location_Server("change_storage_locations",studies,rcon_lims,token,redcap_url,container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code)
  #Change sample status
  change_sample_status_Server("change_sample_status",studies,rcon_lims,token,redcap_url)

}
