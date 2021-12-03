library(shinydashboard)
library(tidyverse)
library(readxl)
library(DT)
library(baRcodeR)
library(redcapAPI)
library(shinycssloaders)
library(vroom)
library(writexl)
library(shiny)
library(tidyselect)
library(waiter)
library(kableExtra)
library(rmarkdown)


shiny_lims <- function(){
    # Packages and Global Environment Stuff -----------------------------------

    #File path to access files in inst folder
    addResourcePath('templates', system.file('templates', package='ShinyLims'))
    addResourcePath('reports', system.file('reports', package='ShinyLims'))


    #REDCAP API
    redcap_url = "https://redcap.ucsf.edu/api/"
    token = "XXXXXX"
    rcon_lims = redcapConnection(url = redcap_url, token = token)

    #Run function (found in utils.R) that returns studies and excel to redcap translation tables
    redcap_info <- REDCap_info()

    # Header ------------------------------------------------------------------
    header<-dashboardHeader(title = "Shiny LIMS")


    # Dashboard sidebar -------------------------------------------------------
    sidebar<-dashboardSidebar(
        sidebarMenu(
            menuItem("Homepage", tabName = "homepage", icon = icon("igloo")),
            #menuItem("Barcode Generator", tabName = "barcode", icon = icon("barcode")),
            menuItem("Box Viewer", tabName = "box_viewer", icon = icon("th")),
            menuItem("Sample Locator", tabName = "sample_locator", icon = icon("search")),
            menuItem("Sample Upload", tabName = "sample_upload", icon = icon("upload")),
            menuItem("Sample Management", tabName = "sample_management", icon = icon("people-carry")),
            menuItem("Inventory QC Report", tabName = "qc_report", icon = icon("check-double"))
            #menuItem("Assay Result Import", tabName = "assay_import", icon = icon("vials"))
        )
    )


    # Body Content ------------------------------------------------------------
    body<-dashboardBody(
        tabItems(
            Homepage_UI("homepage"),
            source(file.path("ui/barcode.R"), local = T)$value,
            sample_upload_UI("sample_upload",redcap_info$studies),
            box_viewer_UI("box_viewer",redcap_info$studies),
            sample_management_UI("sample_management",redcap_info$studies),
            source(file.path("ui/assay_upload.R"), local = T)$value,
            sample_locator_UI("sample_locator",redcap_info$studies),
            qc_report_UI("qc_report")
        )
    )

    # Include dashboard skin, header, sidebar, and body within ui code --------
    ui <- dashboardPage(skin = "purple",
                        header,
                        sidebar,
                        body)



    # Server ------------------------------------------------------------------
    server <- function(input, output, session) {
        source(file.path("server/barcode.R"), local = T)$value
        source(file.path("server/assay_upload.R"), local = T)$value
        sample_management_Server("sample_management",redcap_info$studies,rcon_lims,token,redcap_url,
                                 redcap_info$container,redcap_info$container_code,redcap_info$freezer,redcap_info$freezer_code,
                                 redcap_info$freezer_shelf,redcap_info$freezer_shelf_code)
        box_viewer_Server("box_viewer",redcap_info$studies,rcon_lims,token,redcap_url)
        sample_locator_Server("sample_locator",redcap_info$studies,rcon_lims,token,redcap_url)
        sample_upload_Server("sample_upload",redcap_info$studies,rcon_lims,token,redcap_url,redcap_info$timepoint,
                             redcap_info$timepoint_code,redcap_info$type,redcap_info$type_code,redcap_info$volume_units,redcap_info$volume_units_code,
                             redcap_info$container,redcap_info$container_code,redcap_info$freezer,redcap_info$freezer_code,redcap_info$freezer_shelf,
                             redcap_info$freezer_shelf_code)
        qc_report_Server("qc_report")
    }


    # Shiny app function ------------------------------------------------------
    shinyApp(ui, server)

}
