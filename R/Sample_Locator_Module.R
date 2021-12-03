# UI ----------------------------------------------------------------------
sample_locator_UI <- function(id,studies){
  tabItem(tabName = "sample_locator",
          fluidRow(
            #Box for selecting study
            box(solidHeader = TRUE,
                status = "primary",
                width = 3,

                #Display options for selecting a study - 1 study at a time to avoid loading in too much data at once
                #Gets studies list from REDCap_info function, initializes default study selected to blank
                selectizeInput(NS(id,"sample_locator_studies"), label = h5("Select Study"),
                               choices = studies$event_name, options = list(onInitialize = I('function() { this.setValue(""); }'))),

                tags$hr(),

                #Upload intput for csv file of 1 column containing sample IDs to return locations for
                fileInput(NS(id,"sample_locator_file"), "Upload CSV File",
                          multiple = FALSE, accept = ".csv"
                ),
                helpText("FILE MUST BE CSV WITH 1 COLUMN OF SAMPLE IDS. MAKE SURE TO INLCUDE HEADER."),

                tags$hr(),

                #Action button to find sample locations
                actionButton(NS(id,"sample_locator_find"), "Get Sample Locations"),

                tags$hr(),

                #Download button for getting an excel file with location information
                downloadButton(NS(id,"sample_locator_download"), "Download Locations")

            ),

            #Display sample locations in table format
            box(solidHeader = TRUE,
                status = "primary",
                width = 9,
                withSpinner(DT::dataTableOutput(NS(id,"sample_locations"))))
          )
  )
}


# Server ------------------------------------------------------------------

sample_locator_Server <- function(id,studies,rcon_lims,token,redcap_url) {
  moduleServer(id, function(input, output, session) {
    #When action button is selected and sample file uploaded and study selected, code below runs
    sample_locations <- eventReactive(input$sample_locator_find,{

      req(input$sample_locator_file)
      req(input$sample_locator_studies)

      tryCatch(
        {
          sample_ids <- read_csv(input$sample_locator_file$datapath)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )

      #Rename sample id column so that it is always consistent
      colnames(sample_ids) <- "sample_id"

      #Get event name based on selected study. Uses get_event_name function defined in R/utils.R
      event <- get_event_name(studies,input$sample_locator_studies)

      #Export sample location info for selected study
      redcap_export <- exportRecords(rcon = rcon_lims,fields = c("record_id","sample_id","container_type","container_matthay_lab_id","grid_position",
                                                                 "freezer_number","freezer_shelf_number","sample_status"),
                                     events = event)

      #Filter for samples in the uploaded file
      sample_location_info <- redcap_export %>% filter(sample_id %in% sample_ids$sample_id) %>%
        select(record_id,sample_id:sample_status)

      return(sample_location_info)

    })

    #Preview table
    output$sample_locations <- DT::renderDataTable({

      DT::datatable(sample_locations(),
                    options = list(
                      lengthMenu = c(5,20,50),
                      scrollX = TRUE),
                    selection = "single")
    })

    #File to be downloaded - what's displayed in the table
    output$sample_locator_download <- downloadHandler(
      filename = function() {
        paste0("sample locations ",Sys.Date(),".csv")
      },
      content = function(file) {
        write_excel_csv(sample_locations(), file)
      }
    )
  })
}
