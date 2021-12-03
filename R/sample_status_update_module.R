# UI ----------------------------------------------------------------------
change_sample_status_UI <- function(id,studies){
  tabPanel("Update Sample Status",
           fluidRow(
             #Loading indicator
             use_waiter(),

             #Define position for the upload success notification - middle of the page
             tags$head(
               tags$style(
                 HTML(".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }
             "
                 )
               )
             ),

             box(solidHeader = TRUE,
                 status = "primary",
                 width = 3,

                 #Choice of what status to assign to uploaded sample IDs - LABEL/VALUE COMBINATIONS MUST MATCH WHAT IS IN REDCAP, GETS UPLOADED DIRECTLY
                 radioButtons(NS(id, "sample_status"), "Select sample status to assign to IDs",
                              choices = list("Stored" = 1, "Distributed" = 2, "Destroyed" = 3 ,"Hold" = 4, "No volume remaining" = 5, "Unknown" = 99),
                              selected = character(0)),

                 #Text field for distribution recipient - only appears if distributed is selected
                 uiOutput(NS(id,"distribution_recipient")),


                 #Display options for selecting a study - 1 study at a time to avoid loading in too much data at once
                 #Gets studies list from REDCap_info function, initializes default study selected to blank
                 selectizeInput(NS(id,"studies"), label = "Select Study",
                                choices = studies$event_name, options = list(onInitialize = I('function() { this.setValue(""); }'))),

                 tags$hr(),

                 #File upload - column csv file with sample IDs
                 fileInput(NS(id,"sample_status_file"), "Upload CSV File",
                           multiple = FALSE, accept = ".csv"
                 ),
                 helpText("FILE MUST BE CSV WITH 1 COLUMN OF SAMPLE IDS. MAKE SURE TO INLCUDE HEADER."),


                 tags$hr(),

                 #Button must be clicked to initiate upload into redcap
                 actionButton(NS(id,"change_status"), "Update sample status")
             ),
             tabBox(
               width = 9,
               #Display of current sample status and distribution recipients
               tabPanel("Current Sample Status",
                        withSpinner(DT::dataTableOutput(NS(id,"current_status")))),
               #Samples not yet in REDCap/ IDS without a match
               tabPanel("Samples not in REDCap",
                        withSpinner(DT::dataTableOutput(NS(id,"not_in_redcap"))))
             )
           )
  )
}


# Server ------------------------------------------------------------------
change_sample_status_Server <- function(id,studies,rcon_lims,token,redcap_url) {
  moduleServer(id, function(input, output, session) {

    #Create UI for distribution recipient textbox only if status distributed is selected
    output$distribution_recipient = renderUI({
      req(input$sample_status)
      value <- isolate(input$distribution_recipient)
      if(input$sample_status == 2){
        textInput(NS(id,"distribution_recipient"), label = "Enter Distribution Recipient", value = value)
      }
    })

    #Load in sample ID file
    sample_status_ids <- reactive({
      #Require sample status, stidues, and file to be uploaded for code to run
      req(input$sample_status)
      req(input$studies)
      req(input$sample_status_file)


      tryCatch(
        {
          df <- read_csv(input$sample_status_file$datapath)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )

      #Make sure column is named appropriately
      colnames(df) <- "sample_id"

      return(df)
    })

    #Load in REDCap data for selected event
    redcap_status <- reactive({
      req(input$sample_status)
      req(input$studies)
      req(input$sample_status_file)

      #Get event name based on selected study. Uses get_event_name function defined in R/utils
      event <- get_event_name(studies,input$studies)

      #Load in sample status info
      redcap_statuses <- exportRecords(rcon = rcon_lims, fields = c("record_id","sample_id","sample_status","sample_distribution_recipient"),
                                       events = event)

      return(redcap_statuses)

    })

    # Select samples from REDCap export that are in list and return current status and distribution recipient
    current_status <- reactive({
      sample_status_ids <- sample_status_ids()
      redcap_status <- redcap_status()

      current_status <- redcap_status %>% filter(sample_id %in% sample_status_ids$sample_id) %>%
        select(record_id,redcap_event_name,sample_id:sample_distribution_recipient)

      return(current_status)

    })

    #Current statuses table
    output$current_status <- DT::renderDataTable({

      DT::datatable(current_status(),
                    options = list(
                      lengthMenu = c(10,50,100),
                      scrollX = TRUE
                    ),
                    selection = "single")
    })

    #Find IDs not in REDCap included in upload file
    output$not_in_redcap <- DT::renderDataTable({

      sample_status_ids <- sample_status_ids()
      redcap_status <- redcap_status()

      missing_from_redcap <- sample_status_ids %>% filter(!(sample_id %in% redcap_status$sample_id))

      DT::datatable(missing_from_redcap,
                    options = list(
                      lengthMenu = c(10,50,100),
                      scrollX = TRUE
                    ),
                    selection = "single")
    })

    #Give choice to cancel upload before it goes through
    observeEvent(input$change_status, {
      req(input$sample_status)
      req(input$studies)
      req(input$sample_status_file)
      showModal(modal(id))
    })

    #If ok is selected, upload new statuses, add distribution recipient only if distributed status is selected
    observeEvent(input$ok, {

      removeModal()

      #Add spinner to show something is happening when user clicks upload
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())

      #Format file for upload - change sample_status to what was selected, add distribution recipient if distributed sample status selected
      upload_file <- current_status() %>% mutate(sample_status = input$sample_status,
                                               sample_distribution_recipient = ifelse(input$sample_status == 2,input$distribution_recipient,sample_distribution_recipient))

      #import to redcap, na doens't overwrite, returns count of samples affected
      x<-importRecords(rcon = rcon_lims, upload_file, overwriteBehavior = c("normal"), returnContent = c("count"),
                       returnData = FALSE, logfile = "logfile_sample_status.txt")


      showNotification(paste0(x," samples updated"),type = "message", duration = NULL)


    })

    #If cancel selected, remove modal
    observeEvent(input$cancel, {
      removeModal()
    })

  })
}
