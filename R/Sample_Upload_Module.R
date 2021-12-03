# UI ----------------------------------------------------------------------
sample_upload_UI <- function(id,studies){
  tabItem(tabName = "sample_upload",
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

            #Box on left with download link for template, study selection options, file upload
            box(solidHeader = TRUE,
                status = "primary",
                width = 2,

                #Link for downloading template saved in inst template folder
                h3(a("Click Here to Download Sample Upload Template",
                     target = "_blank",
                     href = "templates/Sample_Upload_Template.xlsx"),
                   align = "center"),

                tags$hr(),

                #Display options for selecting a study - 1 study at a time to avoid loading in too much data at once
                #Gets studies list from REDCap_info function, initializes default study selected to blank
                selectizeInput(NS(id,"upload_studies"), label = h5("SELECT STUDY"),
                               choices = studies$event_name, options = list(onInitialize = I('function() { this.setValue(""); }'))),

                tags$hr(),

                #File upload for excel sample upload file (from template)
                fileInput(NS(id,"sample_upload_file"), h5("Sample level information to upload"),
                          multiple = FALSE, accept = ".xlsx"
                ),
                helpText("File must be in format of template and uploaded as .xlsx"),

                tags$hr(),

                #Action button to upload samples - will give a warning to cancel before upload takes place
                actionButton(NS(id,"upload_samples"), "Upload")


            ),

            #Tables to the right that appear once a file has been uploaded. WithSpinner just adds a signal that it is loading
            tabBox(width = 10,
                   #Preview table
                   tabPanel("Upload Preview",
                            withSpinner(DT::dataTableOutput(NS(id,"upload_sample_preview")))),
                   #Table of samples that have already been uploaded into redcap
                   tabPanel("Samples Already in REDCap",
                            DT::dataTableOutput(NS(id,"upload_already_uploaded")))
            )

          )
  )
}


# Server ------------------------------------------------------------------
sample_upload_Server <- function(id,studies,rcon_lims,token,redcap_url,timepoint,timepoint_code,type,type_code,volume_units,volume_units_code,
                                 container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code) {
  moduleServer(id, function(input, output, session) {
    #Read in sample upload file
    samples_for_upload<-reactive({
      #Require file to be uploaded and study to be selected for code to run
      req(input$sample_upload_file)
      req(input$upload_studies)

      #Read in file and return safe error if parsing error occurs
      tryCatch(
        {
          df <- read_excel(input$sample_upload_file$datapath)
        },
        error = function(e) {
          stop(safeError(e))
        }
      )

      return(df)

    })

    #Export existing redcap samples for the selected study
    redcap_samples <- reactive({

      #Require file to be uploaded for code to run
      req(input$sample_upload_file)

      #Get event name based on selected study. Uses get_event_name function defined in R/utils.R
      event <- get_event_name(studies,input$upload_studies)

      #Export record_id and sample_id and study from redcap, filtering for the study identified in the function above. This prevents having to export everything
      in_redcap <- exportRecords(rcon = rcon_lims,fields = c("record_id","sample_id"), events = event)

      in_redcap <- in_redcap %>% select(record_id,sample_id)

      return(in_redcap)

    })

    #Find samples in uploaded list not already in REDCap, format to match REDCap format
    samples_to_upload <- reactive({

      #Filter for samples not already uploaded
      samples_for_upload <- samples_for_upload() %>% filter(!(`Scan Barcode` %in% redcap_samples()$sample_id))

      #Extract study specific info from barcode, format columns for upload using R/parse_barcode.R
      samples_for_upload_cleaned <- parse_barcode(samples_for_upload,input$upload_studies)

      return(samples_for_upload_cleaned)

    })

    #Preview the samples already uploaded
    output$upload_already_uploaded <- DT::renderDataTable({
      #Filter for samples already present in redcap
      already_uploaded <- redcap_samples() %>% filter(sample_id %in% samples_for_upload()$`Scan Barcode`)

      DT::datatable(already_uploaded,
                    options = list(
                      lengthMenu = c(10,25,100),
                      scrollX = TRUE
                    ),
                    selection = "single")
    })

    #Preview the new samples to upload
    output$upload_sample_preview <- DT::renderDataTable({

      DT::datatable(samples_to_upload(),
                    options = list(
                      lengthMenu = c(10,25,100),
                      scrollX = TRUE
                    ),
                    selection = "single")
    })

    #Display warning message once upload button is clicked
    observeEvent(input$upload_samples, {
      showModal(modal(id))
    })

    #If ok is elected on warning message, format template into redcap acceptable format
    observeEvent(input$ok,{

      removeModal()

      #Add spinner to show something is happening when user clicks upload
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())

      #Preview file from above
      samples_to_upload <- samples_to_upload()
      #Samples already in redcap, used for finding the next record ID
      in_redcap <- redcap_samples()


      #Find last record ID in REDCap for the selected study
      in_redcap <- in_redcap %>% separate(record_id, into = c("study","number"),sep = "_", convert = TRUE)
      last_record <- max(in_redcap$number,na.rm = TRUE)

      #Recode values, rename columns, format df for upload - uses R/sample_upload_formatting.R
      ready_for_sample_upload <- format_for_upload(studies,input$upload_studies,last_record,samples_to_upload,
                                                   timepoint,timepoint_code,type,type_code,volume_units,volume_units_code,
                                                   container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code)

      #Import into redcap, blanks will not overwrite, returns a count of number of samples uploaded
      import_output<-importRecords(rcon = rcon_lims, ready_for_sample_upload, overwriteBehavior = c("normal"), returnContent = c("count"),
                                   returnData = FALSE, logfile = "logfile_sample_upload.txt")



      #Show notification message that x number of samples were uploaded
      showNotification(paste0(import_output," samples uploaded"),type = "message", duration = NULL)


    })


    #If cancel is selected instead of ok on warning message, nothing happens and window disappears
    observeEvent(input$cancel, {
      removeModal()
    })
  })
}
