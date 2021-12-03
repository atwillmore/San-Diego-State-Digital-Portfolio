# UI ----------------------------------------------------------------------
change_storage_location_UI <- function(id,studies){
  tabPanel("Change sample storage locations",
           fluidRow(
             #Indicator that task is in progress
             use_waiter(),
             box(solidHeader = TRUE,
                 status = "primary",
                 width = 2,

                 # Excel template for bulk location change, saved in inst folder under templates
                 h3(a("Click Here to Download Bulk Move Template",
                      target = "_blank",
                      href = "templates/Bulk move template.xlsx"),
                    align = "center"),

                 tags$hr(),
                 # Level at which IDs will be uploaded for sample movement - first column in template
                 radioButtons(NS(id, "move_level"), "Select level of ID provided in column A",
                              choices = list("Sample" = "sample", "Box" = "box", "Freezer Shelf" = "freezer_shelf" ,"Freezer" = "freezer"),
                              selected = character(0)),


                 #Display options for selecting a study - 1 study at a time to avoid loading in too much data at once
                 #Gets studies list from REDCap_info function, initializes default study selected to blank
                 selectizeInput(NS(id,"studies"), label = "Select Study",
                                choices = studies$event_name, options = list(onInitialize = I('function() { this.setValue(""); }'))),

                 tags$hr(),

                 #File upload - xlsx file, must match template format
                 fileInput(NS(id,"moving_file"), "Upload .xlsx file with IDs to move", accept = ".xlsx"),
                 helpText("Leave column blank if nothing to update."),


                 tags$hr(),

                 #Button must be clicked to initiate upload into redcap
                 actionButton(NS(id,"move_upload"), "Change Storage Location")
             ),
             tabBox(
               width = 10,
               #Counts of samples being transferred from one location to another broken down by freezer/shelf and box
               tabPanel("Sample Count Summary",
                        fluidRow(
                          h3(textOutput(NS(id,"move_sample_count")), align = "center"),
                          box(width = 6,
                              DT::dataTableOutput(NS(id,"freezer_summary"))),
                          box(width = 6,
                              withSpinner(DT::dataTableOutput(NS(id,"move_box_summary")))))
               ),
               #Check for conflicts with existing locations in redcap - if box/grid position is already taken by another sample
               tabPanel("Conflicts",
                        withSpinner(DT::dataTableOutput(NS(id,"conflicts")))),
               #Ids not in REDCap. If ID isn't present, samples won't be moved
               tabPanel("IDs not found in REDCap",
                        withSpinner(DT::dataTableOutput(NS(id,"move_missing_id"))))
             )
           )
  )
}


# Server ------------------------------------------------------------------

change_storage_location_Server <- function(id,studies,rcon_lims,token,redcap_url,container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code) {
  moduleServer(id, function(input, output, session) {

    #Load in uploaded file
    moving_file <- reactive({
      #require file upload, level selected, study selected for code to run
      req(input$moving_file)
      req(input$move_level)
      req(input$studies)

      tryCatch(
        {
          df <- read_excel(input$moving_file$datapath)
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        }
      )

      #Make sure all columns are character type
      df <- df %>% mutate_all(as.character)

      return(df)
    })

    #Load in redcap locations
    redcap_locations <- reactive({

      req(input$moving_file)
      req(input$move_level)

      #Get event name based on selected study. Uses get_event_name function defined in R/utils
      event <- get_event_name(studies,input$studies)

      #Export location info from redcap
      redcap_locations <- exportRecords(rcon = rcon_lims, fields = c("record_id","sample_id","container_matthay_lab_id","freezer_shelf_number",
                                                                     "freezer_number","grid_position","container_type","sample_status"),
                                        events = event)

      #Make sure all columns are type character, only include samples that are of status stored or hold
      redcap_locations <- redcap_locations %>% mutate_all(as.character) %>%
        filter(sample_status %in% c("Stored","Hold")) %>%
        select(-sample_status)

      return(redcap_locations)

    })

    #Join locations to what is already stored in redcap, only including samples/IDs found in upload file and redcap
    upload_file <- reactive({

      moving_file <- moving_file()
      redcap_locations <- redcap_locations()

      if(input$move_level == "sample"){
        samples_moving <- redcap_locations %>% inner_join(moving_file,by = c("sample_id" = "Existing ID"), suffix = c("_current","_update"))
      }
      else if(input$move_level == "box"){
        samples_moving <- redcap_locations %>% inner_join(moving_file,by = c("container_matthay_lab_id" = "Existing ID"), suffix = c("_current","_update")) %>%
          rename(container_matthay_lab_id_current = container_matthay_lab_id)
      }
      else if(input$move_level == "freezer_shelf"){
        samples_moving <- redcap_locations %>% inner_join(moving_file,by = c("freezer_shelf_number" = "Existing ID"), suffix = c("_current","_update")) %>%
          rename(freezer_shelf_number_current = freezer_shelf_number)
      }
      else if(input$move_level == "freezer"){
        samples_moving <- redcap_locations %>% inner_join(moving_file,by = c("freezer_number" = "Existing ID"), suffix = c("_current","_update")) %>%
          rename(freezer_number_current = freezer_number)
      }

      return(samples_moving)

    })

    #Conflicts
    output$conflicts <- DT::renderDataTable({

      moving_file <- moving_file()
      redcap_locations <- redcap_locations()

      #Look for any conflicts in box/grid position that might arise from the move - right now only relevant for sample level changes
      conflicts <- move_conflicts(moving_file,redcap_locations,input$move_level)

      DT::datatable(conflicts,
                    options = list(
                      lengthMenu = c(10,50,100),
                      scrollX = TRUE
                    ),
                    selection = "single")
    })

    #Find IDs not in REDCap included in upload file
    output$move_missing_id <- DT::renderDataTable({

      moving_file <- moving_file()
      redcap_locations <- redcap_locations()

      if(input$move_level == "sample"){
        missing_ids <- moving_file %>% filter(!(`Existing ID` %in% redcap_locations$sample_id))
      }
      else if(input$move_level == "box"){
        missing_ids <- moving_file %>% filter(!(`Existing ID` %in% redcap_locations$container_matthay_lab_id))
      }
      else if(input$move_level == "freezer_shelf"){
        missing_ids <- moving_file %>% filter(!(`Existing ID` %in% redcap_locations$freezer_shelf_number))
      }
      else if(input$move_level == "freezer"){
        missing_ids <- moving_file %>% filter(!(`Existing ID` %in% redcap_locations$freezer_number))
      }

      DT::datatable(missing_ids,
                    options = list(
                      lengthMenu = c(10,50,100),
                      scrollX = TRUE
                    ),
                    selection = "single")
    })

    #Total count of samples being moved
    output$move_sample_count <- renderText(paste0("Number of total samples moved: ",nrow(upload_file())))

    #Box level movement summary
    output$move_box_summary <- DT::renderDataTable({

      preview_file <- upload_file()

      #Find number of samples moved from 1 box to another. If no box ID update given, use what's in REDCap (since that's how it will be uploaded)
      box <- preview_file %>% group_by(container_matthay_lab_id_current,container_matthay_lab_id_update) %>%
        tally() %>%
        mutate(container_matthay_lab_id_update = ifelse(is.na(container_matthay_lab_id_update),container_matthay_lab_id_current,container_matthay_lab_id_update))

      colnames(box) <- c("From","To","# Samples Moved")

      DT::datatable(box,
                    options = list(
                      lengthMenu = c(10,50,100),
                      scrollX = TRUE,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                    selection = "single",
                    caption = htmltools::tags$caption("Box Level Summary",style="color:black"))
    })

    #Freezer and freezer shelf level summaries
    output$freezer_summary <- DT::renderDataTable({

      preview_file <- upload_file()

      #Find number of samples moved between freezers/shelfs. If no update given, use what's in REDCap (since that's how it will be uploaded).
      #Join freezer and shelf into one field for ease of viewing
      freezer <- preview_file %>% group_by(freezer_shelf_number_current,freezer_shelf_number_update,freezer_number_current,freezer_number_update) %>%
        tally() %>%
        mutate(freezer_shelf_number_update = ifelse(is.na(freezer_shelf_number_update),freezer_shelf_number_current,freezer_shelf_number_update),
               freezer_number_update = ifelse(is.na(freezer_number_update),freezer_number_current,freezer_number_update),
               freezer_current = paste0("Freezer ",freezer_number_current,", Shelf ",freezer_shelf_number_current),
               freezer_update = paste0("Freezer ",freezer_number_update,", Shelf ",freezer_shelf_number_update)) %>%
        ungroup() %>%
        select(freezer_current,freezer_update,n)

      colnames(freezer) <- c("From","To","# Samples Moved")

      DT::datatable(freezer,
                    options = list(
                      lengthMenu = c(10,50,100),
                      scrollX = TRUE,
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                    ),
                    selection = "single",
                    caption = htmltools::tags$caption("Freezer Level Summary",style="color:black"))
    })

    #Give choice to cancel upload before it goes through
    observeEvent(input$move_upload, {
      req(input$moving_file)
      req(input$move_level)
      req(input$studies)
      showModal(modal(id))
    })

    #If ok is selected, upload new storage locations
    observeEvent(input$ok, {

      removeModal()

      #Add spinner to show something is happening when user clicks upload
      waiter <- waiter::Waiter$new()
      waiter$show()
      on.exit(waiter$hide())

      #Format file for upload only including relevant columns
      upload_file <- sample_move_upload_formatting(upload_file(),container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code)

      #Import changes - NA won't overwrite, returns count of how many samples were updated
      x<-importRecords(rcon = rcon_lims, upload_file, overwriteBehavior = c("normal"), returnContent = c("count"),
                       returnData = FALSE, logfile = "logfile_location_change.txt")


      showNotification(paste0(x," samples moved"),type = "message", duration = NULL)


    })

    #If cancel is selected, remove modal and upload canceled
    observeEvent(input$cancel, {
      removeModal()
    })

  })
}
