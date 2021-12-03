# UI ----------------------------------------------------------------------
box_viewer_UI <- function(id,studies){
  tabItem(tabName = "box_viewer",
          fluidRow(

            #Box for selecting study
            box(solidHeader = TRUE,
                status = "primary",
                width = 6,

                #Display options for selecting a study - only loads boxes for 1 study at a time to avoid loading in too much data at once
                #Gets studies list from REDCap_info function, initializes default study selected to blank
                selectizeInput(NS(id,"box_studies"), label = h5("Select Study"),
                               choices = studies$event_name, options = list(onInitialize = I('function() { this.setValue(""); }'))),
            ),

            #Box for selecting box, options are populated in the server - much faster for when options get into 1000s
            box(solidHeader = TRUE,
                status = "primary",
                width = 6,

                selectizeInput(NS(id,"box_select"), h5("Select Box"),
                               choices = NULL, options = list(maxOptions = 20,
                                                              onInitialize = I('function() { this.setValue(""); }')))
            ),

            #Display box grid
            box(solidHeader = TRUE,
                status = "primary",
                width = 12,
                withSpinner(DT::dataTableOutput(NS(id,"box_grid"))))
          )
  )
}


# Server ------------------------------------------------------------------

box_viewer_Server <- function(id,studies,rcon_lims,token,redcap_url) {
  moduleServer(id, function(input, output, session) {


    #Load in Location and Remaining Volume Info
    box_samples <- reactive({

      req(input$box_studies)

      #Get event name based on selected study. Uses get_event_name function defined in R/utils
      event <- get_event_name(studies,input$box_studies)

      #Export location and remaining volume information
      redcap_export <- exportRecords(rcon = rcon_lims,fields = c("record_id","sample_id","container_type","container_matthay_lab_id","grid_position",
                                                                 "freezer_number","freezer_shelf_number","sample_volume_remaining","sample_volume_units",
                                                                 "sample_status"),
                                     events = event)

      #Filter for only container types that can be displayed in a grid, sample status held or stored
      redcap_export <- redcap_export %>% filter(container_type %in% c("Plastic box (5x10)","Plastic box (10x10)","96-Well Box","Plastic Box (9x9)",
                                                                      "Cardboard box (9x9)","Cardboard box (6x6)","Cardboard box (4x4)","Cardboard box (8x8)"),
                                                sample_status %in% c("Stored","Hold")) %>%
        select(sample_id:sample_status)

      return(redcap_export)


    })

    #Update the options in the box selecting dropdown
    observe({
      #Filter for the first occurence of each box_id
      boxes <- box_samples() %>% distinct(container_matthay_lab_id)

      #Update selection input with the box ids exported from redcap
      freezeReactiveValue(input, "box_select")
      updateSelectizeInput(session,"box_select", choices = boxes$container_matthay_lab_id, server = TRUE)

    })

    #Display grid for selected box_id
    output$box_grid <- DT::renderDataTable({

      #require box to be selected for code to run
      req(input$box_select)

      #Filter for only samples in the selected box
      specified_box <- box_samples() %>% filter(container_matthay_lab_id == input$box_select)

      #Populate grid based on box type using R/box_grid_filler.R file
      grid <- grid_filler(specified_box)

      #Create final table to be displayed, make it look more like a grid than a table, format blank cells as blue and include freezer and shelf
      DT::datatable(grid,
                    selection = list(mode = "single",target = "cell"), class = "cell-border", rownames = TRUE,
                    options = list(paging = FALSE, ordering = FALSE, info = FALSE,
                                   columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                    caption = htmltools::tags$caption(paste("Freezer: ", specified_box[1,"freezer_number"], "Shelf: ", specified_box[1,"freezer_shelf_number"]),style="color:purple")) %>%
        formatStyle(names(grid),backgroundColor = styleEqual(c(""), c('blue')))
    })

  })
}
