# UI ----------------------------------------------------------------------
qc_report_UI <- function(id){
  tabItem(
    tabName = "qc_report",
          #Download button for html report
          downloadButton(NS(id,"report"), "Generate QC Report")
  )
}


# Server ------------------------------------------------------------------

qc_report_Server <- function(id) {
  moduleServer(id, function(input, output, session) {


    #Output for html report
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf" - going with HTML for now to avoid issues with latex
      filename = "QC report.html",
      content = function(file) {

        #Notification message that report is being generated, disappears once done
        render_in_progress <- showNotification(
          "Rendering report...",
          duration = NULL,
          closeButton = FALSE
        )
        on.exit(removeNotification(render_in_progress), add = TRUE)


        #Copy report to working directory
        report_path <- file.path(getwd(), "report.Rmd")
        file.copy("reports/Calfee_Matthay Lab Inventory QC.Rmd", report_path, overwrite = TRUE)


        # Knit the document
        rmarkdown::render(report_path,
                          output_file = file,
                          envir = new.env(parent = globalenv())
        )
      }
    )


  })
}
