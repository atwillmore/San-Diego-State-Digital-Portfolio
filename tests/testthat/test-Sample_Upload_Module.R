#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests

test_that("Check that upload file loads in correctly", {
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)
  testServer(sample_upload_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url,
                                               REDCap_info()$timepoint,REDCap_info()$timepoint_code,REDCap_info()$type,REDCap_info()$type_code,
                                               REDCap_info()$volume_units,REDCap_info()$volume_units_code,REDCap_info()$container,REDCap_info()$container_code,
                                               REDCap_info()$freezer,REDCap_info()$freezer_code,REDCap_info()$freezer_shelf,REDCap_info()$freezer_shelf_code),
             {

               session$setInputs(upload_studies = "Test", sample_upload_file = list(datapath = "Earli_Upload_Test.xlsx"))


               #Check that columns are correct
               expect_equal(colnames(samples_for_upload()), c("Scan Barcode",	"Patient ID (If not in Barcode)","Timepoint (If not in Barcode)",
                                                                 "Time Unit (If not in Barcode)",	"Sample Type (If not in Barcode)",
                                                                 "Date and Time of Collection (MM/DD/YY HR:MIN)",	"Sample Volume",
                                                                 "Sample Volume Units",	"Container Type",	"Scan container barcode",
                                                                 "Grid Position",	"Freezer number",	"Freezer Shelf Number",
                                                                 "Date Stored in Freezer",	"Staff member checking in"))

               expect_equal(samples_for_upload()$`Scan Barcode`, c("xxx-01",
                                                                   "xxx-08a",
                                                                   "xxx-16",
                                                                   "xxx-12"))



  })
})

test_that("Check REDCap Export Works", {
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)
  testServer(sample_upload_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url,
                                               REDCap_info()$timepoint,REDCap_info()$timepoint_code,REDCap_info()$type,REDCap_info()$type_code,
                                               REDCap_info()$volume_units,REDCap_info()$volume_units_code,REDCap_info()$container,REDCap_info()$container_code,
                                               REDCap_info()$freezer,REDCap_info()$freezer_code,REDCap_info()$freezer_shelf,REDCap_info()$freezer_shelf_code),
             {

               session$setInputs(upload_studies = "Test", sample_upload_file = list(datapath = "Example Files/Earli_Upload_Test.xlsx"))

               #Check that test export includes 101 D0 1
               expect_true("101 D0 1" %in% redcap_samples()$sample_id)



             })
})
