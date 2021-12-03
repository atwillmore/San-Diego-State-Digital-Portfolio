#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests



# Check File Load in Works Fine -------------------------------------------
test_that("Check that upload file loads in correctly", {
  #REDCAP API
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)

  #Call change_storage_location_server with args and inputs (file in testthat folder, file path only works when running all tests with test())
  testServer(change_storage_location_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url,
                                                         REDCap_info()$container,REDCap_info()$container_code,
                                               REDCap_info()$freezer,REDCap_info()$freezer_code,REDCap_info()$freezer_shelf,REDCap_info()$freezer_shelf_code),
             {

               session$setInputs(move_level = "sample", studies = "Test", moving_file = list(datapath = "Bulk move example.xlsx"))


               #Check that columns are correct
               expect_equal(colnames(moving_file()), c("Existing ID",
                                                              "container_matthay_lab_id",	"freezer_shelf_number",
                                                              "freezer_number",	"container_type",	"grid_position"))
               #Check that uploaded IDs are correct
               expect_equal(moving_file()$`Existing ID`, c("101 D0 1",
                                                                   "101 D0 2",
                                                                   "test",
                                                                   "test2"))



             })
})


# Confirm REDCap Export Works as Expected ---------------------------------
test_that("Check that redcap export works correctly", {

  #API Info
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)

  testServer(change_storage_location_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url,
                                                         REDCap_info()$container,REDCap_info()$container_code,
                                                         REDCap_info()$freezer,REDCap_info()$freezer_code,REDCap_info()$freezer_shelf,REDCap_info()$freezer_shelf_code),
             {
               session$setInputs(move_level = "sample", studies = "Test", moving_file = list(datapath = "Bulk move example.xlsx"))


               #Check that something loads in the Test arm sample id
               expect_true(length(redcap_locations()$sample_id) > 0)


             })
})


# REDCap and Upload File Merge Check --------------------------------------
test_that("Check that redcap export and upload file merge as expected for each sample type", {
  #API Info

  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)

  testServer(change_storage_location_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url,
                                                         REDCap_info()$container,REDCap_info()$container_code,
                                                         REDCap_info()$freezer,REDCap_info()$freezer_code,REDCap_info()$freezer_shelf,REDCap_info()$freezer_shelf_code),
             {

               session$setInputs(move_level = "sample", studies = "Test", moving_file = list(datapath = "Bulk move example.xlsx"))


               #Check that for sample level, there is at least 1 row present - should be but changes to redcap might change things
               #Check that colnames are correct
               expect_true(nrow(upload_file()) > 0)
               expect_equal(colnames(upload_file()), c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance",
                                                       "sample_id","container_type_current","container_matthay_lab_id_current","grid_position_current",
                                                       "freezer_number_current","freezer_shelf_number_current","sample_storage_complete","container_matthay_lab_id_update",
                                                       "freezer_shelf_number_update","freezer_number_update","container_type_update","grid_position_update"))


               #Check that colnames are correct for box level
               session$setInputs(move_level = "box", studies = "Test", moving_file = list(datapath = "Bulk move example.xlsx"))

               expect_equal(colnames(upload_file()), c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance",
                                                       "sample_id","container_type_current","container_matthay_lab_id_current","grid_position_current",
                                                       "freezer_number_current","freezer_shelf_number_current","sample_storage_complete","container_matthay_lab_id_update",
                                                       "freezer_shelf_number_update","freezer_number_update","container_type_update","grid_position_update"))

               #Check that colnames are correct for freezer level
               session$setInputs(move_level = "freezer", studies = "Test", moving_file = list(datapath = "Bulk move example.xlsx"))

               expect_equal(colnames(upload_file()), c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance",
                                                       "sample_id","container_type_current","container_matthay_lab_id_current","grid_position_current",
                                                       "freezer_number_current","freezer_shelf_number_current","sample_storage_complete","container_matthay_lab_id_update",
                                                       "freezer_shelf_number_update","freezer_number_update","container_type_update","grid_position_update"))

               #Check that colnames are correct for freezer shelf level
               session$setInputs(move_level = "freezer_shelf", studies = "Test", moving_file = list(datapath = "Bulk move example.xlsx"))

               expect_equal(colnames(upload_file()), c("record_id","redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance",
                                                       "sample_id","container_type_current","container_matthay_lab_id_current","grid_position_current",
                                                       "freezer_number_current","freezer_shelf_number_current","sample_storage_complete","container_matthay_lab_id_update",
                                                       "freezer_shelf_number_update","freezer_number_update","container_type_update","grid_position_update"))


             })

})



