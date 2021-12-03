#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests
test_that("check that columns have the correct names and text to code translation is done correctly", {

  #Get translations tables from REDCap_info function defined in utils
  translation_tables <- REDCap_info()

  #Create Example File
  test_move_file <- tibble(record_id = c(1:10),
                              redcap_event_name = "test",
                              redcap_repeat_instance = NA,
                              redcap_repeat_instrument = NA,
                              sample_id = c(1:10),
                              container_type_current = "Rack",
                              container_matthay_lab_id_current = 12010,
                              grid_position_current = c(1:10),
                              freezer_number_current = 3,
                              freezer_shelf_number_current = 3,
                              sample_storage_complete = "Complete",
                              container_matthay_lab_id_update = 12010,
                              freezer_number_update = "2",
                              freezer_shelf_number_update = "1 = Top",
                              container_type_update = "Rack",
                              grid_position_update = c(11:20))

  test_move_file_ready<- sample_move_upload_formatting(test_move_file,translation_tables$container,translation_tables$container_code,
                                                       translation_tables$freezer,translation_tables$freezer_code,translation_tables$freezer_shelf,
                                                       translation_tables$freezer_shelf_code)

  #Check that column names are correct
  expect_equal(colnames(test_move_file_ready),c("record_id","sample_id","container_matthay_lab_id","container_type","freezer_number","freezer_shelf_number"))
  #Check that columns are properly populated by randomly selecting 1 entry from each column
  expect_equal(as.character(test_move_file_ready[5,3]),"12010")
  expect_equal(as.character(test_move_file_ready[5,4]),"5")
  expect_equal(as.character(test_move_file_ready[5,5]),"2")
  expect_equal(as.character(test_move_file_ready[5,6]),"1")


})
