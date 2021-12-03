#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests

test_that("Check that correct columns are pulled", {

  #Redcap API info
  redcap_url = "https://redcap.ucsf.edu/api/"
  token = "XXXXXXXX"
  rcon_lims = redcapConnection(url = redcap_url, token = token)


  #Use test server to call box_viewer_server, pass necessary arguments and inputs
  testServer(box_viewer_Server, args = list(studies = REDCap_info()$studies, rcon_lims = rcon_lims, token = token, redcap_url = redcap_url), {
    session$setInputs(box_studies = "Test", box_select = "test1")

    #Check that correct columns are pulled as expected
    expect_setequal(colnames(box_samples()), c("sample_id","container_type","container_matthay_lab_id","grid_position",
                 "freezer_number","freezer_shelf_number","sample_volume_remaining","sample_volume_units",
                 "sample_status"))

  })
})
