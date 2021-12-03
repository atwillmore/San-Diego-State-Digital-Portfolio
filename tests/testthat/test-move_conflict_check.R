#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests
test_that("move conflict function correctly returns table with and without conflicts", {

  #Generate test redcap data
  test_redcap_locations <- data.frame(record_id = c(1:10),sample_id = c(1:10),container_type = "Cardboard box (6x6)",container_matthay_lab_id = "12010",
                                 grid_position = c(1:10),freezer_number = 3,freezer_shelf = 3)
  #Generate test moving file - no conflicts, same box new grid
  test_moving_file_new_grid <- tibble(`Existing ID` = c(1:10),container_type = "Cardboard box (6x6)",container_matthay_lab_id = "12010",
                                 grid_position = c(11:20),freezer_number = NA,freezer_shelf = NA)
  #Generate test moving file - no conflicts, same grid, new box
  test_moving_file_new_box <- tibble(`Existing ID` = c(1:10),container_type = "Cardboard box (6x6)",container_matthay_lab_id = "120101",
                             grid_position = c(1:10),freezer_number = NA,freezer_shelf = NA)
  #Generate test moving file - conflicts and ignores conflicts where existing grid samples are also being moved to new location
  test_moving_file_conflicts <- tibble(`Existing ID` = c(1:6),container_type = "Cardboard box (6x6)",container_matthay_lab_id = NA,
                                     grid_position = c(4:9),freezer_number = NA,freezer_shelf = NA)


  move_conflicts_new_grid<-move_conflicts(test_moving_file_new_grid,test_redcap_locations,"sample")
  move_conflicts_new_box<-move_conflicts(test_moving_file_new_box,test_redcap_locations,"sample")
  move_conflicts_non_sample<-move_conflicts(test_moving_file_new_box,test_redcap_locations,"freezer")
  move_conflicts_conflicts<-move_conflicts(test_moving_file_conflicts,test_redcap_locations,"sample")

  #Check that when level is not sample or if there are no conflicts, empty df is returned. If there are conflicts, they should be returned
  expect_equal(dim(move_conflicts_new_grid), c(0,7))
  expect_equal(dim(move_conflicts_new_box), c(0,7))
  expect_equal(dim(move_conflicts_non_sample), c(0,7))
  expect_equal(move_conflicts_conflicts$grid_position_update, c(7,8,9))

})
