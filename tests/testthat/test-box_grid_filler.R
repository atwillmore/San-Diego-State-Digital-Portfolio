#Check test coverage using: devtools::test_coverage_active_file()
# devtools::test() runs all tests
test_that("box fill function produces correct dimensions, fills in correct direction", {

  #Generate test data for 5x10
  test_box_5_10 <- data.frame(sample_id = c(1:50), sample_volume_units = "mL", sample_volume_remaining = "10",container_type = "Plastic box (5x10)",
                              grid_position = c(1:50))
  #Check that missing grid position is handled correctly
  test_box_5_10$grid_position[30] <- NA
  test_5_10 <-grid_filler(test_box_5_10)
  #Generate test data for 10x10
  test_box_10_10 <- data.frame(sample_id = c(1:100), sample_volume_units = "mL", sample_volume_remaining = "10",container_type = "Plastic box (10x10)",
                              grid_position = c(1:100))
  test_10_10 <-grid_filler(test_box_10_10)
  #Generate test data for 96 well
  test_box_96 <- data.frame(sample_id = c(1:96), sample_volume_units = "mL", sample_volume_remaining = "10",container_type = "96-Well Box",
                               grid_position = c(1:96))
  test_96 <-grid_filler(test_box_96)
  #Generate test data for 9x9
  test_box_9_9 <- data.frame(sample_id = c(1:81), sample_volume_units = "mL", sample_volume_remaining = "10",container_type = "Plastic Box (9x9)",
                               grid_position = c(1:81))
  test_9_9 <-grid_filler(test_box_9_9)
  #Generate test data for 6x6
  test_box_6_6 <- data.frame(sample_id = c(1:36), sample_volume_units = "mL", sample_volume_remaining = "10",container_type = "Cardboard box (6x6)",
                             grid_position = c(1:36))
  test_6_6 <-grid_filler(test_box_6_6)
  #Generate test data for 4x4
  test_box_4_4 <- data.frame(sample_id = c(1:16), sample_volume_units = "mL", sample_volume_remaining = "10",container_type = "Cardboard box (4x4)",
                             grid_position = c(1:16))
  test_4_4 <-grid_filler(test_box_4_4)
  #Generate test data for 8x8
  test_box_8_8 <- data.frame(sample_id = c(1:64), sample_volume_units = "mL", sample_volume_remaining = "10",container_type = "Cardboard box (8x8)",
                             grid_position = c(1:64))
  test_8_8 <-grid_filler(test_box_8_8)

  #Check that dimensions are correct for each container type
  expect_equal(dim(test_5_10), c(5,10))
  expect_equal(dim(test_10_10), c(10,10))
  expect_equal(dim(test_96), c(12,8))
  expect_equal(dim(test_9_9), c(9,9))
  expect_equal(dim(test_6_6), c(6,6))
  expect_equal(dim(test_4_4), c(4,4))
  expect_equal(dim(test_8_8), c(8,8))

  #Check that the direction of fill is correct (by column rather than by row)
  expect_equal(test_5_10[2,1], "11 :  10 mL")
  expect_equal(test_5_10[3,10], "")
  expect_equal(test_10_10[2,1], "11 :  10 mL")
  expect_equal(test_96[2,1], "9 :  10 mL")
  expect_equal(test_9_9[2,1], "10 :  10 mL")
  expect_equal(test_6_6[2,1], "7 :  10 mL")
  expect_equal(test_4_4[2,1], "5 :  10 mL")
  expect_equal(test_8_8[2,1], "9 :  10 mL")

})
