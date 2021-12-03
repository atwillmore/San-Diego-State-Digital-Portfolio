#Format upload file to match REDCap formatting
sample_move_upload_formatting <-function(upload_file,container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code){

  #Select update columns, re-label data based on translation tables from utils.R
  upload_file <- upload_file %>% select(-contains("current")) %>%
    select(record_id,sample_id,container_matthay_lab_id = container_matthay_lab_id_update, container_type = container_type_update,
           freezer_number = freezer_number_update, freezer_shelf_number = freezer_shelf_number_update) %>%
    mutate(container_type = factor(container_type, levels = container, labels = container_code),
           freezer_number = factor(freezer_number, levels = freezer, labels = freezer_code),
           freezer_shelf_number = factor(freezer_shelf_number, levels = freezer_shelf, labels = freezer_shelf_code),
           across(everything(),as.character))

  return(upload_file)

}
