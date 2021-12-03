format_for_upload <- function(study_list,selected_study,last_record,upload_df,timepoint,timepoint_code,type,type_code,volume_units,volume_units_code,
                              container,container_code,freezer,freezer_code,freezer_shelf,freezer_shelf_code){
  #Update the column names to reflect what's in redcap
  colnames(upload_df) <- c("sample_id","patient_id","timepoint","sample_type","date_time_collection","sample_volume","sample_volume_units",
                           "container_type","container_matthay_lab_id","grid_position","freezer_number","freezer_shelf_number","date_stored","stored_by")

  #Get event name based on selected study. Uses get_event_name function defined in utils.R
  event <- get_event_name(study_list,selected_study)

  #Prepare template for upload - create record_id using highest number in redcap+1, reassign values w/ translation column. Mark samples as stored
  ready_for_sample_upload <- upload_df %>% mutate(record_id = paste(tolower(selected_study),last_record + row_number(),sep = "_"),
                                                  sample_type = factor(sample_type, levels = type, labels = type_code),
                                                  sample_volume_units = factor(sample_volume_units, levels = volume_units, labels = volume_units_code),
                                                  container_type = factor(container_type, levels = container, labels = container_code),
                                                  freezer_number = factor(freezer_number, levels = freezer, labels = freezer_code),
                                                  freezer_shelf_number = factor(freezer_shelf_number, levels = freezer_shelf, labels = freezer_shelf_code),
                                                  sample_storage_complete = 2,
                                                  sample_status = 1,
                                                  redcap_event_name = event,
                                                  across(everything(),as.character)) %>%
    select(record_id,everything())

  return(ready_for_sample_upload)
}
