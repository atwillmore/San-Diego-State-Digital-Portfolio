#Function for identifying conflicts in storage location while moving samples at the sample ID level
move_conflicts <- function(moving_file,redcap_locations,move_level){

  if(move_level == "sample"){

    #Joing by ids found in both redcap and upload file. If container or grid isn't included in upload file, populate with what's in redcap for
    # that sample. Create location identifier with new_location = box_gridposition
    comparison <- inner_join(moving_file,redcap_locations,by = c("Existing ID"="sample_id"),suffix = c("_update","_current")) %>%
      mutate(container_matthay_lab_id_update = ifelse(is.na(container_matthay_lab_id_update),container_matthay_lab_id_current,container_matthay_lab_id_update),
             grid_position_update = ifelse(is.na(grid_position_update),grid_position_current,grid_position_update),
             new_location = str_c(container_matthay_lab_id_update,grid_position_update,sep = "_")) %>%
      select(sample_id = `Existing ID`, container_matthay_lab_id_update,grid_position_update,new_location)

    #Filter for locations in redcap found in the upload file. Exclude if sample Ids are included in both the new and current list
    conflicts <- redcap_locations %>% mutate(new_location = str_c(container_matthay_lab_id,grid_position,sep = "_")) %>%
      filter(new_location %in% comparison$new_location) %>%
      left_join(comparison, by = "new_location", suffix = c("_current","_update")) %>%
      filter(!(sample_id_current %in% .$sample_id_update)) %>%
      select(record_id,sample_id_current,container_matthay_lab_id,grid_position,sample_id_update:grid_position_update)



  }
  #If movemement level is other than sample, return empty dataframe
  else{
    conflicts <- data.frame(record_id=character(),
                            sample_id_current=character(),
                            container_matthay_lab_id=character(),
                            grid_position=character(),
                            sample_id_update=character(),
                            container_matthay_lab_id_update=character(),
                            grid_position_update = character())
  }

  return(conflicts)

}
