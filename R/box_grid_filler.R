#Function for populating grid for box viewer. First creates empty vector of length grid size.
#Then iterates through the length of the vector and if grid position in redcap matches the vector index, populates, otherwise moves to the next
sample_vector <- function(grid_size,grid_info){

  #Create blank vecotr of length grid size
  vect <- vector(mode = "character",length = grid_size)

  #Loops of grid info, if grid number in blank vector = redcap grid location, populates index in vector with sample id and remaining volume
  for(row in 1:grid_size){
    for(i in 1:nrow(grid_info)){
      if(is.na(grid_info[i,"grid_position"])){
        next
      }
      else if(row == grid_info[i,"grid_position"]){
        vect[row] = paste(grid_info[i,"sample_id"],": ", grid_info[i,"sample_volume_remaining"], grid_info[i,"sample_volume_units"])
      }
    }
  }


  #Returns populated vector
  return(vect)

}

#For each container type: run the sample_vector function then use the result to populate the matrix to be displayed
grid_filler <- function(grid_info){
  if(grid_info[1,"container_type"] == "Plastic box (5x10)"){

    vect <- sample_vector(50,grid_info)

    grid <- as.data.frame(matrix(data = vect, nrow = 5, ncol = 10, byrow = TRUE))
  }
  else if(grid_info[1,"container_type"] == "Plastic box (10x10)"){

    vect <- sample_vector(100,grid_info)

    grid <- as.data.frame(matrix(data = vect, nrow = 10, ncol = 10, byrow = TRUE))
  }
  else if(grid_info[1,"container_type"] == "96-Well Box"){

    vect <- sample_vector(96,grid_info)

    grid <- as.data.frame(matrix(data = vect, nrow = 12, ncol = 8, byrow = TRUE))
  }
  else if(grid_info[1,"container_type"] == "Plastic Box (9x9)" || grid_info[1,"container_type"] == "Cardboard box (9x9)"){

    vect <- sample_vector(81,grid_info)

    grid <- as.data.frame(matrix(data = vect, nrow = 9, ncol = 9, byrow = TRUE))
  }
  else if(grid_info[1,"container_type"] == "Cardboard box (6x6)"){

    vect <- sample_vector(36,grid_info)

    grid <- as.data.frame(matrix(data = vect, nrow = 6, ncol = 6, byrow = TRUE))
  }
  else if(grid_info[1,"container_type"] == "Cardboard box (4x4)"){

    vect <- sample_vector(16,grid_info)

    grid <- as.data.frame(matrix(data = vect, nrow = 4, ncol = 4, byrow = TRUE))
  }
  else if(grid_info[1,"container_type"] == "Cardboard box (8x8)"){

    vect <- sample_vector(64,grid_info)

    grid <- as.data.frame(matrix(data = vect, nrow = 8, ncol = 8, byrow = TRUE))
  }

  return(grid)

}
