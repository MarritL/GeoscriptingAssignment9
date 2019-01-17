# Authors: Anne-Juul Welsink and Marrit Leenstra
# 17th January 2019
# Exercise 9, Geoscripting, Wageningen University 

# Function to download and unpack data from url to a user-specified data directory.
# Input: 
#    url: a url or vector of urls
#    dest_directory: directory where the data is saved and unzipped
# Output:
#    files saved in user-specified data directory. 

retrieveData <- function(url, dest_directory){
  
  # Create output folder if needed
  if (!dir.exists(dest_directory) ){
    dir.create(dest_directory, showWarnings = FALSE)
  } 
  
  # Get path of data directory and specify filename
  data_directory <- list.files(pattern = dest_directory, full.names = TRUE)
  filename <- "/exercise9_data.zip"
    
  # Download and unzip files
  download.file(url = url, destfile = paste0(data_directory, filename), mode="wb")
  unzip(paste0(data_directory, filename), exdir = dest_directory, overwrite = TRUE)  
  
  # Delete zip file
  file.remove(paste0(data_directory, filename))
  
}





