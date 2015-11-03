#########################################################
#
#         Install the needed dependancies
#
#########################################################
if(require("XLConnect")){
  print("XLConnect is loaded correctly")
} else {
  print("trying to install XLConnect")
  install.packages("XLConnect")
  if(require("XLConnect")){
    print("XLConnect are installed and loaded")
  } else {
    stop("could not install XLConnect, yaml, gdata")
  }
}

#########################################################
#
#       Loading the data
#
#########################################################

spectrophotometric_data_workbook <- loadWorkbook("GCP_spectrophot_data.xlsx")

spectrophotometric_data <- readWorksheet(spectrophotometric_data_workbook, sheet = getSheets(spectrophotometric_data_workbook))

#########################################################
#
#    Functions to calculate the necessary values
#
#########################################################

# Calculate the quadrature error
get.quad.error <- function(error.1, error.2) {
  error <- sqrt(error.1^2 + error.2^2)
  return(error);
}

