#########################################################
#
#         Install the needed dependancies
#
#########################################################
if(require("XLConnect") && require("ggplot2") && require("scatterplot3d")){
  print("XLConnect and ggplot2 are loaded correctly")
} else {
  print("Trying to install XLConnect")
  install.packages("XLConnect")
  print("Trying to install ggplot2")
  install.packages("ggplot2")
  print("Trying to install scatterplot3d")
  install.packages("scatterplot3d")
  if(require("XLConnect") && require("ggplot2") && require("scatterplot3d")){
    print("XLConnect, ggplot2, and scatterplot3d are installed and loaded")
  } else {
    stop("could not install XLConnect, ggplot2, scatterplot3d")
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

#########################################################
#
#   Functions to graph the data
#
#########################################################

# Returns the scatter points of log(sigma) vs log(Mass) to plot with ggplot2
graph.sigma.mass <- function(sheet, colorcode, shapecode) {
  re.data <- sheet$LREJB_KPC_DEV
  re.error <- sheet$E_LRE_DEVAF814W
  sigma.data <- sheet$LSIGMA_COR # log10(sigma) in km/s
  sigma.error <- sheet$E_LSIGMA # Error in Sigma
}

# Returns the scatter points of log(re) vs log(Mass) to plot with ggplot2
graph.re.mass <- function(sheet, colorcode, shapecode) {
  re.data <- sheet$LREJB_KPC_DEV
  re.error <- sheet$E_LRE_DEVAF814W
  mass.data <- sheet$LMASS_DEV
  mass.error <- sheet$E_LMASS_DEV
  return(geom_point())
}

# Returns the scatter points of 1.30log(sigma) - 0.82log(<I>e) vs log(re) to plot with ggplot2
graph.sigmaI.re <- function(sheet, res, colorcode, shapecode) {
  re.data <- sheet$LREJB_KPC_DEV
  re.error <- sheet$E_LRE_DEVAF814W
  ie.data <- sheet$LIEJB_DEV
  ie.error <- sheet$e_lieJB_DEV # 
  sigma.data <- sheet$LSIGMA_COR # log10(sigma) in km/s
  sigma.error <- sheet$E_LSIGMA # Error in Sigma
}
#########################################################
#
#   Go through whichever sheets are needed and graph
#
##########################################################

#####################
#
#  3D Plot
#
#####################
field.sigma <- spectrophotometric_data$FieldGalaxies$LSIGMA_COR
field.sigma.error <- spectrophotometric_data$FieldGalaxies$E_LSIGMA
field.ie <- spectrophotometric_data$FieldGalaxies$LIEJB_DEV
field.ie.error <- spectrophotometric_data$FieldGalaxies$e_lIeJB_DEV
field.re <- spectrophotometric_data$FieldGalaxies$LREJB_KPC_DEV
field.re.error <- spectrophotometric_data$FieldGalaxies$E_LRE_DEVAF814W

scatterplot3d(field.ie, field.re, field.sigma, angle = 115, 
              xlab = "Log Lr", ylab = "Log Sigma", zlab = "Log Re",
              color = "blue", type = "p", pch = 16)
