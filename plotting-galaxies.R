#########################################################
#
#         Install the needed dependancies
#
#########################################################
if(require("XLConnect") && require("ggplot2") && require("scatterplot3d")){
  print("XLConnect and ggplot2 are loaded correctly")
} else {
  print("Trying to install XLConnect")
  install.packages("XLConnect", dependencies = TRUE)
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
#  Yellow is Coma, small red is z < .8, big is z > .8, Sample 2 = blue x <.8, 
#####################
field.sample.one.LORDSHFT.data <- subset(spectrophotometric_data$FieldGalaxies, REDSHIFT < 0.8 & SAMPLE == 1);
field.sample.one.HIRDSHFT.data <- subset(spectrophotometric_data$FieldGalaxies, REDSHIFT > 0.8 & SAMPLE == 1);

field.sample.two.LORDSHFT.data <- subset(spectrophotometric_data$FieldGalaxies, REDSHIFT < 0.8 & SAMPLE == 2);
field.sample.two.HIRDSHFT.data <- subset(spectrophotometric_data$FieldGalaxies, REDSHIFT > 0.8 & SAMPLE == 2);

field.one.LO.sigma <- field.sample.one.LORDSHFT.data$LSIGMA_COR
field.one.HI.sigma <- field.sample.one.HIRDSHFT.data$LSIGMA_COR

field.two.LO.sigma <- field.sample.two.LORDSHFT.data$LSIGMA_COR
field.two.HI.sigma <- field.sample.two.HIRDSHFT.data$LSIGMA_COR


field.one.LO.sigma.error <- field.sample.one.LORDSHFT.data$E_LSIGMA
field.one.HI.sigma.error <- field.sample.one.HIRDSHFT.data$E_LSIGMA
field.two.LO.sigma.error <- field.sample.two.LORDSHFT.data$E_LSIGMA
field.two.HI.sigma.error <- field.sample.two.HIRDSHFT.data$E_LSIGMA

field.one.LO.ie <- field.sample.one.LORDSHFT.data$LIEJB_DEV
field.one.HI.ie <- field.sample.one.HIRDSHFT.data$LIEJB_DEV
field.two.LO.ie <- field.sample.two.LORDSHFT.data$LIEJB_DEV
field.two.HI.ie <- field.sample.two.HIRDSHFT.data$LIEJB_DEV

field.one.LO.re <- field.sample.one.LORDSHFT.data$LREJB_KPC_DEV
field.one.HI.re <- field.sample.one.HIRDSHFT.data$LREJB_KPC_DEV
field.two.LO.re <- field.sample.two.LORDSHFT.data$LREJB_KPC_DEV
field.two.HI.re <- field.sample.two.HIRDSHFT.data$LREJB_KPC_DEV

coma.data <- spectrophotometric_data$Coma
coma.re <- coma.data$lreJB_kpc_DEV
coma.ie <- coma.data$lIeJB_cor
coma.sigma <- coma.data$lsigma_cor

###########################################################################################
#
#
#    Creating the stacked plots for publication
#
#
###########################################################################################

##########################
# Fundamental Plane graphs
##########################

# Side On Graph
fundamental_plane_headon <- ggplot() +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "red")) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "purple")) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "blue")) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "green")) +
  geom_point(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor), color = "yellow")) +
  geom_smooth(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor)), method = "lm", se = FALSE) +
  xlab('logre [kpc]') +
  ylab('1.3log(sigma) - 0.82log<I>')

fundamental_plane_headon

# Face On Graph
fundamental_plane_headon <- ggplot() +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "red")) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "purple")) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "blue")) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV), color = "green")) +
  geom_point(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor), color = "yellow")) +
  geom_smooth(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor), method = "lm", se = FALSE) +
  xlab('logre [kpc]') +
  ylab('1.3log(sigma) - 0.82log<I>')

fundamental_plane_headon
