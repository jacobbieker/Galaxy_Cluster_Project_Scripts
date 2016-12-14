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


coma.re <- spectrophotometric_data$Coma$lreJB_kpc_DEV
coma.ie <- spectrophotometric_data$Coma$lIeJB_cor
coma.sigma <- spectrophotometric_data$Coma$lsigma_cor

fundamental.plane <- scatterplot3d(field.one.LO.sigma, field.one.LO.ie, field.one.LO.re, angle = 135, 
              xlab = "Log Sigma", ylab = "Log Ie", zlab = "Log Re", main = "Fundamental Plane",
              color = "red", type = "p", pch = 16, cex.symbols = 1.3,
              xlim = c(0.5, 2.1), ylim = c(-2, 2), zlim = c(0, 2.2),
              axis = TRUE, tick.marks = TRUE, box = TRUE)
fundamental.plane$points3d(coma.sigma, coma.ie, coma.re, col="yellow", type="p", pch=20, cex=1.2)
fundamental.plane$points3d(field.one.HI.sigma, field.one.HI.ie, field.one.HI.re, col="red", type="p", pch=20, cex = 1.5)
fundamental.plane$points3d(field.two.HI.sigma, field.two.HI.ie, field.two.HI.re, col="blue", type="p", pch=20, cex = 1.5)
fundamental.plane$points3d(field.two.LO.sigma, field.two.LO.ie, field.two.LO.re, col="blue", type="p", pch=20, cex = 1.3)


"
ms0451p6m0305.sigma <- spectrophotometric_data$MS0451p6m0305member$LSIGMA_COR
ms0451p6m0305.sigma.error <- spectrophotometric_data$MS0451p6m0305member$E_LSIGMA
ms0451p6m0305.ie <- spectrophotometric_data$MS0451p6m0305member$LIEJB_DEV
ms0451p6m0305.ie.error <- spectrophotometric_data$MS0451p6m0305member$e_lIeJB_DEV
ms0451p6m0305.re <- spectrophotometric_data$MS0451p6m0305member$LREJB_KPC_DEV
ms0451p6m0305.re.error <- spectrophotometric_data$MS0451p6m0305member$E_LRE_DEVAF814W

print(length(ms0451p6m0305.sigma))
print(length(ms0451p6m0305.re))
print(length(ms0451p6m0305.ie))

fundamental.plane$points3d(ms0451p6m0305.ie, ms0451p6m0305.re, ms0451p6m0305.sigma)

RXJ0152p7m1357.sigma <- spectrophotometric_data$RXJ0152p7m1357member$LSIGMA_COR
RXJ0152p7m1357.sigma.error <- spectrophotometric_data$RXJ0152p7m1357member$E_LSIGMA
RXJ0152p7m1357.ie <- spectrophotometric_data$RXJ0152p7m1357member$LIEJB_DEV
RXJ0152p7m1357.ie.error <- spectrophotometric_data$RXJ0152p7m1357member$e_lIeJB_DEV
RXJ0152p7m1357.re <- spectrophotometric_data$RXJ0152p7m1357member$LREJB_KPC_DEV
RXJ0152p7m1357.re.error <- spectrophotometric_data$RXJ0152p7m1357member$E_LRE_DEVAF814W

fundamental.plane$points3d(RXJ0152p7m1357.ie, RXJ0152p7m1357.re, RXJ0152p7m1357.sigma)

RXJ1226p9p3332.sigma <- spectrophotometric_data$RXJ1226p9p3332member$LSIGMA_COR
RXJ1226p9p3332.sigma.error <- spectrophotometric_data$RXJ1226p9p3332member$E_LSIGMA
RXJ1226p9p3332.ie <- spectrophotometric_data$RXJ1226p9p3332member$LIEJB_DEV
RXJ1226p9p3332.ie.error <- spectrophotometric_data$RXJ1226p9p3332member$e_lIeJB_DEV
RXJ1226p9p3332.re <- spectrophotometric_data$RXJ1226p9p3332member$LREJB_KPC_DEV
RXJ1226p9p3332.re.error <- spectrophotometric_data$RXJ1226p9p3332member$E_LRE_DEVAF814W

fundamental.plane$points3d(RXJ1226p9p3332.ie, RXJ1226p9p3332.re, RXJ1226p9p3332.sigma)
"