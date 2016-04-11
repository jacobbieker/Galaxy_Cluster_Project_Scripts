#########################################################
#
#         Install the needed dependancies
#
#########################################################
if(require("XLConnect") && require("ggplot2") && require(gtable) && require("quantreg") && require("scatterplot3d") && require("scales")){
  print("XLConnect and ggplot2 are loaded correctly")
} else {
  print("Trying to install XLConnect")
  install.packages("XLConnect", dependencies = TRUE)
  print("Trying to install ggplot2")
  install.packages("ggplot2", dependencies = TRUE)
  print("Trying to install scatterplot3d")
  install.packages("scatterplot3d", dependencies = TRUE)
  print("installing gtable")
  install.packages("gtable", dependencies = TRUE)
  print("installing scales")
  install.packages("scales", dependencies = TRUE)
  print("Installing quantreg")
  install.packages("quantreg", dependencies = TRUE)
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

add.tick.marks <- function(graph_to_add_ticks_to) {
  ################################
  # Adding tick marks on all sides
  ################################
  # Convert the plot to a grob
  gt <- ggplotGrob(graph_to_add_ticks_to)
  
  # Get the position of the panel in the layout
  panel <-c(subset(gt$layout, name=="panel", se=t:r))
  
  ## For the bottom axis
  # Get the row number of the bottom axis in the layout
  rn <- which(gt$layout$name == "axis-b")
  
  # Extract the axis (tick marks only)
  axis.grob <- gt$grobs[[rn]]
  axisb <- axis.grob$children[[2]]  # Two children - get the second
  axisb  # Note: two grobs - tick marks and text
  
  # Get the tick marks
  xaxis = axisb$grobs[[1]]  # NOTE: tick marks first
  xaxis$y = xaxis$y - unit(0, "cm")  # Position them inside the panel
  
  # Add a new row to gt, and insert the revised xaxis grob into the new row.
  gt <- gtable_add_rows(gt, unit(0, "lines"), panel$t-1)
  gt <- gtable_add_grob(gt, xaxis, l = panel$l, t = panel$t, r = panel$r, name = "ticks")
  
  ## Repeat for the left axis
  # Get the row number of the left axis in the layout
  panel <-c(subset(gt$layout, name=="panel", se=t:r))
  rn <- which(gt$layout$name == "axis-l")
  
  # Extract the axis (tick marks and axis text)
  axis.grob <- gt$grobs[[rn]]
  axisl <- axis.grob$children[[2]]  # Two children - get the second
  axisl  # Note: two grobs -  text and tick marks
  
  # Get the tick marks
  yaxis = axisl$grobs[[2]] # NOTE: tick marks second
  yaxis$x = yaxis$x - unit(0, "cm") # Position them inside the panel
  
  # Add a new column to gt, and insert the revised yaxis grob into the new column.
  gt <- gtable_add_cols(gt, unit(0, "lines"), panel$r)
  gt <- gtable_add_grob(gt, yaxis, t = panel$t, l = panel$r+1, name = "ticks")
  
  # Turn clipping off
  gt$layout[gt$layout$name == "ticks", ]$clip = "off"
  
  # Draw it
  grid.draw(gt)
  
  #######################
  # End adding tick marks
  ######################
  
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

RXJ <- subset(spectrophotometric_data$RXJ1226p9p3332member, NUMBERI == 55 | NUMBERI == 229 | NUMBERI == 293 | NUMBERI == 310 | NUMBERI == 423 | NUMBERI == 441 | NUMBERI == 462 | NUMBERI == 491 | NUMBERI == 512 | NUMBERI == 529 | NUMBERI == 534 | NUMBERI == 557 | NUMBERI == 563 | NUMBERI == 641 | NUMBERI == 650 | NUMBERI == 703 | NUMBERI == 709 | NUMBERI == 760 | NUMBERI == 801 | NUMBERI == 899 | NUMBERI == 1047 | NUMBERI == 1164 | NUMBERI == 1170 | NUMBERI == 1199 | NUMBERI == 56 | NUMBERI == 104 | NUMBERI == 648 | NUMBERI == 675);

RXJ.sigma <- RXJ$LSIGMA_COR
RXJ.sigma.error <- RXJ$E_LSIGMA

field.one.LO.sigma <- field.sample.one.LORDSHFT.data$LSIGMA_COR
field.one.HI.sigma <- field.sample.one.HIRDSHFT.data$LSIGMA_COR

field.two.LO.sigma <- field.sample.two.LORDSHFT.data$LSIGMA_COR
field.two.HI.sigma <- field.sample.two.HIRDSHFT.data$LSIGMA_COR

field.one.LO.sigma.error <- field.sample.one.LORDSHFT.data$E_LSIGMA
field.one.HI.sigma.error <- field.sample.one.HIRDSHFT.data$E_LSIGMA
field.two.LO.sigma.error <- field.sample.two.LORDSHFT.data$E_LSIGMA
field.two.HI.sigma.error <- field.sample.two.HIRDSHFT.data$E_LSIGMA

RXJ.ie <- RXJ$LIEJB_DEV
field.one.LO.ie <- field.sample.one.LORDSHFT.data$LIEJB_DEV
field.one.HI.ie <- field.sample.one.HIRDSHFT.data$LIEJB_DEV
field.two.LO.ie <- field.sample.two.LORDSHFT.data$LIEJB_DEV
field.two.HI.ie <- field.sample.two.HIRDSHFT.data$LIEJB_DEV

RXJ.re <- RXJ$LREJB_KPC_DEV
field.one.LO.re <- field.sample.one.LORDSHFT.data$LREJB_KPC_DEV
field.one.HI.re <- field.sample.one.HIRDSHFT.data$LREJB_KPC_DEV
field.two.LO.re <- field.sample.two.LORDSHFT.data$LREJB_KPC_DEV
field.two.HI.re <- field.sample.two.HIRDSHFT.data$LREJB_KPC_DEV

coma.data <- spectrophotometric_data$Coma
coma.re <- coma.data$lreJB_kpc_DEV
coma.ie <- coma.data$lIeJB_cor
coma.sigma <- coma.data$lsigma_cor

##########################################################################################
#
#
#  Calculate the errors not given in the data sheet and add to the data.frame
#
#
###########################################################################################

field.sample.one.HIRDSHFT.data$lSIGMA_lLG_IE_E <- sqrt((1.3*field.sample.one.HIRDSHFT.data$E_LSIGMA)^2 + (0.82*field.sample.one.HIRDSHFT.data$e_lIeJB_DEV)^2)
field.sample.one.LORDSHFT.data$lSIGMA_lLG_IE_E <- sqrt((1.3*field.sample.one.LORDSHFT.data$E_LSIGMA)^2 + (0.82*field.sample.one.LORDSHFT.data$e_lIeJB_DEV)^2)
field.sample.two.HIRDSHFT.data$lSIGMA_lLG_IE_E <- sqrt((1.3*field.sample.two.HIRDSHFT.data$E_LSIGMA)^2 + (0.82*field.sample.two.HIRDSHFT.data$e_lIeJB_DEV)^2)
field.sample.two.LORDSHFT.data$lSIGMA_lLG_IE_E <- sqrt((1.3*field.sample.two.LORDSHFT.data$E_LSIGMA)^2 + (0.82*field.sample.two.LORDSHFT.data$e_lIeJB_DEV)^2)
RXJ$lSIGMA_lLG_IE_E <- sqrt((1.3*RXJ$E_LSIGMA)^2 + (0.82*RXJ$e_lIeJB_DEV)^2)

coma.data$lSIGMA_lLG_IE_E_154 <- sqrt(((1.3/1.54)^2)*(coma.data$e_lsigma)^2)
field.sample.one.HIRDSHFT.data$lSIGMA_lLG_IE_E_154 <- sqrt(((1.3/1.54)^2)*(field.sample.one.HIRDSHFT.data$E_LSIGMA)^2 + ((0.82/1.54)^2)*(field.sample.one.HIRDSHFT.data$e_lIeJB_DEV)^2)
field.sample.one.LORDSHFT.data$lSIGMA_lLG_IE_E_154 <- sqrt(((1.3/1.54)^2)*(field.sample.one.LORDSHFT.data$E_LSIGMA)^2 + ((0.82/1.54)^2)*(field.sample.one.LORDSHFT.data$e_lIeJB_DEV)^2)
field.sample.two.HIRDSHFT.data$lSIGMA_lLG_IE_E_154 <- sqrt(((1.3/1.54)^2)*(field.sample.two.HIRDSHFT.data$E_LSIGMA)^2 + ((0.82/1.54)^2)*(field.sample.two.HIRDSHFT.data$e_lIeJB_DEV)^2)
field.sample.two.LORDSHFT.data$lSIGMA_lLG_IE_E_154 <- sqrt(((1.3/1.54)^2)*(field.sample.two.LORDSHFT.data$E_LSIGMA)^2 + ((0.82/1.54)^2)*(field.sample.two.LORDSHFT.data$e_lIeJB_DEV)^2)
RXJ$lSIGMA_lLG_IE_E_154 <- sqrt(((1.3/1.54)^2)*(RXJ$E_LSIGMA)^2 + ((0.82/1.54)^2)*(RXJ$e_lIeJB_DEV)^2)

coma.data$lREJB_lIE_lSIGMA_270 <- sqrt(((1.3/2.7)^2)*(coma.data$e_lsigma)^2)
field.sample.one.HIRDSHFT.data$lREJB_lIE_lSIGMA_270 <- sqrt(((2.22/2.7)^2)*(field.sample.one.HIRDSHFT.data$E_LRE_DEVAF814W)^2 + ((0.82/2.7)^2)*(field.sample.one.HIRDSHFT.data$e_lIeJB_DEV)^2+2*(2.22/2.7)*(0.82/2.7)*(-0.97)*(field.sample.one.HIRDSHFT.data$E_LRE_DEVAF814W)*(field.sample.one.HIRDSHFT.data$e_lIeJB_DEV) + ((1.3/2.7)^2)*(field.sample.one.HIRDSHFT.data$E_LSIGMA)^2)
field.sample.one.LORDSHFT.data$lREJB_lIE_lSIGMA_270 <- sqrt(((2.22/2.7)^2)*(field.sample.one.LORDSHFT.data$E_LRE_DEVAF814W)^2 + ((0.82/2.7)^2)*(field.sample.one.LORDSHFT.data$e_lIeJB_DEV)^2+2*(2.22/2.7)*(0.82/2.7)*(-0.97)*(field.sample.one.LORDSHFT.data$E_LRE_DEVAF814W)*(field.sample.one.LORDSHFT.data$e_lIeJB_DEV) + ((1.3/2.7)^2)*(field.sample.one.LORDSHFT.data$E_LSIGMA)^2)
field.sample.two.HIRDSHFT.data$lREJB_lIE_lSIGMA_270 <- sqrt(((2.22/2.7)^2)*(field.sample.two.HIRDSHFT.data$E_LRE_DEVAF814W)^2 + ((0.82/2.7)^2)*(field.sample.two.HIRDSHFT.data$e_lIeJB_DEV)^2+2*(2.22/2.7)*(0.82/2.7)*(-0.97)*(field.sample.two.HIRDSHFT.data$E_LRE_DEVAF814W)*(field.sample.two.HIRDSHFT.data$e_lIeJB_DEV) + ((1.3/2.7)^2)*(field.sample.two.HIRDSHFT.data$E_LSIGMA)^2)
field.sample.two.LORDSHFT.data$lREJB_lIE_lSIGMA_270 <- sqrt(((2.22/2.7)^2)*(field.sample.two.LORDSHFT.data$E_LRE_DEVAF814W)^2 + ((0.82/2.7)^2)*(field.sample.two.LORDSHFT.data$e_lIeJB_DEV)^2+2*(2.22/2.7)*(0.82/2.7)*(-0.97)*(field.sample.two.LORDSHFT.data$E_LRE_DEVAF814W)*(field.sample.two.LORDSHFT.data$e_lIeJB_DEV) + ((1.3/2.7)^2)*(field.sample.two.LORDSHFT.data$E_LSIGMA)^2)
RXJ$lREJB_lIE_lSIGMA_270 <- sqrt(((2.22/2.7)^2)*(RXJ$E_LRE_DEVAF814W)^2 + ((0.82/2.7)^2)*(RXJ$e_lIeJB_DEV)^2+2*(2.22/2.7)*(0.82/2.7)*(-0.97)*(RXJ$E_LRE_DEVAF814W)*(RXJ$e_lIeJB_DEV) + ((1.3/2.7)^2)*(RXJ$E_LSIGMA)^2)


###########################################################################################
#
#
#    Creating the stacked plots for publication
#
#
###########################################################################################

# Error bar end length
error.bar.end.length = 0.00

##########################
# Fundamental Plane graphs
##########################

# Side On Graph

# Get the coef for the line of best fit for Coma
coma.data$lSIGMA_lIE <- (1.3*coma.data$lsigma_cor)-(0.82*coma.data$lIeJB_cor)

fundamental_plane_headon <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = RXJ, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "gray", size=2) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "red", size=5) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "red", size=2) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "blue", size=5) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "blue", size=2) +
  geom_point(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor)), color = "yellow", size=2) +
  geom_point(data = RXJ, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor)), color = "black", size=2, shape=21) +
  xlab('logre [kpc]') +
  ylab('1.3log(σ) - 0.82log<I>') + 
  # Currently calculated by coef(lm(data=coma.data, lSIGMA_lIE ~ lreJB_kpc_DEV))
  geom_abline(intercept = 0.4259036, slope=1.0079013) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  # Coma Error Bar
  geom_errorbar(aes(x=1.4, ymin=-0.5 - mean(coma.data$e_lsigma, na.rm = TRUE), ymax=-0.5 + mean(coma.data$e_lsigma, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="yellow") +
  # Sample 1 high redshift error
  geom_errorbar(aes(x=1.2, ymin=-0.5 - mean(field.sample.one.HIRDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), ymax=-0.5 + mean(field.sample.one.HIRDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=-0.5, x=1.2, xmin=1.2 - mean(field.sample.one.HIRDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), xmax=1.2 + mean(field.sample.one.HIRDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  # Sample 1 low redshift error
  geom_errorbar(aes(x=1.3, ymin=-0.5 - mean(field.sample.one.LORDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), ymax=-0.5 + mean(field.sample.one.LORDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=-0.5, x=1.3, xmin=1.3 - mean(field.sample.one.LORDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), xmax=1.3 + mean(field.sample.one.LORDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black") +
  
  # Sample 2 high redshift error
  geom_errorbar(aes(x=1.1, ymin=-0.5 - mean(field.sample.two.HIRDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), ymax=-0.5 + mean(field.sample.two.HIRDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=-0.5, x=1.1, xmin=1.1 - mean(field.sample.two.HIRDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), xmax=1.1 + mean(field.sample.one.HIRDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  # Sample 2 low redshift error
  geom_errorbar(aes(x=1.0, ymin=-0.5 - mean(field.sample.two.LORDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), ymax=-0.5 + mean(field.sample.two.LORDSHFT.data$lSIGMA_lLG_IE_E, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=-0.5, x=1.0, xmin=1.0 - mean(field.sample.two.LORDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), xmax=1.0 + mean(field.sample.two.LORDSHFT.data$E_LRE_DEVAF814W, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black") +
  
  # RXJ error
  geom_errorbar(aes(x=0.9, ymin=-0.5 - mean(RXJ$lSIGMA_lLG_IE_E, na.rm = TRUE), ymax=-0.5 + mean(RXJ$lSIGMA_lLG_IE_E, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="gray") +
  geom_errorbarh(aes(y=-0.5, x=0.9, xmin=0.9 - mean(RXJ$E_LRE_DEVAF814W, na.rm = TRUE), xmax=0.9 + mean(RXJ$E_LRE_DEVAF814W, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="gray")
  
add.tick.marks(fundamental_plane_headon)

# Face On Graph

# Getting the Coma Cluster line of best fit
coma.data$lREJB_lIEJB_lSIGMA <- ((2.22*coma.data$lreJB_kpc_DEV)-(0.82*coma.data$lIeJB_cor)+(1.3*coma.data$lsigma_cor))/2.70
coma.data$lIEJB_lSIGMA <- ((1.3*coma.data$lIeJB_cor)+(0.82*coma.data$lsigma_cor))/1.54

fundamental_plane_faceon <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = RXJ, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "gray", size=2) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "red", size=5) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "red", size=2) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "blue", size=5) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "blue", size=2) +
  geom_point(data = RXJ, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = ((2.22*lreJB_kpc_DEV)-(0.82*lIeJB_cor)+(1.3*lsigma_cor))/2.70, y = ((1.3*lIeJB_cor)+(0.82*lsigma_cor))/1.54), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = ((2.22*lreJB_kpc_DEV)-(0.82*lIeJB_cor)+(1.3*lsigma_cor))/2.70, y = ((1.3*lIeJB_cor)+(0.82*lsigma_cor))/1.54), color = "black", size=2, shape=21) +
  geom_smooth(data = coma.data, aes(x = ((2.22*lreJB_kpc_DEV)-(0.82*lIeJB_cor)+(1.3*lsigma_cor))/2.70, y = ((1.3*lIeJB_cor)+(0.82*lsigma_cor))/1.54, method = "lm", se = FALSE)) +
  xlab('(2.22logre - 0.82log<I>e + 1.3log(σ))/2.70') +
  ylab('(1.3log<I>e + 0.82log(σ))/1.54') +
  # Currently calculated by coef(lm(data=coma.data, lSIGMA_lIE ~ lreJB_kpc_DEV))
  geom_abline(intercept = 3.6978053, slope=-0.6993457) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  # Coma Error Bar
  geom_errorbarh(aes(y=2.3, x=0.0, xmin=0.0 - mean(coma.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), xmax=0.0 + mean(coma.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="yellow") +
  geom_errorbar(aes(x=0.0, ymin=2.3 - mean(coma.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), ymax=2.3 + mean(coma.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="yellow") +
  # Sample 1 high redshift error
  geom_errorbar(aes(x=-0.1, ymin=2.3 - mean(field.sample.one.HIRDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), ymax=2.3 + mean(field.sample.one.HIRDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=2.3, x=-0.1, xmin=-0.1 - mean(field.sample.one.HIRDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), xmax=-0.1 + mean(field.sample.one.HIRDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  # Sample 1 low redshift error0
  geom_errorbar(aes(x=-0.2, ymin=2.3 - mean(field.sample.one.LORDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), ymax=2.3 + mean(field.sample.one.LORDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=2.3, x=-0.2, xmin=-0.2 - mean(field.sample.one.LORDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), xmax=-0.2 + mean(field.sample.one.LORDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black") +
  
  # Sample 2 high redshift error
  geom_errorbar(aes(x=-0.3, ymin=2.3 - mean(field.sample.two.HIRDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), ymax=2.3 + mean(field.sample.two.HIRDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=2.3, x=-0.3, xmin=-0.3 - mean(field.sample.two.HIRDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), xmax=-0.3 + mean(field.sample.one.HIRDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  # Sample 2 low redshift error
  geom_errorbar(aes(x=-0.4, ymin=2.3 - mean(field.sample.two.LORDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), ymax=2.3 + mean(field.sample.two.LORDSHFT.data$lSIGMA_lLG_IE_E_154, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=2.3, x=-0.4, xmin=-0.4 - mean(field.sample.two.LORDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), xmax=-0.4 + mean(field.sample.two.LORDSHFT.data$lREJB_lIE_lSIGMA_270, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black") +

  # RXJ error
  geom_errorbar(aes(x=0.1, ymin=2.3 - mean(RXJ$lSIGMA_lLG_IE_E_154, na.rm = TRUE), ymax=2.3 + mean(RXJ$lSIGMA_lLG_IE_E_154, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="gray") +
  geom_errorbarh(aes(y=2.3, x=0.1, xmin=0.1 - mean(RXJ$lREJB_lIE_lSIGMA_270, na.rm = TRUE), xmax=0.1 + mean(RXJ$lREJB_lIE_lSIGMA_270, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="gray")


add.tick.marks(fundamental_plane_faceon)

##################
# Velocity Dispersion vs log M/L
##################

lsigma.vs.logml <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = RXJ, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "gray", size=2) +
  geom_point(data = RXJ, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = LML_JB_DEV), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "black", size=2, shape=21) +
  xlab('log(σ)') +
  ylab('log(M/Lb) [M/L]') +
  # Currently calculated by coef(lm(data=coma.data, lML_JB_DEV ~ lsigma_cor)) slope: 1.07*log(sigma), -1.560
  geom_abline(intercept = -0.8569, slope=0.7535) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  # Coma Error Bar
  geom_errorbarh(aes(y=-0.3, x=2.7, xmin=2.7 - mean(coma.data$e_lsigma, na.rm = TRUE), xmax=2.7 + mean(coma.data$e_lsigma, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="yellow") +
  geom_errorbar(aes(x=2.7, ymin=-0.3 - mean(coma.data$e_lMgb, na.rm = TRUE), ymax=-0.3 + mean(coma.data$e_lMgb, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="yellow") +
  # Sample 1 high redshift error
  geom_errorbar(aes(x=2.55, ymin=-0.3 - mean(field.sample.one.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=-0.3 + mean(field.sample.one.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=-0.3, x=2.55, xmin=2.55 - mean(field.sample.one.HIRDSHFT.data$E_LSIGMA, na.rm = TRUE), xmax=2.55 + mean(field.sample.one.HIRDSHFT.data$E_LSIGMA, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  # Sample 1 low redshift error0
  geom_errorbar(aes(x=2.7, ymin=0.0 - mean(field.sample.one.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=0.0 + mean(field.sample.one.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=0.0, x=2.7, xmin=2.7 - mean(field.sample.one.LORDSHFT.data$E_LSIGMA, na.rm = TRUE), xmax=2.7 + mean(field.sample.one.LORDSHFT.data$E_LSIGMA, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black") +
  
  # Sample 2 high redshift error
  geom_errorbar(aes(x=2.55, ymin=0.0 - mean(field.sample.two.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=0.0 + mean(field.sample.two.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=0.0, x=2.55, xmin=2.55 - mean(field.sample.two.HIRDSHFT.data$E_LSIGMA, na.rm = TRUE), xmax=2.55 + mean(field.sample.one.HIRDSHFT.data$E_LSIGMA, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  # Sample 2 low redshift error
  geom_errorbar(aes(x=2.65, ymin=0.3 - mean(field.sample.two.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=0.3 + mean(field.sample.two.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=0.3, x=2.65, xmin=2.65 - mean(field.sample.two.LORDSHFT.data$E_LSIGMA, na.rm = TRUE), xmax=2.65 + mean(field.sample.two.LORDSHFT.data$E_LSIGMA, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black") +

  # RXJ error
  geom_errorbar(aes(x=2.50, ymin=0.3 - mean(RXJ$E_LML_JB_DEV, na.rm = TRUE), ymax=0.3 + mean(RXJ$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="gray") +
  geom_errorbarh(aes(y=0.3, x=2.50, xmin=2.50 - mean(RXJ$E_LSIGMA, na.rm = TRUE), xmax=2.50 + mean(RXJ$E_LSIGMA, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="gray")


add.tick.marks(lsigma.vs.logml)

###############################
# log Mass vs log(Mass/L)
###############################

# Getting the weights for the rq value
coma.data$lm_vs_lml_weights <- (abs(0.24*coma.data$lMass_DEV + coma.data$lML_JB_DEV + 1.754))/(sqrt(0.24^2+1))
# Starting values for plot
starting.x <- 12.3

lm.vs.logml <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LMASS_DEV, y = LML_JB_DEV), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lMass_DEV, y = lML_JB_DEV), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = lMass_DEV, y = lML_JB_DEV), color = "black", size=2, shape=21) +
  xlab('log(Mass)') +
  ylab('log(M/Lb) [M/L]') +
  # Calcuated by quantreg's rq(coma.data$lML_JB_DEV ~ coma.data$lMass_DEV)
  geom_abline(intercept = -1.6587, slope = 0.2262) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  # Sample 1 high redshift error
  geom_errorbar(aes(x=starting.x, ymin=-0.3 - mean(field.sample.one.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=-0.3 + mean(field.sample.one.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=-0.3, x=starting.x, xmin=starting.x - mean(field.sample.one.HIRDSHFT.data$E_LMASS_DEV, na.rm = TRUE), xmax=starting.x + mean(field.sample.one.HIRDSHFT.data$E_LMASS_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  # Sample 1 low redshift error0
  geom_errorbar(aes(x=starting.x - 0.3, ymin=-0.3 - mean(field.sample.one.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=-0.3 + mean(field.sample.one.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="red") +
  geom_errorbarh(aes(y=-0.3, x=starting.x - 0.3, xmin=starting.x-0.3 - mean(field.sample.one.LORDSHFT.data$E_LMASS_DEV, na.rm = TRUE), xmax=starting.x - 0.3 + mean(field.sample.one.LORDSHFT.data$E_LMASS_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black") +
  
  # Sample 2 high redshift error
  geom_errorbar(aes(x=starting.x, ymin=0.0 - mean(field.sample.two.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=0.0 + mean(field.sample.two.HIRDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=0.0, x=starting.x, xmin=starting.x - mean(field.sample.two.HIRDSHFT.data$E_LMASS_DEV, na.rm = TRUE), xmax=starting.x + mean(field.sample.one.HIRDSHFT.data$E_LMASS_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  # Sample 2 low redshift error
  geom_errorbar(aes(x=starting.x-0.3, ymin=0.0 - mean(field.sample.two.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), ymax=0.0 + mean(field.sample.two.LORDSHFT.data$E_LML_JB_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="blue") +
  geom_errorbarh(aes(y=0.0, x=starting.x-0.3, xmin=starting.x-0.3 - mean(field.sample.two.LORDSHFT.data$E_LMASS_DEV, na.rm = TRUE), xmax=starting.x-0.3 + mean(field.sample.two.LORDSHFT.data$E_LMASS_DEV, na.rm = TRUE), width = error.bar.end.length, height=error.bar.end.length), color="black")


add.tick.marks(lm.vs.logml)

##########################################
#
# Spectrometry data Graphs
#
##########################################

#############
# Log(sigma) vs redshift
#############

lsigma.vs.redshift <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = REDSHIFT, y = LSIGMA_COR), color = "black", size=2, shape=21) +
  xlab('Redshift') +
  ylab('log(σ)') +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver())
  

add.tick.marks(lsigma.vs.redshift)

#############
# log(sigma) vs log(C4668)
############

lsigma.vs.lc4668 <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = LC4668_COR), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "black", size=2, shape=21) +
  xlab('log(σ)') +
  ylab('log(C4668)') +
  # Calcuated by quantreg's rq(coma.data$lML_JB_DEV ~ coma.data$lMass_DEV)
  #geom_abline(intercept = -1.6587, slope = 0.2262) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver())

add.tick.marks(lsigma.vs.lc4668)

#############
# log(sigma) vs log(Fe4383)
#############

lsigma.vs.lfe4383 <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = LFE4383_COR), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "black", size=2, shape=21) +
  xlab('log(σ)') +
  ylab('log(Fe4383)') +
  # Calcuated by quantreg's rq(coma.data$lML_JB_DEV ~ coma.data$lMass_DEV)
  #geom_abline(intercept = -1.6587, slope = 0.2262) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver())

add.tick.marks(lsigma.vs.lfe4383)

#############
# log(sigma) vs CN3883
#############

lsigma.vs.cn3883 <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = CN3883_COR), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "black", size=2, shape=21) +
  xlab('log(σ)') +
  ylab('CN3883') +
  # Calcuated by quantreg's rq(coma.data$lML_JB_DEV ~ coma.data$lMass_DEV)
  #geom_abline(intercept = -1.6587, slope = 0.2262) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver())
 
add.tick.marks(lsigma.vs.cn3883)

#############
# log(sigma) vs H(delta) + H(Gamma)
#############

lsigma.vs.lHdgA <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHdgA_cor), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "black", size=2, shape=21) +
  xlab('log(σ)') +
  ylab('H(Gamma) + H(Delta)') +
  # Calcuated by quantreg's rq(coma.data$lML_JB_DEV ~ coma.data$lMass_DEV)
  #geom_abline(intercept = -1.6587, slope = 0.2262) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver())

add.tick.marks(lsigma.vs.lHdgA)
#############
# log(sigma) vs log(H(zeta))
#############

lsigma.vs.lhzeta <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "red", size=5) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "red", size=2) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "blue", size=5) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "blue", size=2) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LSIGMA_COR, y = lHzetaA_cor), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "yellow", size=2) +
  geom_point(data = coma.data, aes(x = lsigma_cor, y = lML_JB_DEV), color = "black", size=2, shape=21) +
  xlab('log(σ)') +
  ylab('log(H(zeta))') +
  # Calcuated by quantreg's rq(coma.data$lML_JB_DEV ~ coma.data$lMass_DEV)
  #geom_abline(intercept = -1.6587, slope = 0.2262) +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=20), minor_breaks = waiver())

add.tick.marks(lsigma.vs.lhzeta)

