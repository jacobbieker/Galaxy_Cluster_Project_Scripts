#########################################################
#
#         Install the needed dependancies
#
#########################################################
if(require("XLConnect") && require("ggplot2") && require(gtable) && require("scatterplot3d") && require("scales")){
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
fundamental_plane_headon <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "red", size=5) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "red", size=2) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "blue", size=5) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "blue", size=2) +
  geom_point(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor)), color = "yellow", size=4) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = LREJB_KPC_DEV, y = (1.3*LSIGMA_COR)-(0.82*LIEJB_DEV)), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor)), color = "black", size=4, shape=21) +
  geom_smooth(data = coma.data, aes(x = lreJB_kpc_DEV, y = (1.3*lsigma_cor)-(0.82*lIeJB_cor)), method = "lm", se = FALSE) +
  xlab('logre [kpc]') +
  ylab('1.3log(σ) - 0.82log<I>') + 
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver())

add.tick.marks(fundamental_plane_headon)

# Face On Graph
fundamental_plane_faceon <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "red", size=5) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "red", size=2) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "blue", size=5) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "blue", size=2) +
  geom_point(data = field.sample.one.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.one.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=2, shape=21) +
  geom_point(data = field.sample.two.HIRDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=5, shape=21) +
  geom_point(data = field.sample.two.LORDSHFT.data, aes(x = ((2.22*LREJB_KPC_DEV)-(0.82*LIEJB_DEV) + (1.3*LSIGMA_COR))/2.70, y = ((1.3*LIEJB_DEV)+(0.82*LSIGMA_COR))/1.54), color = "black", size=2, shape=21) +
  geom_point(data = coma.data, aes(x = ((2.22*lreJB_kpc_DEV)-(0.82*lIeJB_cor)+(1.3*lsigma_cor))/2.70, y = ((1.3*lIeJB_cor)+(0.82*lsigma_cor))/1.54), color = "yellow", size=4) +
  geom_point(data = coma.data, aes(x = ((2.22*lreJB_kpc_DEV)-(0.82*lIeJB_cor)+(1.3*lsigma_cor))/2.70, y = ((1.3*lIeJB_cor)+(0.82*lsigma_cor))/1.54), color = "black", size=4, shape=21) +
  geom_smooth(data = coma.data, aes(x = ((2.22*lreJB_kpc_DEV)-(0.82*lIeJB_cor)+(1.3*lsigma_cor))/2.70, y = ((1.3*lIeJB_cor)+(0.82*lsigma_cor))/1.54, method = "lm", se = FALSE)) +
  xlab('(2.22logre - 0.82log<I>e + 1.3log(σ))/2.70') +
  ylab('(1.3log<I>e + 0.82log(σ))/1.54') +
  # Change the tick marks
  scale_x_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver()) +
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver())

add.tick.marks(fundamental_plane_faceon)

##################
# Velocity Dispersion vs log M/L
##################

lsigma.vs.logml <- ggplot() + theme_bw() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black", size = 1),
    panel.grid = element_blank()
  ) +
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
  scale_y_continuous(breaks = pretty_breaks(n=10), minor_breaks = waiver())

add.tick.marks(lsigma.vs.logml)
