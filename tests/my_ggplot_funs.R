# No 1:1 abline, no reg line --------------------------------------------------
my_ggplot_ms <- function(data, xvar, yvar, group, xaxis.lab, yaxis.lab, xaxis.lim, 
                         yaxis.lim, xaxis.break, yaxis.break,
                         shape.col, legend.pos, shape.pch){
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) + #  expand=c(0,0)
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + #  expand=c(0,0)
    geom_point(aes_string(shape = group, col = group), cex=1.3) + # ), cex = 0.6
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 8) + # base_family
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6; legend.text=element_text(size=8)
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col)
  
  return(plot)
}



# With 1:1 line -----------------------------------------------------------

my_ggplot_1to1_ms <- function(data, xvar, yvar, xaxis.lab, yaxis.lab, xaxis.lim, 
                              yaxis.lim, xaxis.break, yaxis.break, legend.pos = "none"){

  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + 
    geom_abline(slope=1, intercept=0, size=0.1) + #  ADD 1:1 line
    geom_point(cex=1.3) + # ), cex = 0.6
    theme_bw(base_size = 8) +
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=6), legend.title=element_text(size=7), # size = 6; legend.text=element_text(size=8)
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos)  # guides(colour = guide_legend(override.aes = list(size=1.5))) +

  return(plot)
}

# With 1:1 line, by Species -----------------------------------------------------------

my_ggplot_1to1_group_ms <- function(data, xvar, yvar, group, xaxis.lab, yaxis.lab, xaxis.lim, 
                              yaxis.lim, xaxis.break, yaxis.break, legend.pos = "none", shape.pch, shape.col){
  
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) + 
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + 
    geom_abline(slope=1, intercept=0, size=0.1) + #  ADD 1:1 line
    geom_point(aes_string(shape = group, col = group), cex=1.3) + # ), cex = 0.6
    theme_bw(base_size = 8) +
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=6), legend.title=element_text(size=7), # size = 6; legend.text=element_text(size=8)
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col)  
  
  return(plot)
}




# Used by myplot_simVSsat_forvar_ms()  ----------------------------------------------------------------
fmt_dcimals <- function(decimals=2){                                                                      # 
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}

my_ggplot_ms_colgroup <- function(data, xvar, yvar, group, ngroup, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                                  yaxis.lim, xaxis.break, yaxis.break, text.x, text.y,
                                  shape.col, legend.pos, shape.pch, col.group, col.label = "", legend.just = c(1,1), legend.box = "vertical", 
                                  shape.guide = TRUE, col.guide = TRUE){
  r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break, labels = fmt_dcimals(2)) + #  expand=c(0,0)
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + #  expand=c(0,0)
    # geom_smooth(method='lm', se=FALSE, col="grey50", size=0.5, linetype=2) +
    geom_point(aes_string(shape = group, col = col.group), cex=1.3) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 8, base_family = "Helvetica") + # base_family
    theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos, legend.justification=legend.just, legend.box=legend.box) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch, guide=shape.guide) + 
    scale_colour_manual(name="", values=shape.col, labels=col.label, guide=col.guide)
  
  return(plot)
}

# Plot LAIt vs sim. and sat. BRF ------------------------------------------
# Depends on my_ggplot_ms_colgroup()


# Debug (to delete)
# paras.res = temp
# forest.var = "LAItrue" 
# shape.col = c("dark orange", "dark blue") 
# shape.pch = pts.pchs[1:4] 
# xlab = expression(bold(LAI[true]))
# 
# temp %>% select(Green:SWIR2, LAItrue) %>% map_df(max)


# (a) LAItrue, S2 ---------------------------------------------------------

myplot_simVSsat_LAIt_S2 <- function(paras.res, forest.var, shape.col, shape.pch, xlab,
                                            xaxis.lims = list(green=c(0,6.5), red=c(0,6.5), re1=c(0,6.5), re2=c(0,6.5), re3=c(0,6.5), nir=c(0,6.5), swir1=c(0,6.5), swir2=c(0,6.5)),
                                            yaxis.lims = list(green=c(0,0.1), red=c(0,0.05), re1 = c(0,0.15), re2=c(0,0.5), re3=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.3), swir2=c(0,0.2)),
                                            xaxis.breaks = list(green=seq(0,6.5,1), red=seq(0,6.5,1), re1=seq(0,6.5,1), re2=seq(0,6.5,1), re3=seq(0,6.5,1), nir=seq(0,6.5,1), swir1=seq(0,6.5,1), swir2=seq(0,6.5,1)), 
                                            yaxis.breaks = list(green=seq(0,0.1,0.02), red=seq(0,0.05,0.01), re1 = seq(0,0.15,0.03), re2=seq(0,0.5,0.1), re3=seq(0,0.6,0.1), nir=seq(0,0.6,0.10), swir1=seq(0,0.3,0.05), swir2=seq(0,0.2,0.04))
) {
  
  
  
  plots.list <- list(
    green = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Green, Green, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                            xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.75), shape.pch = shape.pch, # legend.pos = c(1,1)
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") +
      guides(col = guide_legend(label.theme = element_text(size=7, angle=0)),
             shape = FALSE), 
    
    red = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Red, Red, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                            xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.84), shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") + 
      guides(col = FALSE, # col = guide_legend(label.theme = element_text(size=7, angle=0)
             shape = guide_legend(label.theme = element_text(size=7, angle=0))),        
    
    re1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE1, RE1, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF RE1", xaxis.lim = xaxis.lims$re1, yaxis.lim = yaxis.lims$re1, 
                            xaxis.break = xaxis.breaks$re1, yaxis.break = yaxis.breaks$re1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"), 
    
    re2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE2, RE2, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF RE2", xaxis.lim = xaxis.lims$re2, yaxis.lim = yaxis.lims$re2, 
                            xaxis.break = xaxis.breaks$re2, yaxis.break = yaxis.breaks$re2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"),
    
    re3 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE3, RE3, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF RE3", xaxis.lim = xaxis.lims$re3, yaxis.lim = yaxis.lims$re3, 
                            xaxis.break = xaxis.breaks$re3, yaxis.break = yaxis.breaks$re3,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"),
    
    nir = paras.res %>%  dplyr::select(one_of(forest.var), Sat.NIR, NIRnarrow, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                            xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR1, SWIR1, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                            xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR2, SWIR2, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                            xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch)
  )
  
  return(plots.list)
  
}


# (ax) LAItrue, S2, by site type ---------------------------------------------------------

myplot_simVSsat_LAIt_S2_byFertile <- function(paras.res, forest.var, shape.col, shape.pch, xlab,
                                    xaxis.lims = list(green=c(0,6.5), red=c(0,6.5), re1=c(0,6.5), re2=c(0,6.5), re3=c(0,6.5), nir=c(0,6.5), swir1=c(0,6.5), swir2=c(0,6.5)),
                                    yaxis.lims = list(green=c(0,0.1), red=c(0,0.05), re1 = c(0,0.15), re2=c(0,0.5), re3=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.3), swir2=c(0,0.2)),
                                    xaxis.breaks = list(green=seq(0,6.5,1), red=seq(0,6.5,1), re1=seq(0,6.5,1), re2=seq(0,6.5,1), re3=seq(0,6.5,1), nir=seq(0,6.5,1), swir1=seq(0,6.5,1), swir2=seq(0,6.5,1)), 
                                    yaxis.breaks = list(green=seq(0,0.1,0.02), red=seq(0,0.05,0.01), re1 = seq(0,0.15,0.03), re2=seq(0,0.5,0.1), re3=seq(0,0.6,0.1), nir=seq(0,0.6,0.10), swir1=seq(0,0.3,0.05), swir2=seq(0,0.2,0.04))
) {
  
  
  
  plots.list <- list(
    green = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Green, Green, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                            xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.75), shape.pch = shape.pch, # legend.pos = c(1,1)
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") +
      guides(col = guide_legend(label.theme = element_text(size=7, angle=0)),
             shape = FALSE), 
    
    red = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Red, Red, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                            xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.84), shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") + 
      guides(col = FALSE, # col = guide_legend(label.theme = element_text(size=7, angle=0)
             shape = guide_legend(label.theme = element_text(size=7, angle=0))),        
    
    re1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE1, RE1, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF RE1", xaxis.lim = xaxis.lims$re1, yaxis.lim = yaxis.lims$re1, 
                            xaxis.break = xaxis.breaks$re1, yaxis.break = yaxis.breaks$re1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"), 
    
    re2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE2, RE2, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF RE2", xaxis.lim = xaxis.lims$re2, yaxis.lim = yaxis.lims$re2, 
                            xaxis.break = xaxis.breaks$re2, yaxis.break = yaxis.breaks$re2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"),
    
    re3 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE3, RE3, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF RE3", xaxis.lim = xaxis.lims$re3, yaxis.lim = yaxis.lims$re3, 
                            xaxis.break = xaxis.breaks$re3, yaxis.break = yaxis.breaks$re3,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"),
    
    nir = paras.res %>%  dplyr::select(one_of(forest.var), Sat.NIR, NIRnarrow, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                            xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR1, SWIR1, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                            xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR2, SWIR2, Site_type_lab) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Site_type_lab")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Site_type_lab", col.group = "variable", ngroup = 3, pts = 2, 
                            xlab, "BRF SWIR2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                            xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch)
  )
  
  return(plots.list)
  
}

# (b) LAItrue, L8 -------------------------------------------------------------

myplot_simVSsat_LAIt_L8 <- function(paras.res, forest.var, shape.col, shape.pch, xlab,
                                    xaxis.lims = list(green=c(0,6.5), red=c(0,6.5), nir=c(0,6.5), swir1=c(0,6.5), swir2=c(0,6.5)),
                                    yaxis.lims = list(green=c(0,0.1), red=c(0,0.05), nir=c(0,0.6), swir1=c(0,0.3), swir2=c(0,0.2)),
                                    xaxis.breaks = list(green=seq(0,6.5,1), red=seq(0,6.5,1), nir=seq(0,6.5,1), swir1=seq(0,6.5,1), swir2=seq(0,6.5,1)), 
                                    yaxis.breaks = list(green=seq(0,0.1,0.02), red=seq(0,0.05,0.01), nir=seq(0,0.6,0.10), swir1=seq(0,0.3,0.05), swir2=seq(0,0.2,0.04))
) {
  
  
  
  plots.list <- list(
    green = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Green, Green, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                            xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.75), shape.pch = shape.pch, # legend.pos = c(1,1)
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") +
      guides(col = guide_legend(label.theme = element_text(size=7, angle=0)),
             shape = FALSE), 
    
    red = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Red, Red, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                            xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.84), shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") + 
      guides(col = FALSE, # col = guide_legend(label.theme = element_text(size=7, angle=0)
             shape = guide_legend(label.theme = element_text(size=7, angle=0))),  
    
    
    nir = paras.res %>%  dplyr::select(one_of(forest.var), Sat.NIR, NIR, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                            xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR1, SWIR1, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                            xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR2, SWIR2, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                            xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch)
  )
  
  return(plots.list)
  
}

# (c) LAItrue, S2, Nilson ---------------------------------------------------------

myplot_simVSsat_LAIt_S2_Nilson <- function(paras.res, forest.var, shape.col, shape.pch, xlab,
                                    xaxis.lims = list(green=c(0,8), red=c(0,8), re1=c(0,8), re2=c(0,8), re3=c(0,8), nir=c(0,8), swir1=c(0,8), swir2=c(0,8)),
                                    yaxis.lims = list(green=c(0,0.1), red=c(0,0.05), re1 = c(0,0.15), re2=c(0,0.5), re3=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.3), swir2=c(0,0.2)),
                                    xaxis.breaks = list(green=seq(0,8,1), red=seq(0,8,1), re1=seq(0,8,1), re2=seq(0,8,1), re3=seq(0,8,1), nir=seq(0,8,1), swir1=seq(0,8,1), swir2=seq(0,8,1)), 
                                    yaxis.breaks = list(green=seq(0,0.1,0.02), red=seq(0,0.05,0.01), re1 = seq(0,0.15,0.03), re2=seq(0,0.5,0.1), re3=seq(0,0.6,0.1), nir=seq(0,0.6,0.10), swir1=seq(0,0.3,0.05), swir2=seq(0,0.2,0.04))
) {
  
  
  
  plots.list <- list(
    green = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Green, Green, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                            xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.75), shape.pch = shape.pch, # legend.pos = c(1,1)
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") +
      guides(col = guide_legend(label.theme = element_text(size=7, angle=0)),
             shape = FALSE), 
    
    red = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Red, Red, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                            xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.84), shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") + 
      guides(col = FALSE, # col = guide_legend(label.theme = element_text(size=7, angle=0)
             shape = guide_legend(label.theme = element_text(size=7, angle=0))),        
    
    re1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE1, RE1, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF RE1", xaxis.lim = xaxis.lims$re1, yaxis.lim = yaxis.lims$re1, 
                            xaxis.break = xaxis.breaks$re1, yaxis.break = yaxis.breaks$re1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"), 
    
    re2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE2, RE2, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF RE2", xaxis.lim = xaxis.lims$re2, yaxis.lim = yaxis.lims$re2, 
                            xaxis.break = xaxis.breaks$re2, yaxis.break = yaxis.breaks$re2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"),
    
    re3 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE3, RE3, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF RE3", xaxis.lim = xaxis.lims$re3, yaxis.lim = yaxis.lims$re3, 
                            xaxis.break = xaxis.breaks$re3, yaxis.break = yaxis.breaks$re3,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical"),
    
    nir = paras.res %>%  dplyr::select(one_of(forest.var), Sat.NIR, NIRnarrow, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                            xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR1, SWIR1, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                            xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR2, SWIR2, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                            xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch)
  )
  
  return(plots.list)
  
}

# (d) LAItrue, L8, Nilson -------------------------------------------------------------

myplot_simVSsat_LAIt_L8_Nilson <- function(paras.res, forest.var, shape.col, shape.pch, xlab,
                                    xaxis.lims = list(green=c(0,8), red=c(0,8), nir=c(0,8), swir1=c(0,8), swir2=c(0,8)),
                                    yaxis.lims = list(green=c(0,0.1), red=c(0,0.05), nir=c(0,0.6), swir1=c(0,0.3), swir2=c(0,0.2)),
                                    xaxis.breaks = list(green=seq(0,8,1), red=seq(0,8,1), nir=seq(0,8,1), swir1=seq(0,8,1), swir2=seq(0,8,1)), 
                                    yaxis.breaks = list(green=seq(0,0.1,0.02), red=seq(0,0.05,0.01), nir=seq(0,0.6,0.10), swir1=seq(0,0.3,0.05), swir2=seq(0,0.2,0.04))
) {
  
  
  
  plots.list <- list(
    green = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Green, Green, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                            xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.75), shape.pch = shape.pch, # legend.pos = c(1,1)
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") +
      guides(col = guide_legend(label.theme = element_text(size=7, angle=0)),
             shape = FALSE), 
    
    red = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Red, Red, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                            xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                            shape.col = shape.col, legend.pos = c(1,1), legend.just = c(0.98,0.84), shape.pch = shape.pch, # theme(legend.justification=c(1,0), legend.position=c(1,0))
                            col.label = c("Observed", "Modelled"), legend.box = "vertical") + 
      guides(col = FALSE, # col = guide_legend(label.theme = element_text(size=7, angle=0)
             shape = guide_legend(label.theme = element_text(size=7, angle=0))),  
    
    
    nir = paras.res %>%  dplyr::select(one_of(forest.var), Sat.NIR, NIR, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                            xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR1, SWIR1, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                            xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch),
    
    swir2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR2, SWIR2, Dominant_species) %>% collect %>% 
      melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
      my_ggplot_ms_colgroup(., xvar = forest.var, yvar = "value", group = "Dominant_species", col.group = "variable", ngroup = 4, pts = 2, 
                            xlab, "BRF SWIR2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2, 
                            xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
                            shape.col = shape.col, legend.pos = "none", shape.pch = shape.pch)
  )
  
  return(plots.list)
  
}




# With error bar ----------------------------------------------------------

my_ggplot_errbar_ms <- function(data, xvar, yvar, shp.group, col.group, pts, xaxis.lab, yaxis.lab, xaxis.lim, 
                                yaxis.lim, xaxis.break, yaxis.break,
                                shape.col, shape.pch, dataerr, xerr, yerr, yminerr, ymaxerr,
                                legend.pos = "none", legend.just = c(1,1), legend.box = "vertical"){
  # r <- as.numeric(round(cor(data[,xvar], data[,yvar]), 3))
  plot <- ggplot(data, aes_string(x = xvar , y = yvar, color = col.group)) + 
    scale_y_continuous(name=yaxis.lab, limits=yaxis.lim, breaks=yaxis.break) +       # Expand
    scale_x_continuous(name=xaxis.lab, limits=xaxis.lim, breaks=xaxis.break) + 
    geom_errorbar(data = dataerr, aes_string(x = xerr, y = yerr, ymin = yminerr, ymax = ymaxerr),         # Add error bar
                  width = 0.5, col = shape.col[2], size = 0.3, alpha = 0.7) +                   # alpha to "lighten" the colour?                         
    
    geom_point(aes_string(shape = shp.group, col = col.group), cex=pts) + # ), cex = 0.6
    # geom_text(x = text.x, y = text.y, label = paste("r = ",r,sep="")) + # ), cex = 2
    # geom_text(x = 98, y = 0.1, label='(a)', cex=3) +
    # geom_text(aes(label=FID0, pos=3), size=3, hjust=-0.5, vjust=0, col="gray") +
    theme_bw(base_size = 8, base_family = "Helvetica") +
    theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), panel.background = element_blank(), 
          axis.text=element_text(size=8), axis.title.x=element_text(size=10,face="bold"), # size = 7; size = 8
          axis.title.y=element_text(size=10,face="bold"), # size = 8
          aspect.ratio=1, legend.text=element_text(size=8), legend.title=element_text(size=7), # size = 6
          legend.background=element_rect(fill="transparent"),
          legend.position=legend.pos, legend.justification=legend.just, legend.box=legend.box) + # guides(colour = guide_legend(override.aes = list(size=1.5))) +
    scale_shape_manual(name="", values=shape.pch) + 
    scale_colour_manual(name="", values=shape.col, labels = c("Observed", "Modelled"))
  
  return(plot)
}





# With error bar --------------------------------------------------------------------

# # Debug (to delete)
# paras.res = temp 
# forest.var = "LAItrue" 
# shape.col = c("dark orange", "dark blue")
# shape.pch = c(0,1,7,2) 
# xlab = expression(bold(LAI[true]))
# pts.cex = 1.5
# # temp %>% select(Green:SWIR2, LAItrue) %>% map_df(max)


myplot_simVSsat_LAIt_S2_Nilson_errbar <- 
  function(paras.res, forest.var, 
           xaxis.lims = list(green=c(0,8), red=c(0,8), re1=c(0,8), re2=c(0,8), re3=c(0,8), nir=c(0,8), swir1=c(0,8), swir2=c(0,8)),
           yaxis.lims = list(green=c(0,0.1), red=c(0,0.05), re1 = c(0,0.15), re2=c(0,0.5), re3=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.3), swir2=c(0,0.2)),
           xaxis.breaks = list(green=seq(0,8,1), red=seq(0,8,1), re1=seq(0,8,1), re2=seq(0,8,1), re3=seq(0,8,1), nir=seq(0,8,1), swir1=seq(0,8,1), swir2=seq(0,8,1)), 
           yaxis.breaks = list(green=seq(0,0.1,0.02), red=seq(0,0.05,0.01), re1 = seq(0,0.15,0.03), re2=seq(0,0.5,0.1), re3=seq(0,0.6,0.1), nir=seq(0,0.6,0.10), swir1=seq(0,0.3,0.05), swir2=seq(0,0.2,0.04)),
           xlab, shape.pch, shape.col, pts.cex) {
    
    
    plots.list <- list(
      green = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Green, Green_mean, Dominant_species) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF green", xaxis.lim = xaxis.lims$green, yaxis.lim = yaxis.lims$green, 
                            xaxis.break = xaxis.breaks$green, yaxis.break = yaxis.breaks$green,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = c(1,1), legend.just = c(0.98,0.75),   # legend.pos = c(0.9,0.7)
                            dataerr = paras.res, xerr = forest.var, yerr = "Green_mean", yminerr = "Green_min", ymaxerr = "Green_max") +
        guides(col = guide_legend(label.theme = element_text(size=7, angle=0)), shape = FALSE),
      
      red = paras.res %>%  dplyr::select(one_of(forest.var), Sat.Red, Red_mean, Dominant_species) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF red", xaxis.lim = xaxis.lims$red, yaxis.lim = yaxis.lims$red, 
                            xaxis.break = xaxis.breaks$red, yaxis.break = yaxis.breaks$red,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = c(1,1), legend.just = c(0.98,0.84),
                            dataerr = paras.res, xerr = forest.var, yerr = "Red_mean", yminerr = "Red_min", ymaxerr = "Red_max") +
      guides(col = FALSE, shape = guide_legend(label.theme = element_text(size=7, angle=0))),
      
      re1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE1, RE1_mean, Dominant_species) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF RE1", xaxis.lim = xaxis.lims$re1, yaxis.lim = yaxis.lims$re1, 
                            xaxis.break = xaxis.breaks$re1, yaxis.break = yaxis.breaks$re1,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                            dataerr = paras.res, xerr = forest.var, yerr = "RE1_mean", yminerr = "RE1_min", ymaxerr = "RE1_max"),
      
      re2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE2, RE2_mean, Dominant_species) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF RE2", xaxis.lim = xaxis.lims$re2, yaxis.lim = yaxis.lims$re2, 
                            xaxis.break = xaxis.breaks$re2, yaxis.break = yaxis.breaks$re2,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                            dataerr = paras.res, xerr = forest.var, yerr = "RE2_mean", yminerr = "RE2_min", ymaxerr = "RE2_max"),
      
      re3 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.RE3, RE3_mean, Dominant_species) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF RE3", xaxis.lim = xaxis.lims$re3, yaxis.lim = yaxis.lims$re3, 
                            xaxis.break = xaxis.breaks$re3, yaxis.break = yaxis.breaks$re3,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                            dataerr = paras.res, xerr = forest.var, yerr = "RE3_mean", yminerr = "RE3_min", ymaxerr = "RE3_max"),
      
      nir = paras.res %>%  dplyr::select(one_of(forest.var), Sat.NIR, NIRnarrow_mean, Dominant_species) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF NIR", xaxis.lim = xaxis.lims$nir, yaxis.lim = yaxis.lims$nir, 
                            xaxis.break = xaxis.breaks$nir, yaxis.break = yaxis.breaks$nir,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                            dataerr = paras.res, xerr = forest.var, yerr = "NIRnarrow_mean", yminerr = "NIRnarrow_min", ymaxerr = "NIRnarrow_max"),
      
      swir1 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR1, SWIR1_mean, Dominant_species) %>% collect %>% 
        melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%  
        my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex, 
                            xlab, "BRF SWIR1", xaxis.lim = xaxis.lims$swir1, yaxis.lim = yaxis.lims$swir1, 
                            xaxis.break = xaxis.breaks$swir1, yaxis.break = yaxis.breaks$swir1,
                            shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
                            dataerr = paras.res, xerr = forest.var, yerr = "SWIR1_mean", yminerr = "SWIR1_min", ymaxerr = "SWIR1_max") # ,
      
      # swir2 = paras.res %>%  dplyr::select(one_of(forest.var), Sat.SWIR2, SWIR2_mean, Dominant_species) %>% collect %>%
      #   melt(., id.vars = c(forest.var, "Dominant_species")) %>% collect %>%
      #   my_ggplot_errbar_ms(., xvar = forest.var, yvar = "value", shp.group = "Dominant_species", col.group = "variable", pts = pts.cex,
      #                       xlab, "BRF SWIR2", xaxis.lim = xaxis.lims$swir2, yaxis.lim = yaxis.lims$swir2,
      #                       xaxis.break = xaxis.breaks$swir2, yaxis.break = yaxis.breaks$swir2,
      #                       shape.col = shape.col, shape.pch = shape.pch, legend.pos = "none",
      #                       dataerr = paras.res, xerr = forest.var, yerr = "SWIR2_mean", yminerr = "SWIR2_min", ymaxerr = "SWIR2_max")
    )
    
  }
