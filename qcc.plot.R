#' @title plot.qcc
#' @author Scrucca, L. (qcc package)
#' @author Hopper, T. J. (ggplot/grid modification to plot.qcc)
#' @description Implementation of plot.qcc using ggplot2 and grid
#' @details 
#' @import grid
#' @import ggplot2
#' @import gtable
#' @param x A qcc object
#' @param add.stats A boolean flag controlling whether summary statistics are printed on the graph.
#' @param chart.all All boolean flag controlling whether all (old and new) statistics are plotted, or only one or the other
#' @param label.limits A character vector with three elements containing the labels for the lower control limit line, the upper control limit line and the center line.
#' @param title A character string containing the desired plot title. If not supplied, a default will be created.
#' @param xlab A character string containing the desired plot x-axis label. If not supplied, a default will be created.
#' @param ylab A character string containing the desired plot y-axis label. If not supplied, a default will be created.
#' @param ylim A two-element numeric vector containing desired limits for the y axis. If not supplied, a default will be created.
#' @param axes.las An integer indicating the desired orientation of axis labels. See ?par for details. Defaults to 0.
#' @param digits An integer indicating the number of digits to print. See ?getOption for details. Defaults to getOption("digits")
#' @param restore.par A boolean indicating whether or not graphic parameters should be restored. Defaults to TRUE.
#' @param font.size The desired font size in points (pts). Defaults to 12 pts.

library(ggplot2)  # Used for plotting
library(grid)     # Used to create plot title and statistics regions
library(gtable)   # Used to align annotations outside the plot region

gg.plotqcc <- function(x, add.stats = TRUE, chart.all = TRUE, 
                       label.limits = c("LCL", "UCL", "CL"),
                       title = NULL, xlab = NULL, ylab = NULL, ylim = NULL, axes.las = 0,
                       digits =  getOption("digits"),
                       restore.par = TRUE, font.size = 12, plot.new = TRUE, ...) 
{
  object <- x  # Argh.  Really want to use 'object' anyway
  if ((missing(object)) | (!inherits(object, "qcc")))
    stop("an object of class `qcc' is required")
  
  # collect info from object
  type <- object$type
  std.dev <- object$std.dev
  data.name <- object$data.name
  center <- object$center
  stats <- object$statistics
  limits <- object$limits 
  lcl <- limits[,1]
  ucl <- limits[,2]
  newstats <- object$newstats
  newdata.name <- object$newdata.name
  violations <- object$violations
  #' Set up labels
  #' Set up observation indices
  if(chart.all) { 
    v.statistics <- c(stats, newstats)
    v.indices <- 1:length(v.statistics) 
  }  else { 
    if(is.null(newstats)) { 
      v.statistics <- stats
      v.indices <- 1:length(v.statistics) 
    } else { 
      v.statistics <- newstats 
      v.indices <- seq(length(stats)+1, length(stats)+length(newstats)) 
    }
  }
  if(is.null(ylim)) ylim <- range(v.statistics, limits, center)
  if(is.null(ylab)) ylab <- c("Group summary statistics")
  if(is.null(xlab)) xlab <- c("Group")
  #' create a data frame for use by ggplot
  qc.data <- data.frame(df.indices <- v.indices, df.statistics <- as.vector(v.statistics)) 
  
  #' Set x-axis limit explicitly so we can control the appearance
  #' and re-use for other ggplot objects in a grid arrangement.
  xlim <- range(v.indices)
  
  if (is.null(title)) {          # Need to create a plot title
    if (is.null(newstats))  {    # Just for the qcc data used to calculate limits
      main.title <- paste(type, "Chart\nfor", data.name)
    } else {                     # Also have new data (not used for limits calcs)
      if (chart.all){            # Plotting both old and new data
        main.title <- paste(type, "Chart\nfor", data.name, 
                            "and", newdata.name)
      } else {                   # Plotting only the new data
        main.title <- paste(type, "Chart\nfor", newdata.name) 
      }
    }
  } else {main.title <- paste(title)}  # Plot title given by the user
  
  #' Determine significant figures
  #' If \code{digits} is provided (i.e., \code{digits != getOption("digits")}), then use that, 
  #' otherwise estimate the correct number of significant measurement digits. 
  #' Assume that if the numbers in $statistics include a decimal, then the largest number
  #' of significant figures is the actual significant figures and everything else has been
  #' truncated. For integer $statistics, assume the smallest number of digits is the 
  #' significant figures.
  #' TODO: Fix problem with too many digits in fake data.
  if(digits == getOption("digits")) {
    #print("digits equals getOption")
    has.dec <- FALSE
    sig.dig <- rep(0, length(v.statistics))
    if(any(v.statistics %% 1 > 0)) {
      has.dec <- TRUE
    }
    for(i in 1:length(v.statistics)) {
      sig.dig[i] <- length(gregexpr("[[:digit:]]", as.character(v.statistics[i]))[[1]])
    }
    if(has.dec) {
      sig.figs <- max(sig.dig) # assume numbers with decimals imply significant figures
    } else {
      sig.figs <- min(sig.dig) # assume the smallest number implies the significant figures
    }
  } else {
    #print("digits doesn't equal getOption")
    sig.figs <- digits
  }
  
  #' If the default limit labels are used, create new labels using the last values
  #' for center, limits[1] and limits[2].
  if(label.limits[1] == "LCL" && label.limits[2] == "UCL" && label.limits[3] == "CL") {
    label.limits[3] <- as.character(signif(center[length(center)], digits = sig.figs))
    label.limits[1] <- as.character(signif(lcl[length(lcl)], digits = sig.figs))
    label.limits[2] <- signif(ucl[length(ucl)], digits = sig.figs)
  }
  
  # plot Shewhart chart
  
  #' Set up the plot
  #' Expand the x axis manually so we can control the position
  #' of CL, UCL and LCL labels outside the plot
  #' Set the plot margins to allow space for the limit labels
  #' TODO: adjust axis and tick labels based on axes.las
  # las
  #   0: parallel to axis
  #   1: horizontal
  #   2: perpendicular to axis
  #   3: vertical
  # angle = [0, 360]
  #  angle=if(las=0) 90 else ...
  #  angle=if(las=0) 0 else ...
  # bp + theme(axis.title.x = element_text(face="bold", colour="#990000", size=20),
  #            axis.text.x  = element_text(angle=90, vjust=0.5, size=16))
  if (axes.las != 0) {
    #' Adjust axis title orientation based on "las" value.
    #' TODO: Need to calculate appropriate angle.
  }
  qc.gplot <- ggplot(data = qc.data, environment = environment(), 
                    aes_string(x = df.indices, y = df.statistics)) +
    theme(
      text = element_text(size = font.size), 
      plot.margin = unit(c(1,1,1,1), "mm")) +
    scale_x_continuous(expand = c(0, 0.5), limits = xlim)
  #' Plot dots and connecting lines for the statistic variable
  qc.gplot <- qc.gplot + 
    geom_line(x = df.indices, y = df.statistics) +
    geom_point(x = df.indices, y = df.statistics, shape = 20) 
  qc.gplot <- qc.gplot + ylim(ylim)
  
  #' Add graph labels
  #' The plotting of the graph title may need to change when using grid
  #' main.title will be in a separate viewport
  qc.gplot <- qc.gplot + labs(x = xlab, y = ylab)
  
  #' Add center line
  if(length(center) == 1) {
    #' If there are not steps, just plot a horizontal line for the
    #' individuals average.
    qc.gplot <- qc.gplot + geom_hline(yintercept = center)
    #else lines(indices, c(center, center[length(center)]), type="s")
  } else {
    #' otherwise, we need to plot a stepped center line
    qc.gplot <- qc.gplot + 
      geom_step(aes(x = df.indices, y = c(center, center[length(center)])), direction="hv", )
  }
  
  #' Add control limit lines
  if(length(lcl) == 1) {
    #' Likewise for the UCL and LCL lines
    qc.gplot <- qc.gplot + geom_hline(yintercept = lcl, linetype = 2)
    qc.gplot <- qc.gplot + geom_hline(yintercept = ucl, linetype = 2)
  } else {
    qc.gplot <- qc.gplot + geom_step(aes_string(x = df.indices, y = lcl[df.indices]), 
                                     direction = "hv", linetype = 2)
    qc.gplot <- qc.gplot + geom_step(aes_string(x = df.indices, y = ucl[df.indices]), 
                                     direction = "hv", linetype = 2)
  }
  
  #' Violating runs
  #' Identify violating runs.
  if(is.null(qcc.options("violating.runs")))
    stop(".qcc.options$violating.runs undefined. See help(qcc.options).")
  index.v <- rep(NA, length(violations$violating.runs))
  if(length(violations$violating.runs > 0)) { 
    index.v <- violations$violating.runs
    if(!chart.all & !is.null(newstats)) { 
      index.v <- index.v - length(stats) 
      index.v <- index.v[index.v>0] 
    }
    
    v.data <- data.frame(v.index = qc.data$df.indices[index.v], v.statistics = qc.data$df.statistics[index.v])
    #' Replot points in violating runs in the adjusted color.
    #' TODO: define data frame with violatinns$violating.runs and corresponding statistics values.
    qc.gplot <- qc.gplot + 
      geom_point(data = v.data, 
                 aes_string(x = v.data$v.index, y = v.data$v.statistics), 
                 colour = qcc.options("violating.runs")$col, 
                 shape = qcc.options("violating.runs")$pch)
  }
  
  #' Points beyond limits
  #' Identify points beyond limits
  index.b <- rep(NA, length(violations$beyond.limits))
  if(is.null(qcc.options("beyond.limits")))
    stop(".qcc.options$beyond.limits undefined. See help(qcc.options).")
  if(length(violations$beyond.limits > 0)) { 
    index.b <- violations$beyond.limits
    if(!chart.all & !is.null(newstats)) { 
      index.b <- index.b - length(stats) 
      index.b <- index.b[index.b>0] 
    }
    #' Replot points that are beyond limits.
    qc.gplot <- qc.gplot + geom_point(aes_string(x = df.indices[index.b], y = df.statistics[index.b]), 
                                      colour = qcc.options("beyond.limits")$col, 
                                      shape = qcc.options("beyond.limits")$pch)
  }
  
  #' New Statistics
  #' Plot and label a vertical break line to mark points used for limits calculation from
  #' added points.
  if(chart.all & (!is.null(newstats))) { 
    len.obj.stats <- length(object$statistics)
    len.new.stats <- length(v.statistics) - len.obj.stats
    qc.gplot <- qc.gplot + geom_vline(xintercept = len.obj.stats + 0.5, linetype = "dotted")
    
  }
  
  #' Generation of QC plot is complete.
  #' Prepare objects to add annotations outside of plot 
  qc.gt <- ggplot_gtable(ggplot_build(qc.gplot))
  qc.index <- subset(qc.gt$layout, name == "panel")
  
  
  #' Add labels "LCL," "UCL," "CL" to control limits and center line.
  qc.df.limitslab <- data.frame(index = 0, y = qc.data$df.statistics[length(qc.data$df.statistics)])
  qc.p3 <- ggplot(qc.data, aes_string(x = df.indices, y = df.statistics), environment = environment()) +
    geom_blank() +
    theme_minimal() +
    theme(line = element_blank(),
          text = element_blank(),
          panel.background = element_rect(colour = NA)) +
    guides(colour = "none") +
    scale_x_continuous(expand = c(0, 0)) +
    ylim(ylim)
  
  qc.p3 <- qc.p3 +
    geom_text(data = qc.df.limitslab,
              aes( x = 0, limits[1]),
              label = label.limits[1], 
              hjust = 0)
  qc.p3 <- qc.p3 +
    geom_text(data = qc.df.limitslab,
              aes( x = 0, y = limits[2]),
              label = label.limits[2], 
              hjust = 0)
  qc.p3 <- qc.p3 +
    geom_text(data = qc.df.limitslab,
              aes( x = 0, y = center),
              label = label.limits[3], 
              hjust = 0)
  
  qc.g3 <- gtable_filter(ggplotGrob(qc.p3), "panel")
  
  #' Add gtable grobs to draw annotation
  #' One to the right for the UCL, LCL and center line labels
  qc.gt <- gtable_add_cols(x=qc.gt, 
                           widths=unit(x=1, units="strwidth", 
                                       data=paste(max(nchar(label.limits)), "M", sep="")), 
                           pos=-1)
#   print(max(nchar(label.limits)))
#   print(unit(x = 1, units = "strwidth", data = paste(max(nchar(label.limits)), "M", sep="")))
#   print(convertUnit(unit(x = 1, units = "strwidth", data = paste(max(nchar(label.limits)), "M", sep="")), "npc"))
  qc.gt <- gtable_add_grob(qc.gt, qc.g3, 
                           t = qc.index$t, 
                           l = ncol(qc.gt), 
                           b = qc.index$b, 
                           r = ncol(qc.gt))
  
  
  #mtext(label.limits, side = 4, at = c(rev(lcl)[1], rev(ucl)[1]), 
  #      las = 1, line = 0.1, col = gray(0.3))
  #qc.gplot <- qc.gplot + annotate()
  #mtext("CL", side = 4, at = rev(center)[1], 
  #      las = 1, line = 0.1, col = gray(0.3))
  #' annotate() plots only inside the plot region; 
  #' need another solution for text outside the plot region
  #' custom_annotation() seems to be right, or
  #' arrangeGrob + textGrob, but Grob positioning may be difficult
  #' For other solutions, try: 
  #' http://stackoverflow.com/questions/10525957/how-to-draw-lines-outside-of-plot-area-in-ggplot2
  #' http://stackoverflow.com/questions/12409960/ggplot2-annotate-outside-of-plot
  #' https://groups.google.com/forum/#!topic/ggplot2/54q_qdTE1L0
  #' http://stackoverflow.com/questions/10197738/add-a-footnote-citation-outside-of-plot-area-in-r
  #' control font size in textGrob with gp = gpar(fontsize = XX)
  
  
  if(chart.all & (!is.null(newstats))) { 
    qc.df.nslabel <- data.frame(index = v.indices[length(v.indices)], y = 0)
    qc.p2.label2 <- paste("New data in", object$newdata.name)
    qc.p2.label2.x <- len.obj.stats + len.new.stats/2
    qc.p2 <- ggplot(qc.data, aes(x = df.indices, y = df.statistics), environment = environment()) +
      geom_blank() +
      theme_minimal() +
      theme(line = element_blank(),                   # Prevent display axis lines, etc.
            text = element_blank(),                   # Prevent display of labels, etc.
            panel.background = element_rect(colour = NA)) +
      scale_x_continuous(expand = c(0, 0.5), limits = xlim) +
      guides(colour = "none")
    
    qc.p2 <- qc.p2 +
      geom_text(data = qc.df.nslabel,
                aes(x = len.obj.stats / 2, y = 0), 
                label = paste("Calibration data in", data.name), 
                hjust = 0.5,
                vjust = 0) +
      geom_text(data = qc.df.nslabel,
                aes(x = qc.p2.label2.x, y = 0), 
                label = qc.p2.label2, 
                hjust = 0.5,
                vjust = 0)
    
    #' Get just the panel from qc.p2
    qc.g2 <- gtable_filter(ggplotGrob(qc.p2), "panel")
    #' If needed, one above for the newstats labels
    qc.gt <- gtable_add_rows(qc.gt, unit(2*font.size, "points"), pos = 0)
    qc.gt <- gtable_add_grob(x = qc.gt, grobs = qc.g2, 
                             t = 1, 
                             l = 4, 
                             b = 1, 
                             r = 4)
  }
  
  #' The user may call qcc.plot as part of their own code or function
  #' for building up a graph object. 
  if (plot.new) {
    grid.newpage()
  }
  
  qc.vp.main <- viewport(gp = gpar(fontsize = font.size))
  pushViewport(qc.vp.main)
  qc.vp.top.height = convertUnit(unit(3, "lines"), "npc")
  qc.vp.bot.height = convertUnit(unit(0, "lines"), "npc")
  
  #' Set up the top viewport, pinning it to the top of the parent viewport.
  qc.vp.top <- viewport(x = unit(0.5, "npc"), 
                        y = unit(1, "npc"), 
                        height =qc.vp.top.height, 
                        width = unit(1, "npc"), 
                        just = c("centre", "top"), 
                        name = "vptop", 
                        gp = gpar(fontsize = as.numeric(font.size)+2)) # was "lines"
  
  #' Add statistics to the plot (number of groups, limits, etc.)
  if(add.stats) { # computes the x margins of the figure region
    qc.vp.bot.height <- convertUnit(unit(6, "lines"), "npc")
    qc.vp.bot <- viewport(y = unit(0, "npc"), 
                          height =qc.vp.bot.height, 
                          just = c("centre", "bottom"), 
                          name = "vpstat", 
                          gp = gpar(fontsize = font.size))
    pushViewport(qc.vp.bot)
    #grid.rect(gp = gpar(fill = "grey50"))
    stats.x <- unit(rep(NA, 6), "npc")
    stats.y <- unit(rep(NA, 3), "lines")
    stats.x[1] <- unit(0.1, "npc")
    stats.x[2] <- unit(0.3, "npc")
    stats.x[3] <- unit(0.4, "npc")
    stats.x[4] <- unit(0.5, "npc")
    stats.x[5] <- unit(0.65, "npc")
    stats.x[6] <- unit(0.9, "npc")
    stats.y[1] <- unit(3, "lines")
    stats.y[2] <- unit(2, "lines")
    stats.y[3] <- unit(1, "lines")
    grid.text(c("Number of groups ="), 
              x = stats.x[1],
              y = stats.y[1],
              just = c("left"),
              name = "numgroupslab")
    grid.text(as.character(length(v.statistics)),
              x = stats.x[2],
              y = stats.y[1],
              just = c("left"),
              name = "numgroups")
    #     center <- object$center
    if(length(center) == 1) { 
      grid.text(c("Center ="),
                x = stats.x[1],
                y = stats.y[2],
                just = c("left"),
                name = "centerlab")
      grid.text(label.limits[3],
                x = stats.x[2],
                y = stats.y[2],
                just = c("left"),
                name = "centerstat")
    } else {
      grid.text(c("Center is variable"),
                x = stats.x[2],
                y = stats.y[2],
                just = c("left"),
                name = "centerlab")
    }
    grid.text(c("StdDev ="),
              x = stats.x[1],
              y = stats.y[3],
              just = c("left"),
              name = "stdevlab")
    grid.text(as.character(signif(x=std.dev, sig.figs)),
              x = stats.x[2],
              y = stats.y[3],
              just = c("left"),
              name = "stdevstat")
    
    if(length(unique(lcl)) == 1) {
      grid.text(c("LCL ="),
                x = stats.x[3],
                y = stats.y[2],
                just = c("left"),
                name = "lcllabel")
      grid.text(label.limits[1],
                x = stats.x[4],
                y = stats.y[2],
                just = c("left"),
                name = "lclstat")
    } else {
      grid.text(c("LCL is variable"),
                x = stats.x[3],
                y = stats.y[2],
                just = c("left"),
                name = "lcllabel")
    }
    if(length(unique(ucl)) == 1) {
      grid.text(c("UCL ="),
                x = stats.x[3],
                y = stats.y[3],
                just = c("left"),
                name = "ucllabel")
      grid.text(label.limits[2],
                x = stats.x[4],
                y = stats.y[3],
                just = c("left"),
                name = "uclstat")
    } else { 
      grid.text(c("UCL is variable"),
                x = stats.x[3],
                y = stats.y[3],
                just = c("left"),
                name = "ucllabel")
    }
    if(!is.null(violations)) {
      grid.text(c("Number beyond limits ="),
                x = stats.x[5],
                y = stats.y[2],
                just = c("left"),
                name = "beyondlabel")
      grid.text(as.character(length(unique(violations$beyond.limits))),
                x = stats.x[6],
                y = stats.y[2],
                just = c("left"),
                name = "beyondstat")        
      grid.text(c("Number violating limits ="),
                x = stats.x[5],
                y = stats.y[3],
                just = c("left"),
                name = "violatinglabel")
      grid.text(as.character(length(unique(violations$violating.runs))),
                x = stats.x[6],
                y = stats.y[3],
                just = c("left"),
                name = "violatingstat")        
    }
    popViewport()
  }
  
  #' Set up the viewports
  #' 
  qc.vp.plt.height = unit(1 - as.numeric(qc.vp.bot.height) - as.numeric(convertUnit(qc.vp.top.height, "npc")), "npc")
  qc.vp.plt.y = unit(as.numeric(qc.vp.bot.height) + as.numeric(qc.vp.plt.height) / 2, "npc")
  qc.vp.plt <- viewport(y = qc.vp.plt.y, 
                        height =qc.vp.plt.height, 
                        just = c("centre","center"), 
                        name = "vpplot", 
                        gp = gpar(fontsize = font.size))
  #                        xscale = ggplot_build(qc.gplot)$panel$ranges[[1]]$x.range,
  #                        yscale = ggplot_build(qc.gplot)$panel$ranges[[1]]$y.range)
  
  
  pushViewport(qc.vp.top)
  #grid.rect(gp = gpar(fill = "gray80"), 
  #          name = "titlerect")
  grid.text(main.title, name = "titletext", 
            gp = gpar(fontsize = as.numeric(font.size) + 2, fontface = "bold"),
            x = 0.5, 
            y = unit(1, "npc") - unit(1, "lines"), 
            just = c("centre","center"))
  
  popViewport()
  
  #' Plot the graph
  pushViewport(qc.vp.plt)
  
  #' Draw the graph
  #grid.rect()
  grid.draw(qc.gt)
  
  
  popViewport()
  
  invisible()
}

#' possibly instead of as.environment("package:qcc") we need to use getNamespace("qcc")
#new.namespace <- getNamespace("qcc")
### new.namespace <- as.environment("package.qcc")
#unlockBinding(sym="plot.qcc", env=new.namespace);
#assignInNamespace(x="plot.qcc", value=gg.plot.qcc, ns=asNamespace("qcc"), envir=new.namespace);
#assign("plot.qcc", gg.plot.qcc, envir=new.namespace);
#lockBinding(sym="plot.qcc", env=new.namespace);