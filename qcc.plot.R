if(require(ggplot2) == FALSE)  # Used for plotting
  stop("Could not load library ggplot2. Please install ggplot2 and then source() this file.")
if(require(grid) == FALSE)     # Used to create plot title and statistics regions
  stop("Could not load library grid. Please install grid and then source() this file.")
if(require(gtable) == FALSE)   # Used to align annotations outside the plot region
  stop("Could not load library gtable. Please install gtable and then source() this file.")

#' A waiver object. Copied from ggplot2 
#'
#' A waiver is a "flag" object, similar to \code{NULL}, that indicates the
#' calling function should just use the default value.  It is used in certain
#' functions to distinguish between displaying nothing (\code{NULL}) and
#' displaying a default value calculated elsewhere (\code{waiver()})
#'
#' @export
#' @keywords internal
waiver <- function() structure(NULL, class = "waiver")

is.waive <- function(x) inherits(x, "waiver")

#' @title plot.qcc
#' @author Scrucca, L. (qcc package)
#' @author Hopper, T. J. (ggplot/grid rewrite of plot.qcc) \email{tomhopper@@gmail.com}
#' @copyright (C) 2014 Thomas J. Hopper The MIT License
#' @description Implementation of plot.qcc using ggplot2 and grid
#' @details 
#' @import grid
#' @import ggplot2
#' @import gtable
#' @param x A qcc object to plot.
#' @param add.stats A boolean flag controlling whether summary statistics are
#'      printed on the graph.
#' @param chart.all All boolean flag controlling whether all (old and new) 
#'      statistics are plotted, or only one or the other
#' @param label.limits A character vector with to elements containing the 
#'      labels for the lower control limit line and the upper control limit line. The default
#'      argument now uses \code{\link{waiver()}} from \link{ggplot2}.
#' @param title A character string containing the desired plot title. If not 
#'      supplied, a default will be created. If set to element_blank(), the title will
#'      not be printed and the control chart will be expanded (i.e. the space normally allocated
#'      to the title will be given over to plotting the data).
#' @param xlab A character string containing the desired plot x-axis label. 
#'      If not supplied, a default will be created.
#' @param ylab A character string containing the desired plot y-axis label. 
#'      If not supplied, a default will be created.
#' @param ylim A two-element numeric vector containing desired limits for the 
#'      y axis. If not supplied, a default will be created.
#' @param axes.las An integer indicating the desired orientation of axis labels. 
#'      See \code{?par} for details. Defaults to 0.
#' @param digits An integer indicating the number of digits to print. See 
#'      \code{?getOption} for details. Defaults to getOption("digits")
#' @param restore.par A boolean indicating whether or not graphic parameters 
#'      should be restored. Defaults to TRUE.
#' @param font.size The desired font size in points (pts). Defaults to 12 pts.
#' @param label.cl A character vector with one element containing the
#'      label for the central limit line.
#' @return A \code{grid} object containing the complete plot.
#' TODO: FIX: "Error: `breaks` and `labels` must have the same length" when using newdata argument
#' TODO: Add ability to control breaks on x-axis to avoid overlapping labels
#'        Alt: come up with a pretty labeller that works.
#' TODO: Add ability to control axis orientation, using axes.las.
#' TODO: Work out a cleaner layout for the stats grid, especially one that maintains
#'      spacing when resized to larger sizes (i.e. variable positioning of text).
#' TODO: Add some user control over the theme, e.g. by adding a parameter "theme" and
#'      passing "theme_grey" or "theme_bw."
#' ADDED: Limit digits to getOption(), and try to estimate a smaller value from the data.
#' ADDED: option to control point sizes. Use \code{cex} for backward compatibility
#'      and \code{size} for ggplot2 compatibility.
#' FIXED: label.limits requires three arguments instead of two; make three optional
#' FIXED: when label.limits supplied, text box reports LCL and UCL = labels; should should actual values, and labels only used on graph
#' FIXED: CL, UCL, LCL labels grid panel is too narrow (showing 40 instead 
#'  of 400 and 10 instead of 1030). Used \code{paste(..., collapse = '')}.
#' FIXED: violating.runs only colors first point.
#' FIXED: beyond.limits only plots only one (first?) point.
#' FIXED: variable limits do not plot
#' FIXED: limit labels plot in wrong location.
#' ADDED: Ability to disable plot main title with title = element_blank()

plot.qcc <- function(x, add.stats = TRUE, chart.all = TRUE, 
                     label.limits = waiver(),
                     title = NULL, xlab = NULL, ylab = NULL, ylim = NULL, axes.las = 0,
                     digits =  getOption("digits"),
                     restore.par = TRUE, font.size = 12, size = 4, cex,
                     plot.new = TRUE,
                     label.cl = waiver(), ...) 
{
  object <- x  # Argh.  Really want to use 'object' anyway
  if ((missing(object)) | (!inherits(object, "qcc")))
    stop("an object of class `qcc' is required")
  
  #' if point size is the default and \code{cex} is given, we want to change \code{size} 
  if (size == 4 & !missing(cex)) { 
    if (size != cex) {
      size <- cex
    }
  }
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
  #' Set up observation indices
  if(chart.all) { 
    v.statistics <- c(stats, newstats)
    v.indices <- 1:length(v.statistics) 
  } else { 
    if(is.null(newstats)) { 
      v.statistics <- stats
      v.indices <- 1:length(v.statistics) 
    } else { 
      v.statistics <- newstats 
      v.indices <- seq(length(stats)+1, length(stats)+length(newstats)) 
    }
  }
  
  #' Set y-axis limits explicitly so we can re-use them to control
  #' the layout and appearance of other elements in the grid.
  if(is.null(ylim)) ylim <- range(v.statistics, limits, center)
  #' Set x-axis limit explicitly so we can control the appearance
  #' and re-use for other ggplot objects in a grid arrangement.
  xlim <- range(v.indices)
  
  #' Set up labels
  #' Set axis labels if not provided by the user
  if(is.null(ylab)) ylab <- c("Group summary statistics")
  if(is.null(xlab)) xlab <- c("Group")
  
  #' Create a main graph title. If provided by the user, use that.
  if (!inherits(x=title, what="element_blank")){
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
  }
  
  #' Determine significant figures
  #' If \code{digits} is provided (i.e., \code{digits != getOption("digits")}), then use that, 
  #' otherwise estimate the correct number of significant measurement digits. 
  #' Find the largest number of digits in $statistics, then take the smaller of that and 
  #' getOption("digits")
  if(digits == getOption("digits")) {
    #' Assume user did not set a value.
    has.dec <- FALSE
    sig.dig <- rep(0, length(stats))
    for(i in 1:length(stats)) {
      sig.dig[i] <- length(gregexpr("[[:digit:]]", as.character(stats[i]))[[1]])
    }
    sig.figs <- max(sig.dig) # assume numbers with decimals imply significant figures
    if (sig.figs > getOption("digits")) sig.figs <- getOption("digits")
  } else {
    sig.figs <- digits
  }
  
  #' If the default limit labels are used, create new labels using the last values
  #' for center, limits[1] and limits[2].
  #' Possibilities: label.limits is numeric; label.cl is default or text
  #'                label.cl is numeric; label.limits is default or text
  #' Desired result: label.limits is a character vector with 3 elements
  if(is.waive(label.limits)) {
    label.limits <- c(as.character(signif(lcl[length(lcl)], digits = sig.figs)),
                      as.character(signif(ucl[length(ucl)], digits = sig.figs)))
  } else{
    if(is.numeric(label.limits)) {
      label.limits.text <- as.character(c(NA, NA))
      label.limits.text[1] <- as.character(signif(label.limits[1], digits = sig.figs))
      label.limits.text[2] <- as.character(signif(label.limits[2], digits = sig.figs))
      label.limits <- label.limits.text
    }
  }
  if(is.waive(label.cl)) {
    label.cl <- as.character(signif(center[length(center)], digits = sig.figs))
  } else {
    if(is.numeric(label.cl)) {
      label.cl <- as.character(signif(label.cl, digits = sig.figs))
    }
  }
  
  label.limits <- c(label.limits, label.cl)
  
  #' create a data frame for use by ggplot
  qc.data <- data.frame(df.indices <- v.indices, df.statistics <- as.vector(v.statistics)) 
  
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
  if(is.null(names(stats))) {
    xlabs = as.character(qc.data$df.indices) # xlabs = as.character(indices) 
  } else { 
    xlabs = c(as.character(names(stats)), as.character(names(newstats)))
  }
  
  qc.gplot <- ggplot(data = qc.data, environment = environment(), 
                     aes(x = df.indices, y = df.statistics)) +
    theme(
      text = element_text(size = font.size), 
      plot.margin = unit(c(1,1,1,1), "mm")) +
    scale_x_continuous(expand = c(0, 0.5), limits = xlim, breaks = qc.data$df.indices, labels = xlabs)
  ###
  ## Code works to here
  ###
  #' Plot dots and connecting lines for the statistic variable
  qc.gplot <- qc.gplot + 
    geom_line(colour = "grey40") + 
    geom_point(shape = 20, size = size) 
  ###
  # Code is broken here
  ###
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
    print(center)
    qc.gplot <- qc.gplot + 
      geom_step(aes(x = df.indices, y = c(center, center[length(center)])), direction="hv")
  }
  
  #' Add control limit lines
  if(length(lcl) == 1) {
    #' Likewise for the UCL and LCL lines
    qc.gplot <- qc.gplot + geom_hline(yintercept = lcl, linetype = 2)
    qc.gplot <- qc.gplot + geom_hline(yintercept = ucl, linetype = 2)
  } else {
    #' For variable limits, plot stepped lines for UCL and LCL
    varlimits.df <- data.frame(x.l = qc.data$df.indices, yu.l = ucl[qc.data$df.indices], yl.l = lcl[qc.data$df.indices])
    qc.gplot <- qc.gplot + geom_step(data = varlimits.df, 
                                     aes(x = x.l, y = yl.l), 
                                     direction = "hv", linetype = 2)
    qc.gplot <- qc.gplot + geom_step(data = varlimits.df, 
                                     aes(x = x.l, y = yu.l), 
                                     direction = "hv", linetype = 2)
  }
  
  #' Violating runs
  #' Identify violating runs.
  if(is.null(qcc.options("violating.runs")))
    stop(".qcc.options$violating.runs undefined. See help(qcc.options).")
  index.r <- rep(NA, length(violations$violating.runs))
  if(length(violations$violating.runs > 0)) { 
    index.r <- violations$violating.runs
    if(!chart.all & !is.null(newstats)) { 
      index.r <- index.r - length(stats) 
      index.r <- index.r[index.r>0] 
    }
    #' Create a data frame to (over)plot violating run points.
    df.runs <- data.frame(x.r = qc.data$df.indices[index.r], y.r = qc.data$df.statistics[index.r])
    #' Replot points in violating runs in the adjusted color.
    qc.gplot <- qc.gplot + 
      geom_point(data = df.runs, 
                 aes(x = x.r, y = y.r), 
                 colour = qcc.options("violating.runs")$col, 
                 shape = qcc.options("violating.runs")$pch,
                 size = size)
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
    #' Create a data frame to (over)plot beyond limit points.
    df.beyond <- data.frame(x.b = qc.data$df.indices[index.b], y.b = df.statistics[index.b])
    #' Replot points that are beyond limits.
    qc.gplot <- qc.gplot + 
      geom_point(data = df.beyond, aes(x = x.b, y = y.b), 
                 colour = qcc.options("beyond.limits")$col,
                 shape = qcc.options("beyond.limits")$pch,
                 size = size)
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
  #' First, set up a data frame for plotting.
  qc.df.limitslab <- data.frame(x.ll = c(0,0,0), y.ll = c(limits[length(limits[,1]),1], limits[length(limits[,2]),2], center[length(center)]))
  #' Create a new ggplot object for the labels plot.
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
              aes( x = 0, y = y.ll[1]),
              label = label.limits[1], 
              hjust = 0)
  qc.p3 <- qc.p3 +
    geom_text(data = qc.df.limitslab,
              aes( x = 0, y = y.ll[2]),
              label = label.limits[2], 
              hjust = 0)
  qc.p3 <- qc.p3 +
    geom_text(data = qc.df.limitslab,
              aes( x = 0, y = y.ll[3]),
              label = label.limits[3], 
              hjust = 0)
  
  #' The labels plot is complete; now just grab the "panel" portion of it
  #' for actual display.
  qc.g3 <- gtable_filter(ggplotGrob(qc.p3), "panel")
  
  #' Add gtable columns to draw annotation
  #' to the right for the UCL, LCL and center line labels
  qc.gt <- gtable_add_cols(x=qc.gt, 
                           widths=unit(x=1, units="strwidth", 
                                       data=paste(rep("M",max(nchar(label.limits))), sep = '', collapse = '')), 
                           pos=-1)
  #' Add out labels plot object into the plot grob
  qc.gt <- gtable_add_grob(qc.gt, qc.g3, 
                           t = qc.index$t, 
                           l = ncol(qc.gt), 
                           b = qc.index$b, 
                           r = ncol(qc.gt))
  
  #' If we're plotting newstats, we need another gtable row above the main
  #' plot for the "calibration data..." and "new data in..." labels.
  if(chart.all & (!is.null(newstats))) { 
    #' Set up a data frame for plotting
    qc.df.nslabel <- data.frame(index = v.indices[length(v.indices)], y = 0)
    #' Create the newdata label
    qc.p2.label2 <- paste("New data in", object$newdata.name)
    #' Calculate the position of the newdata label
    qc.p2.label2.x <- len.obj.stats + len.new.stats/2
    #' Create the ggplot object
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
    #' Add the newstats label plot above the main plat
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
  
  #' Explicitly create a parent viewport for the whole plot window so that
  #' we are sure to have dimensional information for positioning.
  qc.vp.main <- viewport(gp = gpar(fontsize = font.size))
  pushViewport(qc.vp.main)
  
  #' If the user does not want a graph title, make the title viewport
  #' zero height. Otherwise, make it 4 lines high.
  if (inherits(x=title, what="element_blank")) {
    qc.vp.top.height = unit(0, "npc")
  } else {
    qc.vp.top.height = convertUnit(unit(4, "lines"), "npc")
  }
  
  #' Set the bottom (stats) panel height to zero. If the user
  #' wanted stats printed, we'll expand this later.
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
    #' Set up a tabular layout for the statistics
    stats.x <- unit(rep(NA, 6), "npc")
    stats.y <- unit(rep(NA, 3), "lines")
    stats.x[1] <- unit(0.1, "npc")
    stats.x[2] <- unit(0.3, "npc")
    stats.x[3] <- unit(0.4, "npc")
    stats.x[4] <- unit(0.5, "npc")
    stats.x[5] <- unit(0.58, "npc")
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
              x = stats.x[3],
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
      grid.text(as.character(signif(center[length(center)], digits = sig.figs)),
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
      grid.text(as.character(signif(lcl[length(lcl)], digits = sig.figs)),
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
      grid.text(as.character(signif(ucl[length(ucl)], digits = sig.figs)),
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
  
  #' Set up the main plot viewport
  #' 
  qc.vp.plt.height = unit(1 - as.numeric(qc.vp.bot.height) - as.numeric(qc.vp.top.height), "npc")
  qc.vp.plt.y = unit(as.numeric(qc.vp.bot.height) + as.numeric(qc.vp.plt.height) / 2, "npc")
  qc.vp.plt <- viewport(y = qc.vp.plt.y, 
                        height =qc.vp.plt.height, 
                        just = c("centre","center"), 
                        name = "vpplot", 
                        gp = gpar(fontsize = font.size))
  #                        xscale = ggplot_build(qc.gplot)$panel$ranges[[1]]$x.range,
  #                        yscale = ggplot_build(qc.gplot)$panel$ranges[[1]]$y.range)
  
  #' Draw the main graph title
  if (!inherits(x=title, what="element_blank")){ 
    pushViewport(qc.vp.top)
    #grid.rect(gp = gpar(fill = "gray80"), 
    #          name = "titlerect")
    grid.text(main.title, name = "titletext", 
              gp = gpar(fontsize = as.numeric(font.size) + 2, fontface = "bold"),
              x = 0.5, 
              y = unit(1, "npc") - unit(1, "lines"), 
              just = c("centre","center"))
    
    popViewport()
  }  
  
  #' Plot the graph
  pushViewport(qc.vp.plt)
  
  grid.draw(qc.gt)
  
  
  popViewport()
  
  invisible()
}

#' Replace the qcc package plot function with our custom function.
unlockBinding(sym="plot.qcc", env=getNamespace("qcc"));
assignInNamespace(x="plot.qcc", value=plot.qcc, ns=asNamespace("qcc"), envir=getNamespace("qcc"));
assign("plot.qcc", plot.qcc, envir=getNamespace("qcc"));
lockBinding(sym="plot.qcc", env=getNamespace("qcc"));
