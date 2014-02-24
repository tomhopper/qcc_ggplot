library(qcc)
library(ggplot2)  # Used for plotting
library(grid)     # Used to create plot title and statistics regions
library(gtable)   # Used to align annotations outside the plot region
#library(gridExtra)

#' The data
my.xmr.raw <- c(5045,4350,4350,3975,4290,4430,4485,4285,3980,3925,3645,3760,3300,3685,3463,5200)
base.text.size <- 14
#' Create the qcc objects
my.xmr.x <- qcc(my.xmr.raw, type = "xbar.one", plot = TRUE)
my.xmr.mr <- qcc(matrix(cbind(my.xmr.raw[1:length(my.xmr.raw)-1], my.xmr.raw[2:length(my.xmr.raw)]), ncol=2), type="R", plot = FALSE)
#' turn data into a data frame for ggplot
my.xmr.df <- data.frame(index = 1:length(my.xmr.raw), x = my.xmr.raw)

#' plot.margin order: top, right, bottom, left
# make pretty axis limits so we can use them later for positioning text
my.ylim <- range(c(my.xmr.x$statistics, my.xmr.x$limits[2], my.xmr.x$limits[1]))
my.xlim <- range(1:length(my.xmr.x$statistics))

#' calculate the significant digits for display
my.xmr.raw.sigdig <- rep(NA, length(my.xmr.raw))
for(i in 1:length(my.xmr.raw)) {
  if(my.xmr.raw[i] %% 1 > 0) my.xmr.raw.dec <- TRUE
  my.xmr.raw.sigdig[i] <- length(gregexpr("[[:digit:]]", as.character(my.xmr.raw[i]))[[1]])
}
my.xmr.raw.sig.figs <- min(my.xmr.raw.sigdig)

# Set font sizes to be consistent between ggplot elements and grid elements
# TODO: adopt automatically-resizing text clas:
# http://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/


#' Determine significant figures
#' Assume that if the numbers in $statistics include a decimal, then the largest number
#' of significant figures is the actual significant figures and everything else has been
#' truncated. Otherwise, assume the smallest number of digits is the significant figures.
my.xmr.raw.dec <- FALSE
for(i in 1:length(my.xmr.raw)) {
  if(my.xmr.raw[i] %% 1 > 0) my.xmr.raw.dec <- TRUE
  my.xmr.raw.sigdig[i] <- length(gregexpr("[[:digit:]]", as.character(my.xmr.raw[i]))[[1]])
}
if(my.xmr.raw.dec) {
  my.xmr.raw.sigfig <- max(my.xmr.raw.sigdig)
} else {
  my.xmr.raw.sigfig <- min(my.xmr.raw.sigdig)
}

# create the graph
my.p <- ggplot(data = my.xmr.df) +
  theme(text = element_text(size = base.text.size), plot.margin = unit(c(0,0,0,0), "line")) +
#   theme(text = element_text(size = base.text.size),
#         plot.margin = unit(c(0,nchar(as.character(signif(my.xmr.x$center, my.xmr.raw.sig.figs)))+1,0,0), "char")) +
  scale_x_continuous(expand = c(0, 0.5), limits = my.xlim) +
  geom_point(aes(x = index,y = x),shape = 20, size = 4) +
  geom_line(aes(x = index,y = x)) +
  geom_point(aes(x = index[x > 5100],y = x[x > 5100]),shape = 19, size = 4, colour = '#ff0033')

my.p <- my.p +
  ylim(my.ylim)

# draw the UCL and LCL lines
my.p <- my.p +
  stat_hline(yintercept = my.xmr.x$limits[2],linetype = 2) +
  stat_hline(yintercept = my.xmr.x$limits[1],linetype = 2)  # size = 0.25 may be too thin

# draw the center line
my.p <- my.p +
  stat_hline(yintercept = my.xmr.x$center, linetype = 1)

#' Create a vertical reference line...we will try to label to the left and right
#' of this in my.vp.top
my.p <- my.p + geom_vline(xintercept = 10)

#' Add gtable grobs to draw annotation
my.gt <- ggplotGrob(my.p)
index <- subset(my.gt$layout, name == "panel")


#' Create the UCL, LCL, center line lables
my.df.right <- data.frame(index = 0, y = my.xmr.x$statistics[length(my.xmr.x$statistics)])
my.p3 <- ggplot(my.xmr.df, aes(x = index, y = x)) +
  geom_blank() +
  theme_minimal() +
  theme(line = element_blank(),
        text = element_blank(),
        panel.background = element_rect(fill = "grey50", linetype = 2)) +
  guides(colour = "none") +
  scale_x_continuous(expand = c(0, 0)) +
  ylim(my.ylim)

my.p3 <- my.p3 +
  geom_text(data = my.df.right,
            aes( x = 0, y = my.xmr.x$limits[1]),
            label = as.character(signif(my.xmr.x$limits[1], my.xmr.raw.sig.figs)), 
            hjust = 0)
my.p3 <- my.p3 +
  geom_text(data = my.df.right,
            aes( x = 0, y = my.xmr.x$limits[2]),
            label = as.character(signif(my.xmr.x$limits[2], my.xmr.raw.sig.figs)), 
            hjust = 0)
my.p3 <- my.p3 +
  geom_text(data = my.df.right,
            aes( x = 0, y = my.xmr.x$center),
            label = as.character(signif(my.xmr.x$center, my.xmr.raw.sig.figs)), 
            hjust = 0)

my.g3 <- gtable_filter(ggplotGrob(my.p3), "panel")

#' Add gtable grobs to draw annotation
#' One to the right for the UCL, LCL and center line labels
my.gt <- gtable_add_cols(x=my.gt, 
                         widths=unit(x=1, units="strwidth", data=as.character(signif(my.xmr.x$limits[1], my.xmr.raw.sig.figs)*10)), 
                         pos=-1)
my.gt <- gtable_add_grob(my.gt, my.g3, 
                         t = index$t, 
                         l = ncol(my.gt), 
                         b = index$b, 
                         r = ncol(my.gt))

my.newstats.flag <- TRUE
if(my.newstats.flag) {
  #' Create the newstats labels graph
  my.df.top <- data.frame(index = my.xmr.df$index[length(my.xmr.df$index)], y = 0)
  my.p2 <- ggplot(my.xmr.df, aes(x = index, y = x)) +
    geom_blank() +
    theme_minimal() +
    theme(line = element_blank(),
          text = element_blank(),
          panel.background = element_rect(fill = "grey50", linetype = 2)) +
    scale_x_continuous(expand = c(0, 0.5), limits = my.xlim) +
    guides(colour = "none")
  
  my.p2 <- my.p2 +
    geom_text(data = my.df.top,
              aes(x = 10 - 0.5, y = 1), 
              label = "Left-side Text", 
              hjust = 1,
              vjust = 0) +
    geom_text(data = my.df.top,
              aes(x = 10 + 0.5, y = 1), 
              label = "Right-side Text", 
              hjust = 0,
              vjust = 0)
  
  #' Get just the panel from my.p2
  my.g2 <- gtable_filter(ggplotGrob(my.p2), "panel")
  #' If needed, one above for the newstats labels
  my.gt <- gtable_add_rows(my.gt, unit(2*base.text.size, "points"), pos = 0)
  my.gt <- gtable_add_grob(x = my.gt, grobs = my.g2, 
                           t = 1, 
                           l = 4, 
                           b = 1, 
                           r = 4)
}

# Label the limit lines
# addition to xmin / xmax, 0.6 works for 16 units, probably not enough for 100
# calculate by grid.convert 0.5 char to native
#' TODO: instead of text labels, use the numeric values, rounded to the correct
#' number of significant digits with signif()

# my.p <- my.p +
#   annotation_custom(grob = textGrob(as.character(signif(my.xmr.x$limits[1], my.xmr.raw.sig.figs)), hjust = 0, gp = gpar(fontsize = base.text.size)), 
#                     xmin = length(my.xmr.x$statistics) + 0.6,
#                     xmax = length(my.xmr.x$statistics) + 0.6, 
#                     ymin = my.xmr.x$limits[1], 
#                     ymax = my.xmr.x$limits[1])
# my.p <- my.p +
#   annotation_custom(grob = textGrob(as.character(signif(my.xmr.x$limits[2], my.xmr.raw.sig.figs)), hjust = 0, vjust = 0.5, gp = gpar(fontsize = base.text.size)), 
#                     xmin = length(my.xmr.x$statistics) + 0.6,
#                     xmax = length(my.xmr.x$statistics) + 0.6, 
#                     ymin = my.xmr.x$limits[2], 
#                     ymax = my.xmr.x$limits[2])
# # label it
# my.p <- my.p +
#   annotation_custom(grob = textGrob(as.character(signif(my.xmr.x$center, my.xmr.raw.sig.figs)), hjust = 0, vjust = 0.5, gp = gpar(fontsize = base.text.size)), 
#                     xmin = length(my.xmr.x$statistics) + 0.6,
#                     xmax = length(my.xmr.x$statistics) + 0.6, 
#                     ymin = my.xmr.x$center, 
#                     ymax = my.xmr.x$center)



#' Turn off clipping for the ggplot graph so that we can include the CL, USL, LSL annotations
#' Not needed if we used gtable instead
#my.gt <- ggplot_gtable(ggplot_build(my.p))
#my.gt$layout$clip[my.gt$layout$name == "panel"] <- "off"


#' Set up the grid graphics viewports
#' top: 3 - 4 lines for title, subtitle and segments labels
#' middle: graph
#' bottom: statistics. only create bottom viewport if add.stats == TRUE
#' 

#' Create a new graph
#' TODO: determine if this screws up an existing graph and viewport arrangement,
#' as might be created by the user to place the individuals and moving range charts
#' on a single page.
grid.newpage()

#' Set the hight of the top and bottom viewports
my.vp.top.height = convertUnit(unit(3, "lines"), "npc")
my.vp.bot.height = convertUnit(unit(0, "lines"), "npc")

#' Set up the top viewport, pinning it to the top of the parent viewport.
my.vp.top <- viewport(x = unit(0.5, "npc"), 
                      y = unit(1, "npc"), 
                      height = my.vp.top.height, 
                      width = unit(1, "npc"), 
                      just = c("centre", "top"), 
                      name = "vptop", 
                      gp = gpar(fontsize = base.text.size))

#' For debugging purposes, create a viewport covering the whole page and mark the height 0 - 10
pushViewport(my.vp.parent <- 
               viewport(x = 0.5, y = 0.5, width = 1, height = 1, just = c("centre","center")))
for(i in 0:10) {
  grid.text(as.character(i), x=unit(0, "npc"), y=unit(i/10, "npc"), just=c("left","center"))
}

add.stats <- TRUE

if(add.stats) {
  my.vp.bot.height = convertUnit(unit(6, "lines"), "npc")
  my.vp.bot <- viewport(y = unit(0, "npc"), 
                        height = my.vp.bot.height, 
                        just = c("centre", "bottom"), 
                        name = "vpstat", 
                        gp = gpar(fontsize = base.text.size))
}

my.vp.plt.height = unit(1 - as.numeric(my.vp.bot.height) - as.numeric(my.vp.top.height), "npc")
my.vp.plt.y = unit(as.numeric(my.vp.bot.height) + as.numeric(my.vp.plt.height) / 2, "npc")
my.vp.plt <- viewport(y = my.vp.plt.y, 
                      height = my.vp.plt.height, 
                      just = c("centre","center"), 
                      name = "vpplot", 
                      gp = gpar(fontsize = base.text.size),
                      xscale = ggplot_build(my.p)$panel$ranges[[1]]$x.range,
                      yscale = ggplot_build(my.p)$panel$ranges[[1]]$y.range)

pushViewport(my.vp.top)
grid.rect(gp = gpar(fill = "gray80"), 
          name = "titlerect")
grid.text("Plot Title", name = "titletext", 
          gp = gpar(fontsize = base.text.size + 2, fontface = "bold"),
          x = 0.5, 
          y = unit(1, "npc") - unit(1, "lines"), 
          just = c("centre","center"))
grid.text("Plot SubTitle Text", name = "subtitletext", 
          gp = gpar(fontsize = base.text.size, fontface = "bold"),
          x = 0.5, 
          y = unit(1, "npc") - unit(2, "lines"), 
          just = c("centre","center"))
# grid.text(label="plot some text", name = "group1text",
#           gp = gpar(fontsize = base.text.size - 2, fontface = "plain"),
#           x = 0.25, 
#           y = unit(x=0, units="npc") + unit(x = 0.5, units = "lines"), 
#           just = c("centre","center"))
upViewport()

pushViewport(my.vp.bot)
grid.rect(gp = gpar(fill = "gray50"), name = "statsrect")
grid.text("some stats", name = "statstext")
#left.text.x.l <- unit()
upViewport()

pushViewport(my.vp.plt)

#' Draw the graph
grid.rect()
grid.draw(my.gt)


upViewport(0)

