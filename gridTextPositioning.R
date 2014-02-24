grid.newpage()
font.size <- unit(12, "points")
my.x <- unit(rep(0.1, 3), "npc")
my.y1 <- unit(c(0.2, 0.5, 0.8), "npc")
my.y2 <- unit(c(3,2,1), "lines")

vp.main <- viewport(name = "vpmain", gp = gpar(fontsize = unit(as.numeric(font.size) *2, "points")))
pushViewport(vp.main)

grid.points(x = my.x, y = my.y1, pch = 1)
grid.points(x = my.x, y = unit(c(1, 2, 3), "char"), pch = 2)
grid.text(label=paste("String text", c(1,2,3)), x = my.x, y = my.y2)
convertUnit(unit(c(1, 2, 3), "char"), "npc")
convertUnit(unit(c(1, 2, 3), "lines"), "npc")
convertUnit(unit(x=3, units="strheight", data = "String"), "npc")

vp.bot.height = convertUnit(unit(6, "lines"), "npc")
vp.bot <- viewport(y = unit(0, "npc"), 
                   height =vp.bot.height, 
                   just = c("centre", "bottom"), 
                   name = "vpbot", 
                   just = c("centre", "bottom"),
                   gp = gpar(fontsize = font.size))
pushViewport(vp.bot)
grid.points(x = my.x+unit(0.1,"npc"), y = my.y1, pch = 3)
grid.points(x = my.x+unit(0.2,"npc"), y = unit(c(1, 2, 3), "char"), pch = 4)
grid.text(label=paste("String text", c(1,2,3)), x = my.x+unit(0.2,"npc"), y = my.y2)
convertUnit(unit(c(1, 2, 3), "char"), "npc")
convertUnit(unit(c(1, 2, 3), "lines"), "npc")
convertUnit(unit(x=3, units="strheight", data = "String"), "npc")

