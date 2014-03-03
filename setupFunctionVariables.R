library(qcc)

#' XmR
my.xmr.raw <- c(5045,4350,4350,3975,4290,4430,4485,4285,3980,3925,3645,3760,3300,3685,3463,5200)
my.xmr.new <- round(c(runif(5, 3500, 4000)))
x <- qcc(my.xmr.raw, type = "xbar.one", title = "Individuals Chart\nfor Wheeler sample data")
x <- qcc(my.xmr.raw, type = "xbar.one", newdata = my.xmr.new)
x <- qcc(matrix(cbind(my.xmr.raw[1:length(my.xmr.raw)-1], my.xmr.raw[2:length(my.xmr.raw)]), ncol = 2), type = "R", title = "Moving Range Chart\nfor Wheeler sample data")

#' xbar-R
my.xbr.raw <- c(45,46,43,44,
                56,53,51,52,
                45,46,42,45,
                52,53,51,54,
                46,44,44,46,
                55,56,43,56)
my.xbr.raw <- matrix(data=my.xbr.raw, ncol = 4, byrow = TRUE)
x <- qcc(my.xbr.raw, type = "xbar")
x <- qcc(my.xbr.raw, type = "R")

#' variable limits
data(pistonrings)
attach(pistonrings)
out <- c(9, 10, 30, 35, 45, 64, 65, 74, 75, 85, 99, 100)
diameter <- qcc.groups(pistonrings$diameter[-out], sample[-out])
x <- qcc(diameter[1:25,], type="xbar", plot = TRUE)
x <- qcc(diameter[1:25,], type="R", plot = TRUE)
x <- qcc(diameter[1:25,], type="S", plot = TRUE)
x <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot = TRUE)
x <- qcc(diameter[1:25,], type="R", newdata=diameter[26:40,], plot = TRUE)
x <- qcc(diameter[1:25,], type="S", newdata=diameter[26:40,], plot = TRUE)
detach(pistonrings)

data(circuit)
attach(circuit)
qcc(x[trial], sizes=size[trial], type="c")
# remove out-of-control points (see help(circuit) for the reasons)
inc <- setdiff(which(trial), c(6,20))
qcc(x[inc], sizes=size[inc], type="c", labels=inc)
qcc(x[inc], sizes=size[inc], type="c", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
qcc(x[inc], sizes=size[inc], type="u", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
detach(circuit)

data(pcmanufact)
attach(pcmanufact)
qcc(x, sizes=size, type="u")
detach(pcmanufact)

data(dyedcloth)
attach(dyedcloth)
qcc(x, sizes=size, type="u")
# standardized control chart
detach(dyedcloth)

data(orangejuice)
attach(orangejuice)
qcc(D[trial], sizes=size[trial], type="p")

# remove out-of-control points (see help(orangejuice) for the reasons)
inc <- setdiff(which(trial), c(15,23))
q1 <- qcc(D[inc], sizes=size[inc], type="p")
qcc(D[inc], sizes=size[inc], type="p", newdata=D[!trial], newsizes=size[!trial]) 
detach(orangejuice)

data(orangejuice2)
attach(orangejuice2)
names(D) <- sample
qcc(D[trial], sizes=size[trial], type="p")
q2 <- qcc(D[trial], sizes=size[trial], type="p", newdata=D[!trial], newsizes=size[!trial])
detach(orangejuice2)
