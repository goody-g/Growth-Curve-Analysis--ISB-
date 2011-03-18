#BGN FUNCTION
plot.gAnalysis <-function(g.analysis.all, well, name="lastplot", svg = F, graph.log=F, control=gControl()) {
# Plots the metadata, spline fit and parameters for a bioscreen well in different styles depending on input.

# Args
#	g.analysis.all - an object of class "gAnalysis" containing information about a set of wells.
#	well - the index (as in the index in g.analysis.all) for the well wanted to graph, or "all" if want all graphed (different type of graph entirely)
#	name - the name for the svg if svg is being plotted.
#	svg - logical, either to plot to an svg file or not. Saved in control$root.folder
#	control -  control object of class "gControl". See documentation for more information.	
#		$root.folder - the folder on the local computer containing each database folder, a character string formatted for R. See documentation.
#		$no.list - attributes to not list on the graphs, such as "File.Name"
#	graph.log - logical stating whether the y-axis of the crowth curve data should be plotted on a logrithmic scale. (TODO - make work)

# Returns - a graph or svg file (open in chrome or firefox or other svg viewer)

#TODO: Keeps opening new windows - fix. 
#	Coloring isn't always right, doesn't always work.
#	Add LOG scale to x axis(!)


# Load the Grid package. TODO: Add a check to see whether package is already loaded first, or error if it can't be loaded
library(grid)

if (svg == T){
library(RSVGTipsDevice)
}

########################################### For graphing into R #########################################################
if (svg == F) { #if svg=T

#Open R on-screen device.
quartz(width=unit(11, "inches"), height=unit(8.5, "inches")) # "windows" changed to "quartz" for MAC by ST #Change this?
grid.newpage()

# ## # ## # ## # ## # ## # ## # ## # ## # For a single well view # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## #
if (well != "all") {
g.analysis.well<- g.analysis.all[[well]]

# Get general layout information 
{
# X axis length
spline.xscale <- c(0, (max(g.analysis.well$spline$x) + .05*max(g.analysis.well$spline$x)))
spline.yscale <- c((min(g.analysis.well$raw.data) - .05*min(g.analysis.well$raw.data)), (max(g.analysis.well$raw.data) + .05*max(g.analysis.well$raw.data)))

# Sidebar Info

well.info <- g.analysis.well$Info
well.param <- g.analysis.well$parameters

#  Make a list of all info labels which don't have NA answers (so are worth displaying)

all.attr.names <- names(well.info)
all.param.names <- names(well.param)

attr.names <- NULL
param.names <- NULL

# Only include each label if it has a non-NA attribute and it is not included in the control$no.list list.
for (i in 1:length(all.attr.names)){ #for each name
if (is.na(g.analysis.well$Info[[all.attr.names[i]]]) == F) { #if it is interesting (not NA)
if (is.element(all.attr.names[i], control$no.list) == F) { #if it isn't part of the no.list list
attr.names <- c(attr.names, all.attr.names[i]) #keep it to add to the graph
}}}
### Vector of corresponding non-null attribute labels
attr.info <- NULL

for (i in 1:length(attr.names)) {
attr.info <- c(attr.info, as.character(g.analysis.well$Info[[attr.names[i]]])) #add information for each "interesting" well label to attr.info
}

# # AGAIN FOR PARAMETERS: Only include each label if it has a non-NA attribute and it is not included in the control$no.list list.
for (i in 1:length(all.param.names)){
if (is.na(g.analysis.well$parameters[[all.param.names[i]]]) == F) { #if it is interesting (not na)
if (is.element(all.param.names[i], control$no.list) == F) { # if it isn't a part of hte no.list list
param.names <- c(param.names, all.param.names[i])	#keep it to add to the graph
}}}

### Vector of corresponding non-null attribute labels
param.info <- NULL

for (i in 1:length(param.names)) {
param.info <- c(param.info, as.character(format(g.analysis.well$parameters[[param.names[i]]], digits=3))) #keep info part to add to the graph.
}


### Maximum width (=width of sidebar.attr section)
attr.names.width <- convertWidth(max(stringWidth(attr.names))+unit(2, "mm"), "cm")
attr.info.width <- convertWidth(max(stringWidth(attr.info))+unit(2, "mm"), "cm")

param.names.width <- convertWidth(max(stringWidth(param.names))+unit(2, "mm"), "cm")
param.info.width <- convertWidth(max(stringWidth(param.info))+unit(2, "mm"), "cm")

sidebar.width <- attr.names.width+attr.info.width

info.height <- convertWidth(unit(length(attr.names), "char")+unit((length(attr.names)-1)*2, "mm"), "cm")

### TODO Add an errorcheck/solution for info height being too tall, sidebar being too wide

}


# # # # # # # # # # # # # # # Functions for graphing different regions # # # # # # # # # # # # # # # # # # # # # # # # # #
sidebar.attr <- function(g.analysis.well, sidebar.width, control){ #TODO need ... ?

# Place the parent viewport, info, creating layout parts with width to match needed for labels and attr columns. 
info <- viewport(y=unit(1, "npc"),just=c("center", "top"), height=unit(info.height, "cm") , layout=grid.layout(length(attr.names),2, widths=unit(c(attr.names.width, attr.info.width), c("cm", "cm"))))
pushViewport(info)

#Add attribute names
for (i in 1:length(attr.names)) {
pushViewport(viewport(layout.pos.row=i, layout.pos.col=1)) 
grid.rect(gp=gpar(fill="white"))
grid.text(attr.names[i]) # TODO add font styles
popViewport()
}

# Add attribute info

for (i in 1:length(attr.names)) {
pushViewport(viewport(layout.pos.row=i, layout.pos.col=2))
grid.rect(gp=gpar(fill="white"))
grid.text(attr.info[i])
popViewport()
}
popViewport() #Escape info viewport

} 

sidebar.param <- function(g.analysis.well, control) { #TODO need ... ?
parameters <- viewport(just=c("center"), layout=grid.layout(nrow=length(param.names), ncol=2, widths=unit(c(param.names.width, param.info.width), c("cm", "cm"))))
pushViewport(parameters)

#Add attribute names
for (i in 1:length(param.names)) { #for each parameter name
pushViewport(viewport(layout.pos.row=i, layout.pos.col=1)) 
grid.rect(gp=gpar(fill="white"))
grid.text(param.names[i]) # TODO add font styles
popViewport()
} #for each parameter name


for (i in 1:length(param.names)) { #for each parameter name
pushViewport(viewport(layout.pos.row=i, layout.pos.col=2))
grid.rect(gp=gpar(fill="white"))
grid.text(param.info[i])
popViewport()
} #for each parameter name

popViewport() #Escape parameters viewport
} # function: sidebar.param

fit <- function(g.analysis.well, control, spline.xscale){
# Place viewports 
fit <- viewport(just="center")
plot <- plotViewport(margins = c(2, 3.5, 1, 1)) #margins for axis, etc, #s in lines, bottom, left, top, right
pushViewport(fit)
#grid.rect()
pushViewport(plot)

x <-g.analysis.well$raw.time
y<- g.analysis.well$raw.data

# Plot the bioscreen data points
raw.data <- dataViewport(g.analysis.well$raw.time, g.analysis.well$raw.data, xscale=spline.xscale, yscale=spline.yscale) #spline.xscale
pushViewport(raw.data)
grid.rect()
grid.xaxis(gp=gpar(fontsize=8))
grid.yaxis(gp=gpar(fontsize=8))
grid.text("Time (in hours)", y=unit(-2, "lines"), gp=gpar(fontsize=10))
grid.text("Optical Density", x=unit(-3, "lines"), gp=gpar(fontsize=10), rot=90)

grid.points(x, y, gp=gpar(pch=21, col="darkorange", alpha=.5, cex=.7))
grid.lines(x=g.analysis.well$spline$x, y=g.analysis.well$spline$y, default.units="native", gp=gpar(col="darkred", lwd=3))

# A
#grid.segments(x0=unit(0, "native"), x1=g.analysis.well$graph$time.A, y0=g.analysis.well$parameters$A, y1=g.analysis.well$parameters$A,
#	default.units="native", arrow=arrow(type="open", ends="last", length=unit(1, "native")), gp=gpar(col="chocolate3", type=2))
#grid.rect(y=unit(g.analysis.well$parameters$A, "native"), x=unit(.2 , "npc"), width=unit(strwidth(paste("A: ", format(g.analysis.well$parameters$A, digits=3)), "inches"), "inches"), gp=gpar(col="white", fill="white"), just="centre", height=unit(1, "char"))
#grid.text(x=unit(.2 , "npc"), label=paste("A:", format(g.analysis.well$parameters$A, digits=3)), y=unit(g.analysis.well$parameters$A, "native"), gp=gpar(col="chocolate3"))

# Mu	
mu.low.x 	<- 	as.numeric(g.analysis.well$graph$time.mu) -10
mu.high.x 	<-	as.numeric(g.analysis.well$graph$time.mu)+10
mu.low.y 	<- 	as.numeric(g.analysis.well$graph$y.mu)-10*as.numeric(g.analysis.well$parameters$mu)
mu.high.y 	<- as.numeric(g.analysis.well$graph$y.mu+10*g.analysis.well$parameters$mu)

grid.segments(x0=unit(mu.low.x, "native"), x1=unit(mu.high.x, "native"), y0= unit(mu.low.y, "native"), y1=unit(mu.high.y, "native"),
	gp=gpar(col="blue", lwd=3))
grid.text(label=paste("Maximum Growth Rate:", format(g.analysis.well$parameters$mu, digits=3)), 
	#x=unit(mu.high.x, "native")-unit(strwidth(paste("Maximum Growth Rate:", format(g.analysis.well$parameters$mu, digits=3)), "inches"), "inches"),
	x=unit(mu.high.x, "native")-unit(5, "native"),
	y=unit(mu.high.y, "native") - unit(1, "lines"),
	just="right",
	gp=gpar(col="blue", fontsize=10))

# Lambda
grid.segments(x0=unit(0, "native"), x1=unit(g.analysis.well$parameters$lambda, "native"), y0=0, y1=0, 
	gp=gpar(col="darkolivegreen4", lwd=3, lend="butt"), arrow=arrow(angle=90, length=unit(.1, "inches")))
grid.rect(x=unit(1.5, "native"), y=unit(0, "native")+unit(.7, "lines"), 
	width=unit(strwidth(paste("Lag:", format(g.analysis.well$parameters$lambda, digits=3)), "inches"), "inches")-unit(1, "native"),
	height=unit(1, "lines"),
	just=c("left", "centre"),
	gp=gpar(col="darkolivegreen4", alpha=.5))
grid.text(label=paste("Lag:", format(g.analysis.well$parameters$lambda, digits=3)),
	x=unit(2, "native"), y=unit(0, "native")+unit(.7, "lines"), just="left",
	gp=gpar(col="darkolivegreen4", fontsize=10))

#TODO	
# Trajectory
 #traj.slope <- .003# g.analysis.well$parameters$trajectory
#grid.segments(x0=unit(tail(x, n=1)-15, "native"), x1=unit(tail(x, n=1)+5, "native") ,y0=unit(tail(y, n=1)-traj.slope*15, "native"), y1=unit(tail(y, n=1)+traj.slope*5, "native"), 
#	gp=gpar(col="red", lwd=3))
#x0=unit(tail(x, n=1)-20, "native"), x1=unit(tail(x, n=1)+5, "native"), 

#y0=unit((.4-(20*.003)), "native"), y1=unit((.4+(5*.003)), "native"),

	#gp=gpar(col="red", lwd=3))

 
#grid.segments(x0=20, x1=90, y0=.5, y1=.7)
#x0=unit(tail(x, n=1)-20, "native"), x1=unit(tail(x, n=1)+5, "native"), y0=unit(tail(x, n=1)-(20*traj.slope), "native"), y1=unit(tail(x, n=1)+(5*traj.slope), "native"),
	#gp=gpar(col="red", lwd=3))
 
 
popViewport() #raw.data
popViewport() #plot
popViewport() #fit
}

deriv <- function(g.analysis.well, control, spline.xscale){
deriv <- viewport(just="center")
plot <- plotViewport(margins = c(2, 3.5, 1, 1)) 
pushViewport(deriv)
pushViewport(plot)

x=g.analysis.well$deriv$x
y=g.analysis.well$deriv$y


# Plot the deriv points
deriv.points <- dataViewport(x, y, xscale=spline.xscale) #spline.xscale
pushViewport(deriv.points)
grid.rect()
grid.xaxis(gp=gpar(fontsize=8))
grid.yaxis(gp=gpar(fontsize=8))
grid.text("Time (in hours)", y=unit(-2, "lines"), gp=gpar(fontsize=10))
grid.text("Derivative(OD)", x=unit(-3.5, "lines"), gp=gpar(fontsize=10), rot=90)

grid.lines(x=g.analysis.well$deriv$x, y=g.analysis.well$deriv$y, default.units="native", gp=gpar(col="darkblue", lwd=3))
grid.segments(x0=unit(0, "native"), x1=unit(max(spline.xscale), "native"), y0=unit(0, "native"), y1=unit(0, "native"), gp=gpar(col="darkgoldenrod"))


popViewport() #deriv.points
popViewport() #plot
popViewport() #deriv
}

secderiv <- function(g.analysis.well, control, spline.xscale){

secderiv <- viewport(just="center")
plot <- plotViewport(margins = c(3, 3.5, 1, 1)) #,
pushViewport(secderiv)
#grid.rect()
pushViewport(plot)

x=g.analysis.well$secderiv$x
y=g.analysis.well$secderiv$y

# Plot secderiv points
secderiv.points <- dataViewport(x, y, xscale=spline.xscale) #spline.xscale
pushViewport(secderiv.points)
grid.rect()
grid.xaxis(gp=gpar(fontsize=8))
grid.yaxis(gp=gpar(fontsize=8))
grid.text("Time (in hours)", y=unit(-2, "lines"), gp=gpar(fontsize=10))
grid.text("2nd Derivative(OD)", x=unit(-3.5, "lines"), gp=gpar(fontsize=10), rot=90)

grid.lines(x=g.analysis.well$secderiv$x, y=g.analysis.well$secderiv$y, default.units="native", gp=gpar(col="darkblue", lwd=3))
grid.segments(x0=unit(0, "native"), x1=unit(max(spline.xscale), "native"), y0=unit(0, "native"), y1=unit(0, "native"), gp=gpar(col="darkgoldenrod"))

popViewport() #secderiv.points
popViewport() #plot
popViewport() #secderiv
}

#######################################################
{ #using functions, draw to screen
background <- viewport(width=unit(11, "inches"), height=unit(8.5, "inches")) # TODO: take away size specs?
pushViewport(background)
grid.rect()

v.gAnalysis <- viewport(just=c("centre", "centre"), width=unit(25.4, "cm"), height=unit(7, "inches"), layout=grid.layout(nrow=1, ncol=2,  #TODO fix or remove sizes
	))
	widths=unit.c(sidebar.width, unit(1, "null"))
pushViewport(v.gAnalysis)
grid.rect(gp=gpar(col="blue"))

#Sidebar
v.sidebar <-  viewport(layout.pos.col=1, layout=grid.layout(nrow=2))
pushViewport(v.sidebar)

pushViewport(viewport(layout.pos.row=1))
sidebar.attr(g.analysis.well, sidebar.width, control)
popViewport() 

pushViewport(viewport(layout.pos.row=2))
sidebar.param(g.analysis.well, control)
popViewport() 

popViewport() #v.sidebar

v.graphs <-viewport(layout.pos.col=2, layout=grid.layout(nrow=4, heights=unit(c(.05,.55,.2,.2), "npc")))
pushViewport(v.graphs)

v.title <- viewport(layout.pos.row=1)
pushViewport(v.title)
grid.text("Growth Curve Analysis", just="top", y=unit(0, "inches")+unit(.3, "lines"))
popViewport() #v.title

v.fit <- viewport(layout.pos.row=2)
pushViewport(v.fit)
fit(g.analysis.well, control, spline.xscale)
popViewport() #fit

v.deriv <- viewport(layout.pos.row=3)
pushViewport(v.deriv)
deriv(g.analysis.well, control, spline.xscale)
popViewport()#deriv

v.secderiv <- viewport(layout.pos.row=4)
pushViewport(v.secderiv)
secderiv(g.analysis.well, control, spline.xscale)
popViewport() #secderiv



popViewport() #v.graphs
popViewport() #v.gAnalysis
popViewport() #background
} # draw to screen
} # End if only one well

# # # # # # # # # # # # # # # # # # # # # For many well view # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

if (well=="all"){
spline.fits.x <- list()
spline.fits.y <- list()
color.info <- NULL
x.max <- NULL
y.max <- NULL

y.min <- NULL


#Read in information about the all the wells.
for (i in 1:length(g.analysis.all)){ #for each well in g.analysis
g.analysis.well <- g.analysis.all[[i]]
spline.fits.x[[i]] <- g.analysis.well$spline$x
spline.fits.y[[i]] <- g.analysis.well$spline$y
color.info <- append(color.info, g.analysis.well$Info$Date)

x.max <- append(x.max, max(g.analysis.well$spline$x))
y.max <- append(y.max, max(g.analysis.well$spline$y))

y.min <- append(y.min, min(g.analysis.well$spline$y))


} # for each well in g.analysis

spline.xscale <- c(0, 1.01*(max(x.max)))

if (graph.log == F){
spline.yscale <- c(1.01*(min(y.min)), 1.01*(max(y.max)))
} else {
spline.yscale <- c(1.01*(log(min(y.min))), 1.01*(log(max(y.max))))
}

color.numbers <- as.numeric(as.factor(color.info))
custom.colors=rep(control$color.scheme, length=max(color.numbers))

grid.newpage()
# Place viewports 
fit <- viewport(just="center") #TODO? Don't need this?
plot <- plotViewport(margins = c(2, 3.5, 1, 1)) #margins for axis, etc, in lines, bottom, left, top, right
pushViewport(fit)
#grid.rect()
pushViewport(plot)

# Plot data points
raw.data <- dataViewport(yscale=spline.yscale, xscale=spline.xscale) 
pushViewport(raw.data)
grid.rect()
grid.xaxis(gp=gpar(fontsize=8))
grid.yaxis(gp=gpar(fontsize=8))
grid.text("Time (in hours)", y=unit(-2, "lines"), gp=gpar(fontsize=10))

if (graph.log == T){
grid.text("ln(Optical Density)", x=unit(-3, "lines"), gp=gpar(fontsize=10), rot=90)
} else{
grid.text("Optical Density", x=unit(-3, "lines"), gp=gpar(fontsize=10), rot=90)
}



#Colors for different dates?

if (graph.log == F) {
for (l in 1:length(spline.fits.x)){ #add each line to the graph
grid.lines(x=spline.fits.x[[l]], y=spline.fits.y[[l]], default.units="native", gp=gpar(col=custom.colors[color.numbers[l]], lwd=1)) #color.numbers[l]
} #for l in 1:length(spline.fits.x)
} else {
for (l in 1:length(spline.fits.x)){ #add each line to the graph
grid.lines(x=spline.fits.x[[l]], y=log(spline.fits.y[[l]]), default.units="native", gp=gpar(col=custom.colors[color.numbers[l]], lwd=1)) #color.numbers[l]
} #for l in 1:length(spline.fits.x)
}

popViewport() #raw.data
popViewport() #plot
popViewport() #fit

} # if well=="all"

} # if svg=F

####################################### For graphing to SVG ###############################################################
if (svg == T) {

# ## # ## # ## # ## # ## # ## # ## # ## # For a single well view # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## #
	#TODO: ADD
# # # # # # # # # # # # # # # # # # # # # For many well view # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
if (well=="all"){ #if want all the wells graphed

#Initialize variables
spline.fits.x <- list()
spline.fits.y <- list()
color.info <- NULL 
x.max <- NULL
y.max <- NULL

roll.info <- list()

#First Round: Extract information from each well needed to graph. TODO could basically get rid of this section? except to find the appropriate x axis....
for (i in 1:length(g.analysis.all)){ #For each well

g.analysis.well <- g.analysis.all[[i]]

if (graph.log == T){
spline.fits.x[[i]] <- log(g.analysis.well$spline$x) #TODO Add based
spline.fits.y[[i]] <- log(g.analysis.well$spline$y)
} else {
spline.fits.x[[i]] <- g.analysis.well$spline$x
spline.fits.y[[i]] <- g.analysis.well$spline$y
}

color.info <- append(color.info, g.analysis.well$Info$Date)
x.max <- append(x.max, max(g.analysis.well$spline$x))
y.max <- append(y.max, max(g.analysis.well$spline$y))

# Make info to have for rollover
## Basics
roll.title <- paste(i, ":", g.analysis.well$Info$Initials, g.analysis.well$Info$Date, " Well ", g.analysis.well$Info$Well.Number, sep= "")
roll.line1 <- paste(g.analysis.well$Info$Background, ":", g.analysis.well$Info$Media, sep="")
roll.line2 <- ""
no.list <- c(control$no.list, "Initials", "Date", "Well.Number", "Background", "Media", "Label", "Well.Name")

attr.names <- names(g.analysis.well$Info)
## All additional information
for (a in 1:length(attr.names)){
if (is.na(g.analysis.well$Info[[attr.names[a]]]) == F) {
if (is.element(attr.names[a], no.list) == F) {
roll.line2 <- paste(roll.line2, ":", g.analysis.well$Info[[attr.names[[a]]]], sep="")
}}
}


## Put it all together
roll.info[[i]] <- list(title=roll.title, line1=roll.line1, line2=roll.line2)

} # for each well

spline.xscale <- c(0, 1.01*(max(x.max)))
spline.yscale <- c(0, 1.01*(max(y.max)))

#Make Legend
legend.width=unit(max(strwidth(as.character(color.info), units="inches")), "inches")
color.numbers=as.numeric(as.factor(color.info))
custom.colors=rep(control$color.scheme, length=max(color.numbers))


#Graphing function
# Open Device

devSVGTips(file=paste(control$root.folder, "/", name,".svg", sep=""), width=11, height=7, toolTipMode=2, title=name)

fit <- viewport(just="center", layout=grid.layout(nrow=1, ncol=3, widths=unit.c(unit(1, "inches"), unit(1, "null"), legend.width+unit(.5, "inches")))) #TODO? Don't need this?
plot <- plotViewport(margins = c(3, 3.5, 1, 1), layout.pos.row=1, layout.pos.col=2) #margins for axis, etc, in lines, bottom, left, top, right
pushViewport(fit)
grid.rect()
pushViewport(plot)

# Plot the bioscreen data points TODO(GG) lighten color
raw.data <- dataViewport(yscale=spline.yscale, xscale=spline.xscale) 
pushViewport(raw.data)
grid.rect()
grid.xaxis(gp=gpar(fontsize=8))
grid.yaxis(gp=gpar(fontsize=8))
grid.text("Time (in hours)", y=unit(-2, "lines"), gp=gpar(fontsize=10))
grid.text("Optical Density", x=unit(-3, "lines"), gp=gpar(fontsize=10), rot=90)

#Second Round: graph each well
for (n in 1:length(spline.fits.x)){
spline.fit.x <- spline.fits.x[[n]]
spline.fit.y <- spline.fits.y[[n]]

roll.info.well <- roll.info[[n]]
#Apply the SVG to each point TODO: do I really have to do this?
for (p in 1:length(spline.fit.x)) {
setSVGShapeToolTip(title=roll.info.well$title, desc1=roll.info.well$line1, desc2=roll.info.well$line2)
grid.points(x=spline.fit.x[p], y=spline.fit.y[p], default.units="native", pch=".", gp=gpar(col=custom.colors[color.numbers[n]]))
} #for each point
} #for l in 1:length(spline.fits.x)

popViewport() #raw.data
popViewport() #plot

##### LEGEND
{
legend <- viewport(just="right", layout.pos.col=3, width=unit(.5, "inches")+legend.width, layout=grid.layout(ncol=2, nrow=max(color.numbers)+1,
	widths=unit.c(unit(.5, "inches"), legend.width), heights=unit.c(rep(unit(1.1, "lines"), times=max(color.numbers)))))
	
pushViewport(legend)

pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))
grid.text("Color", just="bottom")
popViewport()

pushViewport(viewport(layout.pos.row=1, layout.pos.col=2))
grid.text("Date", just="bottom")
popViewport()

for (d in 1:max(color.numbers)){ #Add each row to legend
pushViewport(viewport(layout.pos.row=d+1, layout.pos.col=1))
grid.roundrect(just=c("centre", "centre"), gp=gpar(fill=custom.colors[d]))
popViewport()

pushViewport(viewport(layout.pos.row=d+1, layout.pos.col=2, just="left"))
grid.text("test")#as.character(color.info[d]))
popViewport()

} #for each color

######LEGEND
popViewport() #for legend
}
popViewport() #fit

dev.off()
} # if well=="all"

} # for graphing to SVG

} #plot.gAnalysis
#END FUNCTION
