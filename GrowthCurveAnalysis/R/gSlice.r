#BGN FUNCTION
gSlice <- function(g.analysis, compare=NULL, svg=F, name="lastslices", control=gControl()){
# Draws 2-d slices of various axis of parameter-space. Maps values of parameter1 against parameter 2, coloring for another parameter
#
# Args
#	g.analysis - a list of class "gAnalsysis" for all the wells to be compared
#	compare - a list of two character vectors, compare$info and compare$param which contain the parameters/info to be compared/plotted in parameter spcae
#	control -  a object of type "gControl" which provides additional information
#		control$color.by - color by parameter, which is described in legend
#		control$compare - in case user one is not given
# Returns
#	A lattice-type plot, currently just R based (TODO SVG, tooltips) with every pair of comparisons between parameters, slices of parameter-space.
if (svg == T){
library(RSVGTipsDevice)
}

if (is.null(compare)==T) {
compare=control$compare
}

color.by <- control$color.by
custom.colors<-control$color.scheme
#Get information to graph from g.analysis
param.values <- list()
info.values <- list()
meta.information <- list(Date = NULL, Well= NULL, Initials=NULL, Label=NULL)
color.info.vector <- NULL
for (i in 1:length(g.analysis)) { # for each well
spline.bs.well <- g.analysis[[i]]

color.info.vector <- append(color.info.vector, as.character(spline.bs.well$Info[[color.by]]))

for (n in 1:length(compare$info)){ #for each compare$info
info <- as.character(spline.bs.well$Info[[compare$info[n]]])
info.values[[compare$info[n]]] <- append(info.values[[compare$info[n]]], info)
} # for each compare$info
 
if (svg == T) {
meta.information$Date <- append(meta.information$Date, spline.bs.well$Info$Date)
meta.information$Well.Number <- append(meta.information$Well.Number, spline.bs.well$Info$Well.Number)
meta.information$Initials <- append(meta.information$Initials, spline.bs.well$Info$Initials)
meta.information$Label <- append(meta.information$Label, spline.bs.well$Info$Label)
}
 
for (n in 1:length(compare$param)){ # for each compare$param
param <- spline.bs.well$parameters[[compare$param[n]]]
param.values[[compare$param[n]]] <- append(param.values[[compare$param[n]]], param)
} # for each compare$param
} # for each well
#Now have param.values, info.values vector with character vector for each information

# Fix information for graphing
num.box <- length(compare$param)+length(compare$info)
slice.info <- list()
slice.param <- list()
for  (i in 1:length(compare$info)){
if (is.numeric(type.convert(info.values[[compare$info[i]]])) == T) {
slice.info[[i]] <- list(data=as.numeric(info.values[[compare$info[i]]]), 
	scale=c(min(as.numeric(info.values[[compare$info[i]]]))*.9, max(as.numeric(info.values[[compare$info[i]]]))*1.1),
	name=compare$info[i])
} else {
slice.info[[i]] <- list(data=as.numeric(as.factor(info.values[[compare$info[i]]])),
	scale=c(min(as.numeric(as.factor(info.values[[compare$info[i]]])))*.9, max(as.numeric(as.factor(info.values[[compare$info[i]]])))*1.1),
	name=compare$info[i], levels=levels(as.factor(info.values[[compare$info[i]]])))
}} # for each info

for  (i in 1:length(compare$param)){ 

slice.param[[i]] <- list(data=as.numeric(param.values[[compare$param[i]]]),
	scale = c(min(as.numeric(param.values[[compare$param[i]]]))*.9, max(as.numeric(param.values[[compare$param[i]]]))*1.1),
	name=compare$param[i])
} # for each param

color.info <- list(numbers=as.numeric(as.factor(color.info.vector)), names=levels(as.factor(color.info.vector)))

# Begin graphing
grid.newpage()

if (svg == T) {
devSVGTips(file=paste(control$export.folder, "/", name,".svg", sep=""), width=11, height=7, toolTipMode=1, title=name)
}

slices <- viewport(layout.pos.row=2, 
	# width=unit(10, "inches"), height=unit(8, "inches")
	layout=grid.layout(nrow=length(compare$param)+2, ncol=num.box+2, 
		widths=unit.c(unit(3.5, "lines"), rep(unit(1, "null"), times=num.box), unit(1, "lines")), 
		heights=unit.c(unit(3, "lines"), rep(unit(1, "null"), times=length(compare$param)), unit(1, "lines"))))
pushViewport(slices)

#### Info columns first


for (k in 1:length(slice.info)){ #for each column
current.xaxis <- slice.info[[k]]

pushViewport(viewport(layout.pos.row=1, layout.pos.col=k+1))
grid.text(current.xaxis$name, just=c("center", "center"), x=unit(.5, "npc"), y=unit(.7, "npc"), gp=gpar(fontsize=10))
popViewport()

for (r in 1:length(slice.param)){
current.yaxis <- slice.param[[r]]

pushViewport(viewport(layout.pos.row=r+1, layout.pos.col=1))
grid.text(current.yaxis$name, just=c("center", "center"), x=unit(.6, "lines"), y=unit(.5, "npc"), rot=90, gp=gpar(fontsize=10))
popViewport()

box <- viewport(layout.pos.row=r+1, layout.pos.col=k+1, )
pushViewport(box)
graph <- dataViewport(xscale = current.xaxis$scale, yscale=current.yaxis$scale, xData=current.xaxis$data, yData=current.yaxis$data)
#grid.roundrect()

pushViewport(graph)
grid.rect()
#X Axis TODO: Maybe extend to each box?
if (r == 1){ #only for the first row
if (is.null(current.xaxis$levels) == T) { #if has levels
grid.xaxis(main=F, gp=gpar(fontsize=8)) #puts the x axis at the top
} else {
grid.xaxis(main=F, at=c(1:max(current.xaxis$data)), label=current.xaxis$levels, gp=gpar(fontsize=8))
} #if has levels
} #only for the first row

#Y Axis TODO: Maybe extend to each box?
if (k == 1){ #only for the first row
if (is.null(current.yaxis$levels) == T) { #if has levels
grid.yaxis(gp=gpar(fontsize=8)) #puts the x axis at the top
} else {
grid.xaxis(main=F, at=c(1:max(current.xaxis$data)), label=current.xaxis$levels, gp=gpar(fontsize=8))
} #if has levels
} #only for the first row

for (p in 1:length(current.xaxis$data)){
#if (svg == T) {
#setSVGShapeToolTip(title=meta.information$Label[k], desc1=paste(meta.information$Initials[k], meta.information$Date[k], ": Well", meta.information$Well.Number[k], sep=""))
#}
grid.points(x=current.xaxis$data[p], y=current.yaxis$data[p], pch=19, size=unit(.05, "inches"), gp=gpar(col=custom.colors[color.info$numbers[p]]))
} # for each point

popViewport() #graph
popViewport() #box
} # for each row

} # for each column

#Add text labels for param along x axis
for (k in 1:length(slice.param)){ #for each column
current.xaxis <- slice.param[[k]]

pushViewport(viewport(layout.pos.row=k+1, layout.pos.col=k+length(slice.info)+1))
grid.text(current.xaxis$name, just=c("center", "center"), x=unit(.5, "npc"), y=unit(.5, "npc"), gp=gpar(fontsize=10))
popViewport()
}


#Make row and column indicies for pyramid
rows.t<- NULL

#Add in points
for (i in 1:(length(slice.param)-1)){
rows.t <- append(rows.t, (rep(c(length(slice.param):2), length=i))) 
}
rows <- rev(rows.t)

cols <- rep(c(1:(length(slice.param)-1)), times=c((length(slice.param)-1):1)) #()

for (y in 1:length(rows)){ #for every box in pyramid
current.xaxis <- slice.param[[cols[y]]]
current.yaxis <- slice.param[[rows[y]]]

box <- viewport(layout.pos.row=1+rows[y], layout.pos.col=1+length(slice.info)+cols[y])
pushViewport(box)
graph <- dataViewport(xscale = current.xaxis$scale, yscale=current.yaxis$scale, xData=current.xaxis$data, yData=current.yaxis$data)
#rid.roundrect()

pushViewport(graph)
grid.rect()

#Grid x-axis on first row of boxes
if (rows[y]==cols[y]+1){ #if on first row
grid.xaxis(main=F, gp=gpar(fontsize=8)) #puts the x axis at the top
} #if on first row

for (p in 1:length(current.xaxis$data)){
if (svg == T) {
setSVGShapeToolTip(title=meta.information$Label[p], desc=paste(meta.information$Initials[p], meta.information$Date[p], ": Well", meta.information$Well.Number[p], sep=""), sub.special=F) #paste(meta.information$Initials[p], meta.information$Date[p], ": Well", meta.information$Well.Number[p], sep="")
}
grid.points(x=current.xaxis$data[p], y=current.yaxis$data[p], pch=19, size=unit(.05, "inches"), gp=gpar(col=custom.colors[color.info$numbers[p]]))
} # for each point

popViewport() # graph
popViewport() #box

} #for each box in the pyramid
#Legend:
#Decide how many boxes it can take up. if n is number param variabes, equals floor(n/2) square
legend=T
if (legend == T){
legend.side <- floor(length(slice.param)*.5)
legend <- viewport(layout.pos.col=c((length(slice.param)+length(slice.info)+2-legend.side):(length(slice.param)+length(slice.info)+1)), 
	layout.pos.row=c(2:(1+legend.side)),
	layout=grid.layout(ncol=2, nrow=max(color.info$numbers), widths=unit.c(unit(.5, "inches"), unit(1, "null"))))
pushViewport(legend)
grid.rect(gp=gpar(lwd=2))


for (d in 1:max(color.info$numbers)){
pushViewport(viewport(layout.pos.row=d, layout.pos.col=1))
grid.roundrect(just=c("centre", "centre"), gp=gpar(fill=custom.colors[d]))
popViewport()

pushViewport(viewport(layout.pos.row=d, layout.pos.col=2, just="left"))
grid.text(paste(color.info$names[d]))#as.character(color.info[d]))
popViewport()
}


#TODO: add legendy stuff, isn't gridding the right area - maybe this is better?
popViewport() #legend
} #if legend = T
popViewport() #slices

if (svg == T){
dev.off()
}
#for (q in 1:num.box)
#graph x vs y in each box for param rows
	# color points by color.info
#replace numbers on axis with levels




#list(param = slice.param, info=slice.info)
}
#END FUNCTION