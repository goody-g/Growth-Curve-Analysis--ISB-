# BGN FUNCTION
gSummary <- function(g.analysis, control = gControl()){
# Summarizes a g.analysis file, prints to screen

# Args 
#	g.analysis - a object of class "gAnalysis" produced by gAnalyis

# Returns
#	a summary of the gAnalysis file, like factors

if (class(g.analysis) != "gAnalysis"){ #error check
stop ("Must provide an object of class gAnalysis. See function gAnalysis")
} #error check

num.wells <-length(g.analysis)

to.display <- list(NumberOfWells = num.wells)

for (i in 1:num.wells){
g.analysis.well <- g.analysis[[i]]

if (class(g.analysis.well) != "gAnalysisWell"){ #error check
stop("Please provide a object created by the gAnalysis function. Each well must be of class gAnalysisWell")
} #error check

info.names <- names(g.analysis.well$Info)
well.info <- g.analysis.well$Info
num.names <- length(info.names)


for (j in 1:num.names){
attr.name <- info.names[j]
if (is.element(attr.name, control$no.display) == F){
if(is.na(well.info[[attr.name]]) == F){

attr <- as.character(well.info[[attr.name]])

if (is.null(to.display[[attr.name]])==T){
#if the attribute is not part of the display attributes yet
to.display[[attr.name]] <- data.frame(row.names = c(attr), appearances=1)
} else {

to.display.attr <- to.display[[attr.name]]

#Add to appearences or create own row
if (is.element(attr, row.names(to.display.attr)) == T ){
#add to appearences
to.display.attr[attr,"appearances"] <-(to.display.attr[attr,"appearances"]+1)
to.display[[attr.name]] <- to.display.attr
} else{
to.display[[attr.name]] <- rbind(to.display[[attr.name]], data.frame(row.names = c(attr), appearances=1))
} #add
} #if column already part of display
} #only add if attribute is not NA
} #only if not in control$no display
} #for each name

#Add version information
if (is.element("version", control$no.display) == F){
if(is.null(g.analysis.well$version) == F){


vers <- as.character(g.analysis.well$version)


if (is.null(to.display[["Version"]])==T){
#if the attribute is not part of the display attributes yet
to.display[["Version"]] <- data.frame(row.names = c(vers), appearances=1)
} else {

to.display.vers <- to.display[["Version"]]

#Add to appearences or create own row
if (is.element(vers, row.names(to.display.vers)) == T ){
#add to appearences
to.display.vers[vers,"appearances"] <-(to.display.vers[vers,"appearances"]+1)
to.display[["Version"]] <- to.display.vers
} else{
to.display[["Version"]] <- rbind(to.display[["Version"]], data.frame(row.names = c(vers), appearances=1))
} #add
} #if column already part of display
} #only add if attribute is not NA
} #only if not in control$no display

} #for each well

to.display
} #END FUNCTION