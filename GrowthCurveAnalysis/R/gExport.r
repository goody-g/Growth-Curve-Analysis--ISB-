#BGN FUNCTION
gExport <- function(g.analysis, name="LastExport", control = gControl()){
# saves the parameters and meta information in a tab-delimited spreadsheet for use outside of gFunctions

# Args
#	g.analysis - an object of class gAnalysis produced by gAnalysis or a list of such objects
#   name - a name for the exported file
# 	control - 
#		$export.folder - folder for exported files to be saved to.

# Returns
#	saves a tab-delimited spreadsheet to Exports folder


if (class(g.analysis) == "gAnalysis"){
	all.wells <- g.analysis
} else {
	if (class(g.analysis) == "list"){
	all.wells <- list()
		for (i in 1:length(g.analysis)){
			if (class(g.analysis[[i]]) != "gAnalysis") {
			stop("Must provide a list of gAnalysis objects")
			} else {
			all.wells <- c(all.wells, g.analysis[[i]])
			} # if is a list of gAnalysis files			
		} #for each element of the list file
	} else {# if is list
		stop ("Need to provide either a gAnalysis object or a list of gAnalysis objects")
	} #if isn't list
}# if is list or ganalysis file

already.added <- NULL
to.export.info <- data.frame()
to.export.parameters <- data.frame()

#Add information from each well to to.export
for (i in 1:length(all.wells)){
g.analysis.well <- all.wells[[i]]

if (class(g.analysis.well) != "gAnalysisWell"){
stop ("The wells in the provided g.Analysis object are not of class gAnalysisWell")}

well.id <- paste(g.analysis.well$Info$Date, g.analysis.well$Info$Initials, g.analysis.well$Info$Well.Number)


if (is.element(well.id, already.added) == F){
already.added <- c(already.added, well.id)

well.info <- g.analysis.well$Info
well.info.names <- names(well.info)


for (y in 1:length(well.info.names)){
to.export.info[i,well.info.names[y]] <- as.character(well.info[[well.info.names[y]]])
}#for each well.info.names


well.parameters <- g.analysis.well$parameters
well.parameters.names <- names(well.parameters)

for (j in 1:length(well.parameters.names)){
to.export.parameters[i,well.parameters.names[j]] <- as.character(well.parameters[[well.parameters.names[j]]])
}#for each well.info.names


} #if isn't already added
} #add each well to to.export
to.export <- cbind(to.export.info, to.export.parameters)

export.file.name <- paste(control$export.folder, "/", name, ".txt", sep="")

if(file.exists(control$export.folder) == F) {
dir.create(control$export.folder)
}

write.table(to.export, file=export.file.name, append=F, sep="\t", row.names=F)

to.export
} #END FUNCTION