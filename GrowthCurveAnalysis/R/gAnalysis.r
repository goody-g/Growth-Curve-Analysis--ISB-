#BGN FUNCTION
gAnalysis <-function(chosen.wells, control = gControl()) {
# Calls functions gSplineFit, gInfo to analyze wells, produce object of class gAnalysis which can then be graphed/analyzed.
	#TODO: Add gBootstrap
	
# Args	
#	chosen.wells - A dataframe with columns "Date", "Initials" and "Well.Number" to identify wells to be analyzed by gAnalysis.
# 
#	control - control object of class "gControl". See documentation for more information. Contains options to customize functions.
#		$root.folder - the folder on the local computer containing each database folder, a character string formatted for R. See documentation.

# Returns
#	g.analysis - contains analysis for each chosen well, including information about the well itself. class: "gAnalysis". Can be passed to gSlice, plot.gAnalysis or gSummary.	

#Prepare/Iniitialize variables
num.samp <- dim(chosen.wells)[1]
g.analysis.all = list()

# For each sample in chosen.well, open and perform analysis
for (i in 1:num.samp) { #for each sample in chosen.wells

#i <- floor(runif(1, 1, 121))
# Access results file for sample i

chosen.well <- chosen.wells[i, ]
date <- as.character(chosen.well$Date)
well.number <- as.character(chosen.well$Well.Number)
initials <- as.character(chosen.well$Initials)

results.file.name <- paste(control$root.folder, "/", date, initials, "/", "Results", initials, date, ".csv", sep="")
results.file <- read.csv(results.file.name, header=T)

# Find column of growth curve data from results file using information in  from chosen.file$Well.Number and the name in the eader
numbers.head <-paste("Well.",well.number, sep="")
g.data <-as.vector(as.numeric(as.matrix(results.file[[numbers.head]])))

# Concert the time to decimal time:

hms.time <- c(as.character(results.file$Time))

decimal.time=unlist(
		lapply(
			lapply(strsplit(hms.time, ":"), as.numeric),
			function(hms.time){
				sum(hms.time*c(1, 1/60, 1/3600))
			}
		)
	)


# Perfrom gSplineFit - fit a spline and extract parameters
g.analysis.well <- gSplineFit(time=decimal.time, data=g.data, control=control, for.bs=F)


# Perform gBootstrap - performs gSplineFit with subsets of original data. Only if any bootstrap samples wanted.
if(control$num.bs.samp == 0){
} else {
g.analysis.well[["Bootstrap"]] <- gBootstrap(time=decimal.time, data=g.data,  control=control)
}
#TODO (GG): Bootstrap and below error

# Add error
#param.names <- names(spline.bs.well$parameters.bs)
#parameters.err <- list()
#for (s in 1:length(param.names)){
#sd(spline.bs.well$parameters.bs[[param.names[s]]])-> parameters.err[[param.names[s]]]
#}

#spline.bs.well[["parameters.err"]]<-parameters.err

# Perform gInfo - Add well meta-information from database.
g.analysis.well[["Info"]] <- gInfo(date, initials, well.number, control=control)


class(g.analysis.well) <- "gAnalysisWell"

g.analysis.all[[i]] <- g.analysis.well
}# for each sample in chosen.wells

class(g.analysis.all) <- "gAnalysis"

#list(g.analysis.all, time.el)
g.analysis.all
}
#END FUNCTION