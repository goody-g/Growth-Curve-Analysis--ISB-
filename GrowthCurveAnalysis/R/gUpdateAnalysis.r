gUpdateAnalysis <- function(replace.old=TRUE, replace.id=NULL, control=gControl()){
# Updates all the gAnalysis files in the database (for implementing changes to the gAnalysis code).

# Args
#	replace.old - logical indicated whether to save old gAnalysis files in Archive folder or erase.
#	replace.id - name to append to "Analysis" in archive. Preferably Date in YYYYMMDD form
#	control - control object of class "gControl". See documentation for more information.
#		$root.folder - the home directory for the database
#		$experiments - user-updated list in gControl of the unique identifiers of all the experiments (date and initials).
#	

# Returns
#	in each experiment folder an updated R variable, result of running gAnalysis on every well in experiment.
# 	depending on replace.old, former saved gAnalyis variable to Archive folder, subsetted with date.

#Make sure that replace.id isn't NULL if it shouldn't be.
if (replace.old == F){
if (is.null(replace.id) == T){
stop("Variable replace.date needed to move old gAnalysis to Archive folder")
}} #if replace.old=F, is.null(replace.id)=T


experiments=control$experiments

dim(experiments)[1] -> num.experiments

for (i in 1:num.experiments) {#for each experiment
experiment <- experiments[i,]

analysis.file.name <- paste(control$root.folder, "/", as.character(experiment$Date), as.character(experiment$Initials), "/", "Analysis", ".rda", sep="")
archive.file.name <- paste(control$root.folder, "/", as.character(experiment$Date), as.character(experiment$Initials), "/", "Archive", "/", "Analysis", replace.id, ".rda", sep="")

if (replace.old == F){
local({load(analysis.file.name)
save(old.analysis.file, file=archive.file.name)})
}

# Run gAnlaysis
matches <- list(Date=as.character(experiment$Date), Initials=as.character(experiment$Initials))
chosen.wells <- gChoose(matches)
analysis.file <- gAnalysis(chosen.wells)

save(analysis.file, file=analysis.file.name)


} #for each experiment
analysis.file.name
}