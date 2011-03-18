
# USER: For initial set up, define local location of growth curve analysis functions
gcaf.folder <- "/Users/karlyn/Documents/GrowthCurveAnalysis/R"

# Sourcing each function. When a new function is written, add it to list. When functions are updated, just source this function into R for all the functions to be updated.
# TODO: make these functions a package so to avoid all this.
if ( FALSE ) { ## added by DJR to see if it is packageable...
source(paste(gcaf.folder, "/", "gControl.r", sep="")) #for gControl 
source(paste(gcaf.folder, "/", "gChoose.r", sep="")) #for gChoose
source(paste(gcaf.folder, "/", "gAnalysis.r", sep="")) #for gAnalysis
source(paste(gcaf.folder, "/", "gSplineFit.r", sep="")) #for gSplineFit
source(paste(gcaf.folder, "/", "low.integrate.r", sep="")) #for low.integrate
source(paste(gcaf.folder, "/", "gInfo.r", sep="")) #for gInfo
source(paste(gcaf.folder, "/", "plot.gAnalysis.r", sep="")) #for plot
source(paste(gcaf.folder, "/", "gSummary.r", sep="")) #for gSummary
source(paste(gcaf.folder, "/", "gSlice.r", sep="")) #for gSlice
source(paste(gcaf.folder, "/", "gExport.r", sep="")) #for gExport
source(paste(gcaf.folder, "/", "gPrepareDatabase.r", sep="")) #for gPrepareDatabase
source(paste(gcaf.folder, "/", "gUpdateAnalysis.r", sep="")) #for gUpdateAnalysisDatabase

rm(gcaf.folder)
}