# To start: source("~/Documents/GrowthCurveAnalysis/R/gFunctions.r")


#BGN FUNCTION
gControl <- function(
	root.folder= "/Volumes/kbeer/BaligaLab/GrowthCurves/Database", # USER: Location of the Database folder. UPDATE this!
	export.folder = "/Volumes/kbeer/BaligaLab/GrowthCurves/Export", # Location for default exporting of gAnalysis
	spar = .5,  # spar parameter for smooth.spline in gSplineFit
	cv.spline = TRUE,  # Cross-validation technique for smooth.spline when not for bootstrapping
	num.bs.samp =0,  # Number bootstrap samples
	bs.spar = spar, # smoothing parameter for smooth.spline in gSplineFit when for bootstrapping
	cv.bs = FALSE, # Cross-validation technique for smooth.spline when for bootstrapping
	color.by= "Background", #"Knockout",  #"Date", # What to color the points by in gSlice. #TODO won't work with knockout for some reason, use in plot.gAnalysis
	no.display = c( #in summary
		"File.Name",
		"Well.Number",
		"Well.Name"
		),
	no.list=c( #on plots
		"File.Name"
		#"Well.Name",
		#"A",
		#"time.A",
		#"initial.od"
		),
	
	color.scheme=c( #TODO: Make this prettier. Used in gSlice, plot.gAnalysis etc.
		"red",
		"blue",
		"chartreuse",
		"black",
		"darkgoldenrod",
		"darkmagenta",
		"yellow",
		"purple",
		"pink",
		"chartreuse4", 
		"burlywood", 
		"blueviolet",
		"white",
		"white",
		"white",
		"white",
		"white",
		"white"
	),
	compare= list( #for gSlice
		param = c(
		"A",
		"mu",
		"lambda",
		"time.mu",
		"y.mu",
		"max.secderiv.time"
		),
		info = c(
		"Media",
		"Background",
		"Well.Number"
		)
	),
	
	graph.output = "", #TODO
	log = F, #TODO
	disp.digits = 7, #TODO Use this variable in functions
	output.graph.to = "R", #remove this
	split.points=4, #was for gSplitPoints
	database.columns=c(  #TODO: Where is this?
		"File.Name", #Get rid of
		"Initials",
		"Date",
		"Well.Number",
		"Well.Name", 
		"Biological.Replicate",
		"Technical.Replicate",
		"Media", 
		"NaCl.Concentration",
		"MgSO4.Concentration",
		"KCl.Concentration",
		"Arg.Concentration",
		"Pyruvate.Concentration",
		"Glycerol.Concentration",
		"Asp.Concentration",
		"Background",
		"Knockout", 
		"Overexpression",
		"Temp",
		"pH"		
			),
	parameter.columns=c( #TODO This doesn't seem to be in use for anything.
		A,
		mu,
		lambda,
		time.mu,
		time.A,
		y.mu,
		time.A, 
		y.mu, 
		intial.od,
		integral,
		time.max,
		trajectory,
		max.secderiv.index,
		max.secderiv.time,
		max.secderiv
	)
	) {
# Creates the gAnalysis control variable, g.control. Allows user inputs and default values
#
# Args:
#	root.folder - the folder containing the database informatoin
#	spar - the smoothing parameter to use with the smooth.spline function for the spline fit. Must be between 0 and 1.
#	cv.spline - a logical value stating whether to perform generalized cross validation or not-generalized on the spline fit.
#	num.bs.samp - the number of bootstrapping samples to take. This function won't allow it over 1000 but that can be tweaked.
#	bs.spar - the smoothing parameter for the smooth.spline function for the bootstrapping. Defaults to the same as spar
#	cv.bs - logical object, whether generalized (F) or ordinary cross-validation (F) on spline fits for bs. Generalized is faster.
#	etc.. see documentation
#
# Return
#	A list of class gControl listing control options, user defined of default.

# TODO: Add more error checks
if ((is.character(root.folder)==FALSE) | (file.exists(root.folder) == FALSE))
	stop("Control root.folder does not exists or is not a character vector of one element. Fix the default?")
	
if ((is.numeric(spar)==FALSE) | (spar > 1) | (spar < 0) | length(spar) != 1)
	stop("Control spar value must be a number between 0 and 1.")
	
if ((is.logical(cv.spline) == FALSE))
	stop("Control cv.spline must be a logical value.")

if ((is.numeric(num.bs.samp) == FALSE) | (num.bs.samp < 0) | (num.bs.samp >1000) | (length(num.bs.samp) != 1))
	stop("Control num.bs.samp must be a number bigger than zero, must also be less than 1000 but can change that.")

if ((is.numeric(bs.spar)==FALSE) | (bs.spar > 1) | (bs.spar < 0) | (length(bs.spar) != 1))
	stop("Control bs.spar value must be a number between 0 and 1.")

if ((is.logical(cv.bs) == FALSE))
	stop("Control cv.bs must be a logical value.")	

# Make a list of the experiments in the database. To remove an experiment from experiments without deleting it, add an "x" to the beginning of the folder name.
experiments <- NULL
experiments <- data.frame(Date=NULL, Initials=NULL)
j=0

#Find the names of all folders in the database file

poss.exp <- list.files(root.folder)

for (i in 1:length(poss.exp)){
is.exp <- poss.exp[i]

if (nchar(is.exp) >= 9){
date <- substr(is.exp, start=1, stop=9)

if (is.numeric(type.convert(date)) == T){
initials <- substr(is.exp, start=10, stop=nchar(is.exp))

if (file.exists(paste(root.folder, "/", date, initials, "/Results", initials, date, ".csv", sep=""))){
j=j+1
experiments <- rbind(experiments, data.frame(Date=date, Initials=initials))
} #if contains results file

} #if is numeric
} #if is long enoughposs

} #for each file/folder

g.control <- list(root.folder = root.folder, spar = spar, cv.spline = cv.spline, num.bs.samp = num.bs.samp, 
	bs.spar = bs.spar, cv.bs=cv.bs, database.columns=database.columns, graph.output=graph.output, no.list=no.list,
	disp.digits=disp.digits, output.graph.to=output.graph.to, experiments=experiments, color.by=color.by, log=log, 
	color.scheme=color.scheme, compare=compare, no.display = no.display, export.folder = export.folder)
class(g.control) <-"gControl"

g.control
}
#END FUNCTION