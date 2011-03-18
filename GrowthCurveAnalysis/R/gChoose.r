
gChoose <- function(matches, not.matches=NULL, control=gControl()) {
# Finds and makes chosen.wells variable for all experiments with designated parameters

# Args 
#	matches - a list of database column names and chosen results, ex matches$Background=c("NRC-1", "ura3") which would select all with background NRC-1 or ura3
#	not.matches - a similar list containing types to exclude, ex: not.matches$Background="ura3". Vectors are treated as "nor"
#	control - control object of class "gControl". See documentation for more information.	
#		$experiments - a dataframe from gControl of the unique identifiers of all the experiments (date and initials).

# Returns
#	gChoose - a data.frame of Initials, Date, and Well.Number for each chosen well, for gAnalysis

#TODO: Figure out an "and" option for choosing (now have an or, multiple elements in vector. And could be a & and use strsplit?)


#TODO: For some reason >> matches <- list(Media="CM", Overexpression=NA, Background="NRC-1") works but >> matches <- list(Media="CM", Background="NRC-1", Overexpression=NA) gets NA results.
# Also >> gChoose(matches = list(Media="CM", Background="ura3", Knockout=NA, Temp=c(37, NA)), not.matches(Date=c(20080415, 20080422)))


#Initialize Variables

experiments=control$experiments

match.cats <- names(matches)
not.match.cats <- names(not.matches)
chosen.wells <- data.frame()
chosen.wells.info <- list() #To check choosing was working correctly

# Open each database file and read in
for (i in 1:dim(experiments)[1]) { #for each experiment
experiment <- experiments[i, ]
date <- as.character(experiment$Date)
initials <- as.character(experiment$Initials)

database.file.name=paste(control$root.folder, "/", date, initials, "/", 
	"Database", initials, date, ".txt", sep="")

database.file <- read.delim(database.file.name, header=TRUE)
database.file.keep <- database.file

# Choose matching wells
for (j in 1:length(match.cats)){ #For each matching category

match.cat <- as.character(match.cats[j])
test.matches <- matches[[match.cat]]

union.match.index <- NULL # for the or values

# Don't bother if already not keeping any of the database file.
if (is.null(database.file.keep) == F) { 

# Find the matching wells for each or (later find the union of those)
for (k in 1:length(test.matches)){
test.match <- test.matches[k]

if (is.na(test.match)) { #if requiring NA
	if (is.null(database.file[[match.cat]]) == FALSE){ #if match.cat column isn't empty, decide which rows to keep.
		match.index <- as.numeric(which(is.na(database.file.keep[[match.cat]])))
		} else	{
		match.index <- c(1:dim(database.file.keep)[1]) # if the column is null, keep every well (null is like NA)
	}
} else { # END: if requiring NA. BEGIN: If requiring certain parameter
	if (is.null(database.file[[match.cat]]) == FALSE){ #column isn't empty
		match.index <- as.numeric(which(database.file.keep[[match.cat]] == test.match))
		#database.file.keep <- database.file.keep[match.index, ]
	} else {
		database.file.keep <- NULL #if col is empty, don't keep any. 
	}
} #if requiring certain parameter

union.match.index <- union(union.match.index, match.index)

} #for each or in matches (each test.match)
#keep the wells that had any of the or options for that match cat.
database.file.keep <- database.file.keep[union.match.index, ]
} #if database.file.keep isn't null yet
} # for each matching category

# For the kept rows, check that the well isn't excluded by not match
# Provided any exclusions are stipulated
if (is.null(not.matches) == F){

# For each not.match category
for (j in 1:length(not.match.cats)) {

not.match.cat <- as.character(not.match.cats[j])
test.not.matches <- not.matches[[not.match.cat]]

intersect.not.match.index <- NULL

for (k in 1:length(test.not.matches)){
test.not.match <- test.not.matches[k]

if (is.null(database.file.keep) == F) { 

if (is.na(test.not.match)) { #if requiring NA
	if (is.null(database.file[[not.match.cat]]) == FALSE){ #if the column exists
		not.match.index <- as.numeric(which(is.na(database.file.keep[[not.match.cat]]) == F)) #keep those which aren't NA
	} else { #if the column doesn't exist 
		not.match.index <- c(1:dim(database.file.keep)[1])#keep all the wells, since null is equivalent to NA
	}
} else { # Is requiring certain parameter
	if (is.null(database.file[[not.match.cat]]) == FALSE){ #and the column exists
	not.match.index <- as.numeric(which(database.file.keep[[not.match.cat]] != test.not.match)) #keep those which don't match the not.match
	} else { #if the column doesn't exist
	not.match.index <- NULL #none match, so don't keep any
	}	
} #is requiring certain parameters
} #if database file isn't null yet 
intersect.not.match.index <- intersect(intersect.not.match.index, not.match.index)## typo corrected "instersect.not.match.index" by ST
} # for each or in match
database.file.keep <- database.file.keep[intersect.not.match.index, ]
} #for each not.match.cat

} #if not.match.cats isn't null

#Add matching wells to list
if (is.null(database.file.keep)==F){ #if isn't null
if (dim(database.file.keep)[[1]] != 0) { #if isn't of zero length

#Add matching wells to list

chosen.wells <- rbind(chosen.wells, data.frame(Date=as.character(database.file.keep[["Date"]]),
	Initials=as.character(database.file.keep[["Initials"]]),
	Well.Number=as.character(database.file.keep[["Well.Number"]])))

} #if isn't zero length
} #if isn't null

} #for each experiment

#class(chosen.wells) <- "gChoose" - TODO: for some reason this won't work with the data.frame

#list(chosen.wells,stuff)

return(chosen.wells)
}