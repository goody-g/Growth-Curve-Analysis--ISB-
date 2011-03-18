#BGN FUNCTION
gInfo <- function(date, initials, well.number, control=gControl()) {
# From basic identifying information (chosen.well) loads database info.
#
# Args
#	chosen.well - a list including "Initials", "Date" and "Well.Number" for a single well
#	control - control object of class "gControl". See documentation for more information.
#		$database.columns - a list of all possible database columns, so all gInfo contains same information.
#Return
#	gInfo - a list of class gInfo containing database information

#TODO : error checks

#Read in appropriate Database entry
database.file.name <- paste(control$root.folder, "/", date, initials, "/Database", initials, date, ".txt", sep="")
database.file <- read.delim(database.file.name, header=TRUE)

#Choose the appropriate row
well.index <- as.numeric(which(database.file$Well.Number == well.number))
database.file.row <- database.file[well.index, ]

#Make more usable - the list of all information fields
dcols <- control$database.columns
num.dcol = length(dcols)

#Initialize variables
g.info <- list()

#TODO is using the control with a list of all the databse columns a smart way to do it, or perhaps just names(database.file)
#FOR all the poosible column names (in a list), value = database result if it exists, else, NA.
for (i in 1:num.dcol) {
dcol <- dcols[i]

if (is.null(database.file.row[[dcol]]) == TRUE){ #if database is null, make g.info version NA
is.na(g.info[[dcol]]) <- TRUE
} else{
if ((is.na(database.file.row[[dcol]]) == TRUE) | (nchar(as.character(database.file.row[[dcol]]), type= "chars", allowNA=TRUE) == 0)) { #if database is NA or empty make g.info version NA
is.na(g.info[[dcol]]) <- TRUE
}

else {
g.info[[dcol]] <- database.file.row[[dcol]]
}
}
}
class(g.info) <- "gInfo"

g.info
}
#END FUNCTION