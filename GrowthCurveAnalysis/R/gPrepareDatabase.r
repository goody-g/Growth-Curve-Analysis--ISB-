#BGN FUNCTION
gPrepareDatabase <- function(initials, date, control=gControl()) {
# Prepares a databse with all column names from gControl and well numbers and labels from the associated layout file (provided it has been properly formatted. See documentation.)

# Args 
#	initials - intials of the experimenter.
		#	EX: FYL, LP, GG, ST
#	date - date of the experiment as listed in the database in YYYYMMDD. This, with the experimenter's initials, uniquely specify an experiment.
# 	control - control object of class "gControl". See documentation for more information.
#		$root.folder - folder on the local computer containing each database folder, a character string formatted for R. See documentation.
		
# Other Args
#	layout file - reads in a corresponding layout file - must be prepared in accordance to guidelines and be in a folder called Layouts.
#	An appropriately named folder within the Database directory containing the results file. ##DATE##II

# Returns 
#	saves a database file to the appropriate folder to be edited with excel and used for further analysis.


#Prepare file names
file.location <- control$root.folder
database.file.name <- paste(file.location, "/", date, initials, "/", "Database", initials, date ,".txt", sep="")
labels.file.name <- paste(file.location, "/", date, initials, "/", "Layout", initials, date, ".txt", sep="") #This could be changed if the layout files were in another location.
results.file.name <- paste(file.location, "/", date, initials, "/Results", initials, date, ".csv", sep="")

#Open and read layout file.

# # Open layout file as a matrix
labels.file <- read.delim(labels.file.name, header=FALSE)
labels.matrix.file <- as.matrix(labels.file)
dim.labels <- dim(labels.matrix.file)
row <- as.numeric(dim.labels[1]) 
col <- as.numeric(dim.labels[2])	
num.names=(row - 1)*(col - 1)

# # Create an index system for targetting Well.Name within label file
index.labels.r=rep(c(2:row), times=(col - 1)) 
index.labels.c=rep(c(2:col), each=(row - 1))

well.numbers=NULL
well.names=NULL

# # Add each Well.Name and Well.Number to well.names and well.number
for (i in 1:num.names) {

hund=as.double(labels.matrix.file[1,index.labels.c[[i]]])
ones=as.double(labels.matrix.file[index.labels.r[[i]],1])

well.numbers[[i]] <- (hund + ones)

well.names[[i]]=labels.matrix.file[index.labels.r[i],index.labels.c[i]]

}
# Prepare the tab-delim database file. 
# USER: Comment out columns which are not needed or add columns which are (also add them to gControl$database.columns)

database.list=list(
	File.Name =paste("Results", initials, date, ".csv", sep=""),  #results.file.name,
	Initials = initials,
	Date = date,
	Well.Name = well.names, 
	Well.Number = well.numbers,
	Biological.Replicate = NA,
	Technical.Replicate = NA,
	Background = NA,
	Knockout = NA,
	Overexpression = NA,
	Temp = NA,
	pH = NA,
	Media = NA, 
	NaCl.Concentration = NA,
	MgSO4.Concentration = NA, 
	KCl.Concentration = NA, 
	Arg.Concentration = NA,
	Pyruvate.Concentration = NA,
	Asp.Concentration = NA, 
	Glycerol.Concentration = NA
	#Culture.OD = NA,
	#Cu2p.Concentration=NA
	#Mn.Concentration = NA,
	#Fe.Concentration = NA,
	#Ni.Concentration = NA,
	#Co.Concentration = NA,
	#Zn.Concentration=NA
	)

#Not allow to write over existing database files (save from losing A LOT of work).
	
if (file.exists(database.file.name) ==F) {
write.table(database.list, file=database.file.name, append=F, sep ="\t", row.names=F)
 }
 else {
 stop("The database file already exists. Please delete manually if wish to override.")
 }
 
}
#END FUNCTION