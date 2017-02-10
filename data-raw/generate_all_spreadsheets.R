#--------------------#
# BUILD SPREADSHEETS #
#--------------------#

# Connect to the server and use the code for the appropriate computer connnection
# below before running the code.

# windows server pathway:
server="Z:/"

# mac server pathway:
server=

#----------#
# Metadata #
#----------#

# Produce the metadata spreadsheet, which contains all metadata .csv files
# bound together with checks and tags added.
metadata.df <- load_metadata(pathway=server)

# Produce the correct file path to save on the server
save.path = paste(server, "Metadata/AllMetadataDates.csv", sep="")

# Save file on server
write.csv(metadata.df, file = save.path, row.names=FALSE)

#----------#
# Keywords #
#----------#

# Produce the keyword spreadsheet, which contains all keywords from the metadata
# seperated into individual strings with tags added.
keyword.df <- create_keyword_df(dataframe=metadata.df)

# Produce the correct file path to save on the server
save.path = paste(server, "Metadata/AllKeywords.csv", sep="")

# Save file on server
write.csv(keyword.df, file = save.path, row.names=FALSE)

#-----------#
# Locations #
#-----------#

# Produce the location spreadsheet, which contains all location .csv files
# bound together with tags added.
location.df <- load_locations(pathway="Z:/")

# Produce the correct file path to save on the server
save.path = paste(server, "Metadata/AllLocations.csv", sep="")

# Save file on server
write.csv(location.df, file = save.path, row.names=FALSE)

#----------------------#
# Project Descriptions #
#----------------------#

# Produce the project description spreadsheet, which contains all description
# .txt files bound together with project name added.
project_description.df <- load_project_description(pathway="Z:/")

# Produce the correct file path to save on the server
save.path = paste(server, "Metadata/AllDescriptions.csv", sep="")

# Save file on server
write.csv(project_description.df, file = save.path, row.names=FALSE)

#-----------#
# Last Step #
#-----------#

# Remember to copy and paste any edited .csv files into the datacleaning
# and shiny packages, followed by running the script in "generate-data.R" to
# make the spreadsheets readily useable by others.

