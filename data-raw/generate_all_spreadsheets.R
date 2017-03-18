#--------------------#
# BUILD SPREADSHEETS #
#--------------------#

library(datacleaning)

# Connect to the server and use the code for the appropriate computer connnection
# below before running the code.

# windows server pathway:
server="Z:/"

# mac server pathway:
# server=

#----------#
# Metadata #
#----------#

# Produce the metadata spreadsheet, which contains all metadata .csv files
# bound together with checks and tags added.
# This process takes a long time as it generates a full file list for all files
# with the appropriate pattern ("Metadata.xls*"). There is a vector provided at
# the bottom of this page which is accurate to the server from the date given.
# This vector can be fed into the latter code below if there has been no changes
# to the metadata. If changes have been made/you are unsure, use the former code.
metadata_results <- load_metadata(pathway=server)
metadata_results <- load_metadata(pathway=server, file.list=file.list)

# Seperate out the contents of the list
metadata.df <- metadata_results[[1]]
comments <- metadata_results[[2]]

# Produce the correct file path to save on the server
save.path = paste(server, "Metadata/AllMetadata.csv", sep="")

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
location.df <- load_locations(pathway=server)

# Produce the correct file path to save on the server
save.path = paste(server, "Metadata/AllLocations.csv", sep="")

# Save file on server
write.csv(location.df, file = save.path, row.names=FALSE)

#----------------------#
# Project Descriptions #
#----------------------#

# Produce the project description spreadsheet, which contains all description
# .txt files bound together with project name added.
project_description.df <- load_project_description(pathway=server)

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

#---------------------------------------------#
# File list for metadata locations (18/03/17) #
#---------------------------------------------#

file.list <- c("Z://AfricaGIS/Metadata/AfricaGIS Metadata.xlsx",
               "Z://AMR/Metadata/AMR Metadata.xlsx",
               "Z://AnimalSamples/Metadata/AnimalSamples Metadata.xlsx",
               "Z://AnthraxSurveyProject/Metadata/AnthraxSurveyProject Metadata.xlsx",
               "Z://BacterialZoonoses/Metadata/BacterialZoonoses Metadata.xlsx",
               "Z://CameraTrap/Metadata/CameraTrap Metadata.xlsx",
               "Z://CAREProjectRabies/Metadata/CAREProjectRabies Metadata.xlsx",
               "Z://CollarDatabase/Metadata/CollarDatabase Metadata.xlsx",
               "Z://DFIDBrucella2001/Metadata/DFIDBrucella Metadata.xlsx",
               "Z://DFIDTuberculosis/Metadata/DFIDTuberculosis Metadata.xlsx",
               "Z://FecalSamples/Metadata/FecalSamples Metadata.xlsx",
               "Z://FMDV/Metadata/FootandMouthDisease Metadata.xlsx",
               "Z://FryxellTransects/Metadata/FryxellTransects Metadata.xlsx",
               "Z://GroundTransects/Metadata/GroundTransects Metadata.xlsx",
               "Z://LeptospirosisInNorthernTanzania/Metadata/Leptospirosis Metadata.xlsx",
               "Z://LionKills/Metadata/LionKills Metadata.xlsx",
               "Z://LivestockMalaria/Metadata/LivestockMalaria Metadata.xlsx",
               "Z://MalignantCatarralFever/Metadata/MalignantCatarrhalFever Metadata.xlsx",
               "Z://Rabies/Metadata/Rabies Metadata.xlsx",
               "Z://SerengetiCDV/Metadata/SerengetiCDV Metadata.xlsx",
               "Z://SerengetiGIS/Metadata/SerengetiGIS Metadata.xlsx",
               "Z://TANAPACaseBook/Metadata/TANAPA Metadata.xlsx",
               "Z://VegetationSampling/Metadata/VegetationSampling Metadata.xlsx",
               "Z://WaterSamples/Metadata/WaterSamples Metadata.xlsx")
