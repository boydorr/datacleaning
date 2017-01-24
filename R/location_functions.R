#' @title read.location
#' @description Reads in location spreadsheets in the .csv format; designed to
#' be used within the for() loop in the \code{load_locatino} function.
#' @param filename an object containing the ith directory path to each location
#' file (produced in the \code{load_locations} function).
#' @return a dataframe object containing Location, Location Type and Location ID
#' information for each Project.

read.location <- function(filename)
{
  # Read in Metadata spreadsheet
  location <- read.csv(filename, header=TRUE)

  return(location)
}

#' @title build.location.tag
#' @description Creates a location tag (Project.Name--location.file.name) and
#' attaches this to the dataframe as a new column.
#' @param filename an object containing the ith directory path to each location
#' file (produced in the \code{load_locations} function).
#' @param location a dataframe object created in the \code{read_location} function.
#' @return a dataframe with the location tag attached.

build.location.tag <- function(filename, location)
{
  # Extract Project name
  string <- gsub("^(.*)/.*", "\\1", filename)
  project.name <- sub("/.*", "", string)

  # Print message to tell user where the build is
  print(paste0("Current Project being loaded: ", project.name))

  # Extract location file name
  location.name <- gsub(".*/(.*)$", "\\1", filename)

  # Create tag
  location$location.tag<- with(location,
                               paste(project.name, location.name, sep="--"))

  # Remove file extensions from unique tag
  location$location.tag <- gsub("\\..*$", "", location$location.tag)

  return(location)
}

#' @title build.extra.tags
#' @description Creates a tag for each location level (i.e. Village, ward,
#' district and region) in the format of: V--Region--Distict--Ward--Village;
#' W--Region--Distict--Ward etc... These are then attached to the dataframe as
#' new columns.
#' @param filename an object containing the ith directory path to each location
#' file (produced in the \code{load_locations} function).
#' @param location a dataframe object created in the \code{read_location} function.
#' @return a dataframe with the extra location tags attached.

build.extra.tags <- function(filename, location)
{
  # Create Region tag
  location$Region_ID<- with(location,
                            paste("Region", Region, sep="-"))

  # Create District tag
  location$District_ID<- with(location,
                              paste("District", Region, District, sep="-"))

  # Create Ward tag
  location$Ward_ID<- with(location,
                          paste("Ward", Region, District, Ward, sep="-"))

  # Create Village tag
  location$Village_ID<- with(location,
                              paste("Village", Region, District, Ward, Village, sep="-"))

  return(location)
}
