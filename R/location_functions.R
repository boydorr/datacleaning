#' @title read.location
#' @description Reads in location spreadsheets in the .csv format; designed to
#' be used within the for() loop in the \code{load_locatino} function. It will
#' then check forhe presence of a column for each Village, Ward, District and
#' Region - if any are missing, it will create a column labelled appropriately
#' which is populated with NAs
#' @param filename an object containing the ith directory path to each location
#' file (produced in the \code{load_locations} function).
#' @return a dataframe object containing Village, Ward, District and Region
#' information for each Project.

read.location <- function(filename)
{
  # Read in Metadata spreadsheet
  location <- read.csv(filename, header=TRUE)

  # Add in column containing NAs if missing from dataframe
  if("Village" %in% colnames(location)==FALSE)
  {
    location$Village <- NA
  } else if("Ward" %in% colnames(location)==FALSE)
  {
    location$Ward <- NA
  } else if("District" %in% colnames(location)==FALSE)
  {
    location$District <- NA
  } else if("Region" %in% colnames(location)==FALSE)
  {
    location$Region <- NA
  }

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
  # Create Village tag
  location$village.tag<- with(location,
                              paste("V", Region, District, Ward, Village, sep="--"))

  # Create Ward tag
  location$ward.tag<- with(location,
                              paste("W", Region, District, Ward, sep="--"))

  # Create District tag
  location$district.tag<- with(location,
                              paste("D", Region, District, sep="--"))

  # Create Region tag
  location$region.tag<- with(location,
                              paste("R", Region, sep="--"))

  return(location)
}
