#' @title Load in Location Spreadsheets
#'
#' @description Loads in every location spreadsheet located on the Boyd Orr
#' server (providing it has "-Location.csv" at the end of the name string) and
#' binds all to a single dataframe.
#'
#' @param pathway This is the path for the base location of where all the files
#' are stored.
#'
#' @details This function reads in each spreadsheet individually and attaches
#' the location tag in the same format as the \code{load_metadata} function:
#' "Project.Name--Location.File.Name".
#'
#' @return A single dataframe containing all of the individual location sheets
#' for each project bound together with a location tag.
#'
#' @export

load_locations <- function(pathway)
{
  # Print message to inform user of progress - the next part takes a while!
  print("Searching for Location data...")

  # Find location files
  file.list <- list.files(path=pathway, pattern="-Locations.csv$",
                          recursive=TRUE, full.names = TRUE)

  # Extract Project name
  file.names <- list.files(path=pathway, pattern="-Locations.csv$",
                           recursive=TRUE, full.names = FALSE)

  # create empty dataframe
  location.df <- data.frame()

  # Load in and check metadata spreadsheets
  for(i in 1:length(file.list))
  {
    # Read in metadata
    location <- datacleaning:::read.location(filename=file.list[i])

    # Build location tag
    location.tagged <- datacleaning:::build.location.tag(filename=file.names[i],
                                                         location)

    # Reorder columns
    location.ordered <- location.tagged[,c("location.tag", "Region",
                                           "District", "Ward", "Village")]

    # Add additional tags
    location.complete <- datacleaning:::build.extra.tags(filename=file.names[i],
                                                            location.ordered)

    # Bind data to dataframes
    location.df <- rbind(location.df, location.complete)
  }

  # Return unique rows only
  location.df <- unique(location.df)

  return(location.df)
}
