#' @title Load in Project Descriptions
#'
#' @description Loads in every project description .txt file located on the Boyd
#' Orr server (providing it has "- Overview.txt" at the end of the name string)
#' and binds all to a single dataframe. The function will produce a "bing" noise
#' when complete.
#'
#' @param pathway This is the path for the base location of where all the files
#' are stored.
#'
#' @return A single dataframe containing all of the individual project description
#' files for each project bound together alongside the Project Name.
#'
#' @export

load_project_description <- function(pathway)
{
  # Print message to inform user of progress
  print("Searching for Project Descriptions...")

  # Find location files
  file.list <- list.files(path=pathway, pattern="- Overview.txt$",
                          recursive=TRUE, full.names = TRUE)

  # Extract Project name
  file.names <- list.files(path=pathway, pattern="- Overview.txt$",
                           recursive=TRUE, full.names = FALSE)

  # create empty dataframe
  project.df <- data.frame()

  # Load in and check metadata spreadsheets
  for(i in 1:length(file.list))
  {
    # Read in metadata
    project <- datacleaning:::read.project.description(filename=file.list[i])

    # Build Project Name tag
    project <- datacleaning:::add.projectname(filename=file.names[i],
                                                           project)

    names(project)[1] <- "description"

    # Bind data to dataframes
    project.df <- rbind(project.df, project)
  }

  beepr::beep(sound="ping")
  return(project.df)
}
