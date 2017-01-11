#' @title Load in Metadata Spreadsheets
#'
#' @description Loads in every Metadata spreadsheet located on the Boyd Orr
#' server (providing it has "Metadata.xlsx" at the end of the name string) and
#' binds all to a single dataframe. Prior to binding each spreadsheet, it runs a
#' series of tests (see detail section) to highlight missing/mis-named folders
#' and files.
#'
#' @param pathway This is the path for the base location of where all the files
#' are stored.
#'
#' @details This function reads in each spreadsheet individually, allowing a
#' series of tests to be run on each before it is joined to the grouped dataframe.
#' Each test will flag a warning or stop the function if a file is missing or
#' mis-named. These checks include:
#'
#' * Check there is only one project per Metadata sheet.
#'
#' * Check the dataset folder and file names are correct, and present in the
#' directory.
#'
#' * Check the dataset files exist in both the Raw and Processed folders in the
#' directory.
#'
#' * Check the location file is not N/A, and exists in the directory.
#'
#' * Adds two new columns, containing a unique tag
#' (Project.Folder--Dataset.Folder--Dataset.Name) and a location tag
#' (Project.Folder--Dataset.Location.Name).
#'
#' @return A single dataframe containing all of the individual Metadata sheets
#' for each project bound together.
#'
#' @export

load_metadata <- function(pathway)
{
  # Print message to inform user of progress - the next part takes a while!
  print("Searching for Metadata...")

  # Find metadata locations
  file.list <- list.files(path=pathway, pattern=" Metadata.xlsx$",
                          recursive=TRUE, full.names = TRUE)

  # create empty dataframe
  metadata.df <- data.frame()

  # Load in and check metadata spreadsheets
  for(i in 1:length(file.list))
  {
    # Read in metadata
    metadata <- read.meta(filename=file.list[i])

    # Check number of project.folders is correct
    datacleaning:::check.project.folder(metadata)

    # Check dataset.folders are correct
    datacleaning:::check.dataset.folder(pathway, metadata)

    # Check datasets are correct
    datacleaning:::check.dataset.name.raw(pathway, metadata)
    datacleaning:::check.dataset.name.processed(pathway, metadata)

    # Does LOCATION.csv exist? #
    datacleaning:::check.dataset.location.file(pathway, metadata)

    # Does SHAPEFILE exist? #

    # Create unique tag for each dataset
    tagged.metadata <- datacleaning:::create.project.tag(metadata)

    # Create location tag for each dataset
    tagged.metadata <- datacleaning:::create.location.tag(tagged.metadata)

    # Bind data to dataframes
    metadata.df <- rbind(metadata.df, tagged.metadata)
  }
  return(metadata.df)
}
