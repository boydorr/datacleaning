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
#' @return A single dataframe containing all of the individual Metadata sheets
#' for each project bound together.
#'
#' @export

load_metadata <- function(pathway)
{
  # Find metadata locations
  file.list <- list.files(path=pathway, pattern=" Metadata.xlsx$",
                          recursive=TRUE, full.names = TRUE)

  # create empty dataframe
  metadata.df <- data.frame()

  # Load in and check metadata spreadsheets
  for(i in 1:length(file.list))
  {
    # Read in metadata
    # metadata <- read.meta(file.list)
    # This creates an error (Can't find object [i]) so run manually below.

    # Read in Metadata spreadsheet
    metadata <- xlsx::read.xlsx(file.list[i], sheetIndex=1,
                                startRow=2, header=TRUE)

    # Remove column description Row
    correct.metadata <- metadata[-1,]

    # Fix issue of dates being read in wrong #

    # Check number of project.folders is correct
    datacleaning:::check.project.folder(correct.metadata)

    # Check dataset.folders are correct
    datacleaning:::check.dataset.folder(pathway, correct.metadata)

    # Check datasets are correct
    datacleaning:::check.dataset.name.raw(pathway, correct.metadata)
    datacleaning:::check.dataset.name.processed(pathway, correct.metadata)

    # Does LOCATION.csv exist? #
    datacleaning:::check.dataset.location.file(pathway, correct.metadata)

    # Does SHAPEFILE exist? #

    # Create unique tag for each dataset
    tagged.metadata <- datacleaning:::create.unique.tag(correct.metadata)

    # Bind data to dataframes
    metadata.df <- rbind(metadata.df, tagged.metadata)
  }
  return(metadata.df)
}
