
#' @title read.meta
#' @description Reads in Metadata spreadsheets in the .xlsx format, and removes
#' the uneccessary rows which form part of the headers; designed to be used
#' within the for() loop in the \code{load_metadata} Function. It also fixes
#' dates which are originally loaded incorrectly, presenting them in the ISO
#' format (yyyy-mm-dd).
#' @param filename an object containing the ith directory path to each Metadata
#' file (produced in the \code{load_metadata} Function).
#' @return a dataframe containing a single Metadata spreadsheet.

read.meta <- function(filename)
{
  # Read in Metadata spreadsheet
  metadata <- xlsx::read.xlsx(filename, sheetIndex=1,
                              startRow=2, header=TRUE)

  # Remove column description Row
  correct.metadata <- metadata[-1,]

  # Read date colums as numeic rather than character string
  correct.metadata$project.start.date <- as.numeric(as.character(correct.metadata$project.start.date))
  correct.metadata$project.end.date <- as.numeric(as.character(correct.metadata$project.end.date))
  correct.metadata$relevant.start.date <- as.numeric(as.character(correct.metadata$relevant.start.date))
  correct.metadata$relevant.end.date <- as.numeric(as.character(correct.metadata$relevant.end.date))

  # Fix dates to read in ISO rather than Julian
  correct.metadata$project.start.date <- as.Date(correct.metadata$project.start.date, origin="1899-12-30")
  correct.metadata$project.end.date <- as.Date(correct.metadata$project.end.date, origin="1899-12-30")
  correct.metadata$relevant.start.date <- as.Date(correct.metadata$relevant.start.date, origin="1899-12-30")
  correct.metadata$relevant.end.date <- as.Date(correct.metadata$relevant.end.date, origin="1899-12-30")

  return(correct.metadata)
}

#' @title check.project.folder
#' @description Checks that there is only one project per Metadata spreadsheet.
#' This function does not need to be attached to an object.
#' @param metadata dataframe containing a single Metadata spreadsheet.
#' @return Prints the name of the Project which is currently being assessed to
#' allow the user to keep track of progress, and stops the function if there is
#' more than one project in the sheet.

check.project.folder <- function(metadata)
{
  # Get unique names of project folders
  project.folder <- unique(metadata$project.folder)

  # Print the name of the current Project being assessed
  print(paste0("Current Project being assessed: ", project.folder))

  # Is there only one project folder?
  if(length(project.folder)!=1)
  {
    stop("Number of Project Folders is Incorrect")
  }
}

#' @title check.dataset.folder
#' @description Checks the presence of folders and files wihin the directory by
#' matching them to names in the Metadata spreadsheet. These function does not
#' need to be attached to an object.
#' @param pathway the pathway as specified in the original call to
#' \code{load_metadata} which states the base of the directory.
#' @param metadata dataframe containing a single Metadata spreadsheet.
#' @return Stops the function if there is a folder missing from the directory
#' which is present in the Metadata spreadsheet (or Warns if using dataset.name
#' functions) and states the folder/file name which caused the stop.

check.dataset.folder <- function(pathway, metadata)
{
  # Get full path names to dataset folders
  project.paths <- paste(pathway, metadata$project.folder,
                 unique(metadata$dataset.folder), sep="/")

  # Go through each dataset and check for 'existence', with stop if dataset missing
  for(i in 1:length(project.paths))
  {
    if(file.exists(project.paths[i])==FALSE)
    {
      print(paste0("Project Folder does not exist: ", project.paths[i]))
    }
  }
}

#' @title check.dataset.name.raw
#' @rdname  check.dataset.folder

check.dataset.name.raw <- function(pathway, metadata)
{
  # Get full path names to dataset names
  raw.paths <- paste(pathway, metadata$project.folder, metadata$dataset.folder, "Raw",
                 unique(metadata$dataset.name), sep="/")

  # Go through each dataset and check for 'existence', with stop if dataset missing
  for(i in 1:length(raw.paths))
  {
    if(file.exists(raw.paths[i])==FALSE)
    {
      print(paste0("Dataset does not exist in Raw: ", raw.paths[i]))
      }
  }
}


#' @title check.dataset.name.processed
#' @rdname check.dataset.folder

check.dataset.name.processed <- function(pathway, metadata)
{
  # Get full path names to dataset names
  processed.paths <- paste(pathway, metadata$project.folder, metadata$dataset.folder,
                 "Processed", unique(metadata$dataset.name), sep="/")

  # Go through each dataset and check for 'existence', with warning if dataset missing
  for(i in 1:length(processed.paths))
  {
    if(file.exists(processed.paths[i])==FALSE)
    {
      print(paste0("Dataset does not exist in Processed: ", processed.paths[i]))
    }
  }
}


#' @title check.dataset.location.file
#' @description Checks for the presence of missing location files in the Metadata,
#' highlighting when this is the case; if a location file is present, checks
#' that the file exists in the directory by matching them to names in the
#' Metadata spreadsheet. This function does not need to be attached to an object.
#' @inheritParams check.dataset.folder
#' @return A printed message informing the user if the location entry in the
#' Metadata is missing, or if the Metadata entry is missing from the directory.

check.dataset.location.file <- function(pathway, metadata)
{
  # Get full path names to location file names
  location.paths <- paste(pathway, metadata$project.folder, "Metadata",
                          unique(metadata$dataset.location.file), sep="/")

  location.paths <- unique(location.paths)

  # Are any datasets missing their location sets?
  if(any(grepl('/NA$', location.paths))==TRUE)
  {
    print(paste0("Missing Location file entry: ", location.paths))
  } else {
    # For those with Location sets, does the Metadata match the directory?
    # Go through each dataset and check for 'existence', with message if dataset missing
    for(i in 1:length(location.paths))
    {
      if(file.exists(location.paths[i])==FALSE)
      {
        print(paste0("Location file does not exist in Metadata Folder: ",
                       location.paths[i]))
      }
    }
  }

}


#' @title create.unique.tag
#' @description Creates a unique tag for each dataset by stringing together the
#' Project Name, Dataset Folder and Dataset Name, and removes any file extensions.
#' @inheritParams check.project.folder
#' @return a new column in the Metadata dataframe containing the unique tag for
#' each row

create.unique.tag <- function(metadata)
{
  # Read columns as character strings
  metadata$project.folder <- as.character(metadata$project.folder)
  metadata$dataset.folder <- as.character(metadata$dataset.folder)
  metadata$dataset.name <- as.character(metadata$dataset.name)

  # Create unique string tag - project.folder-dataset.folder-dataset.name
  metadata$unique.tag<- with(metadata,
                             paste(project.folder, dataset.folder, dataset.name, sep="--"))

  # Remove file extensions from unique tag
  metadata$unique.tag <- gsub("\\..*$", "", metadata$unique.tag)
  # Replace space with -

  return(metadata)
}


#' @title create.location.tag
#' @description Creates a location tag for each dataset by stringing together
#' the Project Name and Dataset Location File name, and removes any file
#' extensions.
#' @inheritParams check.project.folder
#' @return a new column in the Metadata dataframe containing the location tag for
#' each row

create.location.tag <- function(metadata)
{
  # Read columns as character strings
  metadata$project.folder <- as.character(metadata$project.folder)
  metadata$dataset.location.file <- as.character(metadata$dataset.location.file)

  # Create unique string tag - project.folder-dataset.folder-dataset.name
  metadata$location.tag<- with(metadata,
                             paste(project.folder, dataset.location.file, sep="--"))

  # Remove file extensions from unique tag
  metadata$location.tag <- gsub("\\..*$", "", metadata$location.tag)
  # Replace space with -

  return(metadata)
}
