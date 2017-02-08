#' @title read.project.description

read.project.description <- function(filename)
{
  # Read in Metadata spreadsheet
  project <- readr::read_file(filename)

  project <- as.data.frame(project)

  return(project)
}


#' @title build.projectname.tag

add.projectname <- function(filename, project)
{
  # Extract Project name
  string <- gsub("^(.*)/.*", "\\1", filename)
  project.name <- sub("/.*", "", string)

  # Print message to tell user where the build is
  print(paste0("Current Project being loaded: ", project.name))

  # Create tag
  project$project.name <- with(project, paste(project.name))

  return(project)
}
