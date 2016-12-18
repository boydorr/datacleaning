#-------------------------------------------------------------------------------
#'# Demo1 #
#-------------------------------------------------------------------------------

#' This demo will show how to use the range of functions within the datacleaning
#' package in order to process location names at the village, ward, district and
#' region level.
#' These function are heirchial, and so fix errors first at Region level, then
#' District, Ward and finally Village.

#-------------------------------------------------------------------------------
#'## Setup ##
#-------------------------------------------------------------------------------

library(datacleaning)

# Load in example dataset
data("example_locations")

# Load in example typos list
data("example_typos_reg")

#' The shapefiles are contained within the "datacleaning" Package as
#' "village_2012" and "ward_2012" - these are already within the Geographic
#' projection WGS84 which is associated with this package.

#-------------------------------------------------------------------------------
#'## 1) Fix and typos at Region level ##
#-------------------------------------------------------------------------------

#' Fix any typos in the regions based on the already available typos_reg. The
#' real ("legal") names are the names in the shapefile

# Run fix_typos() function
corrected_df_reg <- fix_typos(dataframe=example_locations,           # the dataframe containing village, district, ward and region names
                              typo_df = typos_reg,                   # a dataframe containing two known mis-spellings which are VERY different and their correction
                              dataframe_label = "Region",            # the name of the column containing Region names in your dataframe
                              typo_label_correct = "Correct_Region", # the name of the column in typo dataframe stating correct spelling
                              typo_label_wrong = "Typo",             # the name of the column in typo dataframe stating incorrect spelling
                              legal_names = ward_2012$Region_Nam)    # a vector containing real Region names, as found in the shapefile

#-------------------------------------------------------------------------------
#'## 2) Use "check_names" to ensure Region names are correct ##
#-------------------------------------------------------------------------------

#' Using check_names will essentially spell-check all names in the given column,
#' in this case Region. A message will state the results after running through
#' every entry.

# Run check_names() function
doublecheck_rg <- check_names(dataframe=corrected_df_reg,       # the datframe produe bythe previous function
                              label = "Correct_Region",         # the name of the column stating the suggested Region
                              legal_names=ward_2012$Region_Nam) # a vector containing real Region names, as found in the shapefile

#-------------------------------------------------------------------------------
#'## 3) Identify mis-spelt Districts ##
#-------------------------------------------------------------------------------

#' This identifies mis-spelt District names, and proposes a set of similar names
#' which are correct.

# Run find_similar_district() function
new_typos_dis <- find_similar_district(dataframe = corrected_df_reg,          # the dataframe produced in step 1
                                       label="District",                      # the name of the column stating the District names
                                       label_higher_level = "Correct_Region", # the name of the column in typo dataframe stating correct Region spelling
                                       shapefile=ward_2012,                   # the object name of the wards shapefile
                                       distnc = 0.2)                          # an index of how different the suggested correction is from the original

#-------------------------------------------------------------------------------
#'## 4) Fix the typos at District level ##
#-------------------------------------------------------------------------------

#' After retrieveing similar district names, you should look through the resulting
#' dataframe (new_typos_dis in this case) and check everything looks right. If
#' any suggests and wildly different or inccorect (see note below), these need
#' to be changed manually before running the fix_typos() function.
#'
#' **NOTE:**
#' There are known typos involving Morogoro:
#' Typos: morogoro rural, morogoro municipal, morogoro
#' Legal: Morogoro, Morogoro Urban
#' When these appear, you should be aware of which is actually correct before
#' changing permanantly by hand.

# View corrections as character string
new_typos_dis$Correct_District_1 <- as.character(new_typos_dis$Correct_District_1)

# Manually fix wrongly suggested typos using an index to choose the row number
new_typos_dis$Correct_District_1[8] <- "Morogoro"  # Morogoro Rural is simply Morogoro
new_typos_dis$Correct_District_1[9] <- "Serengeti" # Nata is a ward named Natta in the Serengeti District

# View corrections as factor
as.factor(new_typos_dis$Correct_District_1)

# Run fix_typos() function
corrected_df_dis <- fix_typos(dataframe=corrected_df_reg,                # the dataframe produced in step 1
                              typo_df = new_typos_dis,                   # the dataframe produced in step 3
                              dataframe_label = "District",              # the column name of the dataframe specifying Districts
                              typo_label_correct = "Correct_District_1", # the column name stating corrected District names in the typo dataframe
                              typo_label_wrong = "misspelled_districts", # the column name stating misspelled District names in the typo dataframe
                              legal_names = ward_2012$District_N)        # a vector containing real District names, as found in the shapefile

#-------------------------------------------------------------------------------
#'## 5) Use "check_names" to ensure District names are correct ##
#-------------------------------------------------------------------------------

#' As with Region, check_names() is now used to ensure all District spellings are
#' correct as spelled in the shapefile

doublecheck_dis <- check_names(dataframe=corrected_df_dis,       # the dataframe produced by step 4
                               label = "Correct_District",       # the name of the column stating the suggested District
                               legal_names=ward_2012$District_N) # a vector containing real District names, as found in the shapefile


#-------------------------------------------------------------------------------
#'## 6) Identify mis-spelt Wards ##
#-------------------------------------------------------------------------------

#' This identifies mis-spelt Ward names, and proposes a set of similar names
#' which are correct.

#' You can either use the District...
# Run find_similar_ward() function
new_typos_ward_I <- find_similar_ward(dataframe = corrected_df_dis,            # the dataframe produced in step 4
                                      label="Ward",                            # the name of the column in dataframe stating Ward names
                                      higher_level = "District",               # the chosen level to use as a guide - District
                                      label_higher_level = "Correct_District", # the name of the column stating corrected District names
                                      shapefile=ward_2012,                     # the object name of the wards shapefile
                                      distnc = 0.2)                            # an index of how different the suggested correction is from the original

#' ... Or use the Region...
# Run find_similar_ward() function
new_typos_ward_II <- find_similar_ward(dataframe = corrected_df_dis,          # the dataframe produced in step 4
                                       label="Ward",                          # the name of the column in dataframe stating Ward names
                                       higher_level = "Region",               # the chosen level to use as a guide - Region
                                       label_higher_level = "Correct_Region", # the name of the column stating corrected Region names
                                       shapefile=ward_2012,                   # the object name of the wards shapefile
                                       distnc = 0.2)                          # an index of how different the suggested correction is from the original

#-------------------------------------------------------------------------------
#'## 7) Fix the typos at Ward level ##
#-------------------------------------------------------------------------------

# View corrections as character string
new_typos_ward_I$Correct_Ward_1 <- as.character(new_typos_ward_I$Correct_Ward_1)

# Manually fix wrongly suggested typos using an index to choose the row number
new_typos_ward_I$Correct_Ward_1[2] <- "Mvomero"    # There are two Dakawa villages, one in Mvomero District and one in Morogoro District
new_typos_ward_I$Correct_Ward_1[6] <- "Mvomero"    # There are multiple "Wami~" villages, but all are within the Mvomero Ward

# View corrections as factor
as.factor(new_typos_ward_I$Correct_Ward_1)

# Run fix_typos() function
corrected_df_wrd <- fix_typos(dataframe=corrected_df_dis,            # the dataframe produced in step 4
                              typo_df = new_typos_ward_I,            # the dataframe produced in step 6
                              dataframe_label = "Ward",              # the column name of the dataframe specifying Wards
                              typo_label_correct = "Correct_Ward_1", # the column name stating corrected Ward names in the typo dataframe
                              typo_label_wrong = "misspelled_wards", # the column name stating misspelled Ward names in the typo dataframe
                              legal_names = ward_2012$Ward_Name)     # a vector containing real Ward names, as found in the shapefile

#-------------------------------------------------------------------------------
#'## 8) Use "check_names" to ensure Ward names are correct ##
#-------------------------------------------------------------------------------

#' As with Region and District, check_names() is now used to ensure all Ward
#' spellings are correct as spelled in the shapefile

doublecheck_ward <- check_names(dataframe=corrected_df_wrd,      # the dataframe produced by step 7
                                label = "Correct_Ward",          # the name of the column stating the suggested Ward
                                legal_names=ward_2012$Ward_Name) # a vector containing real Ward names, as found in the shapefile

#-------------------------------------------------------------------------------
#'## 9) find_similar_village ##
#-------------------------------------------------------------------------------

#' This identifies mis-spelt Village names, and proposes a set of similar names
#' which are correct.

#' You can either use wards...
# Run find_similar_village() function
new_typos_vill_I <- find_similar_village(dataframe = corrected_df_wrd,        # the dataframe produced in step 7
                                         label="Village",                     # the name of the column in dataframe stating Village names
                                         higher_level = "Ward",               # the chosen level to use as a guide - Ward
                                         label_higher_level = "Correct_Ward", # the name of the column stating corrected Ward names
                                         shapefile=village_2012,              # the object name of the Village shapefile
                                         distnc = 0.2)                        # an index of how different the suggested correction is from the original

#' ... Districts...
# Run find_similar_village() function
new_typos_vill_II <- find_similar_village(dataframe = corrected_df_wrd,            # the dataframe produced in step 7
                                          label="Village",                         # the name of the column in dataframe stating Village names
                                          higher_level = "District",               # the chosen level to use as a guide - District
                                          label_higher_level = "Correct_District", # the name of the column stating corrected District names
                                          shapefile=village_2012,                  # the object name of the Village shapefile
                                          distnc = 0.2)                            # an index of how different the suggested correction is from the original

#' ...or Regions...
# Run find_similar_village() function
new_typos_vill_III <- find_similar_village(dataframe = corrected_df_wrd,          # the dataframe produced in step 7
                                           label="Village",                       # the name of the column in dataframe stating Village names
                                           higher_level = "Region",               # the chosen level to use as a guide - Region
                                           label_higher_level = "Correct_Region", # the name of the column stating corrected Region names
                                           shapefile=village_2012,                # the object name of the Village shapefile
                                           distnc = 0.2)                          # an index of how different the suggested correction is from the original

#-------------------------------------------------------------------------------
#'## 10) Fix the typos at Village level ##
#-------------------------------------------------------------------------------

# View corrections as character string
new_typos_vill_II$Correct_Village_1 <- as.character(new_typos_vill_II$Correct_Village_1)

# Manually fix wrongly suggested typos using an index to choose the row number
new_typos_vill_II$Correct_Village_1[35] <- "Natta"    # There are multiple Nata in other districts, but Natta is in the Serengeti
new_typos_vill_II$Correct_Village_1[43] <- "Mkundi"    # There is a Mkundi in Morogoro, despite not being suggested
new_typos_vill_II$Correct_Village_1[49] <- "Kipera"
new_typos_vill_II$Correct_Village_1[65] <- "Oljoro No. 5" # Oljoro doesn't exist in Simanjiro, but Oljoro No. 5 does

# View corrections as factor
as.factor(new_typos_vill_II$Correct_Ward_1)

# Run ix_typos() function
corrected_df_vill <- fix_typos(dataframe=corrected_df_wrd,               # the dataframe produced in step 7
                               typo_df = new_typos_vill_II,              # the dataframe produced in step 9
                               dataframe_label = "Village",              # the column name of the dataframe specifying Villages
                               typo_label_correct = "Correct_Village_1", # the column name stating corrected Village names in the typo dataframe
                               typo_label_wrong = "misspelled_villages", # the column name stating misspelled Village names in the typo dataframe
                               legal_names = village_2012$Vil_Mtaa_N)    # a vector containing real Village names, as found in the shapefile

#-------------------------------------------------------------------------------
#'## 11) Use "check_names" to ensure Village names are correct ##
#-------------------------------------------------------------------------------

#' As with Region, District and Ward, check_names() is now used to ensure all
#' Village spellings are correct as spelled in the shapefile.

# Run check_names() function
doublecheck_vill <- check_names(dataframe=corrected_df_vill,         # the dataframe produced by step 10
                                label = "Correct_Village",           # the name of the column stating the suggested Village
                                legal_names=village_2012$Vil_Mtaa_N) # a vector containing real Village names, as found in the shapefile

#-------------------------------------------------------------------------------
#'## 12) Fill in blank spaces ##
#-------------------------------------------------------------------------------

#' Folloing the above steps, there is likely to be gaps where a location was
#' missing (e.g. Region was not specified in original dataset). In these cases,
#' the fill_names() function is used to fil in the gaps (see warning).
#'
#' **WARNING:**
#' In order for the function to work, the same shapefile needs to be used for
#' each. If a different shapefile if used, the indices of the names will not
#' match and "Intersect" will not produce any result.
#'
#' It is better to use the Villages shapefile, as this includes information on
#' Villages, Wards, District and Regions.

updated_names <- fill_names(dataframe=corrected_df_vill,               # the dataframe produced in step 10
                            df_vill="Correct_Village",                 # the column stating the corrected Village names
                            df_ward="Correct_Ward",                    # the column stating the corrected Ward names
                            df_distr="Correct_District",               # the column stating the corrected District names
                            df_reg="Correct_Region",                   # the column stating the corrected Region names
                            legal_names_vill=village_2012$Vil_Mtaa_N,  # a vector containing real Village names, as found in the shapefile
                            legal_names_ward=village_2012$Ward_Name,   # a vector containing real Ward names, as found in the shapefile
                            legal_names_distr=village_2012$District_N, # a vector containing real District names, as found in the shapefile
                            legal_names_reg=village_2012$Region_Nam)   # a vector containing real Region names, as found in the shapefile
