################################################################################
############       Data Processing - demo 2.2 (short version)       ############
################################################################################

#-------------------------------------------------------------------------------
#'**Load the necessary libraries**
#-------------------------------------------------------------------------------

library(datacleaning)

#-------------------------------------------------------------------------------
#'**Import research project csv and the typos dataframe**
#-------------------------------------------------------------------------------

data("example_locations_II")

## Data Processing

## There is no Region and no Ward information.

## The typos dataframe:
data("example_typos")

#-------------------------------------------------------------------------------
#'**Data processing**
#-------------------------------------------------------------------------------

## Since the village typo dataframe is already available, I can simply:

## 1) get the real ("legal") names, i.e. the names in the village shapefile
real_vill_names <- village_2012$Vil_Mtaa_N

## 2) fix the misspellings in the village names based on the typo dataframe and on the legal names
corrected_df_vill <- fix_typos(dataframe=example_locations_II, typo_df = example_typos,
                               dataframe_label = "Village",
                               typo_label_correct = "Correct_Village_1",
                               typo_label_wrong = "misspelled_villages",
                               legal_names = real_vill_names)

## 3) check that the village names are correct
doublecheck_vill <- check_names(dataframe=corrected_df_vill,
                                label = "Correct_Village",
                                legal_names=real_vill_names)

## 4) look for mismatches and complete the locations dataset

# 4.1) the legal names, at all levels
real_vill <- village_2012$Vil_Mtaa_N
real_ward <- village_2012$Ward_Name
real_dis <- village_2012$District_N
real_reg <- village_2012$Region_Nam

# 4.2) fill in the missing information if there are no mismatches
updated_names <- fill_names(dataframe=corrected_df_vill, df_vill="Correct_Village",
                            df_ward="Ward", df_distr="District",
                            df_reg="Region", legal_names_vill=real_vill,
                            legal_names_ward=real_ward, legal_names_distr=real_dis,
                            legal_names_reg=real_reg)

#-------------------------------------------------------------------------------
#'** Produce the shapefile at the village/ward/district/region levels **
#-------------------------------------------------------------------------------

study_villages <- plot_villages(dataframe = updated_names, label_vill = "Updated_Villages",
                                label_ward = "Updated_Wards", label_dis = "Updated_Districts",
                                label_reg = "Updated_Regions", shapefile = village_2012)

study_wards <- plot_wards(dataframe = updated_names,  label_ward = "Updated_Wards",
                          label_dis = "Updated_Districts", label_reg = "Updated_Regions",
                          shapefile = ward_2012)

study_districts <- plot_districts(dataframe = updated_names,  label_dis = "Updated_Districts",
                                  label_reg = "Updated_Regions", shapefile = ward_2012)

study_regions <- plot_regions(dataframe = updated_names, label_reg = "Updated_Regions",
                              shapefile = ward_2012)


