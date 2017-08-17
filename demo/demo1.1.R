################################################################################
############        Data Processing - demo 1.1 (full version)       ############
################################################################################

#-------------------------------------------------------------------------------
#'**Load the necessary libraries**
#-------------------------------------------------------------------------------

library(datacleaning)

#-------------------------------------------------------------------------------
#'**Import research project csv and the typos list for the regions**
#-------------------------------------------------------------------------------

example_locations_II <- readr::read_csv(system.file("extdata", "example_locations_II.csv", package="datacleaning"))

## Data Processing

## There is no Region and no Ward information.

#-------------------------------------------------------------------------------
#'** DISTRICTS **
#-------------------------------------------------------------------------------

## the real ("legal") names are the names in the shapefile
real_dis_names <- datacleaning::TZ_all_bound$District_N

## are the district names correct?
doublecheck_dis <- check_names(dataframe=example_locations_II, label = "District",
                               legal_names=real_dis_names)

## The district information is correct.
## We can now move to the village level.

readline("1st step completed. Hit RETURN to move to the 2nd step.")

#-------------------------------------------------------------------------------
#'** VILLAGES **
#-------------------------------------------------------------------------------

## the real ("legal") names are the names in the village shapefile
real_vill_names <- datacleaning::TZ_all_bound$Vil_Mtaa_N

## are the village names correct?
doublecheck_vill <- check_names(dataframe=example_locations_II, label = "Village",
                                legal_names=real_vill_names)

## There are misspellings that need to be fixed.

## Therefore, we need to: (i) identify the misspellings; (ii) propose a set of alternative
## correct names; (iii) fix the misspellings.

## the function "find_similar_village" deals with the tasks in (i) and (ii)
## by producing a typos dataframe;

## the function "fix_typos" deals with the task in (iii)

## the study took place only in the district of Kilosa.
stud_distr <- c("Kilosa")

## Use the available information (district Kilosa) to narrow down the search.

## (i) identify the misspellings; (ii) propose a set of alternative correct names
new_typos_vill <- find_similar_village(dataframe = example_locations_II,
                                       label="Village",
                                       higher_level = "District",
                                       label_higher_level = "District",
                                       shapefile=village_2012,
                                       distnc = 0.2,
                                       only_in_distr = stud_distr)


readline("2nd step completed. Hit RETURN to move to the 3rd step.")

## Manually check the typos and select the right answer. In this example:
## Manzese C = Manzese A
## Masugu = Masugu Kati

## The processed typo dataset:
example_typos <- readr::read_csv(system.file("extdata", "example_typos.csv", package="datacleaning"))

## (iii) fix the misspellings
corrected_df_vill <- fix_typos(dataframe=example_locations_II, typo_df=example_typos,
                               dataframe_label="Village",
                               typo_label_wrong="misspelling",
                               typo_label_correct="correct_name",
                               legal_names=real_vill_names, mixed_typo = F)


## are the village names now correct?
doublecheck_vill <- check_names(dataframe=corrected_df_vill,
                                label = "Correct_Village",
                                legal_names=real_vill_names)

## There are no misspellings.

readline("3rd step completed. Hit RETURN to move to the 4th step.")

#-------------------------------------------------------------------------------
#'** MISMATCHES **
#-------------------------------------------------------------------------------

## Now, we need to check that there are no mismatches between the villages and
## the districts. In other words, we need to make sure that every village in the dataset
## is contained in the specified district. If no mismatches are found, we also
## need to update the available data by filling the information for wards and region.
## This task is accomplished by the function "fill_names".

## WARNING: in order for the function to work, we need to use the same shapefile.
## If we use different shapefiles, the indices of the names will not match and
## "Intersect" will not produce any result.

## In this case, we need to use villages because it has the village names information.

real_vill <- village_2012$Vil_Mtaa_N
real_ward <- village_2012$Ward_Name
real_dis <- village_2012$District_N
real_reg <- village_2012$Region_Nam

updated_names <- fill_names(dataframe=corrected_df_vill, df_vill="Correct_Village",
                            df_ward="Ward", df_distr="District",
                            df_reg="Region", legal_names_vill=real_vill,
                            legal_names_ward=real_ward, legal_names_distr=real_dis,
                            legal_names_reg=real_reg)

readline("4th step completed. Hit RETURN to move to the 5th step.")

#-------------------------------------------------------------------------------
#'** Produce the shapefile at the village/ward/district/region levels **
#-------------------------------------------------------------------------------

study_villages <- plot_villages(dataframe = updated_names, label_vill = "Village",
                                label_ward = "Ward", label_dis = "District",
                                label_reg = "Region", shapefile = village_2012)

study_wards <- plot_wards(dataframe = updated_names,
                             label_ward = "Ward", label_dis = "District",
                             label_reg = "Region", shapefile = ward_2012)

study_districts <- plot_districts(dataframe = updated_names,
                              label_dis = "District",
                              label_reg = "Region", shapefile = district_2012)

study_regions <- plot_regions(dataframe = updated_names,
                                label_reg = "Region", shapefile = region_2012)


