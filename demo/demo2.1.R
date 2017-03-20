#------------------------------------------------------------------------------#
#       DEMO 2.1: Extract location information from a set of coordinates       #
#------------------------------------------------------------------------------#

#----------------------------------------------------#
# Using the fix_coords() function to fix coordinates #
#----------------------------------------------------#
#
# Prior to using the extract_location() function, the fix_coords() function
# should first be used to correct coordinates with errors. These errors include
# coordinates input in a different format, and coordinates outside of a set range
# (see help file at ?fix_coords for the range).
# This function currently only accepts coordiantes in either the UTM 36S ("UTM")
# or Geographic WGS84 ("Geo-WGS84") format.
#
#----------------------#
# Load in example data #
#----------------------#
data("utm_coordinates_err")
data("geo_coordinates_err")
#
#
#---------------------#
# Fix UTM coordinates #
#---------------------#
# using "remove.extra = FALSE" only replaces incorrect coordinates with NA.
utm_coordinates_fix <- fix_coords(utm_coordinates_err, x_label = "x.utm",
                                  y_label = "y.utm", coord_type = "UTM",
                                  remove.extra = FALSE)
#
# In order to continue with processing, you must use "remove.extra = TRUE" which
# will remove both coordinates if either is replaces with NA.
#
utm_coordinates_rm.ext <- fix_coords(utm_coordinates_err, x_label = "x.utm",
                                  y_label = "y.utm", coord_type = "UTM",
                                  remove.extra = TRUE)
#
#----------------------------#
# Fix Geographic coordinates #
#----------------------------#
geo_coordinates_fix <- fix_coords(geo_coordinates_err, x_label = "longitude",
                                  y_label = "latitude", coord_type = "Geo-WGS84",
                                  remove.extra = FALSE)
#
geo_coordinates_rm.ext <- fix_coords(geo_coordinates_err, x_label = "longitude",
                                  y_label = "latitude", coord_type = "Geo-WGS84",
                                  remove.extra = TRUE)

#
#-----------------------------------------------------------------------#
# Using the extract_location() function to collect location information #
#-----------------------------------------------------------------------#
#
# The extract_location() function uses a set of matching x- and y- coordinates,
# and produces a list of results. This list contains:
# [1] A dataframe containing 4 columns - village, ward, district and region.
# [2] A dataframe which is used uniquely for the Tanzania Shiny app, containing
# Location, Location_Type and Loc_ID.
#
# If update.df or add.coords is used in the function call, a third item is returned:
# [3] The original dataframe with updates in new columns.
#
#
# This function is used in the same way for whichever coordinate system you use:
# the only difference comes from the "data.projection" argument.
#
#
#----------------------------------#
# Run function for UTM coordinates #
#----------------------------------#
#
# If you would like to keep the original dataframe intact, update.df and add.coords
# are FALSE by default.
utm_results <- extract_location(dataframe = utm_coordinates_rm.ext,
                                x_label = "x.utm", y_label = "y.utm",
                                data.projection="+proj=utm +zone=36 +south +datum=WGS84",
                                update.df = F, add.coords = F)
#
# Each item from the list is extracted as follows:
utm_loc_info <- utm_results[[1]]
utm_shiny_app_df <- utm_results[[2]]
#
# The first item can be used to find the unique areas within Tanzania the data
# is collected from when the information was originally missing/mis-sampled.
utm_loc_info
#
#
#-----------------------------------------#
# Run function for Geographic coordinates #
#-----------------------------------------#
geo_results <- extract_location(dataframe = geo_coordinates_rm.ext,
                                x_label = "longitude", y_label = "latitude",
                                data.projection="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                                update.df = F, add.coords = F)
#
# Each item from the list is extracted as follows:
geo_loc_info <- geo_results[[1]]
geo_shiny_app_df <- geo_results[[2]]
#
geo_loc_info
