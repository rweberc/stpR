library(tidyverse)
library(usethis)



# Needed utility functions:

# create stpr_data object if none is found
# first see if exists in environment, second see if can read it, third

# function to try to input value to stpr object...
# could enforce certain checks here... also could make sure only one cell was being updated...

# get_project_dictionary()
#     default path would be at top-level of project, but could give another path



# write_stpr

# create_stpr

# Have to deal with updating the metadata... just overwrite?
# also... consider having a mapping object to map old names to new names... when comparing metavalues...


# default path is here::here("/secure_data") ... if not path given, attempt to write folder if needed

# have a function that you could put at the top of a script


# I think still have the project_dictionary yaml
# for items like path, compare_filename, etc.


# What are the data checking scenarios:
# "fixed' value mappings aside from how a given logic parses the field
# initial mappings between categorical fields and their parsed counterparts
# derived mappings of parsed fields to other fields
# derived categorical fields that you expect to parse according to some reliable function (though might just check some logic like > 0)
# not a derived scenario, but a relationship you wanted to (several variables... maybe !is.na() thrown in, etc.)


# philosophy:
# have each pass taken over a project add something that makes it more robust... don't have wasted/lost/double efforts as a structural side-effect of how setups are designed.

# for longer term tracking
# additional data object... project_history... can store fields here that are referenced in the issues file...
# a write-to only file...

# 3 layer of feedback
#     1. in console as working
#     2. in report, overall, to review
#     3. for auto-compare, programmatic checks


# THINGS YOU NEED IN YOUR PROJECT ENVIRONMENT
# 1. Dictionary yaml
# 2. stpR object
# 3. Comparare stpR object


# TODO: add function to make sure stpr_ob, project_dictionary, and stpr_compare_ob are well-formed

# "Metadata"
#
# "Metadata/Alerts"
# "Metadata/Alerts/Errors"
#
# "Metadata/Alerts/Update Issues"
#
# "Metadata/Alerts/Update Issues/Continuous"
# "Metadata/Alerts/Update Issues/Continuous/Changed"
# "Metadata/Alerts/Update Issues/Continuous/New"
# "Metadata/Alerts/Update Issues/Continuous/No Comparison"
#
# "Metadata/Alerts/Update Issues/Mappings"
# "Metadata/Alerts/Update Issues/Mappings/Changed"
# "Metadata/Alerts/Update Issues/Mappings/Missing"
# "Metadata/Alerts/Update Issues/Mappings/New"
# "Metadata/Alerts/Update Issues/Mappings/No Comparison"
#
# "Metadata/Alerts/Update Issues/Filtered"
# "Metadata/Alerts/Update Issues/Filtered/Changed"
# "Metadata/Alerts/Update Issues/Filtered/New"
# "Metadata/Alerts/Update Issues/Filtered/No Comparison"
#
# "Metadata/Allowed Values"
# "Metadata/Allowed Values/Continuous"
# "Metadata/Allowed Values/Filter Cases"
# "Metadata/Allowed Values/Mappings"
# "Metadata/Allowed Values/Question Cases"
#
# "Metadata/Data Features"
#
# "Metadata/Issues"
#
# "Metadata/Line Logs"
#
# "Metadata/Line Logs/Calls"
#
# "Metadata/Line Logs/Comments"
#
# "Metadata/Line Logs/Todos"

