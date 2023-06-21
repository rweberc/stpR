library(tidyverse)
library(usethis)



# Needed utility functions:

# create stpr_data object if none is found
# first see if exists in environment, second see if can read it, third

# function to try to input value to stpr object...
# could enforce certain checks here... also could make sure only one cell was being updated...

# get_project_dictionary()
#     default path would be at top-level of project, but could give another path

# TODO: I have new_map and map_ob/ref_ob... referred to at various times... I should pick one...

# TODO: consider having option for filter/ques cases to return metadata object... could include all the things
# TODO: consider again how you could integrate in the summary-by type options... I think it could be really helpful to have that incorporated as something you're checking regularly
#       could have a global option for whether those run by default or not...

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


# THINGS YOU NEED IN YOUR PROJECT FOLDER
# 1. Dictionary yaml
# 2. stpR object
# 3. Comparare stpR object


# TODO: inside a function, should you never use '<-'?
# update stpr_ within package to stp_

# consider having one eval_ function where you can specify the relevant parameters, all present... then having several others that are just wrappers for having certain defaults

# TODO: add function to make sure stpr_ob, project_dictionary, and stpr_compare_ob are well-formed


# stack difference results when there are more than two "to" columns ('changed per id - only in old', 'changed per id - only in new')
# have a change_type column in the reference object that 'only in old', 'only in new' 'changed'
# add another column in the update entity that would note what data set was compared against


# TODO: update the main functions to have an option to modify what they return... (update fields, all fields, etc.)

# TODO: consider if it's worth having a function of non-negative?


# CHECK:
# when 'do not overwrite' is TRUE, you can stil overwrite the stp_ob in the current data path, right?... but I think you just read in and write out
#   ... for instance, like with log items
#   ... I think it is practically governed by: save_metadata_gobal


# TODO: can't do a factor...
