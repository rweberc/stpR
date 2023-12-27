# scratch file:

# get_project_dictionary()
dc <- get_stp_object()

get_stp_object()$mapping_items

create_stp_ob(reset = TRUE)

# mapping with issue
eval_map(
  data_df = qa_tbl, # test_tbl,
  from = "TRT_RAW",
  to = "trt",
  notes = "trt notes..."
)

# continuous with issue
eval_map(
  data_df = qa_tbl, # test_tbl,
  from = "AGE_RAW",
  to = "age",
  std_proc_na = as.numeric,
  notes = "age notes..."
)

eval_summary(
  summary_df = count(qa_tbl, approach, asa),
  key = "proc_name",
  notes = "summary proc, ASA notes",
  stp_id = "comp_approach_asa"
)

qa_tbl <- keep_cases(
  data_df = qa_tbl,
  stp_id = "age_filter",
  id_vars = "col_id",
  non_id_vars = "age",
  keep_logic = "age >= 18",
  notes = "Remove patients younger than 18 years old"
)

assert_cases(
  logic = qa_tbl %>%
    filter(trt == "Drug A" & stage == "T3") %>%
    nrow() == 0,
  stp_id = 'assert_trt_vs_stage'
)




# TODO: reasonablly, probably don't prefer to have to give each of these an id, but for simplicity, addin the requirement here... Also, have to deal with fact that these really don't need an id, but if have generic id, then each run will just resave a new error... have check for distinct?
# TODO: need a "remove" function to remove a particular item from the stp_ob


# scratch area:
library(tidyverse)
library(stpR)


# Next items:


# assert_distinct() (col_id)
#
# log_item()
#
# later: assert_order() and replace_cases()...
#
# test report output...

# TODO: will need function to save a particular item in an issues object... stp_issues_obj
#       will need to decide the structure of that... but would have to have individual ids
#       cyclical nature of these projeccts... some oject that could be expected to contain the text, the artifact, and the feedback/conversation.
#       limitations: certain type of actions (mapping, filter, order, replace, "id" type data set), accounts for only a limited number of "type" scenarios
#       other issues that came up: what environment to assume existence of...  in database... "null" NULL NA "" etc. translation could get tricky... mixing between languages
#       balance of scripting and programmatic


# TODO: no message if compare is true but there's nothing to compare...
# TODO: need to add some kind of indicator in the mapping_items save when save_summary is used, at least since the 'from' column, should be 'key' in that case, right?
# Could have a random id... and have option then to remove by particular id or by when something was added, etc.

# data_df,
# select_ids, # should update to take vars(); I think this should be fields not in replace fields.
# logic,
# replace_fields,
# replace_values,
# issue = 0,
# rds,
# group_by_vars = NULL


