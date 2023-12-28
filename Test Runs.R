# scratch area:
library(tidyverse)
library(stpR)

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





