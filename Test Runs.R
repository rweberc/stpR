# scratch area:
library(tidyverse)
library(stpR)

test_tbl <- stpR::qa_tbl

# get_project_dictionary()
dc <- get_stp_object()

get_stp_object()$mapping_items

create_stp_ob(reset = TRUE)

# mapping with issue
eval_map(
  data_df = test_tbl,
  from = "TRT_RAW",
  to = "trt",
  notes = "Note multiple raw processed values associated with each treatment type"
)

test_tbl$age <- as.numeric(test_tbl$AGE_RAW)
test_tbl$age[test_tbl$AGE_RAW == "twenty"] <- 20

# 'continuous' field with issue
eval_map(
  data_df = test_tbl,
  from = "AGE_RAW",
  to = "age",
  std_proc_na = as.numeric,
  notes = "Note processing of value of 'twenty' from AGE_RAW"
)

eval_summary(
  summary_df = count(test_tbl, approach, asa),
  key = "proc_name",
  notes = "summary proc, ASA notes",
  stp_id = "comp_approach_asa"
)

test_tbl <- keep_cases(
  data_df = test_tbl,
  stp_id = "age_filter",
  id_vars = "col_id",
  non_id_vars = "age",
  keep_logic = "age >= 18",
  notes = "Remove patients younger than 18 years old"
)

assert_cases(
  logic = test_tbl %>%
    filter(trt == "Drug A" & stage == "T3") %>%
    nrow() == 0,
  stp_id = 'assert_trt_vs_stage'
)

temp_dup_tbl <- test_tbl %>%
  bind_rows(test_tbl %>%
              slice(1))

assert_distinct(
  data_df = temp_dup_tbl,
  group_by_vars = "col_id"
)



# Create report -----------------------------------------------------------

  create_stp_markdown()

#' header
#'
#' desc
#'
#' details
#'
#' @param xyz abc
#'
#' @return
#'
#' @export
#'
#' @examples
#' \donttest{
#'
#'
#' }
