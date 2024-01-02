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
  summary_df = dplyr::count(test_tbl, approach, asa),
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




# Cases to consider for unit tests ----------------------------------------

# example type cases to unit test
# # only get two rows of change... I think as expected
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 2, 1,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 2, 11,
#   "b", 2, 12,
#   "c", 3, 3
# )
#
# # Will get four rows of change for b... one for the change from the perspective of each of the "to" columns
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 1, 11,
#   "b", 2, 12,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 1, 12,
#   "b", 2, 11,
#   "c", 3, 3
# )
#
# # ~ four rows again...
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 9, 20,
#   "b", 10, 21,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 7, 11,
#   "b", 8, 12,
#   "c", 3, 3
# )
#
# # what about when one of these to values is an NA
# old_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 9, 20,
#   "b", 10, 21,
#   "c", 3, 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1, ~ col2,
#   "a", 1, 0,
#   "b", 7, 11,
#   "b", 8, NA,
#   "c", 3, 3
# )
## Looking at case with multiple values but only one 'to' column
# old_map <- tribble(
#   ~ id, ~ col1,
#   "a", 1,
#   "b", 9,
#   "b", 10,
#   "c", 3
# )
#
# new_map <- tribble(
#   ~ id, ~ col1,
#   "a", 1,
#   "b", 100,
#   "b", 200,
#   "c", 3
# )
#
# from <-  "id"
# to <- c("col1", "col2")


# TODO: have init_stp() that would suggest and create the needed metadata objects...


# Cases to check... when you have an NA in the from column:
# tibble(id = c(NA, 1, 2), col1 = c(0, 1, NA)) %>% anti_join(tibble(id = 1:3, col1 = c(0, 1, NA)), by = "id")

# Similarly, when you have NA in the to columns (either in old or new)
# tibble(id = c(NA, 1, 2), col1 = c(0, 1, NA)) %>% anti_join(tibble(id = 1:3, col1 = c(0, 1, NA)), by = "id")



# Cases when you have a single value mapped in multiple ways (either in new or old)

