
set.seed(1000)
n <- 300

qa_tbl <- tibble::tibble(
  col_id = sample(1:n,
                  n,
                  replace = FALSE),
  event_date = sample(seq(as.Date("2011-01-01"),
                          as.Date("2023-12-31"),
                          by = "day"),
                      n),
  age = sample(20:80,
               n,
               replace = TRUE),
  stage = sample(c("T2", "T3", "T4"),
                 size = n,
                 replace = TRUE),
  los = abs(rnorm(n,
                  mean = 50,
                  sd = 25) %>%
              as.integer()),
  ebl = seq(from = 0,
            to = 15000,
            by = 50)[1:n],
  asa = sample(seq(1,5),
               n,
               replace = TRUE),
  trt = sample(c("Drug A", "Drug B"),
               n,
               replace = TRUE),
  approach = case_when(asa <= 3 ~ "laparoscopy",
                       asa > 3 ~ "open"), # create questionable relationship to save out for review
  visit_codes = sample(c("1242", "001", "999", "34342", "1000"),
                    size = n,
                    replace = TRUE)
)

# Problem cases/relationships to make before creating general raw variables
qa_tbl <- qa_tbl %>%
  mutate(age = case_when(col_id == 267 & age == 54 ~ 15,
                    TRUE ~ age),
         trt = case_when(stage == "T4" ~ "Drug B",
                         TRUE ~ trt) # create artificial relationship that may be a fictious relationship in the data
         )


# Reverse create the 'raw' variables
qa_tbl <- qa_tbl %>%
  mutate(across(setdiff(names(qa_tbl), "col_id"),
                ~ as.character(.),
                .names = "{stringr::str_to_upper(.col)}_RAW"))


# Add in 'problem' cases that should occur after the general raw variable create
# These basically represent processing that would be do within the setups themselves...
qa_tbl <- qa_tbl %>%
  mutate(TRT_RAW = case_when(col_id == 208 & TRT_RAW == "Drug B" ~ 'Drug 2',
                             col_id == 53 & TRT_RAW == "Drug A" ~ 'First Group',
                             TRUE ~ TRT_RAW),
         AGE_RAW = case_when(col_id == 29 & AGE_RAW == "20" ~ 'twenty',
                             TRUE ~ AGE_RAW),
         age = case_when(!(col_id %in% c(29)) ~ age),
         visit_type = case_when(visit_codes %in% c("001", "1000") ~ "Inpatient",
                                visit_codes %in% c("1242", "34342") ~ "Outpatient"),
         EVENT_DATE_RAW = case_when(col_id == 143 ~ "10 8, 20",
                                    col_id == 69 ~ "4th May this year",
                                    TRUE ~ EVENT_DATE_RAW),
         event_date = case_when(!(col_id %in% c(69, 143)) ~ event_date)
  )


usethis::use_data(qa_tbl, overwrite = TRUE)

