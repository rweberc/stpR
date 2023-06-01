
qa_tbl <- tibble(
  col_id = sample(1:100, 100, replace = TRUE),
  col_date1 = random_dates <- sample(seq(as.Date("2011-01-01"), as.Date("2023-12-31"), by = "day"), 100),
  Column2 = sample(1:100, 100, replace = TRUE),
  Column3 = sample(letters, 100, replace = TRUE)
)

# TODO: update to a save statement...
qa_tbl
