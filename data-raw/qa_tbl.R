
qa_tbl <- tibble(
  Column1 = 1:100,
  Column2 = sample(1:100, 100, replace = TRUE),
  Column3 = sample(letters, 100, replace = TRUE)
)

qa_tbl
