library(tidyverse)
DER_item_bank <- readxl::read_xlsx("data_raw/DER_item_bank.xlsx") %>%
  mutate(video_file = sprintf("%s.mp4", video_file))
usethis::use_data(DER_item_bank, overwrite = TRUE)
