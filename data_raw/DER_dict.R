DER_dict_raw <- readxl::read_xlsx("data_raw/DER_dict.xlsx")
DER_dict <- psychTestR::i18n_dict$new(DER_dict_raw)
usethis::use_data(DER_dict, overwrite = TRUE)
