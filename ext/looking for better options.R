library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

raw <- readxl::read_excel(path = "./tests/testthat/test_files/test_mkm_read.xlsx", sheet = 1, col_types = 'list', col_names = FALSE)
raw %>% mutate_all(as.character)

txt <- rapply(raw, as.character)
dim(txt) <- dim(raw)



raw2 <- tidyxl::xlsx_cells(path = "./tests/testthat/test_files/test_mkm_read.xlsx", sheets = 1)
# Rows are a pain in tidyverse
# https://github.com/jennybc/row-oriented-workflows
# You can use pmap() OR transpose() + map()
raw2$all <- raw2 %>%
    select(logical, numeric, date, character) %>%
    mutate_all(as.character) %>%
    purrr::pmap_chr(mkm:::unique_not_na)

raw2 %>% select(row, col, all) %>% spread(key = col, value = all) %>% select(-row) %>% as.matrix()


txt <- raw2 %>% select(row, col, all) %>% spread(col, all) %>% select(-row) %>% as.matrix()


# raw2$all <- apply(raw2[,c('logical', 'numeric', 'date', 'character')], 1, mkm:::unique_char_not_na)


nrow(raw2)
length(txt2)
dim(txt2) <- c(length(unique(raw2$row)), length(unique(raw2$col)))




