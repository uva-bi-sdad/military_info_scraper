library(openxlsx)
library(data.table)
library(stringr)
library(readr)
library(rvest)

dt <- setDT(read.xlsx("mil_insts_info.xlsx"))

extract_number <- function(str) {
  str_match(str, "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}") %>%
    str_replace_all(",", "") %>% 
    as.numeric() %>% 
    sum()
}

remove_like <- function(str_lst, regx_vct = "") {
  if (!regx_vct == "") {
    for (i in 1:length(regx_vct)) {
      str_lst <- str_lst[!str_lst %like% regx_vct[i]]
    }
  }
  str_lst
}

keep_like <- function(str_lst, regx_vct = "") {
  keeps <- character()
  if (!regx_vct == "") {
    for (i in 1:length(regx_vct)) {
      keeps <- c(keeps, str_lst[str_lst %like% regx_vct[i]])
    }
    return(keeps)
  }
  return(str_lst)
}

get_matches <- function(str, rgx) {
  str_match_all(str, regex(rgx, ignore_case = T)) %>% 
    .[[1]] %>% 
    .[,1]
}

update_cell <- function(dt, num, i, col) {
  if (is.na(dt[i, get(col)]) & !num==0) {
    dt[i, eval(col) := num]
  }
  dt
}

get_pop_cnt <- function(dt, col, rgx_str_num, rgx_num_str, removes = "", keeps = "") {
  if (!col %in% colnames(dt)) dt[, eval(col) := character()]
  else dt[, eval(col) := NA]
  
  for (rgx in c(rgx_str_num, rgx_num_str)) {
    for (i in 1:nrow(dt)) {
      #browser()
      num <- dt[i]$population %>%
        str_replace_all("U.S.", "US") %>%
        get_matches(rgx) %>%
        remove_like(removes) %>%
        keep_like(keeps) %>%
        extract_number()
      dt <- update_cell(dt, num, i, col)
    }
  }
  dt
}

# Active Duty Count
col <- "pop_active_duty"
rgx_str_num <- "[^0-9]\\s?(active(\\s|-)duty|military)(\\s|:\\s|-\\s|\\s-\\s)([^\\s\\.,]+(\\s|:|-)){0,4}[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}"
rgx_num_str <- "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}\\+?\\s([^\\s\\.,]+\\s){0,4}((active(\\s|-)duty|soldiers)(?!\\W+famili?y\\b)|\\bmilitary\\b(?!\\W+famili?y\\b)(?!\\W+and civilian\\b))"
removes <- c("[Rr]etire", "[Ff]amil")

dt <- get_pop_cnt(dt, col, rgx_str_num, rgx_num_str, removes = removes)


# Active Duty Family Member Count
col <- "pop_active_duty_fam"
rgx_str_num <- "[^0-9]\\s?(famili?y|dependents)(\\s|:\\s|-\\s|\\s-\\s)([^\\s\\.,]+(\\s|:|-)){0,4}[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}"
rgx_num_str <- "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}\\+?\\s([^\\s\\.,]+\\s){0,4}(famili?y|dependents)"
removes <- c("[Rr]etire")

dt <- get_pop_cnt(dt, col, rgx_str_num, rgx_num_str, removes = removes)


# Civilian Count
col <- "pop_civilian"
rgx_str_num <- "[^0-9]\\s?(civil[a-z]*|\\s?federal)(\\s|:\\s|-\\s|\\s-\\s)([^\\s\\.,]+(\\s|:|-)){0,4}[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}"
rgx_num_str <- "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}\\+?\\s([^\\s\\.,]+\\s){0,4}(civil[a-z]*|\\s?federal)"

dt <- get_pop_cnt(dt, col, rgx_str_num, rgx_num_str)


# Contractor Count
col <- "pop_contract"
rgx_str_num <- "[^0-9]\\s?(contract[a-z]*|cme)(\\s|:\\s|-\\s|\\s-\\s)([^\\s\\.,]+(\\s|:|-)){0,4}[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}"
rgx_num_str <- "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}\\+?\\s([^\\s\\.,]+\\s){0,4}([a-z/]*contract|cme)"

dt <- get_pop_cnt(dt, col, rgx_str_num, rgx_num_str)

# Retiree Count
col <- "pop_retired"
rgx_str_num <- "[^0-9]\\s?(retire[a-z]*)(\\s|:\\s|-\\s|\\s-\\s)([^\\s\\.,]+(\\s|:|-)){0,4}[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}"
rgx_num_str <- "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}\\+?\\s([^\\s\\.,]+\\s){0,4}retire([^\\s\\.,]+\\s){0,2}"
removes <- c("[Dd]ependent", "[Ff]amil")

dt <- get_pop_cnt(dt, col, rgx_str_num, rgx_num_str, removes = removes)

# Retired Family Member Count
col <- "pop_retired_fam"
rgx_str_num <- "[^0-9]\\s?(retire[a-z]*)(\\s|:\\s|-\\s|\\s-\\s)([^\\s\\.,]+(\\s|:|-)){0,4}[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}"
rgx_num_str <- "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}\\+?\\s([^\\s\\.,]+\\s){0,4}retire([^\\s\\.,]+\\s){0,2}"
keeps <- c("[Dd]ependent", "[Ff]amil")

dt <- get_pop_cnt(dt, col, rgx_str_num, rgx_num_str, keeps = keeps)

# Reservist/National Guard Count
col <- "pop_resrv_ntgrd"
rgx_str_num <- "[^0-9]\\s?(reserv[a-z]*|[a-z]*guard)(\\s|:\\s|-\\s|\\s-\\s)([^\\s\\.,]+(\\s|:|-)){0,4}[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}[^\\)]"
rgx_num_str <- "[0-9]{1,3},?+[0-9]{1,3},?+[0-9]{1,3}\\+?\\s([^\\s\\.,]+\\s){0,4}(reserv[a-z]*|[a-z]*guard)"

dt <- get_pop_cnt(dt, col, rgx_str_num, rgx_num_str)

write_csv(dt[,.(region, location, name, pop_active_duty, pop_active_duty_fam, pop_civilian, pop_contract, pop_retired, pop_retired_fam, pop_resrv_ntgrd)], "mil_insts_population_parse.csv")
write_csv(dt[,.(region, location, name, population, pop_active_duty, pop_active_duty_fam, pop_civilian, pop_contract, pop_retired, pop_retired_fam, pop_resrv_ntgrd)], "mil_insts_population_parse_with_raw.csv")
openxlsx::write.xlsx(dt[,.(region, location, name, population, pop_active_duty, pop_active_duty_fam, pop_civilian, pop_contract, pop_retired, pop_retired_fam, pop_resrv_ntgrd)], "mil_insts_population_parse.xlsx")

