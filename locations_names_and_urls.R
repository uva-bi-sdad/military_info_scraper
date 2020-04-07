library(data.table)
library(stringr)
library(rvest)

all_mil_inst <- read_html("https://installations.militaryonesource.mil/view-all")

CONUS_inst_locs <- 
  all_mil_inst %>% 
  html_nodes(xpath = '//*[@id="miCONUS"]') %>% 
  html_nodes('.panel-heading')

if (exists("dta_conus")) rm("dta_conus")
for (i in 1:length(CONUS_inst_locs)) {
  loc <- CONUS_inst_locs[i] %>% 
    html_node('.accordion-title') %>% 
    html_text()
  insts <- CONUS_inst_locs[i] %>%
    html_nodes('.installation') %>% 
    html_nodes('a') %>% 
    html_attr('title')
  urls <- CONUS_inst_locs[i] %>%
    html_nodes('.installation') %>% 
    html_nodes('a') %>% 
    html_attr('href') %>%
    str_replace("military-installation", "in-depth-overview")
  
  dt <- data.table(region = "CONUS",
                   location = loc,
                   name = insts,
                   url = urls)
  if (!exists("dta_conus")) dta_conus <- dt
  else dta_conus <- rbindlist(list(dta_conus, dt))
}
dta_conus

OCONUS_inst_locs <- 
  all_mil_inst %>% 
  html_nodes(xpath = '//*[@id="miOCONUS"]') %>% 
  html_nodes('.panel-heading')

if (exists("dta_oconus")) rm("dta_oconus")
for (i in 1:length(OCONUS_inst_locs)) {
  loc <- OCONUS_inst_locs[i] %>% 
    html_node('.accordion-title') %>% 
    html_text()
  insts <- OCONUS_inst_locs[i] %>%
    html_nodes('.installation') %>% 
    html_nodes('a') %>% 
    html_attr('title')
  urls <- OCONUS_inst_locs[i] %>%
    html_nodes('.installation') %>% 
    html_nodes('a') %>% 
    html_attr('href') %>%
    str_replace("military-installation", "in-depth-overview")
  
  dt <- data.table(region = "OCONUS",
                   location = loc,
                   name = insts,
                   url = urls)
  if (!exists("dta_oconus")) dta_oconus <- dt
  else dta_oconus <- rbindlist(list(dta_oconus, dt))
}
dta_oconus

dta_all <- rbindlist(list(dta_conus, dta_oconus))

page_sections <- function(url = "https://installations.militaryonesource.mil/in-depth-overview/fort-rucker") {
  page_url <- url
  page_html <- dataplumbr::try.try_try_try(read_html(page_url))
  if (!page_html=="failed") {
    page_html_chr <- as.character(page_html)
    section_titles_html <- html_nodes(page_html, '.section-title')
    if (exists("page_dt")) rm("page_dt")
    page_dt <- data.table(page_url = character(), 
                          section_title = character(), 
                          section_text = character())
    for ( i in 1:length(section_titles_html)) {
      #browser()
      section_title_html_chr <- as.character(section_titles_html[i]) %>% str_replace_all("\\(", "\\\\(") %>% str_replace_all("\\)", "\\\\)")
      if (length(section_title_html_chr) > 0) {
        section_start <- str_locate(page_html_chr, section_title_html_chr)[[1,1]]
        section_end <- str_locate(str_sub(page_html_chr, section_start), "</p>")[[1,2]] + section_start
        section_html <- read_html(str_sub(page_html_chr, section_start, section_end))
        section_title <- html_nodes(section_html, '.section-title') %>% 
          html_text()
        section_text <- html_nodes(section_html, '.section-text') %>% as.character()
        # section_text <- html_nodes(section_html, '.section-text') %>%
        #   html_text() %>%
        #   str_replace_all("\\n", "")
        page_dt <- rbindlist(list(page_dt, data.table(page_url, section_title, section_text)), fill = TRUE)
      }
    }
    page_dt
  }
}

pages_dt <- data.table(page_url = character(), 
                       section_title = character(), 
                       section_text = character(),
                       region = character(),
                       location = character(),
                       name = character())
for (i in 379:nrow(dta_all)) {
  url <- dta_all[i]$url
  page_dt <- page_sections(url)
  page_dt$region <- dta_all[i]$region
  page_dt$location <- dta_all[i]$location
  page_dt$name <- dta_all[i]$name
  #browser()
  pages_dt <- rbindlist(list(pages_dt, page_dt))
}

pages_dt <- pages_dt[, c(4:6, 1:3)]

saveRDS(pages_dt, "mil_insts.RDS")

unique(pages_dt$section_title)


pages_dt <- readRDS("mil_insts.RDS")

inst_unq <- unique(pages_dt[, .(region, location, name)])

inst_info <- data.table(region = character(),
                        location = character(),
                        name = character(),
                        page_url = character(),
                        mission = character(),
                        history = character(),
                        population = character(),
                        directions = character(),
                        base_transportation = character(),
                        contact_info = character()
                        )

for (i in 1:307) {
  inst <- inst_unq[i]
  n <- inst_info[region == inst$region & location == inst$location & name == inst$name, .N]
  if (n == 0) {
    inst_info <- rbindlist(list(inst_info, data.table(inst$region, inst$location, inst$name, "", "", "", "", "", "", "")), use.names = F)
  }
  # url
  pages_dt_url <- unique(pages_dt[region == inst$region & location == inst$location & name == inst$name, .(page_url)])[, paste(page_url, collapse = " <br/> ")]
  inst_info[region == inst$region & location == inst$location & name == inst$name, page_url := pages_dt_url]
  # mission
  pages_dt_msn <- unique(pages_dt[region == inst$region & location == inst$location & name == inst$name & section_title == "Mission", .(section_text)])[, paste(section_text, collapse = " <br/> ")]
  inst_info[region == inst$region & location == inst$location & name == inst$name, mission := pages_dt_msn]
  # history
  pages_dt_hst <- unique(pages_dt[region == inst$region & location == inst$location & name == inst$name & section_title == "History", .(section_text)])[, paste(section_text, collapse = " <br/> ")]
  inst_info[region == inst$region & location == inst$location & name == inst$name, history := pages_dt_hst]
  # population
  pages_dt_pop <- unique(pages_dt[region == inst$region & location == inst$location & name == inst$name & section_title == "Population", .(section_text)])[, paste(section_text, collapse = " <br/> ")]
  inst_info[region == inst$region & location == inst$location & name == inst$name, population := pages_dt_pop]
  # directions
  pages_dt_dir <- unique(pages_dt[region == inst$region & location == inst$location & name == inst$name & section_title == "Directions", .(section_text)])[, paste(section_text, collapse = " <br/> ")]
  inst_info[region == inst$region & location == inst$location & name == inst$name, directions := pages_dt_dir]
  # base transportation
  pages_dt_trn <- unique(pages_dt[region == inst$region & location == inst$location & name == inst$name & section_title == "Base Transportation", .(section_text)])[, paste(section_text, collapse = " <br/> ")]
  inst_info[region == inst$region & location == inst$location & name == inst$name, base_transportation := pages_dt_trn]
  # contact information
  pages_dt_cnt <- unique(pages_dt[region == inst$region & location == inst$location & name == inst$name & section_title == "Contact Information", .(section_text)])[, paste(section_text, collapse = " <br/> ")]
  inst_info[region == inst$region & location == inst$location & name == inst$name, contact_info := pages_dt_cnt]
}

inst_info[population=="", population := "<div />"]

openxlsx::write.xlsx(inst_info, "mil_insts_info.xlsx")


