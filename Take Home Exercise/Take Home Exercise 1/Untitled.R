pacman::p_load(tidyverse, sf, tmap, httr)

folder_path <- "data/aspatial/acra"
file_list <- list.files(path = folder_path,
                        pattern = "^ACRA*.*\\.csv$",
                        full.names = TRUE)

acra_data <- file_list %>%
  map_dfr(read_csv)

write_rds(acra_data,
          "data/rds/acra_data.rds")
# tidying
biz_56111 <- acra_data %>%
  select(1:24) %>%
  filter(primary_ssic_code == 56111) %>%
  rename(date = registration_incorporation_date) %>%
  mutate(date = as.Date(date),
         YEAR = year(date),
         MONTH_NUM = month(date),
         MONTH_ABBR = month(date,
                            label = TRUE,
                            abbr = TRUE)) %>%
  mutate(
    postal_code = str_pad(postal_code,
                          width = 6, side = "left", pad = "0")) %>%
  filter(YEAR == 2025)
  )


#geocoding
postcodes <- unique(biz_56111$postal_code)

url <- "https://onemap.gov.sg/api/common/elastic/search"

found <- data.frame()
not_found <- data.frame(postcode = character())

for (pc in postcodes) {
  query <- list(
    searchVal = pc,
    returnGeom = "Y",
    getAddrDetails = "Y",
    pageNum = "1"
  )
}

res <- GET(url, query = query)
json <- content(res)

if (json$found != 0) {
  df <- as.data.frame(json$results, stringAsFactors = FALSE)
  df$input_postcode <- pc
  found <- bind_rows(found, df)
} else {
  not_found <- bind_rows(not_found, data.frame(postcode = pc))
}

biz_56111 = biz_56111 %>%
  left_join(found,
            by = c('postal_code' = 'POSTAL'))

biz_56111_sf <- st_as_sf(biz_56111,
                         coords = c("X","Y"),
                         crs=3414)

