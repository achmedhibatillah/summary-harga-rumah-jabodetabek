# CREATED BY ACHMED HIBATILLAH

cat("\014")

#install.packages("rvest")
#install.packages("readr")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("jsonlite")
#install.packages("pbapply")
#install.packages("ggplot2")
#install.packages("sf")
#install.packages("tmap")
#install.packages("geodata")
#install.packages("scales")

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(tmap)
library(geodata)
library(scales)

# DATA YANG DIGUNAKAN:
df <- read_csv("data.csv")

# MEMPEROLEH SUMMARY UNTUK SELURUH DATA
summary_by_general <- df_clean %>%
  summarise(
    mean_price = mean(price_in_rp),
    median_price = median(price_in_rp),
    min_price = min(price_in_rp),
    max_price = max(price_in_rp),
    n = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_price))

print(summary_by_general)

# MENGOLAH DATA MENTAH DENGAN MEMPARSINGNYA
parsed_df <- df %>%
  mutate(
    jumlah_lantai = str_extract(title, "(?i)\\b[0-9]+\\s*(lantai|Lantai|L)\\b"),
    luas_tanah = str_extract(title, "(?i)\\d{1,2}\\s*[xX]\\s*\\d{1,2}"),
    cluster = str_extract(title, "(?i)(Cluster|cluster)\\s+[A-Za-z0-9]+"),
    tipe_rumah = case_when(
      str_detect(title, regex("mewah", TRUE)) ~ "Mewah",
      str_detect(title, regex("minimalis", TRUE)) ~ "Minimalis",
      str_detect(title, regex("cantik", TRUE)) ~ "Cantik",
      TRUE ~ "Lainnya"
    ),
    lokasi_khusus = case_when(
      str_detect(title, regex("jatiasih", TRUE)) ~ "Jatiasih",
      str_detect(title, regex("cibitung", TRUE)) ~ "Cibitung",
      str_detect(title, regex("galaxy", TRUE)) ~ "Galaxy",
      str_detect(title, regex("tambun", TRUE)) ~ "Tambun",
      TRUE ~ NA_character_
    ),
    status_kondisi = case_when(
      str_detect(title, regex("siap huni", TRUE)) ~ "Siap Huni",
      str_detect(title, regex("renov", TRUE)) ~ "Renovasi",
      str_detect(title, regex("baru", TRUE)) ~ "Baru",
      TRUE ~ NA_character_
    ),
    tipe_posisi = case_when(
      str_detect(title, regex("hoek|pojok|hook", TRUE)) ~ "Pojok",
      TRUE ~ NA_character_
    ),
    furnished = case_when(
      str_detect(title, regex("semi.*furnished", TRUE)) ~ "Semi Furnished",
      str_detect(title, regex("furnished", TRUE)) ~ "Furnished",
      TRUE ~ NA_character_
    ),
    akses_tol = str_extract(title, "(?i)[0-9]+\\s*(menit|mnt)\\s*(tol|jalan tol)|dekat(\\s*tol)?"),
    akses_stasiun = str_extract(title, "(?i)[0-9]+\\s*(menit|mnt)\\s*(stasiun)|dekat(\\s*stasiun)?"),
    keunggulan_lain = case_when(
      str_detect(title, regex("premium", TRUE)) ~ "Premium",
      str_detect(title, regex("strategis", TRUE)) ~ "Strategis",
      str_detect(title, regex("bebas banjir", TRUE)) ~ "Bebas Banjir",
      str_detect(title, regex("tinggi", TRUE)) ~ "Tinggi",
      TRUE ~ NA_character_
    ),
    luas_total_m2 = case_when(
      !is.na(luas_tanah) ~ {
        dim <- str_extract_all(luas_tanah, "\\d+")[[1]]
        if (length(dim) == 2) as.numeric(dim[1]) * as.numeric(dim[2]) else NA_real_
      },
      TRUE ~ NA_real_
    )
  )

head(parsed_df %>% 
       select(title, price_in_rp, jumlah_lantai, luas_tanah, luas_total_m2, cluster, tipe_rumah, lokasi_khusus,
              status_kondisi, tipe_posisi, furnished, akses_tol, akses_stasiun, keunggulan_lain), 10)

write_csv(parsed_df, "parsed_file.csv")

# MEMBACA FILE BARU
df <- read_csv("parsed_file.csv")

df_clean <- df %>%
  filter(!is.na(price_in_rp))

# SUMMARY BY DISTRICT
summary_by_district <- df_clean %>%
  group_by(city, district) %>%
  summarise(
    mean_price = mean(price_in_rp),
    median_price = median(price_in_rp),
    min_price = min(price_in_rp),
    max_price = max(price_in_rp),
    n = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_price))

summary_by_district <- summary_by_district %>%
  arrange(city, district)

print(summary_by_district)
View(summary_by_district)
 
# SUMMARY BY CITY
summary_by_city <- df_clean %>%
  group_by(city) %>%
  summarise(
    mean_price = mean(price_in_rp),
    median_price = median(price_in_rp),
    min_price = min(price_in_rp),
    max_price = max(price_in_rp),
    n = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_price))

summary_by_city <- summary_by_city %>%
  arrange(city)

print(summary_by_city)
View(summary_by_city)

# VISUALISASI GEOGRAFI BY CITY
indo_sf <- geodata::gadm("IDN", level = 2, path = tempdir()) %>%
  st_as_sf()

indo_sf <- indo_sf %>%
  mutate(
    city_name = tolower(NAME_2),
    city_group = case_when(
      str_detect(city_name, "bekasi") ~ "bekasi",
      str_detect(city_name, "bogor") ~ "bogor",
      str_detect(city_name, "tangerang") ~ "tangerang",
      str_detect(city_name, "depok") ~ "depok",
      str_detect(city_name, "jakarta barat") ~ "jakarta barat",
      str_detect(city_name, "jakarta pusat") ~ "jakarta pusat",
      str_detect(city_name, "jakarta selatan") ~ "jakarta selatan",
      str_detect(city_name, "jakarta timur") ~ "jakarta timur",
      str_detect(city_name, "jakarta utara") ~ "jakarta utara",
      TRUE ~ NA_character_
    )
  )

merged_sf <- indo_sf %>%
  filter(!is.na(city_group)) %>%
  group_by(city_group) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup()

summary_by_city$city <- tolower(summary_by_city$city)

map_data <- merged_sf %>%
  filter(city_group %in% summary_by_city$city) %>%
  left_join(summary_by_city, by = c("city_group" = "city"))

breaks <- quantile(map_data$mean_price, probs = seq(0, 1, length.out = 11), na.rm = TRUE)

range_labels <- paste0(
  label_number(prefix = "Rp ", scale_cut = cut_short_scale())(breaks[-length(breaks)]),
  " - ",
  label_number(prefix = "Rp ", scale_cut = cut_short_scale())(breaks[-1])
)

tmap_mode("plot")
tm_shape(map_data) +
  tm_polygons(
    fill = "mean_price",
    fill.scale = tm_scale_intervals(
      style = "fixed",
      breaks = breaks,
      labels = range_labels,
      values = "brewer.yl_or_rd"
    ),
    fill.legend = tm_legend(title = "Rata-rata Harga Rumah Area Jabodetabek")
  ) +
  tm_text("city_group", size = 0.7, col = "black") +
  tm_title("Harga Rata-rata Rumah di Area Jabodetabek") +
  tm_layout(legend.outside = TRUE)

