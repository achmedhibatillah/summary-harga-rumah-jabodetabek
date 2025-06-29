cat("\014")

install.packages("rvest")
install.packages("readr")
install.packages("dplyr")
install.packages("stringr")
install.packages("jsonlite")
install.packages("pbapply")
install.packages("ggplot2")
install.packages("sf")
install.packages("tmap")
install.packages("geodata")

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tmap)
library(geodata)

# MENGOLAH DATA MENTAH DENGAN MEMPARSINGNYA
df <- read_csv("data.csv")

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

summary_by_area <- df_clean %>%
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

print(summary_by_area)

summary_by_area <- summary_by_area %>%
  arrange(city, district)
View(summary_by_area)

