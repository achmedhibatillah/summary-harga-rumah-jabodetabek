# CREATED BY ACHMED HIBATILLAH

cat("\014")

#install.packages("rvest")
#install.packages("tidyr")
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
library(tidyr)
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
       select(title, price_in_rp, jumlah_lantai, luas_tanah, luas_total_m2, cluster, tipe_rumah, lokasi_khusus, status_kondisi, tipe_posisi, furnished, akses_tol, akses_stasiun, keunggulan_lain), 10)

write_csv(parsed_df, "parsed_file.csv")

# MEMBACA FILE BARU
df <- read_csv("parsed_file.csv")

df_clean <- df %>%
  filter(!is.na(price_in_rp)) %>%
  filter(str_detect(tolower(city), "jakarta"))

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
    fill.legend = tm_legend(title = "Rata-rata Harga Rumah DKI Jakarta")
  ) +
  tm_text("city_group", size = 0.7, col = "black") +
  tm_title("Harga Rata-rata Rumah di DKI Jakarta") +
  tm_layout(legend.outside = TRUE)

# MENENTUKAN HUBUNGAN
# INDEPENDEN: RATA-RATA HARGA RUMAH (mean_price)
# DEPENDEN: JUMLAH KANTOR (count_office)
lokasi <- read_csv("location.csv")

lokasi_long <- lokasi %>%
  pivot_longer(
    cols = -lokasi,
    names_to = "city_raw",
    values_to = "count_office"
  ) %>%
  filter(lokasi == "kantor") %>%
  select(-lokasi)

lokasi_long <- lokasi_long %>%
  mutate(city = tolower(city_raw)) %>%
  mutate(city = gsub(" ", " ", city))

data_merged <- summary_by_city %>%
  mutate(city = tolower(city)) %>%
  left_join(lokasi_long, by = "city")
print(data_merged)

# KORELASI mean_price dan count_office
correlation <- cor(data_merged$mean_price, data_merged$count_office, use = "complete.obs")
print(paste("Korelasi antara mean_price dan count_office:", correlation))

# REGRESI LINEAR
model <- lm(mean_price ~ count_office, data = data_merged)
summary(model)

# LOG-TRANSFORMASI VARIABEL
data_merged <- data_merged %>%
  mutate(log_mean_price = log(mean_price),
         log_count_office = log(count_office))

cor(data_merged$log_mean_price, data_merged$log_count_office)
summary(lm(log_mean_price ~ log_count_office, data = data_merged))

ggplot(data_merged, aes(x = count_office, y = mean_price)) +
  geom_point(size=4) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Hubungan Jumlah Kantor dan Harga Rumah Rata-rata",
       x = "Jumlah Kantor", y = "Harga Rata-rata") +
  theme_minimal()

ggplot(data_merged, aes(x = count_office, y = mean_price)) +
  geom_point(size=4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_x_log10() +
  labs(title = "Log-Scale: Harga vs Jumlah Kantor", x = "Log Jumlah Kantor", y = "Log Harga Rata-rata") +
  theme_minimal()
