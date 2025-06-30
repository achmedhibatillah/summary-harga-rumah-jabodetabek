# CREATED BY ACHMED HIBATILLAH
# GUNAKAN UNTUK MEMPEROLEH DATA KANTOR DI AREA DKI JAKARTA SECARA REAL-TIME

cat("\014")

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(readr)

wilayah_list <- c("Jakarta Utara", "Jakarta Barat", "Jakarta Timur", "Jakarta Pusat", "Jakarta Selatan")
lokasi_list <- c(
  "kantor" = 'office'
)

get_area_id <- function(area_name) {
  query <- sprintf(
    '[out:json][timeout:25];
    area["name"="%s"]["boundary"="administrative"];
    out ids 1;',
    area_name
  )
  
  res <- httr::POST("http://overpass-api.de/api/interpreter", body = list(data = query), encode = "form")
  
  if (res$status_code != 200) {
    warning("Request failed with status: ", res$status_code)
    return(NA)
  }
  
  content_text <- httr::content(res, as = "text", encoding = "UTF-8")
  cat("Response content:\n", content_text, "\n")
  
  json <- tryCatch(jsonlite::fromJSON(content_text), error = function(e) {
    warning("Failed to parse JSON: ", e$message)
    return(NULL)
  })
  
  if (!is.null(json) && !is.null(json$elements) && nrow(json$elements) > 0) {
    return(json$elements$id[1])
  } else {
    return(NA)
  }
}

get_count <- function(tag, area_id) {
  query <- sprintf(
    '[out:json][timeout:25];
    area(%s)->.searchArea;
    (
      node["%s"](area.searchArea);
      way["%s"](area.searchArea);
      relation["%s"](area.searchArea);
    );
    out count;',
    area_id, tag, tag, tag
  )
  
  res <- httr::POST("http://overpass-api.de/api/interpreter", body = list(data = query), encode = "form")
  content_text <- httr::content(res, as = "text", encoding = "UTF-8")
  
  cat("Response:\n", content_text, "\n")
  
  json <- tryCatch({
    jsonlite::fromJSON(content_text, simplifyVector = FALSE)
  }, error = function(e) {
    cat("Gagal parsing JSON:\n", content_text, "\n")
    return(NULL)
  })
  
  print(json)
  
  if (!is.null(json) && !is.null(json$elements) && length(json$elements) > 0) {
    elem1 <- json$elements[[1]]
    print(elem1)
    if (is.list(elem1) && !is.null(elem1$tags) && "total" %in% names(elem1$tags)) {
      return(as.integer(elem1$tags$total))
    }
  }
  
  warning("Tidak ada total tag, return 0")
  return(0)
}



# Siapkan dataframe kosong
hasil <- data.frame(
  lokasi = names(lokasi_list),
  stringsAsFactors = FALSE
)

# Ambil semua area_id sekali saja
area_ids <- sapply(wilayah_list, get_area_id)

for (i in seq_along(lokasi_list)) {
  nama_lokasi <- names(lokasi_list)[i]
  tag <- lokasi_list[[i]]
  
  for (j in seq_along(wilayah_list)) {
    wilayah <- wilayah_list[j]
    area_id <- area_ids[j]
    cat(sprintf("Processing %s di %s (area_id=%s)...\n", nama_lokasi, wilayah, area_id))
    if (!is.na(area_id)) {
      jumlah <- get_count(tag, area_id)
    } else {
      jumlah <- NA
    }
    hasil[i, wilayah] <- jumlah
    Sys.sleep(1.5)
  }
}

write_csv(hasil, "location.csv")
