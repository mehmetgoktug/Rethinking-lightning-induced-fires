################################################################################
# title: Tidy Fire Data
# author: Mehmet Göktuğ Öztürk - İsmail Bekar
# project: Rethinking lightning-induced fires: Spatial variability and 
#          implications for management policies
################################################################################

# load packages ----------------------------------------------------------------
libs <- c("readr", "dplyr", "tidyr", "stringr", "janitor", "lubridate")
sapply(libs, require, character.only = TRUE) |> suppressPackageStartupMessages()

# function of convert to turkish character
tr_char <- function(data) {
  if (is.data.frame(data)) {
    data <- data |>
      mutate(
        across(where(is.character), 
               ~stringi::stri_trans_general(
                 stringi::stri_trans_tolower(.x, "tr-TR"), "Latin-ASCII")
        )
      )
  } else {
    data <- stringi::stri_trans_general(
      stringi::stri_trans_tolower(data, "tr-TR"), "Latin-ASCII"
    )
  }
  return(data)
}

# 2002 -------------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# read wildfire data
fire_02 <- read_csv("./data/input/ogm/fire_data/fire_2002.csv", skip = 1) 

# convert to tidy data
fire_02 <- fire_02 |> 
  clean_names() |>
  select(
    -c(
      devam_suresi,
      yanma_sekli,
      orman_durumu,
      yangin_no
    )
  ) |>
  rename(
    obm = bolge,
    oim = isletme,
    ois = seflik,
    primary_cause = cikis_ned,
    burnt_area = yanan_alan,
  ) |>
  unite(col = "start_date", cikis_tarihi:saati_6, sep = " ") |>
  unite(col = "end_date", sonus_tarihi:saati_9, sep = " ") |>
  mutate(
    start_date = parse_date_time(
      start_date, 
      orders = c("%d.%m.%y %H", "%d.%m.%y %H.%M", "%d.%m.%y %H.%M.%S")
    ),
    end_date = parse_date_time(
      end_date, 
      orders = c("%d.%m.%y %H", "%d.%m.%y %H.%M", "%d.%m.%y %H.%M.%S")
    ),
    duration = end_date - start_date,
    month = lubridate::month(start_date),
    year = lubridate::year(start_date)) |>
  mutate(
    season = case_when(
      month %in% 09:11 ~ "fall",
      month %in% c(12, 01, 02) ~ "winter",
      month %in% 03:05 ~ "spring",
      TRUE ~ "summer"
    )
  ) |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(across(where(is.character), tr_char)) |> 
  (\(.) mutate(
    .,
    primary_cause = case_when(
      grepl("y", primary_cause) ~ "lightning",
      grepl("^m", primary_cause) ~ "unknown",
      .default = "human"
    )))() |> 
  relocate(start_date, .after = "primary_cause") |>
  relocate(end_date, .after = "start_date") |>
  relocate(duration, .after = "end_date") |>
  relocate(month, .after = "duration") |> 
  relocate(season, .after = "month") |> 
  relocate(year, .after = "season") |>
  drop_na()

# 2003-2012 --------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# read wildfire data
files_03_12 <- list.files("./data/input/ogm/fire_data/", full.names = TRUE)[3:12]
fire_03_12 <- lapply(files_03_12, read_csv, skip = 2)

# convert to tidy data
fire_03_12 <- lapply(
  fire_03_12,
  function(x) {
    x |>
      clean_names() |>
      select(
        c(
          bolge_mudurlugu,
          isletmesi,
          sefligi,
          cikis_tarihi,
          cikis_saati,
          cikis_sebebi,
          sonus_tarihi,
          s_saati,
          yanan_alan
        )
      ) |> 
      rename(
        obm = bolge_mudurlugu,
        oim = isletmesi,
        ois = sefligi,
        primary_cause = cikis_sebebi,
        burnt_area = yanan_alan
      ) |>
      drop_na(ois) |>
      mutate(
        across(
          c(cikis_tarihi, cikis_saati, sonus_tarihi, s_saati), as.character
        )
      ) |>
      mutate(across(where(is.character), ~ str_replace_all(.x, ",", "."))) |>
      mutate(across(where(is.character), ~ str_replace_all(.x, ":", "."))) |>
      unite(col = "start_date", cikis_tarihi:cikis_saati, sep = " ") |>
      unite(col = "end_date", sonus_tarihi:s_saati, sep = " ") |>
      mutate(
        start_date = parse_date_time(
          start_date, 
          orders = c("%d.%m.%y %H", "%d.%m.%y %H.%M", "%d.%m.%y %H.%M.%S")
        ),
        end_date = parse_date_time(
          end_date, 
          orders = c("%d.%m.%y %H", "%d.%m.%y %H.%M", "%d.%m.%y %H.%M.%S")
        ),
        duration = end_date - start_date,
        month = lubridate::month(start_date),
        year = lubridate::year(start_date)
      ) |>
      mutate(
        season = case_when(
          month %in% 09:11 ~ "fall",
          month %in% c(12, 01, 02) ~ "winter",
          month %in% 03:05 ~ "spring",
          TRUE ~ "summer"
        )
      ) |>
      mutate(across(where(is.character), str_trim)) |>
      mutate(across(where(is.character), tr_char)) |> 
      (\(.) mutate(
        .,
        primary_cause = case_when(
          primary_cause == "dogal ndlr" ~ "lightning",
          primary_cause == "dogal-yildirim" ~ "lightning",
          primary_cause == "y" ~ "lightning",
          primary_cause == "dogal" ~ "lightning",
          primary_cause == "bilinmeyen" ~ "unknown",
          primary_cause == "13.10bilinmeyen" ~ "unknown",
          primary_cause == "m" ~ "unknown",
          .default = "human"
        )))() |> 
      relocate(start_date, .after = "primary_cause") |>
      relocate(end_date, .after = "start_date") |>
      relocate(duration, .after = "end_date") |>
      relocate(month, .after = "duration") |> 
      relocate(season, .after = "month") |> 
      relocate(year, .after = "season") 
  }
) |> purrr::list_rbind() |>
  drop_na()

# 2013-2017 --------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# read wildfire data
fire_13_17 <- read_csv("./data/input/ogm/fire_data/fire_2013_2017.csv")

# convert to tidy data
fire_13_17 <- fire_13_17  |>  
  filter(Adet == 1) |> 
  clean_names() |>
  select(
    bolge_mudurlugu,
    isletme_mudurlugu,
    isletme_sefligi,
    yangin_cikisi,
    yangin_ana_nedeni,
    sondurulme_zamani,
    yanan_alan,
    kuzey,
    dogu
  ) |>
  rename(
    obm = bolge_mudurlugu,
    oim = isletme_mudurlugu,
    ois = isletme_sefligi,
    primary_cause = yangin_ana_nedeni,
    burnt_area = yanan_alan, 
    y = kuzey,
    x = dogu
  ) |> 
  mutate(
    start_date = lubridate::dmy_hm(yangin_cikisi),
    end_date = lubridate::dmy_hm(sondurulme_zamani),
    duration = end_date - start_date,
    month = lubridate::month(start_date),
    year = lubridate::year(start_date),
    x = str_replace_all(x, "°", " "),
    x = str_replace_all(x, "'", " "),
    x = str_replace_all(x, "''", ""),
    y = str_replace_all(y, "°", " "),
    y = str_replace_all(y, "'", " "),
    y = str_replace_all(y, "''", "")  
  ) |>  
  separate_wider_delim(
    x,
    delim = " ",
    names = c("d_x", "m_x", "s_x"),
    too_many = "drop"
  ) |>
  separate_wider_delim(
    y,
    delim = " ",
    names = c("d_y", "m_y", "s_y"),
    too_many = "drop"
  ) |>
  mutate(across(c("d_x", "m_x", "s_x", "d_y", "m_y", "s_y"), as.numeric)) |>
  mutate(
    x = d_x + m_x / 60 + s_x / 60 ^ 2,
    y = d_y + m_y / 60 + s_y / 60 ^ 2
  ) |> 
  mutate(
    season = case_when(
      month %in% 09:11 ~ "fall",
      month %in% c(12, 01, 02) ~ "winter",
      month %in% 03:05 ~ "spring",
      TRUE ~ "summer"
    )
  ) |>
  mutate(burnt_area = as.numeric(burnt_area)) |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(across(where(is.character), tr_char)) |> 
  (\(.) mutate(
    .,
    primary_cause = case_when(
      primary_cause == "dogal yildirim" ~ "lightning",
      primary_cause == "mechul" ~ "unknown",
      .default = "human"
    )))() |> 
  select(
    -yangin_cikisi, 
    -sondurulme_zamani,
    -d_x, 
    -m_x, 
    -s_x, 
    -d_y, 
    -m_y, 
    -s_y
    ) |>
  relocate(start_date, .after = "primary_cause") |>
  relocate(end_date, .after = "start_date") |>
  relocate(duration, .after = "end_date") |>
  relocate(month, .after = "duration") |> 
  relocate(season, .after = "month") |> 
  relocate(y, .after = "end_date") |>
  relocate(x, .after = "y")

# 2018 -------------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# due to the non-standardisation of the data, I have organised them separately
# read wildfire data
fire_18 <- read_csv("./data/input/ogm/fire_data/fire_2018.csv")

# convert to tidy data
fire_18 <- fire_18  |>  
  filter(Adet == 1) |> 
  clean_names() |>
  select(
    bolge_mudurlugu,
    isletme_mudurlugu,
    isletme_sefligi,
    yangin_cikisi,
    yangin_ana_nedeni,
    sondurulme_zamani,
    yanan_alan,
    kuzey,
    dogu
  ) |>
  rename(
    obm = bolge_mudurlugu,
    oim = isletme_mudurlugu,
    ois = isletme_sefligi,
    primary_cause = yangin_ana_nedeni,
    burnt_area = yanan_alan, 
    y = kuzey,
    x = dogu
  ) |> 
  mutate(
    sondurulme_zamani = excel_numeric_to_date(
      as.numeric(sondurulme_zamani),
      include_time = TRUE
    )
  ) |>
  mutate(
    start_date = lubridate::ymd_hms(yangin_cikisi),
    end_date = lubridate::ymd_hms(sondurulme_zamani),
    duration = end_date - start_date,
    month = lubridate::month(start_date),
    year = lubridate::year(start_date),
    x = str_replace_all(x, "°", " "),
    x = str_replace_all(x, "'", " "),
    x = str_replace_all(x, "''", ""),
    y = str_replace_all(y, "°", " "),
    y = str_replace_all(y, "'", " "),
    y = str_replace_all(y, "''", "")  
    ) |>
  separate_wider_delim(
    x,
    delim = " ",
    names = c("d_x", "m_x", "s_x"),
    too_many = "drop"
  ) |>
  separate_wider_delim(
    y,
    delim = " ",
    names = c("d_y", "m_y", "s_y"),
    too_many = "drop"
  ) |>
  mutate(across(c("d_x", "m_x", "s_x", "d_y", "m_y", "s_y"), as.numeric)) |>
  mutate(
    x = d_x + m_x / 60 + s_x / 60 ^ 2,
    y = d_y + m_y / 60 + s_y / 60 ^ 2
  ) |> 
  mutate(
    season = case_when(
      month %in% 09:11 ~ "fall",
      month %in% c(12, 01, 02) ~ "winter",
      month %in% 03:05 ~ "spring",
      TRUE ~ "summer"
    )
  ) |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(across(where(is.character), tr_char)) |> 
  (\(.) mutate(
    .,
    primary_cause = case_when(
      primary_cause == "dogal yildirim" ~ "lightning",
      primary_cause == "mechul" ~ "unknown",
      .default = "human"
    )))() |> 
  select(
    -yangin_cikisi, 
    -sondurulme_zamani, 
    -d_x, 
    -m_x, 
    -s_x, 
    -d_y, 
    -m_y, 
    -s_y
  ) |>
  relocate(start_date, .after = "primary_cause") |>
  relocate(end_date, .after = "start_date") |>
  relocate(duration, .after = "end_date") |>
  relocate(month, .after = "duration") |> 
  relocate(season, .after = "month") |>
  relocate(y, .after = "end_date") |>
  relocate(x, .after = "y")

# 2019 -------------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# read wildfire data
fire_19 <- read_csv("./data/input/ogm/fire_data/fire_2019.csv", skip = 1)

# convert to tidy data
fire_19 <- fire_19  |>  
  clean_names() |>
  drop_na(bolge_mudurlugu) |>
  select(
    bolge_mudurlugu,
    isletme_mudurlugu,
    isletme_sefligi,
    yangin_baslama_zamani,
    cikis_nedeni,
    sondurulme_zamani,
    yanan_alan,
    kuzey,
    dogu
  ) |>
  rename(
    obm = bolge_mudurlugu,
    oim = isletme_mudurlugu,
    ois = isletme_sefligi,
    primary_cause = cikis_nedeni,
    burnt_area = yanan_alan, 
    y = kuzey,
    x = dogu
  ) |> 
  mutate(
    start_date = lubridate::dmy(yangin_baslama_zamani),
    end_date = lubridate::dmy(sondurulme_zamani),
    duration = end_date - start_date,
    month = lubridate::month(start_date),
    year = lubridate::year(start_date)
  ) |>  
  separate_wider_delim(
    x,
    delim = ":",
    names = c("d_x", "m_x", "s_x"),
    too_few = "align_start"
  ) |>
  separate_wider_delim(
    y,
    delim = ":",
    names = c("d_y", "m_y", "s_y"),
    too_few = "align_start"
  ) |>
  mutate(across(c("d_x", "m_x", "s_x", "d_y", "m_y", "s_y"), as.numeric)) |>
  mutate(
    x = d_x + m_x / 60 + s_x / 60 ^ 2,
    y = d_y + m_y / 60 + s_y / 60 ^ 2
  ) |> 
  mutate(
    season = case_when(
      month %in% 09:11 ~ "fall",
      month %in% c(12, 01, 02) ~ "winter",
      month %in% 03:05 ~ "spring",
      TRUE ~ "summer"
    )
  ) |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(across(where(is.character), tr_char)) |> 
  filter(primary_cause != "yeniden yanma") |>
  (\(.) mutate(
    .,
    primary_cause = case_when(
      primary_cause == "dogal nedenler" ~ "lightning",
      primary_cause == "faili mechul(nedeni bilinmeyen)" ~ "unknown",
      .default = "human"
    )))() |> 
  select(
    -yangin_baslama_zamani, 
    -sondurulme_zamani,
    -d_x, 
    -m_x, 
    -s_x, 
    -d_y, 
    -m_y, 
    -s_y
  ) |>
  relocate(start_date, .after = "primary_cause") |>
  relocate(end_date, .after = "start_date") |>
  relocate(duration, .after = "end_date") |>
  relocate(month, .after = "duration") |> 
  relocate(season, .after = "month") |>
  relocate(y, .after = "end_date") |>
  relocate(x, .after = "y")


# 2020 -------------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# read wildfire data
fire_20 <- read_csv("./data/input/ogm/fire_data/fire_2020.csv", skip = 1)

# convert to tidy data
fire_20 <- fire_20  |>  
  clean_names() |>
  drop_na(bolge_mudurlugu) |>
  select(
    bolge_mudurlugu,
    isletme_mudurlugu,
    isletme_sefligi,
    yangin_baslama_zamani,
    cikis_nedeni,
    sondurulme_zamani,
    yanan_alan,
    kuzey,
    dogu
  ) |>
  rename(
    obm = bolge_mudurlugu,
    oim = isletme_mudurlugu,
    ois = isletme_sefligi,
    primary_cause = cikis_nedeni,
    burnt_area = yanan_alan,
    y = kuzey,
    x = dogu
  ) |> 
  mutate(
    start_date = lubridate::dmy(yangin_baslama_zamani),
    end_date = lubridate::dmy(sondurulme_zamani),
    duration = end_date - start_date,
    month = lubridate::month(start_date),
    year = lubridate::year(start_date)
  ) |>  
  separate_wider_delim(
    x,
    delim = ":",
    names = c("d_x", "m_x", "s_x"),
    too_few = "align_start"
  ) |>
  separate_wider_delim(
    y,
    delim = ":",
    names = c("d_y", "m_y", "s_y"),
    too_few = "align_start"
  ) |>
  mutate(across(c("d_x", "m_x", "s_x", "d_y", "m_y", "s_y"), as.numeric)) |>
  mutate(
    x = d_x + m_x / 60 + s_x / 60 ^ 2,
    y = d_y + m_y / 60 + s_y / 60 ^ 2
  ) |> 
  mutate(
    season = case_when(
      month %in% 09:11 ~ "fall",
      month %in% c(12, 01, 02) ~ "winter",
      month %in% 03:05 ~ "spring",
      TRUE ~ "summer"
    )
  ) |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(across(where(is.character), tr_char)) |> 
  (\(.) mutate(
    .,
    primary_cause = case_when(
      primary_cause == "dogal nedenler" ~ "lightning",
      primary_cause == "faili mechul(nedeni bilinmeyen)" ~ "unknown",
      .default = "human"
    )))() |> 
  select(
    -yangin_baslama_zamani, 
    -sondurulme_zamani, 
    -d_x, 
    -m_x, 
    -s_x, 
    -d_y, 
    -m_y, 
    -s_y
  ) |>
  relocate(start_date, .after = "primary_cause") |>
  relocate(end_date, .after = "start_date") |>
  relocate(duration, .after = "end_date") |>
  relocate(month, .after = "duration") |> 
  relocate(season, .after = "month") |>
  relocate(y, .after = "end_date") |>
  relocate(x, .after = "y")


# 2021 -------------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# read wildfire data
fire_21 <- read_csv("./data/input/ogm/fire_data/fire_2021.csv", skip = 1)

# convert to tidy data
fire_21 <- fire_21  |>  
  clean_names() |>
  drop_na(bolge_mudurlugu) |>
  select(
    bolge_mudurlugu,
    isletme_mudurlugu,
    isletme_sefligi,
    baslama_tarihi,
    cikis_nedeni,
    kontrol_tarihi,
    yanan_alan,
    kuzey,
    dogu
  ) |>
  rename(
    obm = bolge_mudurlugu,
    oim = isletme_mudurlugu,
    ois = isletme_sefligi,
    primary_cause = cikis_nedeni,
    burnt_area = yanan_alan, 
    y = kuzey,
    x = dogu
  ) |> 
  mutate(
    start_date = lubridate::dmy(baslama_tarihi),
    end_date = lubridate::dmy(kontrol_tarihi),
    duration = end_date - start_date,
    month = lubridate::month(start_date),
    year = lubridate::year(start_date)
  ) |>  
  separate_wider_delim(
    x,
    delim = ":",
    names = c("d_x", "m_x", "s_x"),
    too_few = "align_start"
  ) |>
  separate_wider_delim(
    y,
    delim = ":",
    names = c("d_y", "m_y", "s_y"),
    too_few = "align_start"
  ) |>
  mutate(across(c("d_x", "m_x", "s_x", "d_y", "m_y", "s_y"), as.numeric)) |>
  mutate(
    x = d_x + m_x / 60 + s_x / 60 ^ 2,
    y = d_y + m_y / 60 + s_y / 60 ^ 2
  ) |> 
  mutate(
    season = case_when(
      month %in% 09:11 ~ "fall",
      month %in% c(12, 01, 02) ~ "winter",
      month %in% 03:05 ~ "spring",
      TRUE ~ "summer"
    )
  ) |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(across(where(is.character), tr_char)) |> 
  filter(primary_cause != "yeniden yanma") |>
  (\(.) mutate(
    .,
    primary_cause = case_when(
      primary_cause == "dogal nedenler" ~ "lightning",
      primary_cause == "nedeni bilinmeyen" ~ "unknown",
      .default = "human"
    )))() |> 
  select(
    -baslama_tarihi, 
    -kontrol_tarihi,
    -d_x, 
    -m_x, 
    -s_x, 
    -d_y, 
    -m_y, 
    -s_y
  ) |>
  relocate(start_date, .after = "primary_cause") |>
  relocate(end_date, .after = "start_date") |>
  relocate(duration, .after = "end_date") |>
  relocate(month, .after = "duration") |> 
  relocate(season, .after = "month") |>
  relocate(y, .after = "end_date") |>
  relocate(x, .after = "y")

# 2022 -------------------------------------------------------------------------
## read wildfire data and convert the data to tidy data ------------------------
# read wildfire data
fire_22 <- read_csv("./data/input/ogm/fire_data/fire_2022.csv", skip = 1)

# convert to tidy data
fire_22 <- fire_22  |>  
  clean_names() |>
  drop_na(bolge_mudurlugu) |>
  select(
    bolge_mudurlugu,
    isletme_mudurlugu,
    isletme_sefligi,
    baslama_tarihi,
    cikis_nedeni,
    kontrol_tarihi,
    yanan_alan,
    kuzey,
    dogu
  ) |>
  rename(
    obm = bolge_mudurlugu,
    oim = isletme_mudurlugu,
    ois = isletme_sefligi,
    primary_cause = cikis_nedeni,
    burnt_area = yanan_alan, 
    y = kuzey,
    x = dogu
  ) |> 
  mutate(
    start_date = lubridate::dmy(baslama_tarihi),
    end_date = lubridate::dmy(kontrol_tarihi),
    duration = end_date - start_date,
    month = lubridate::month(start_date),
    year = lubridate::year(start_date)
  ) |>  
  separate_wider_delim(
    x,
    delim = ":",
    names = c("d_x", "m_x", "s_x"),
    too_few = "align_start"
  ) |>
  separate_wider_delim(
    y,
    delim = ":",
    names = c("d_y", "m_y", "s_y"),
    too_few = "align_start"
  ) |>
  mutate(across(c("d_x", "m_x", "s_x", "d_y", "m_y", "s_y"), as.numeric)) |>
  mutate(
    x = d_x + m_x / 60 + s_x / 60 ^ 2,
    y = d_y + m_y / 60 + s_y / 60 ^ 2
  ) |> 
  mutate(
    season = case_when(
      month %in% 09:11 ~ "fall",
      month %in% c(12, 01, 02) ~ "winter",
      month %in% 03:05 ~ "spring",
      TRUE ~ "summer"
    )
  ) |>
  mutate(across(where(is.character), str_trim)) |>
  mutate(across(where(is.character), tr_char)) |> 
  filter(primary_cause != "yeniden yanma") |>
  (\(.) mutate(
    .,
    primary_cause = case_when(
      primary_cause == "dogal nedenler" ~ "lightning",
      primary_cause == "nedeni bilinmeyen" ~ "unknown",
      .default = "human"
    )))() |> 
  select(
    -baslama_tarihi, 
    -kontrol_tarihi,
    -d_x, 
    -m_x, 
    -s_x, 
    -d_y, 
    -m_y, 
    -s_y
  ) |>
  relocate(start_date, .after = "primary_cause") |>
  relocate(end_date, .after = "start_date") |>
  relocate(duration, .after = "end_date") |>
  relocate(month, .after = "duration") |> 
  relocate(season, .after = "month") |>
  relocate(y, .after = "end_date") |>
  relocate(x, .after = "y")

# merge tables -----------------------------------------------------------------
merge_without_coords <- rbind(fire_02, fire_03_12) |> 
  drop_na() |> 
  filter(
    duration > 0 & 
      start_date > "2002-01-01 00:00:00.00" &
      end_date < "2023-02-01 00:00:00.00"
  ) |>
  mutate(y = NA, x = NA) |>
  relocate(y, .after = "end_date") |> 
  relocate(x, .after = "y")
  

merge_with_coords <- rbind(
  fire_13_17, 
  fire_18, 
  fire_19, 
  fire_20, 
  fire_21, 
  fire_22
  ) |> 
  drop_na()

fire_tidy <- bind_rows(merge_without_coords, merge_with_coords)

# create new directory and write data ------------------------------------------
# create ogm directory into the output data
if (!file.exists("./data/output/ogm/fire_data")) {
  dir.create("./data/output/ogm/fire_data", recursive = TRUE)
}

# write tidy fire data
write_csv(fire_tidy, "./data/output/ogm/fire_data/fire_tidy1.csv")

