# Prepare METAR data from Iowa State University

library(dplyr)
library(pmetar)

# Convert numerical wind direction to 8 cardinal directions
wd_to_cardinal <- function(degrees) {
  dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  levs <- c('CALM', 'VRB', dirs)
  card <- sapply(degrees, function(v) {
    if (is.na(v)) {
      c <- 'CALM'
    } else if (v == 0) {
      c <- 'VRB'
    } else {
      c <-  dirs[(as.integer((v + 22.5)/45.0) %% length(dirs)) + 1]
    }
    return(c)
  })
  factor(card, levels = levs)
}

get_season <- function(date) {
  levs <- c("Win", "Spr", "Sum", "Fall")
  factor(levs[(as.integer((date$mon + 1.5)/3.0) %% length(levs) + 1)],
         levels = levs)
}

# Extract WX codes from METAR
extract_wx_code <- function(x, sep = ";") {
  wx_codes <- metarWXcodes %>% dplyr::filter(Type != "Intensity") %>%
    dplyr::filter(Type != "Time") %>% dplyr::filter(Abbreviation != "")
  pattern_abbrev <- apply(wx_codes, 2, paste, collapse = "|")[2]
  out <- c(1:length(x))
  out[1:length(x)] <- ""
  x <- stringr::str_split_fixed(x, pattern = "RMK", n = 2)[, 1]
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[, 1]
  x <- stringr::str_split_fixed(x, pattern = "BECMG", n = 2)[, 1]
  x <- stringr::str_split(x, " RMK", simplify = TRUE)[, 1]
  x <- stringr::str_split(x, " TEMPO", simplify = TRUE)[, 1]
  x <- stringr::str_split(x, " BECMG", simplify = TRUE)[, 1]
  x <- stringr::str_split(x, " FM", simplify = TRUE)[, 1]
  fT <- stringr::str_detect(
    x, pattern = paste0("(\\s|[+]|[-]|RE)(", pattern_abbrev, ")+"))
  codes <- stringr::str_extract_all(
    x[fT],
    pattern = paste0("(\\s|[+]|[-]|RE)(", pattern_abbrev, ")+"),
    simplify = TRUE)
  codes <- apply(codes, 1, function(y) {
    if (all(y == "")) return("")
    y <- trimws(y[y != ""])
    paste(y[!startsWith(y, "RE")], collapse = sep)
  }, simplify = TRUE)
  out[fT] <- codes
  out
}

get_min_vis <- function(x) {
  swith <- function(x) grepl(substr(x, 1, 1), "0123456789", fixed = TRUE)
  ewith <- function(x) grepl(substr(x, nchar(x), nchar(x)), "NESW", fixed = TRUE)
  ret <- sapply(x, function(m) {
    v <- metar_visibility(m, numeric_only = TRUE)
    m <- strsplit(m, " ", fixed = TRUE)[[1]]
    i <- which(m == as.character(v))
    if (length(i) > 0) {
      i <- i[1] + 1
      if (swith(m[i]) & ewith(m[i]))
        v <- as.numeric(gsub("\\D", "", m[i]))
    }
    v
  })
  names(ret) <- NULL
  ret
}

metar <- metar_get_historical( # Download METAR
  "LTFM",
  start_date = "2019-01-01",
  end_date = "2022-12-31",
  from = "iastate")

# Fix BECMG error for pmetar package
metar <- stringr::str_split_fixed(metar, pattern = "BECMG", n = 2)[, 1]

metar <- as.data.frame(metar_decode( # Decode METAR
  metar, numeric_only = TRUE))[c(3, 7, 12:15, 17, 28)]

# Set column names
colnames(metar) <- c("date", "ws", "wd", "temp", "dew", "pres", "vis", "code")

metar <- metar[complete.cases(metar),] # remove NA rows

# Get minimum visibility from METAR code
metar$vis <- get_min_vis(metar$code)

metar <- within( # Organize data
  metar,
  {
    date <- as.POSIXlt(date, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')
    min <- factor(date$min)
    hour <- factor(date$hour)
    day <- factor(date$mday)
    month <- factor(month.abb[date$mon + 1], levels = month.abb)
    season <- get_season(date)
    year <- factor(1900 + date$year)
    pres_anom <- pres - mean(pres)
    spread = temp - dew
    wx <- extract_wx_code(metar$code)
    is_foggy <- grepl('FG', wx) # Find Foggy days
    fog_type <- factor(
      ifelse(grepl('BCFG', wx), 'BCFG',
             ifelse(grepl('PRFG', wx), 'PRFG',
                    ifelse(grepl('FG', wx), 'FG', 'OTHER'))),
      levels = c('FG', 'PRFG', 'BCFG', 'OTHER'))
    wd = wd_to_cardinal(wd)
    vislev = cut(vis, c(0, 999, 4999, 10000), c('low', 'medium', 'high'))
  }
)

metar <- metar[c(1, 15:19, 2:5, 13, 6, 14, 7, 9:12, 8)]

saveRDS(metar, "data/metar.rds")

#
# To prepare data:
# SEE: "01-prepare_data.R"
#
# To understand what is confusion matrix,
# SEE: "confusion_matrix.R"
#
# To have an idea of overview of the data,
# SEE: "03-eda.R"
#
# If you want to see why Linear model is not suitable for prediction,
# SEE: "04-reg_linear.R"
#
# If you want to see results of binomial logistic regression and compare
# with linear regression:
# SEE: '05-reg_logistic.R'
#
# If you want to have an idea how CART models behave for this data set,
# SEE: "06-CART.R"
#
