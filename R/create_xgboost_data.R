#' @title Create xgboost data

#' Function used in merge_covariates to convert site_id
#' from double to character
#' @param x A double (of length 1 with 14 digits or less)
#' @return A character with 14 digits (site_id format)
#' @author Eva Marques
double_to_site_id <- function(x) {
  ndigit <- floor(log10(x)) + 1
  stopifnot("too many digits" = ndigit <= 14)
  if (ndigit < 14) {
    return(paste0(
      paste(replicate(14 - ndigit, "0"), collapse = ""),
      as.character(x)
    ))
  } else {
    return(as.character(x))
  }
}

#' Function to merge covariates in 3 separate of data.frames
#' - one for only-spatial covariates (index = site_id)
#' - one for yearly spatial covariates (index = site_id, year)
#' - one for spatio-temporal covariates (index = site_id, time as days)
#' data.frames are saved as RDS files in the output folder
#' @param covar_folder path character to folder where covariates are stored.
#' This function will read and merge all data stored in
#'  NRTAP_Covars_*.rds files.
#' @author Eva Marques
merge_covariates <- function(covar_folder = "../output") {
  files <- list.files(covar_folder,
    pattern = "NRTAP_Covars_",
    full.names = TRUE
  )
  for (f in files) {
    df <- readRDS(f)
    if (typeof(df$site_id) == "double") {
      df$site_id <- lapply(df$site_id,
        FUN = function(x) {
          double_to_site_id(x)
        }
      )
    }
    df$site_id <- as.character(df$site_id)
    if (nrow(df[duplicated(df), ]) != 0) {
      cat("Warning: ", f, " has duplicated rows.\n")
      df <- df[!duplicated(df), ]
    }
    if ("time" %in% colnames(df)) {
      cat("spatio-temporal ", f, "\n")
      df$time <- as.Date(df$time)
      if (!exists("df_st")) {
        df_st <- df
      } else {
        df_st <- merge(df_st, df, by = c("site_id", "time"), all = TRUE)
      }
    } else if ("year" %in% colnames(df)) {
      cat("yearly ", f, "\n")
      if (!exists("df_y")) {
        df_y <- df
      } else {
        df_y <- merge(df_y, df, by = c("site_id", "year"), all = TRUE)
      }
    } else {
      cat("spatial ", f, "\n")
      if (!exists("df_s")) {
        df_s <- df
      } else {
        df_s <- merge(df_s, df, by = c("site_id"), all = TRUE)
      }
    }
  }
  saveRDS(object = df_st,
          file = paste0(covar_folder, "/df_covariates_spatiotemporal.rds"))
  saveRDS(object = df_s,
          file = paste0(covar_folder, "/df_covariates_spatial.rds"))
  saveRDS(object = df_y,
          file = paste0(covar_folder, "/df_covariates_yearly.rds"))
}
