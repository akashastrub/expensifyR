#' Classifier for subcategory data, using nearest neighbours and master file data
#'
#' @param df_new_expenses Dataframe of new expenses to be categorised.
#' @param path_category_dict Path to dictionary.
#' @param df_old_master_file Dataframe of old master master file.
#'
#' @return Categorised master file.
#' @import stringr readr purrr
#' @importFrom stats dist
#' @export
#'
classify_subcategories <-
  function(df_new_expenses,
           path_category_dict,
           df_old_master_file) {
    # Get shingles from words
    get_shingles <- function(x, k) {
      n_char <- nchar(x)
      # TODO lower x?
      x <-
        stringr::str_c(x, stringr::str_c(rep(" ", k - 1), collapse = ""))
      x <- stringr::str_split(x, "")[[1]]
      shingles <- character(length = n_char)
      for (i in 1:n_char) {
        shingles[i] <- stringr::str_c(x[i:(k + i - 1)], collapse = "")
      }
      return(unique(shingles))
    }

    # Find nearest word to classify
    find_nearest <- function(x, df) {
      sh <- get_shingles(x, k = 3)
      df_dist <- df %>%
        dplyr::rowwise() %>%
        dplyr::mutate(dist = compute_jaccard(desc_sh, sh)) %>%
        dplyr::arrange(desc(dist)) %>%
        dplyr::filter(dist > 0)
      # TODO improve logic!!!
      if (nrow(df_dist) > 0) {
        return(as.character(df_dist$subcategory[[1]]))
      } else {
        return(NA)
      }
    }

    # Compute jaccard
    compute_jaccard <- function(x, y) {
      intersection = length(intersect(x, y))
      union = length(x) + length(y) - intersection
      return (intersection / union)
    }

    # Read in category dictionary
    category_dict <- readr::read_csv(path_category_dict)

    # Training/latest master data pre-processing ####
    # Load in master data and process
    df_train <- df_old_master_file %>%
      dplyr::select(date,
                    description,
                    amount_eur,
                    subcategory)

    # Filter to only include subcategories with more than 2 observations
    df_train <- df_train %>%
      dplyr::group_by(subcategory) %>%
      dplyr::mutate(n_obs = n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n_obs > 2) %>%
      dplyr::select(-n_obs)

    # Encode as factors/classes
    df_train <- df_train %>%
      dplyr::mutate(subcategory = factor(subcategory))

    # Predict new data's subcategory ####
    df_sh <- df_train %>%
      dplyr::filter(nchar(description) > 0) %>%
      # TODO preprocess: filter out descs with different subcategories?
      dplyr::select(description, subcategory) %>%
      dplyr::distinct()

    df_sh <- df_sh %>%
      # TODO preprocess description
      dplyr::mutate(desc_sh = purrr::map(description, k = 3, get_shingles)) %>%
      dplyr::select(-description)

    df_sh_pred <- df_new_expenses %>%
      dplyr::select(date,
                    description,
                    amount_eur) %>%
      dplyr::mutate(subcategory = "") %>%
      dplyr::mutate(description = dplyr::if_else(is.na(description), " ", description)) %>%
      dplyr::select(description, subcategory)

    df_out_jac <- df_sh_pred %>%
      dplyr::mutate(subcategory = purrr::map(description, df = df_sh, find_nearest)) %>%
      dplyr::mutate(subcategory = as.character(subcategory))

    # Output
    df_new_expenses_classified <- df_new_expenses %>%
      dplyr::mutate(subcategory = df_out_jac$subcategory)

    return(df_new_expenses_classified)
  }
