# Nearest neighbours classifier using all data in latest master file



# Classification function
classify_subcategories <- function(df_new_expenses, path_category_dict, latest_master_file) {
  
  # Get shingles from words
  get_shingles <- function(x, k){
    n_char <- nchar(x)
    # TODO lower x?
    x <- stringr::str_c(x, stringr::str_c(rep(" ", k - 1), collapse=""))
    x <- stringr::str_split(x, "")[[1]]
    shingles <- character( length = n_char )
    for( i in 1:n_char ) {
      shingles[i] <- stringr::str_c(x[i:(k + i - 1)], collapse="")
    }
    return(unique(shingles))
  }
  
  # Find nearest word to classify
  find_nearest <- function(x, df) {
    sh <- get_shingles(x, k=3)
    df_dist <- df %>% rowwise() %>% mutate(dist = compute_jaccard(desc_sh, sh)) %>% arrange(desc(dist)) %>% filter(dist > 0)
    # TODO improve logic!!!
    if (nrow(df_dist) > 0) {
      return(as.character(df_dist$subcategory[[1]]))
    } else {return(NA)}
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
  df_train <- latest_master_file %>%
    select(
      date, 
      description, 
      amount_eur, 
      # location, 
      subcategory
    )
  
  # Filter to only include subcategories with more than 2 observations
  df_train <- df_train %>% 
    # TODO review
    # TODO is there a one-liner?
    group_by(subcategory) %>%
    mutate(n_obs = n()) %>%
    ungroup() %>%
    filter(n_obs > 2) %>%
    select(-n_obs)
  
  # Encode as factors/classes
  df_train <- df_train %>%
    mutate(subcategory = factor(subcategory))
  
  # predict new data's subcategory ####
  df_sh <- df_train %>%
    filter(nchar(description) > 0) %>%
    # TODO preprocess: filter out descs with different subcategories?
    select(description, subcategory) %>%
    distinct()
  
  df_sh <- df_sh %>%
    # TODO preprocess description
    mutate(desc_sh = purrr::map(description, k=3, get_shingles)) %>%
    select(-description)
  
  df_sh_pred <- df_new_expenses %>% 
    select(date, 
           description, 
           amount_eur) %>%
    mutate(subcategory = "") %>% 
    mutate(description = if_else(is.na(description), " ", description)) %>%
    select(description, subcategory)
  
  df_out_jac <- df_sh_pred %>%
    mutate(subcategory = purrr::map(description, df=df_sh, find_nearest)) %>%
    mutate(subcategory = as.character(subcategory))

  # OUTPUT
  df_new_expenses_classified <- df_new_expenses %>% 
    mutate(subcategory = df_out_jac$subcategory)
  
  return(df_new_expenses_classified)
}


