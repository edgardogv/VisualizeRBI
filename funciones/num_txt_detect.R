num_txt_detect <- function(df){
  df <- as.data.frame(df)
  temp_list <- lapply(df, function(x){
    if(is.numeric(x)){
      return("numeric")
    } else if(is.character(x) | is.factor(x)){
      return("character")
    }
  }) %>% unlist()
  return(list(Numericas = which(temp_list == "numeric"),
              Texto = which(temp_list == "character")))
}