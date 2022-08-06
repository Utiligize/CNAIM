

check_gb_ref_given <- function(gb_ref_given){
  if(!setequal(names(gb_ref_given), names(gb_ref))) stop("Table list names not matching the desired names")
  for(table_name in names(gb_ref_given)){
    if(!setequal(names(gb_ref_given[[table_name]]), names(gb_ref[[table_name]]))){
      paste0("Col names not matching in table ", table_name) %>% stop()
    }
  }
}
