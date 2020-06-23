# Import Excel spreadsheets -----------------------------------------------
read_excel_allsheets <- function(filename, tibble = FALSE) {

  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, data.frame())
  names(x) <- sheets
  x
}

table_data <- read_excel_allsheets("data-raw/excel-spreadsheets/table_data.xlsx", tibble=T)
table_index <- read_excel_allsheets("data-raw/excel-spreadsheets/table_index.xlsx", tibble=T)

tables <- list()
navne  <- names(table_index)
table_index <- table_index$`1 to 227`

for(i in 1:length(table_data)){
  new_name  <- names(table_data)[i]
  new_name2 <- table_index$`ID (cammel case less than 30 characters)`[which(table_index$Table == new_name)]

  final_name <- new_name2
  data_to_keep <- data.table::as.data.table(table_data[[i]])

  tables[[final_name]]  <- data_to_keep
}

table_index <- table_index[c(1:3)]
table_index <- data.table::as.data.table(table_index)
class(table_index)

names(table_index) <- c("Table", "Name", "ID (camel case)")
gb_ref <- tables
table_index_table <- table_index

for (t in names(gb_ref)){
  if ('Upper' %in% names(gb_ref[[t]])){
    gb_ref[[t]]$Upper[is.na(gb_ref[[t]]$Upper)] <- Inf
  }

  if ('Lower' %in% names(gb_ref[[t]])){
    gb_ref[[t]]$Lower[gb_ref[[t]]$Lower == 0] <- -Inf
  }

  if ('Higher' %in% names(gb_ref[[t]])){
    print(t)
  }
}

# Import example matrix data ----------------------------------------------
example_risk_matrix <- readRDS('data-raw/matrix_data_structure.rds')

save(gb_ref, example_risk_matrix, file = "R/sysdata.rda")
