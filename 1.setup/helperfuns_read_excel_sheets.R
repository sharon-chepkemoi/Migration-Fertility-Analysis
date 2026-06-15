#--------------------------------------------------------------------
# Function to read all excel sheets at once 
#---------------------------------------------------------------------



library(readxl)

## read all sheets in excel file
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename) #List all sheets in an excel spreadsheet
  out <- base::lapply(sheets, function(x) {
    readxl::read_excel(filename, 
                       sheet = x, 
                       col_types = NULL, 
                       na = "", 
                       trim_ws = TRUE
                       )
  }
  ) 
  base::names(out) <- sheets
  out
}
