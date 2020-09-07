setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
files <- list.files(pattern = "^MATH60604A.*\\.Rmd$", full.names = TRUE)
for(file in files){
  pagedown::chrome_print(file)
}
