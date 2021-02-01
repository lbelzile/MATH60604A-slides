# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# files <- list.files(pattern = "^MATH60604A.*\\.Rmd$", full.names = TRUE)
# for(file in files){
#   pagedown::chrome_print(file)
# }

## NEW solution that prints panelsets properly
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# source("https://git.io/xaringan2pdf")
library(xaringanBuilder)
# library(chromote)
files <- paste0("https://lbelzile.github.io/MATH60604A-slides/",
                list.files(include.dirs = TRUE,
                            pattern = "^MATH60604A.*\\.html$",
                            full.names = FALSE))
for(file in files){
  xaringanBuilder::xaringan_to_pdf(input = file,
                  output_file = paste0(tools::file_path_sans_ext(basename(file)), ".pdf"),
                  delay = 0.15,
                  include_partial_slides = FALSE)
}
