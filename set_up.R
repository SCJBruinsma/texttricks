library("devtools")
library(roxygen2)

setwd("~/Dropbox/Texttricks/texttricks")
document()

setwd("..")
install("texttricks")


library("texttricks")
extract_pdf("BDA2009_10.pdf")
