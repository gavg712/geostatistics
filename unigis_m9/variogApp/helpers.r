list.of.packages <- c("geoR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


runGitHub("geostatistics", username = "gavg712", subdir = "unigis_m9/variogApp")

