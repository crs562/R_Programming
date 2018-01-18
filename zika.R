# The CDC Epidemic Prediction Initiative has a GitHub repository with data on Zika cases, including files on cases in Brazil.

library(readr)

zika_file <- "https://raw.githubusercontent.com/cdcepi/zika/master/Brazil/COES_Microcephaly/data/COES_Microcephaly-2016-06-25.csv"

zika_brazil <- read_csv(zika_file)

zika_brazil %>%
  select(location, value, unit)
