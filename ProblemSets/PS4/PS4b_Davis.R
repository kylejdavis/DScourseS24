library(tidyverse)
library(sparklyr)

spark_install(version = '3.0.0')
sc <- spark_connect(master = 'local')
df1 <- as_tibble(iris)
df <- copy_to(sc, df1)
