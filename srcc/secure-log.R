
library(dplyr)
library(tidyr)
library(tidyverse)

getwd()

log = readLines("./data/log/secure-20200510")
log %>% head

test = df[1]



month = regmatches(log, regexpr("[a-zA-Z]+", log))
day = regmatches(log, regexpr("[0-9]+", log))
time = regmatches(log, regexpr("[0-9]+[0-9]+.[0-9]+[0-9]+.[0-9]+[0-9]+", log))

sol = regmatches(log, regexpr("[a-zA-Z]+[-]+[a-zA-Z]+.", log))
os = regmatches(log, regexpr("[a-zA-Z_]+.['[']+[0-9]+.", log))
os_num = regmatches(log, regexpr("[0-9]+[0-9]+[0-9]+", log))

content = regmatches(log, regexpr("[0-9]+[0-9]+[0-9]+.+.", log))
contents = regmatches(content, regexpr("[a-zA-Z]+.+", content))



df = cbind(month, day, time, sol, os, os_num, contents)
df = as.data.frame(df)
df %>% head






#regmatches(log, regexpr("[-_.+a-zA-Z0-9]+[@].+[a-zA-Z0-9]", test))


