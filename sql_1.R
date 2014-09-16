
library(RODBC)
library(dplyr)


channel = odbcConnect("scm",uid = "apoorv.anand",pwd = "eru15ty047")
wwstr = sqlQuery(channel,"select * from amer_raw_wwstr_201514 ;")
chris = sqlQuery(channel,"select * from amer_raw_chris_master;")
