df = paste0(getwd(),"/data/","raw.data.csv") %>% read.csv(header = TRUE)
glimpse(df)

df$intence = as.factor(df$intence)
df$apology.cost = as.factor(df$appology.cost)
df = df[,-(ncol(df) - 1)]
glimpse(df)

df$forgive = 
  Ryu.scaling(
    data = df,
    init.colName = "forgive",
    search.type = "start",
    return.type = "mean.vec",
    checkAlp = T, 
    delete.dfCol = F
  )
df$forgive = 6 - df$forgive
df$sincerity =
  Ryu.scaling(
    data = df,
    init.colName = "sincerity",
    search.type = "start",
    return.type = "mean.vec",
    checkAlp = T, 
    delete.dfCol = F
  )
glimpse(df)  

df$cost.num = 
  ifelse(df$apology.cost == "Non-Costly",0,1)
df$int.num = 
  ifelse(df$intence == "No-Intence",0,1)
df$sex = 
  ifelse(df$sex == 3, "unknown",
         ifelse(df$sex == 1,"female","male"))

df %>% 
  dplyr::select("forgive","sincerity",
                "apology.cost","intence",
                "age","sex") %>% 
  describe()
df %>% 
  dplyr::select("sex") %>% 
  table()

write.csv(df, "./data/data.csv")

