glimpse(df)

# 小さなデータに分割する
subdata.n <- subset(df, intence== "No-Intence") #意図なし
subdata.i <- subset(df, intence== "Intence") #意図あり

# 相関検定のためのデータ準備
# 意図なし条件のデータ格納
r.sincerity.n <- cor(subdata.n$cost.num, subdata.n$sincerity)
r.forgive.n <- cor(subdata.n$cost.num, subdata.n$forgive)
r.sf.n <- cor(subdata.n$sincerity, subdata.n$forgive)
length.n <- length(subdata.n$cost.num)
# 意図あり条件のデータ格納
r.sincerity.i <- cor(subdata.i$cost.num, subdata.i$sincerity)
r.forgive.i <- cor(subdata.i$cost.num, subdata.i$forgive)
r.sf.i <- cor(subdata.i$sincerity, subdata.i$forgive)
length.i <- length(subdata.i$cost.num)
# 意図性無視の全データ格納
r.sincerity <- cor(df$cost.num, df$sincerity)
r.forgive <- cor(df$cost.num, df$forgive)
r.sf <- cor(df$sincerity, df$forgive)
length.all <- length(df$cost.num)


int.sin_cor = cor.test(subdata.i$sincerity,subdata.i$cost.num)
int.for_cor = cor.test(subdata.i$forgive,subdata.i$cost.num)
noint.sin_cor = cor.test(subdata.n$sincerity,subdata.n$cost.num)
noint.for_cor = cor.test(subdata.n$forgive,subdata.n$cost.num)
int.sin_cor

cor.Matrix = matrix(c("Malicious - S","Sincerity",int.sin_cor$estimate,int.sin_cor$conf.int[1],int.sin_cor$conf.int[2],
                      "Malicious - F","Forgiveness",int.for_cor$estimate,int.for_cor$conf.int[1],int.for_cor$conf.int[2],
                      "No Intention - S","Sincerity",noint.sin_cor$estimate,noint.sin_cor$conf.int[1],noint.sin_cor$conf.int[2],
                      "No Intention - F","Forgiveness",noint.for_cor$estimate,noint.for_cor$conf.int[1],noint.for_cor$conf.int[2]),
                    nrow = 4,byrow = T)
cor.Matrix
colnames(cor.Matrix) = c("id","Condition","cor","cor.conf.min","cor.conf.max")
cor.Matrix = as.data.frame(cor.Matrix)
cor.Matrix$cor = as.numeric(cor.Matrix$cor)
cor.Matrix$cor.conf.max = as.numeric(cor.Matrix$cor.conf.max)
cor.Matrix$cor.conf.min = as.numeric(cor.Matrix$cor.conf.min)


p = ggplot(data = cor.Matrix,
           aes(x = reorder(id,cor),
               y = cor,
               color = Condition)) + 
  labs(x = "",y = "Correlation") +
  # scale_y_reverse()+
  theme_classic() +
  geom_pointrange(
    mapping = aes(ymin = cor.conf.min,ymax = cor.conf.max)) +
  geom_hline(yintercept = 0, linetype = "longdash")+
  coord_flip()
print(p)

