library(gridExtra)
glimpse(df)

# X軸のタイトル
xtitle = "Malicious               No　　"
# 誠意の知覚の期待を従属変数とした条件別のプロット
sincerity.plt = 
  ggplot(data=df,
         aes(x=interaction(apology.cost,intence), 
             y=sincerity,
             color=apology.cost)) +
  theme_classic() +
  geom_violin(size = 0.8) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3),
             alpha=0.5) +
  scale_color_discrete(name = "Apology Cost",
                       labels = c("0" = "Non-Costly",
                                  "1" = "Costly")) +
  theme(axis.text.x = element_blank())+
  scale_y_continuous(limits = c(1,5)) +
  stat_summary(fun= mean,geom = "point",
               size = 1.5, colour = "black")+
  stat_summary(fun.data = "mean_se",geom = "errorbar",
               width = .2, lwd = .5,
               color = "black")+
  labs(x = xtitle,y = "Expectations of Sincerity")
# print(sincerity.plt)

# 赦し期待を従属変数とした条件別プロット
forgive.plt = 
  ggplot(data=df,
         aes(x=interaction(apology.cost,intence), y=forgive,color=apology.cost)) +
  theme_classic() +
  geom_violin(size = 0.8) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3),
             alpha=0.5) +
  scale_color_discrete(name = "Apology Cost",
                       labels = c("0" = "Non-Costly",
                                  "1" = "Costly")) +
  theme(axis.text.x = element_blank())+
  scale_y_continuous(limits = c(1,5)) +
  stat_summary(fun= mean,geom = "point",
               size = 1.5, colour = "black")+
  stat_summary(fun.data = "mean_se",geom = "errorbar",
               width = .2, lwd = .5,
               color = "black")+
  labs(x = xtitle,y = "Expectations of Forgiveness")
# print(forgive.plt)

# それぞれ出力して確認
# print(sincerity.plt);print(forgive.plt)

# ２つとも出力
# legendがないグラフの作成
sin.noleg_plt = sincerity.plt + theme(legend.position = "none")
for.noleg_plt = forgive.plt + theme(legend.position = "none") 

# Legendsのみのオブジェクトを作る
tmp <- ggplot_gtable(ggplot_build(forgive.plt))
# tmp$grobs
# for (i in 1:length(tmp$grobs)) {
#   print(i)
#   leg <- tmp$grobs[[i]]
#   grid.arrange(arrangeGrob(for.noleg_plt+labs(title = as.character(i)), sin.noleg_plt, leg, ncol=3, widths=c(3/7, 3/7, 1/7)))
# 
# }

# 並べて表示
# なお、論文に掲載されている画像の大きさはは900 x 450ピクセル
leg = tmp$grobs[[15]] #上記のfor文より特定
grid.arrange(arrangeGrob(for.noleg_plt+labs(title = "(a)"),
                         sin.noleg_plt+labs(title = "(b)"), 
                         leg, 
                         ncol=3, 
                         widths=c(3/7, 3/7, 1/7)))
