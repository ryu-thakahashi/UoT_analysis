.libPaths("C:/R/lib")

library(tidyverse)
library(psych)
library(corrplot)
library(GGally)
library(ggplot2)
library(lsr)
library(gt)

# ----関数----
Ryu.scaling = function(data,init.colName,
                       search.type = c("start","end"),
                       return.type = c("mean.vec","dataframe"),
                       checkAlp = T, delete.dfCol = F) {
  # テスト用
  # df = paste0("C:/Users/Miffy/Documents/StudyAnalysis_Takahashi/SinAndForgive_Intence/new_analysis(20220722)/data/raw.data.csv") %>% read.csv(header = TRUE)
  # data = df; init.colName = "forgive"
  # search.type = "start"
  # return.type = "mean.vec"
  # checkAlp = T; delete.dfCol = F
  
  # 抽出して、.matrixにdata.frame型として代入
  if (search.type == "start") {
    .matrix = data %>% 
      dplyr::select(starts_with(init.colName))
  }else if (search.type == "end") {
    .matrix = data %>% 
      dplyr::select(end_with(init.colName))
  }
  # glimpse(.matrix)# 確認
  
  
  # 逆転する必要がある列を算出
  (.alp = psych::alpha(.matrix,check.keys = T))
  # 逆転項目を逆転させたマトリックスを作成
  .reMatrix = 
    reverse.code(keys = as.vector(.alp$keys),
                 items = .matrix)
  # glimpse(.reMatrix)# 確認
  
  # このままではMatrix型のままなので、data.frame型に
  .reDF = as.data.frame(.reMatrix)
  # 列名を取得
  colND = colnames(.reDF)
  # 逆転した列名の最後尾"-"を"r"に変換
  colND.gsub = gsub("-","r",colND)
  # 整えた列名をもとのdfに代入
  colnames(.reDF) = colND.gsub
  
  # .reDFに抽出&逆転済みのdataframeがある
  # glimpse(.reDF);glimpse(.matrix) #逆転されているか確認
  # psych::alpha(.reDF) # check.keysのエラーメッセージは出ないか確認
  
  # 返す数値を決定
  if (return.type == "mean.vec") {
    result = rowMeans(.reDF)
  }else if (return.type == "dataframe") {
    result = .reDF
  }
  
  # アルファ係数のチェック（ちゃんと逆転されているか）
  if (checkAlp) {
    # アルファ係数が正しく表示されるか(Warning Massegeがでないか)確認
    print(psych::alpha(.reDF,check.keys = T))
  }
  
  # dataに存在して、今回格納した列を削除
  if (delete.dfCol) {
    # グローバル変数dataのうち、今回抽出した列を削除する
    data <<- data[, .colIndex.vec]
  }
  
  return(result)
}


Ryu.corrplot = function(cor_df) {
  corrplot.mixed(cor(cor_df),tl.col = "black",
                lower = "ellipse",upper = "number")
}

Ryu.colname_to_txt = function(df,col.index = 1:ncol(df)) {
  # col.index = 1:ncol(df)
  # col.index = 5:8
  (txt.col = colnames(df[,col.index]))
  (name.vec = paste(colnames(df),collapse = '","'))
  (output = paste('c("',name.vec,'")',collapse = ""))
  (output = gsub(" ","",output))
  write(output,"colnames_df.txt",append = TRUE)
}

Ryu.create_init_folder = function() {
  img.path = "./image"
  src.path = "./src"
  data.path = "./data"
  test.path = "./test"
  Rpro.path = "./\\.Rprofile"
  # imageフォルダが無かったら作る
  if (!dir.exists(img.path)) {
    dir.create(img.path)
    dir.create(src.path)
    dir.create(data.path)
    dir.create(test.path)
    file.copy(Rpro.path,
              "/Users/Miffy/Documents/\\.Rprofile"
              # Rpro.path
              )
    }
  # # srcフォルダが無かったら作る
  # if (!dir.exists(src.path)) {}}
  # # dataフォルダが無かったら作る
  # if (!dir.exists(data.path)) {}
  # # testフォルダ無かったら作る
  # if (!dir.exists(test.path)) {}
  # # .Rprofileがなかったらコピペする
  # if (!file.exists(Rpro)) {}

}


Ryu.move_code.R = function(to = c("src","test")) {
  file.vec = list.files(getwd())
  file.index = grep("\\.R$",file.vec)
  file_name.vec = file.vec[file.index]
  
  .path = paste0("./",to,"/")
  for (i in file_name.vec) {
    now.path = paste0("./",i)
    move.path = paste0(.path,i)
    # print(now_file.path);print(src_file.path)
    
    file.copy(now.path,src.path)
    move.remove(now.path)
    
  }
}

Ryu.fact.analysis = function(data,colName,cut,
                             output.txt = TRUE,
                             type = c("start","end")) {
  # テスト用の初期値
  # data=df;cut=2;colName = "Sin"
  # output.txt=TRUE;type="end"
  
  # 一致する列名を取ってくる
  if (type == "end") {
    print("END")
    .df = data %>% 
      dplyr::select(ends_with(
        colName
      )) %>% 
      na.omit()
  }else if (type == "start"){
    print("START")
    .df = data %>% 
      dplyr::select(starts_with(
        colName
      )) %>% 
      na.omit()
  }
  
  # 中身確認
  # glimpse(.df)
  
  (.corMatrix = cor(.df))
  # eigen(.corMatrix)$values #ガットマン基準
  # VSS.scree(BFI.Matrix) #スクリープロットの出力
  print(fa.parallel(.corMatrix,fm="ml",fa="pc",n.iter = 100));abline(h=1)
  # print(VSS(.corMatrix))
  .fac = fa(.corMatrix, nfactors = cut, fm="ml", rotate="promax")
  print(.fac,sort = T, digits = 3,cutoff = 2)
  
  if (output.txt) {
    
    (fac.mat = .fac$loadings)
    (rName = rownames(fac.mat))
    (fac.df = data.frame(rName,fac.mat[1:nrow(fac.mat),1:ncol(fac.mat)]))
    (index.name = colnames(fac.df))
    
    # ファクターを出力
    file.name = "factor.txt"
    write(paste("Vector Name:",colName),file.name,append = TRUE)
    # cName = paste(colnames(.df),collapse = '","')
    # write(cName,"simple.factName.txt",append = TRUE)
    for (i in 2:(cut+1)) {
      # i = 1
      (high.weight = dplyr::select(fac.df,"rName",index.name[i]))
      high.vec = abs(high.weight[,2])>.4 %>% sort()
      (highName = high.weight[high.vec,])
      (name.vec = paste(rownames(highName),collapse = '","'))
      output = paste0('c("',name.vec,'")',collapse = "")
      write(output,file.name,append = TRUE)
    }
  }
  
}


Ryu.fact.scaling = function(data,colName.vec,checkAlp = T) {
  
  .matrix = data %>% 
    dplyr::select(colName.vec)
  
  # 逆転する必要がある列を算出
  .alp = psych::alpha(.matrix,check.keys = T)
  # 逆転項目を逆転させたマトリックスを作成
  .reMatrix = reverse.code(keys = as.vector(.alp$keys),
                           items = .matrix,
                           mini = 1, maxi = 7)
  
  # このままではMatrix型のままなので、data.frame型に
  .reDF = as.data.frame(.reMatrix)
  # 列名を取得
  colND = colnames(.reDF)
  # 逆転した列名の最後尾"-"を"r"に変換
  colND.gsub = gsub("-","r",colND)
  # 整えた列名をもとのdfに代入
  colnames(.reDF) = colND.gsub
  
  # .reDFに抽出&逆転済みのdataframeがある
  
  # 返す数値を決定
  result = rowMeans(.reDF)
  
  # アルファ係数のチェック（ちゃんと逆転されているか）
  if (checkAlp == T) {
    # アルファ係数が正しく表示されるか(Warning Massegeがでないか)確認
    print(psych::alpha(.reDF,check.keys = T))
    
  }
  
  return(result)
}

Ryu.ggpairs = function(res.data, color.factor) {
  
  # バイオリンプロットの関数化
  my_violin = function(data, mapping) {
    ggplot(data = data, mapping = mapping)+
      geom_violin(trim = T,fill = "#999999",
                  alpha = .3)+
      # geom_hline(yintercept = 1) +
      theme(legend.position = "none") + 
      geom_boxplot(width = .8 ,fill = "white",alpha = .1)+
      theme_minimal()
  }
  # 描画
  ggpairs(data = res.data,
          legend = 1,
          # columns = var.data,
          # columnLabels = var.name,
          mapping = aes(color = color.factor),
          upper = list(continuous = "cor",
                       combo = my_violin,
                       discrete = "blank"),
          diag = list(continuous = wrap("densityDiag",
                                        alpha = .5)),
          lower = list(continuous = wrap("smooth",
                                         se = FALSE,
                                         size = .1),
                       combo = wrap("facethist",
                                    alpha = .6),
                       discreate = "facetbar"))+
    theme_minimal() +
    theme(legend.position = "top")
}

Ryu.return_df = function(data,init.colName,
                         search.type = c("start","end")) {
  
  # 抽出して、.matrixにdata.frame型として代入
  if (search.type == "start") {
    .matrix = data %>% 
      dplyr::select(starts_with(init.colName))
  }else if (search.type == "end") {
    .matrix = data %>% 
      dplyr::select(end_with(init.colName))
  }
  
  return(.matrix)
}

# barchartの関数(2x2の混合要因)
Ryu.barchart_2x2 =
  function(data, col.vec,
           between.label,
           within.label,
           y.label) {
    # テスト用のセット
    # data = df;col.vec = c("samepos","otherpos","condition")
    # between.label = c("control","pathogen")
    # within.label = c("ingroup","outgroup")
    # y.label = "positive attitude"
    
    # バーチャートに組み込む列を抽出 + NAをomit
    bar.df = data %>% 
      select(col.vec) %>% 
      na.omit()
    # betweenで並び替え
    bar.df = bar.df[order(bar.df[,3]),]
    
    # 確認
    # table(bar.df[,3])
    # glimpse(bar.df)
    
    # y軸の値を格納したベクトルを生成
    value.vec = 
      c(bar.df[,1],bar.df[,2])
    
    # ラベルをそれぞれ作成
    betw_con.vec = 
      rep(bar.df[,3],2)
    with_con.vec =
      c(rep("W1",nrow(bar.df)),rep("W2",nrow(bar.df)))
    
    # 確認
    # glimpse(value.vec);glimpse(with_con.vec);glimpse(betw_con.vec)
    
    # ラベルを統一する
    betw_con.vec =
      ifelse(betw_con.vec == betw_con.vec[1],"B1","B2")
    
    # 確認
    # table(betw_con.vec)
    
    # すべてを統合しているdataframeの作成
    bar.df = 
      data.frame(value.vec,with_con.vec,betw_con.vec)
    # glimpse(bar.df)
    
    # withinごとにdataframeを作成
    W1.df = bar.df %>% 
      dplyr::filter(bar.df[,2] == "W1")
    W2.df = bar.df %>% 
      dplyr::filter(bar.df[,2] == "W2")
    
    # 確認
    # table(W1.df$with_con.vec);table(W1.df$betw_con.vec)
    # table(W2.df$with_con.vec);table(W2.df$betw_con.vec)
    # glimpse(W1.df)
    
    # betweenごとにdataframeを作成
    B1W1.vec = W1.df %>% 
      dplyr::filter(W1.df[,3] == "B1") %>% 
      dplyr::select("value.vec")
    # glimpse(B1W1.vec)
    B1W2.vec = W2.df %>% 
      dplyr::filter(W2.df[,3] == "B1") %>% 
      dplyr::select("value.vec")
    B2W1.vec = W1.df %>% 
      dplyr::filter(W1.df[,3] == "B2") %>% 
      dplyr::select("value.vec")
    B2W2.vec = W2.df %>% 
      dplyr::filter(W2.df[,3] == "B2") %>% 
      dplyr::select("value.vec")
    # glimpse(B1W1.vec[[1]])
    
    # 各群ごとにmeanとsdの算出
    mean.vec = 
      c(
        mean(B1W1.vec[[1]]),
        mean(B1W2.vec[[1]]),
        mean(B2W1.vec[[1]]),
        mean(B2W2.vec[[1]])
      )
    sd.vec =
      c(
        sd(B1W1.vec[[1]]),
        sd(B1W2.vec[[1]]),
        sd(B2W1.vec[[1]]),
        sd(B2W2.vec[[1]])
      )
    
    # 指定したlabelを格納し、res.dfに統合する
    B.label = c(rep(between.label[1],2),rep(between.label[2],2))  
    W.label = rep(c(within.label),2)
    res.df =
      data.frame(mean.vec,sd.vec,B.label,W.label)
    # glimpse(res.df)
    
    # barchartの描画
    ggplot(data = res.df,
           mapping = aes(x = B.label,
                         y = mean.vec,
                         fill = W.label)) +
      theme_minimal() + 
      geom_bar(stat = "identity",position = "dodge")+
      geom_errorbar(
        aes(ymin = mean.vec - 1.96*sd.vec,
            ymax = mean.vec + 1.96*sd.vec),
        position = position_dodge(.9),
        width = .3
      )+
      geom_point(
        size = 5,
        position = position_dodge(.9)
      )+
      ylab(y.label)+
      ylim(c(0,8))
    
  }
# barchartの関数(2内x3間の混合要因)
Ryu.barchart_2x3 =
  function(data, col.vec,
           between.label,
           within.label,
           y.label) {
    # テスト用のセット
    data = df;col.vec = c("ingroup_pos","outgroup_pos","cond3")
    between.label = c("disease","control","threat")
    within.label = c("Ingroup","Outgroup")
    y.label = "Positive Attitude"
    
    # バーチャートに組み込む列を抽出 + NAをomit
    bar.df = data %>% 
      dplyr::select(col.vec) %>% 
      na.omit()
    # betweenで並び替え
    bar.df = bar.df[order(bar.df[,3]),]
    
    # 確認
    table(bar.df[,3])
    glimpse(bar.df)
    
    # y軸の値を格納したベクトルを生成
    value.vec = 
      c(bar.df[,1],bar.df[,2])
    
    # ラベルをそれぞれ作成
    betw_con.vec = 
      rep(bar.df[,3],2)
    with_con.vec =
      c(rep("W1",nrow(bar.df)),rep("W2",nrow(bar.df)))
    
    # 確認
    glimpse(value.vec);glimpse(with_con.vec);glimpse(betw_con.vec)
    
    # ラベルを統一する
    third = length(betw_con.vec)/(2*3) %>% 
      round(0)
    third
    betw_con.vec =
      ifelse(betw_con.vec == betw_con.vec[1],"B1",
             ifelse(betw_con.vec == betw_con.vec[third],"B2","B3"))
    
    # 確認
    # table(betw_con.vec)
    
    # すべてを統合しているdataframeの作成
    bar.df = 
      data.frame(value.vec,with_con.vec,betw_con.vec)
    # glimpse(bar.df)
    
    # withinごとにdataframeを作成
    W1.df = bar.df %>% 
      dplyr::filter(bar.df[,2] == "W1")
    W2.df = bar.df %>% 
      dplyr::filter(bar.df[,2] == "W2")
    
    # 確認
    # table(W1.df$with_con.vec);table(W1.df$betw_con.vec)
    # table(W2.df$with_con.vec);table(W2.df$betw_con.vec)
    # glimpse(W1.df)
    
    # betweenごとにdataframeを作成
    B1W1.vec = W1.df %>% 
      dplyr::filter(W1.df[,3] == "B1") %>% 
      dplyr::select("value.vec")
    # glimpse(B1W1.vec)
    B1W2.vec = W2.df %>% 
      dplyr::filter(W2.df[,3] == "B1") %>% 
      dplyr::select("value.vec")
    B2W1.vec = W1.df %>% 
      dplyr::filter(W1.df[,3] == "B2") %>% 
      dplyr::select("value.vec")
    B2W2.vec = W2.df %>% 
      dplyr::filter(W2.df[,3] == "B2") %>% 
      dplyr::select("value.vec")
    B3W1.vec = W1.df %>% 
      dplyr::filter(W1.df[,3] == "B3") %>% 
      dplyr::select("value.vec")
    B3W2.vec = W2.df %>% 
      dplyr::filter(W2.df[,3] == "B3") %>% 
      dplyr::select("value.vec")
    # glimpse(B1W1.vec[[1]])
    
    # 各群ごとにmeanとsdの算出
    mean.vec = 
      c(
        mean(B1W1.vec[[1]]),
        mean(B1W2.vec[[1]]),
        mean(B2W1.vec[[1]]),
        mean(B2W2.vec[[1]]),
        mean(B3W1.vec[[1]]),
        mean(B3W2.vec[[1]])
      )
    sd.vec =
      c(
        sd(B1W1.vec[[1]]),
        sd(B1W2.vec[[1]]),
        sd(B2W1.vec[[1]]),
        sd(B2W2.vec[[1]]),
        sd(B3W1.vec[[1]]),
        sd(B3W2.vec[[1]])
      )
    
    # 指定したlabelを格納し、res.dfに統合する
    B.label = c(rep(between.label[1],2),rep(between.label[2],2),rep(between.label[3],2))  
    W.label = rep(c(within.label),3)
    res.df =
      data.frame(mean.vec,sd.vec,B.label,W.label)
    # glimpse(res.df)
    
    # barchartの描画
    ggplot(data = res.df,
           mapping = aes(x = B.label,
                         y = mean.vec,
                         fill = W.label)) +
      theme_minimal() + 
      geom_bar(stat = "identity",position = "dodge")+
      geom_errorbar(
        aes(ymin = mean.vec - 1.96*sd.vec,
            ymax = mean.vec + 1.96*sd.vec),
        position = position_dodge(.9),
        width = .3
      )+
      geom_point(
        size = 5,
        position = position_dodge(.9)
      )+
      ylab(y.label)+
      ylim(c(0,8))
    
  }


Ryu.create_init_folder()
print("Done reading myfunction.R!!")
