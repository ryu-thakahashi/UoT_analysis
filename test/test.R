# テスト用
# df = paste0("C:/Users/Miffy/Documents/StudyAnalysis_Takahashi/SinAndForgive_Intence/new_analysis(20220722)/data/raw.data.csv") %>% read.csv(header = TRUE)
data = df; init.colName = "forgive"
search.type = "start"
return.type = "mean.vec"
checkAlp = T; delete.dfCol = F

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
if (checkAlp == T) {
  # アルファ係数が正しく表示されるか(Warning Massegeがでないか)確認
  print(psych::alpha(.reDF,check.keys = T))
  
}