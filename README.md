# README
このレポジトリは、東京大学大学院人文社会科学研究科社会心理学専攻の入試で提出した、卒論の分析データおよびコードです。

# フォルダやファイルの説明および中身
## フォルダ
- data
  - 分析のデータが含まれているフォルダです。
  - 今回はraw.data.csvのみ格納されています。
- full_analysis
  - osfでアップロードされているanalysisの元となるコードとRmdが入っています。
- image
  - 論文でも使用されている画像が格納されています。
  - path.pngはスライド上で作成したものです。
- src
  - 分析目的で分けられたRのコードが入っています。
  - handling.R等にある「Ryu.」から始まるコードは自作関数です。
    - こちらについてはmyfunction.Rをご覧ください。
  - test
    - コードが上手くいかないときや関数の修正で使用したコードが入っています。
    - 自分用なので、グチャグチャです。

## ファイル
  - analysis.html
    - osfで公開しているものと同一のhtmlです。
  - myfunction.R
    - 私の自作関数です。
    - srcフォルダに有るコードで自分用に使用するときに使われます。
  - .Rprofile
    - myfunction.Rを読み込むための初期設定ファイルです。
    - このUoT_analysisフォルダがワーキングディレクトリになっていることを前提としています。

