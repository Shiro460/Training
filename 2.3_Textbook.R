# usedcar.csvを使用するために、今まで使用していたデータセットを削除
rm(list=ls())

# usedcar.csvをインポート
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)

# データフレーム、ベクトル、リストなどのRデータ構造の確認
str(usedcars)

# 要約統計量
# yearに対する要約統計量の出力
summary(usedcars$year)

# 複数(priceとmileage）の要約統計量の出力
summary(usedcars[c("price", "mileage")])

# 中心傾向
# 1.平均
# mileageに対する平均の出力
mean(usedcars$mileage)

# 2.中央値
# mileageに対する中央値の出力
median(usedcars$mileage)

# 3.範囲(レンジ)
# mileageに対するレンジの出力
range(usedcars$mileage)
# mileageに対するレンジの差分の出力
diff(range(usedcars$mileage))

# 4.四分位範囲(Interquartile Range)
# mileageに対する四分位範囲の出力
IQR(usedcars$mileage)

# 5.五数要約
# mileageに対する五数要約の出力
quantile(usedcars$mileage)

# 6.任意の分位数
# mileageに対する1パーセンタイルと99パーセンタイルの分位数の出力
quantile(usedcars$mileage, probs = c(0.01, 0.99))

# 数値変数の可視化(箱ひげ図)
boxplot(usedcars$price, main="Boxplot of Used Car Prices", ylab="Private($)")
boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage", ylab="Odometer(mi)")

# 数値変数の可視化(ヒストグラム)
hist(usedcars$price, main="Histgram of Used Car Prices", xlab="Private($)")
hist(usedcars$mileage, main="Histgram of Used Car Mileage", xlab="Odometer(mi)")
# ヒストグラムのビンの数をbreaksオプションで指定
hist(usedcars$mileage, main="Histgram of Used Car Mileage", xlab="Odometer(mi)", breaks = 100)

# 散布度の測定
# 分散
var(usedcars$price)
# 標準偏差
sd(usedcars$price)

# カテゴリ変数の分析
# 1次元表
# 製造年、モデル、色について、テーブルの出力
table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

# モデルの1次元表をmodel_tableに定義し、直接構成比を出力
model_table <- table(usedcars$model)
prop.table(model_table)

# 色の直接構成比を%を単位として出力
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table)*100
round(color_pct, digit = 1)

# 散布図の作成
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi)",
     ylab = "Used Car Price ($)")

# 2次元クロス表(分割表)の作成
# 2次元クロス表は、名義変数の分析に使われる

# gmodelsパッケージをインストール(初回のみ)
# install.packages("gmodels")

# gmodelsパッケージをロード
library(gmodels)

# 地味な色かそうでないかをダミー変数で定義
usedcars$consevative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table (usedcars$consevative)

# 車の色についてクロステーブルの出力
CrossTable(x = usedcars$model, y = usedcars$consevative)

# カイ二乗検定も実行
CrossTable(x = usedcars$model, y = usedcars$consevative, chisq = TRUE)
