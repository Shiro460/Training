# ここまでに作成したデータセットのクリア
rm(list=ls())

# ウィスコンシン州乳がん診断データセットの読み込み
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

# 読み込んだデータセットの内容の確認
str(wbcd)

# IDは不要な情報であるため、IDを除いたデータセットを作成
wbcd <- wbcd[-1]

# データセットからIDが削除されていることを確認
str(wbcd)

# 診断結果(diagnosis)を因子に変換し、ラベルもわかりやすいものに変換
wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# 診断結果が想定通りのものになっているか確認
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# 数値データの正規化
# 正規化するためのnormalize()関数を定義
normalize <- function(x) {
  return ((x-min(x)) / (max(x) - min(x)))
}

# normalize()関数が動作しているかを確認
# 以下の2つの実行結果が同じになれば問題ない
normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

# normalize()関数を個々の要素に適用
# lapplyの構文：lapply(適用する範囲, 適用する関数)
# as.data.frame：lapplyで返却してきたリストをデータフレームに変換する
# is.data.frame：引数で指定されたものがデータフレームかどうかを確認する
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
is.data.frame(wbcd_n)
summary(wbcd_n)

# 訓練データセット(学習器)とテストデータセットの作成
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# 正規化された訓練データセットとテストデータセットに対し
# diagnosisのクラスラベルを格納
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# k近傍法を実行するためのパッケージをインストール(初回のみ)
# install.packages("class")

# classパッケージのロード
library(class)

# knn()によるテストデータの分類
# kの値は、訓練データの総数である429の平方根に近い21に設定
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)

# モデルの性能評価
# gmodelsパッケージのロード
library(gmodels)

# ベクトルの一致度を確認(カイ二乗検定の結果は出力しない)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

# モデルの性能改善
# Zスコア標準化を使用した距離計算の実行
# scale()関数で既存のデータフレームに対して
# Zスコア標準化を使用し、範囲調整を実行
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# Zスコア標準化を使用したデータセットで訓練とシュミレーションの実行
wbcd_train_z <- wbcd_z[1:469, ]
wbcd_test_z <- wbcd_z[470:569, ]
wbcd_train_labels_z <- wbcd[1:469, 1]
wbcd_test_labels_z <- wbcd[470:569, 1]
wbcd_test_pred_z <- knn(train = wbcd_train_z, test = wbcd_test_z, cl = wbcd_train_labels_z, k = 21)
CrossTable(x = wbcd_test_labels_z, y = wbcd_test_pred_z, prop.chisq = FALSE)
