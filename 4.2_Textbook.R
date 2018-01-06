# ここまでに作成したデータセットのクリア
rm(list=ls())

# スパムメールのCSVデータを読み込み
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# インポートしたデータの詳細を確認
str(sms_raw)

# SMS_RAWのSPAMとHAMが文字列データになっているので、因子に変換
sms_raw$type <- factor(sms_raw$type)

# 因子に変換したデータの詳細を確認
str(sms_raw$type)
table(sms_raw$type)

# tmパッケージ(テキストマイニング用のパッケージ)のインストール(初回のみ)
install.packages("tm")

# tmパッケージをロード
library(tm)

# コーパスオブジェクトを作成する
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# コーパスの内容を確認し、訓練データに含まれる
# 5,574通のSMSメッセージが読み込まれているか確認する
print(sms_corpus)
inspect(sms_corpus[1:1000])

# コーパスの内容を小文字だけになるようにメッセージを標準化
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# sms_corpus_cleanが正常に動作したか確認
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

# SMSメッセージから数字やstopwordsを取り除く
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

# 記号類を削除(テキストのやり方)
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# 記号を空白に置換(テキストのコラムのやり方)
# 置換をするメソッドの作成
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", " ", x)
}
# 置換処理
# sms_corpus_clean <- tm_map(sms_corpus_clean, replacePunctuation)

# stemming処理
# SnowballCパッケージのインストール(初回のみ)とロード
# install.packages("SnowballC")
library(SnowballC)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# 余分なスペースの削除
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# 結果の確認
as.character(sms_corpus[1:3])
as.character(sms_corpus_clean[1:3])
