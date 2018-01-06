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

# DTM(Document-Term Matorix：文書索引語行列)の作成
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
# 前処理を行っていない場合の対応方法は以下の通り
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

# 訓練データとテストデータ、またそれぞれのラベルの作成
sms_dtm_train <- sms_dtm[1:4174,]
sms_dtm_test <- sms_dtm[4175:5574,]
sms_train_labels <- sms_raw[1:4174,]$type
sms_test_labels <- sms_raw[4175:5574,]$type

# 訓練データのラベルとテストデータのラベルの違いを確認
# ※それほど違いが大きくなければ問題ない
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

# wordcloudを実行するためのパッケージを準備
install.packages("wordcloud")
library(wordcloud)

# 全体のワードクラウド
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
wordcloud(sms_corpus_clean, min.freq = 50)
wordcloud(sms_corpus_clean, min.freq = 100, random.order = FALSE)

# spamとhamのメッセージのサブセットを作成
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

# spamとhamのワードクラウドの作成
wordcloud(spam$text, max.words = 40, random.order = FALSE)
wordcloud(ham$text, max.words = 40, random.order = FALSE)

# 単純ベイズ法で使用する頻出語を確認
# 出現回数が少ない単語を削除
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

# 各文に頻出単語が含まれるかどうかをカテゴリで示す
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# 単純ベイズ実装が含まれるパッケージのインストールとロード
install.packages("e1071")
library(e1071)

# 単純ベイズのモデル構築
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

# 単純ベイズモデルの性能評価
sms_test_pred <- predict(sms_classifier, sms_test)
install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

# 単純ベイズモデルにラプラス推定量を追加した場合
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))
