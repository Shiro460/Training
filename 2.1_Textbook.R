# ベクトルの作成
subject_name <- c("John Doe", "Jane Doe", "Steve Graves" )
temparature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)

# 因子の作成
gender <- factor(c("MALE", "FEMALE", "MALE"))

# 因子のデータに含まれていないレベルを追加
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O"))

# レベルに順序を追加
symptoms <- factor(c("SEVERE", "MILD", "MODERATE"),
                   levels = c("MILD", "MODERATE", "SEVERE"),
                   ordered = TRUE)

# リストの作成
subject1 <- list(fullname = subject_name[1],
                 temparature = temparature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1],
                 symptoms = symptoms[1])

# データフレームの作成
# stringsAsFactors = FALSEというパラメータを追加することで
# 文字ベクトルを因子に変換せずにデータフレームを作成する
pt_data <- data.frame(subject_name, temparature, flu_status, gender, blood, symptoms, stringsAsFactors = FALSE)


