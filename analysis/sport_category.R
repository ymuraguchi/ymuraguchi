
library(dplyr)
# スポーツ名をアルファベット順に並べて表示
sorted_sports <- sort(unique(data$sport))
print(sorted_sports)

# スポーツ名の統一
data$sport <- gsub("テニス|硬式テニス|軟式テニス", "テニス", data$sport)
data$sport <- gsub("バスケット|バスケ|バスケットボール部", "バスケットボール", data$sport)
data$sport <- gsub("陸上競技|陸上競技部", "陸上", data$sport)
data$sport <- gsub("サッカー|男子サッカー|女子サッカー", "サッカー", data$sport)
data$sport <- gsub("野球|硬式野球|軟式野球", "野球", data$sport)
data$sport <- gsub("水泳|競泳", "水泳", data$sport)
data$sport <- gsub("トライアスロン部", "トライアスロン", data$sport)
data$sport <- gsub("ダンス部", "ダンス", data$sport)
data$sport <- gsub("ボート部", "ボート", data$sport)
data$sport <- gsub("フットサル部", "フットサル", data$sport)
data$sport <- gsub("体操競技", "体操", data$sport)

# 個人競技と団体競技のリスト
individual_sports <- c("テニス", "水泳", "陸上", "ゴルフ", "バドミントン", 
                       "トライアスロン", "ランニング", "ダンス", "ボート", 
                       "体操", "卓球", "スキー", "スノーボード", "柔道", 
                       "剣道", "空手", "アーチェリー", "フィギュアスケート")

team_sports <- c("サッカー", "バスケットボール", "バレーボール", "ラグビー", 
                 "野球", "ハンドボール", "フットサル", "アイスホッケー", 
                 "アメリカンフットボール", "ラクロス", "ホッケー")

# データをサブセットに分ける
individual_data <- data %>% filter(sport %in% individual_sports)
team_data <- data %>% filter(sport %in% team_sports)

# 各グループごとのスポーツ一覧を表示
cat("個人競技のスポーツ一覧:\n")
print(unique(individual_data$sport))

cat("団体競技のスポーツ一覧:\n")
print(unique(team_data$sport))

# 個人競技のスポーツ一覧と人数を表示
cat("個人競技のスポーツ一覧:\n")
print(unique(individual_data$sport))
cat("個人競技のスポーツ数:", length(unique(individual_data$sport)), "\n")
# 個人競技の人数（各スポーツごとの人数）
cat("個人競技のスポーツごとの人数:\n")
print(table(individual_data$sport))
# 団体競技のスポーツ一覧と人数を表示
cat("団体競技のスポーツ一覧:\n")
print(unique(team_data$sport))
cat("団体競技のスポーツ数:", length(unique(team_data$sport)), "\n")
# 団体競技の人数（各スポーツごとの人数）
cat("団体競技のスポーツごとの人数:\n")
print(table(team_data$sport))
# 個人種目の総人数
individual_total <- nrow(individual_data)
cat("個人種目の総人数:", individual_total, "\n")
# 団体種目の総人数
team_total <- nrow(team_data)
cat("団体種目の総人数:", team_total, "\n")

# 団体競技のデータ
team_sports_data <- data %>%
  filter(sport %in% team_sports_list)

# 個人競技のデータ
individual_sports_data <- data %>%
  filter(sport %in% individual_sports_list)

# SS因子得点の計算
data <- data %>%
  mutate(
    ss_factor1 = rowMeans(select(., ss01:ss04), na.rm = TRUE),
    ss_factor2 = rowMeans(select(., ss05:ss08), na.rm = TRUE),
    ss_factor3 = rowMeans(select(., ss09:ss12), na.rm = TRUE)
  )
# 逆転項目の処理（PPFIとPANAS）
# 逆転項目の反転処理
reverse_items <- c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")
max_score <- 7  # 7点スケールの場合

# 逆転処理を適用
data[reverse_items] <- data[reverse_items] %>%
  mutate(across(everything(), ~ max_score + 1 - .))

# 結果を確認
head(data)

# PPFI因子得点の計算
data <- data %>%
  mutate(
    ppfi_factor1 = rowMeans(select(., ppfi_j01:ppfi_j05), na.rm = TRUE),
    ppfi_factor2 = rowMeans(select(., ppfi_j06:ppfi_j10), na.rm = TRUE),
    ppfi_factor3 = rowMeans(select(., ppfi_j11:ppfi_j15), na.rm = TRUE)
  )

# 団体競技での相関分析
team_corr <- team_sports_data %>%
  select(ss_factor1:ss_factor3, ppfi_factor1:ppfi_factor3) %>%
  cor(use = "pairwise.complete.obs")
print("団体競技での相関分析結果:")
print(round(team_corr, 3))  # 小数点3桁に丸める

# 個人競技での相関分析
individual_corr <- individual_sports_data %>%
  select(ss_factor1:ss_factor3, ppfi_factor1:ppfi_factor3) %>%
  cor(use = "pairwise.complete.obs")
print("個人競技での相関分析結果:")
print(round(individual_corr, 3))  # 小数点3桁に丸める

#p値
# 団体競技での相関分析とp値計算
team_corr_res <- rcorr(as.matrix(team_sports_data %>%
                                   select(ss_factor1:ss_factor3, ppfi_factor1:ppfi_factor3)))

team_corr_matrix <- team_corr_res$r
team_p_matrix <- team_corr_res$P

# 団体競技の相関とp値に基づく*の追加
team_corr_with_sig <- matrix(nrow = nrow(team_corr_matrix), ncol = ncol(team_corr_matrix))
for (i in 1:nrow(team_corr_matrix)) {
  for (j in 1:ncol(team_corr_matrix)) {
    if (is.na(team_p_matrix[i, j])) {
      team_corr_with_sig[i, j] <- NA  # NA の場合そのまま
    } else if (team_p_matrix[i, j] < 0.001) {
      team_corr_with_sig[i, j] <- paste0(round(team_corr_matrix[i, j], 3), "***")  # p < .001
    } else if (team_p_matrix[i, j] < 0.01) {
      team_corr_with_sig[i, j] <- paste0(round(team_corr_matrix[i, j], 3), "**")   # p < .01
    } else if (team_p_matrix[i, j] < 0.05) {
      team_corr_with_sig[i, j] <- paste0(round(team_corr_matrix[i, j], 3), "*")    # p < .05
    } else {
      team_corr_with_sig[i, j] <- round(team_corr_matrix[i, j], 3)  # 有意でない場合
    }
  }
}

# 因子名のラベル付け
factor_labels <- c("SS Factor 1", "SS Factor 2", "SS Factor 3", 
                   "PPFI Factor 1", "PPFI Factor 2", "PPFI Factor 3")

# 行列に因子名を適用
colnames(team_corr_with_sig) <- factor_labels
rownames(team_corr_with_sig) <- factor_labels

# 結果の表示
cat("団体競技での相関分析結果 (有意差付き):\n")
print(team_corr_with_sig)

# 個人競技での相関分析とp値計算
individual_corr_res <- rcorr(as.matrix(individual_sports_data %>%
                                         select(ss_factor1:ss_factor3, ppfi_factor1:ppfi_factor3)))

individual_corr_matrix <- individual_corr_res$r
individual_p_matrix <- individual_corr_res$P

# 個人競技の相関とp値に基づく*の追加
individual_corr_with_sig <- matrix(nrow = nrow(individual_corr_matrix), ncol = ncol(individual_corr_matrix))
for (i in 1:nrow(individual_corr_matrix)) {
  for (j in 1:ncol(individual_corr_matrix)) {
    if (is.na(individual_p_matrix[i, j])) {
      individual_corr_with_sig[i, j] <- NA  # NA の場合そのまま
    } else if (individual_p_matrix[i, j] < 0.001) {
      individual_corr_with_sig[i, j] <- paste0(round(individual_corr_matrix[i, j], 3), "***")  # p < .001
    } else if (individual_p_matrix[i, j] < 0.01) {
      individual_corr_with_sig[i, j] <- paste0(round(individual_corr_matrix[i, j], 3), "**")   # p < .01
    } else if (individual_p_matrix[i, j] < 0.05) {
      individual_corr_with_sig[i, j] <- paste0(round(individual_corr_matrix[i, j], 3), "*")    # p < .05
    } else {
      individual_corr_with_sig[i, j] <- round(individual_corr_matrix[i, j], 3)  # 有意でない場合
    }
  }
}

# 個人競技の因子名のラベル付け
rownames(individual_corr_with_sig) <- factor_labels
colnames(individual_corr_with_sig) <- factor_labels

# 結果の表示
cat("個人競技での相関分析結果 (有意差付き):\n")
print(individual_corr_with_sig)

# 団体競技のMとSD
team_summary <- team_sports_data %>%
  summarise(
    ss_factor1_mean = mean(ss_factor1, na.rm = TRUE),
    ss_factor1_sd = sd(ss_factor1, na.rm = TRUE),
    ss_factor2_mean = mean(ss_factor2, na.rm = TRUE),
    ss_factor2_sd = sd(ss_factor2, na.rm = TRUE),
    ss_factor3_mean = mean(ss_factor3, na.rm = TRUE),
    ss_factor3_sd = sd(ss_factor3, na.rm = TRUE),
    ppfi_factor1_mean = mean(ppfi_factor1, na.rm = TRUE),
    ppfi_factor1_sd = sd(ppfi_factor1, na.rm = TRUE),
    ppfi_factor2_mean = mean(ppfi_factor2, na.rm = TRUE),
    ppfi_factor2_sd = sd(ppfi_factor2, na.rm = TRUE),
    ppfi_factor3_mean = mean(ppfi_factor3, na.rm = TRUE),
    ppfi_factor3_sd = sd(ppfi_factor3, na.rm = TRUE)
  )
cat("団体競技の因子ごとのMとSD:\n")
print(team_summary)

# 個人競技のMとSD
individual_summary <- individual_sports_data %>%
  summarise(
    ss_factor1_mean = mean(ss_factor1, na.rm = TRUE),
    ss_factor1_sd = sd(ss_factor1, na.rm = TRUE),
    ss_factor2_mean = mean(ss_factor2, na.rm = TRUE),
    ss_factor2_sd = sd(ss_factor2, na.rm = TRUE),
    ss_factor3_mean = mean(ss_factor3, na.rm = TRUE),
    ss_factor3_sd = sd(ss_factor3, na.rm = TRUE),
    ppfi_factor1_mean = mean(ppfi_factor1, na.rm = TRUE),
    ppfi_factor1_sd = sd(ppfi_factor1, na.rm = TRUE),
    ppfi_factor2_mean = mean(ppfi_factor2, na.rm = TRUE),
    ppfi_factor2_sd = sd(ppfi_factor2, na.rm = TRUE),
    ppfi_factor3_mean = mean(ppfi_factor3, na.rm = TRUE),
    ppfi_factor3_sd = sd(ppfi_factor3, na.rm = TRUE)
  )
cat("個人競技の因子ごとのMとSD:\n")
print(individual_summary)
    


#######ねっとわーく##########
# 必要なライブラリの読み込み
library(igraph)
library(dplyr)

# データの読み込み（例として CSV ファイルから読み込む場合）
data <- read.csv("filtered_data.csv")

# スポーツ名の統一
data$sport <- gsub("テニス|硬式テニス|軟式テニス", "テニス", data$sport)
data$sport <- gsub("バスケット|バスケ|バスケットボール部", "バスケットボール", data$sport)
data$sport <- gsub("陸上競技|陸上競技部", "陸上", data$sport)
data$sport <- gsub("サッカー|男子サッカー|女子サッカー", "サッカー", data$sport)
data$sport <- gsub("野球|硬式野球|軟式野球", "野球", data$sport)
data$sport <- gsub("水泳|競泳", "水泳", data$sport)
data$sport <- gsub("トライアスロン部", "トライアスロン", data$sport)
data$sport <- gsub("ダンス部", "ダンス", data$sport)
data$sport <- gsub("ボート部", "ボート", data$sport)
data$sport <- gsub("フットサル部", "フットサル", data$sport)
data$sport <- gsub("体操競技", "体操", data$sport)

# 個人競技と団体競技のリスト
individual_sports <- c("テニス", "水泳", "陸上", "ゴルフ", "バドミントン", 
                       "トライアスロン", "ランニング", "ダンス", "ボート", 
                       "体操", "卓球", "スキー", "スノーボード", "柔道", 
                       "剣道", "空手", "アーチェリー", "フィギュアスケート")

team_sports <- c("サッカー", "バスケットボール", "バレーボール", "ラグビー", 
                 "野球", "ハンドボール", "フットサル", "アイスホッケー", 
                 "アメリカンフットボール", "ラクロス", "ホッケー")

# データを個人競技と団体競技に分ける
individual_data <- data %>% filter(sport %in% individual_sports)
team_data <- data %>% filter(sport %in% team_sports)

# 団体競技のSS因子の計算 (例: 各因子の平均を取る場合)
team_data$ss_factor1 <- rowMeans(team_data[, c("ss01", "ss02", "ss03", "ss04")], na.rm = TRUE)
team_data$ss_factor2 <- rowMeans(team_data[, c("ss05", "ss06", "ss07", "ss08")], na.rm = TRUE)
team_data$ss_factor3 <- rowMeans(team_data[, c("ss09", "ss10", "ss11", "ss12")], na.rm = TRUE)

# 逆転項目の処理（PPFIとPANAS）
# 逆転項目の反転処理
reverse_items <- c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")
max_score <- 7  # 7点スケールの場合
team_data[reverse_items] <- team_data[reverse_items] %>%
  mutate(across(everything(), ~ max_score + 1 - .))

# 団体競技のPPFI因子の計算 (例: 各因子の平均を取る場合)
team_data$ppfi_factor1 <- rowMeans(team_data[, c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")], na.rm = TRUE)
team_data$ppfi_factor2 <- rowMeans(team_data[, c("ppfi_j06", "ppfi_j07", "ppfi_j08", "ppfi_j09", "ppfi_j10")], na.rm = TRUE)
team_data$ppfi_factor3 <- rowMeans(team_data[, c("ppfi_j11", "ppfi_j12", "ppfi_j13", "ppfi_j14", "ppfi_j15")], na.rm = TRUE)

# 因子間の相関行列の作成（団体競技）
team_corr <- cor(team_data[, c("ss_factor1", "ss_factor2", "ss_factor3", "ppfi_factor1", "ppfi_factor2", "ppfi_factor3")], use = "pairwise.complete.obs")

# 結果の表示（団体競技）
print(team_corr)

# 個人競技のSS因子の計算
individual_data$ss_factor1 <- rowMeans(individual_data[, c("ss01", "ss02", "ss03", "ss04")], na.rm = TRUE)
individual_data$ss_factor2 <- rowMeans(individual_data[, c("ss05", "ss06", "ss07", "ss08")], na.rm = TRUE)
individual_data$ss_factor3 <- rowMeans(individual_data[, c("ss09", "ss10", "ss11", "ss12")], na.rm = TRUE)

# 逆転項目の処理（PPFIとPANAS）
individual_data[reverse_items] <- individual_data[reverse_items] %>%
  mutate(across(everything(), ~ max_score + 1 - .))

# 個人競技のPPFI因子の計算
individual_data$ppfi_factor1 <- rowMeans(individual_data[, c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")], na.rm = TRUE)
individual_data$ppfi_factor2 <- rowMeans(individual_data[, c("ppfi_j06", "ppfi_j07", "ppfi_j08", "ppfi_j09", "ppfi_j10")], na.rm = TRUE)
individual_data$ppfi_factor3 <- rowMeans(individual_data[, c("ppfi_j11", "ppfi_j12", "ppfi_j13", "ppfi_j14", "ppfi_j15")], na.rm = TRUE)

# 因子間の相関行列の作成（個人競技）
individual_corr <- cor(individual_data[, c("ss_factor1", "ss_factor2", "ss_factor3", "ppfi_factor1", "ppfi_factor2", "ppfi_factor3")], use = "pairwise.complete.obs")

# 結果の表示（個人競技）
print(individual_corr)

# 個人競技ネットワーク作成
individual_network <- graph_from_adjacency_matrix(individual_corr, mode = "undirected", diag = FALSE, weighted = TRUE)

# 団体競技ネットワーク作成
team_network <- graph_from_adjacency_matrix(team_corr, mode = "undirected", diag = FALSE, weighted = TRUE)

# 個人競技ネットワークのエッジ色を設定（正の相関は青、負の相関は赤）
E(individual_network)$color <- ifelse(E(individual_network)$weight > 0, "blue", "red")

# 団体競技ネットワークのエッジ色を設定（正の相関は青、負の相関は赤）
E(team_network)$color <- ifelse(E(team_network)$weight > 0, "blue", "red")

# 個人競技ネットワークの可視化
plot(individual_network, main = "個人競技のネットワーク", vertex.size = 10, vertex.label.cex = 0.8)

# 団体競技ネットワークの可視化
plot(team_network, main = "団体競技のネットワーク", vertex.size = 10, vertex.label.cex = 0.8)

# 個人競技ネットワークの可視化（レイアウト調整）
plot(individual_network, layout = layout.fruchterman.reingold, 
     main = "個人競技のネットワーク", vertex.size = 10, vertex.label.cex = 0.3)

# 団体競技ネットワークの可視化（レイアウト調整）
plot(team_network, layout = layout.fruchterman.reingold, 
     main = "団体競技のネットワーク", vertex.size = 10, vertex.label.cex = 0.5)



####お試し
# 必要なパッケージの読み込み
# 必要なライブラリの読み込み
library(qgraph)
library(glasso)

# データの読み込み
data <- read.csv("filtered_data.csv")

# スポーツ名の統一
data$sport <- gsub("テニス|硬式テニス|軟式テニス", "テニス", data$sport)
data$sport <- gsub("バスケット|バスケ|バスケットボール部", "バスケットボール", data$sport)
data$sport <- gsub("陸上競技|陸上競技部", "陸上", data$sport)
data$sport <- gsub("サッカー|男子サッカー|女子サッカー", "サッカー", data$sport)
data$sport <- gsub("野球|硬式野球|軟式野球", "野球", data$sport)
data$sport <- gsub("水泳|競泳", "水泳", data$sport)
data$sport <- gsub("トライアスロン部", "トライアスロン", data$sport)
data$sport <- gsub("ダンス部", "ダンス", data$sport)
data$sport <- gsub("ボート部", "ボート", data$sport)
data$sport <- gsub("フットサル部", "フットサル", data$sport)
data$sport <- gsub("体操競技", "体操", data$sport)

# 個人競技と団体競技のリスト
individual_sports <- c("テニス", "水泳", "陸上", "ゴルフ", "バドミントン", 
                       "トライアスロン", "ランニング", "ダンス", "ボート", 
                       "体操", "卓球", "スキー", "スノーボード", "柔道", 
                       "剣道", "空手", "アーチェリー", "フィギュアスケート")

team_sports <- c("サッカー", "バスケットボール", "バレーボール", "ラグビー", 
                 "野球", "ハンドボール", "フットサル", "アイスホッケー", 
                 "アメリカンフットボール", "ラクロス", "ホッケー")

# 個人競技のデータ
individual_data <- data[data$sport %in% individual_sports, ]

# 団体競技のデータ
team_data <- data[data$sport %in% team_sports, ]

# 個人競技のデータ
individual_data <- data[data$sport %in% individual_sports, ]

# 団体競技のデータ
team_data <- data[data$sport %in% team_sports, ]

＃＃＃＃＃
# 必要なパッケージの読み込み
library(qgraph)
library(bootnet)

# データの読み込み
data <- read.csv("filtered_data.csv")

# スポーツ名の統一
data$sport <- gsub("テニス|硬式テニス|軟式テニス", "テニス", data$sport)
data$sport <- gsub("バスケット|バスケ|バスケットボール部", "バスケットボール", data$sport)
data$sport <- gsub("陸上競技|陸上競技部", "陸上", data$sport)
data$sport <- gsub("サッカー|男子サッカー|女子サッカー", "サッカー", data$sport)
data$sport <- gsub("野球|硬式野球|軟式野球", "野球", data$sport)
data$sport <- gsub("水泳|競泳", "水泳", data$sport)
data$sport <- gsub("トライアスロン部", "トライアスロン", data$sport)
data$sport <- gsub("ダンス部", "ダンス", data$sport)
data$sport <- gsub("ボート部", "ボート", data$sport)
data$sport <- gsub("フットサル部", "フットサル", data$sport)
data$sport <- gsub("体操競技", "体操", data$sport)

# 個人競技と団体競技のリスト
individual_sports <- c("テニス", "水泳", "陸上", "ゴルフ", "バドミントン", 
                       "トライアスロン", "ランニング", "ダンス", "ボート", 
                       "体操", "卓球", "スキー", "スノーボード", "柔道", 
                       "剣道", "空手", "アーチェリー", "フィギュアスケート")

team_sports <- c("サッカー", "バスケットボール", "バレーボール", "ラグビー", 
                 "野球", "ハンドボール", "フットサル", "アイスホッケー", 
                 "アメリカンフットボール", "ラクロス", "ホッケー")

# 個人競技と団体競技に分ける
individual_data <- data[data$sport %in% individual_sports, ]
team_data <- data[data$sport %in% team_sports, ]

# 不要な列 ppfi_j00 を削除
individual_data <- subset(individual_data, select = -ppfi_j00)
team_data <- subset(team_data, select = -ppfi_j00)

# PPFI と SS の項目名
ppfi_columns <- paste0("ppfi_j", sprintf("%02d", 1:15))
ss_columns <- paste0("ss", sprintf("%02d", 1:12))
columns_to_convert <- c(ppfi_columns, ss_columns)

# 数値化
individual_data[, columns_to_convert] <- lapply(individual_data[, columns_to_convert], function(x) as.numeric(as.character(x)))
team_data[, columns_to_convert] <- lapply(team_data[, columns_to_convert], function(x) as.numeric(as.character(x)))

# PPFI の逆転項目の処理
# PPFI は 1～7 の Likert スケール
reverse_items <- paste0("ppfi_j", sprintf("%02d", 1:5))  # 逆転項目
individual_data[, reverse_items] <- 8 - individual_data[, reverse_items]
team_data[, reverse_items] <- 8 - team_data[, reverse_items]

# PPFI 下位尺度の計算
individual_data$PPFI_avoidance <- rowMeans(individual_data[, c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")], na.rm = TRUE)
individual_data$PPFI_acceptance <- rowMeans(individual_data[, c("ppfi_j06", "ppfi_j07", "ppfi_j08", "ppfi_j09", "ppfi_j10")], na.rm = TRUE)
individual_data$PPFI_harnessing <- rowMeans(individual_data[, c("ppfi_j11", "ppfi_j12", "ppfi_j13", "ppfi_j14", "ppfi_j15")], na.rm = TRUE)

team_data$PPFI_avoidance <- rowMeans(team_data[, c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")], na.rm = TRUE)
team_data$PPFI_acceptance <- rowMeans(team_data[, c("ppfi_j06", "ppfi_j07", "ppfi_j08", "ppfi_j09", "ppfi_j10")], na.rm = TRUE)
team_data$PPFI_harnessing <- rowMeans(team_data[, c("ppfi_j11", "ppfi_j12", "ppfi_j13", "ppfi_j14", "ppfi_j15")], na.rm = TRUE)

# SS 下位尺度の計算
individual_data$SS_goal_achievement <- rowMeans(individual_data[, c("ss01", "ss02", "ss03", "ss04")], na.rm = TRUE)
individual_data$SS_practical_skills <- rowMeans(individual_data[, c("ss05", "ss06", "ss07", "ss08")], na.rm = TRUE)
individual_data$SS_ideal_performance <- rowMeans(individual_data[, c("ss09", "ss10", "ss11", "ss12")], na.rm = TRUE)

team_data$SS_goal_achievement <- rowMeans(team_data[, c("ss01", "ss02", "ss03", "ss04")], na.rm = TRUE)
team_data$SS_practical_skills <- rowMeans(team_data[, c("ss05", "ss06", "ss07", "ss08")], na.rm = TRUE)
team_data$SS_ideal_performance <- rowMeans(team_data[, c("ss09", "ss10", "ss11", "ss12")], na.rm = TRUE)

# 下位尺度データを抽出
individual_data_scales <- individual_data[, c("PPFI_avoidance", "PPFI_acceptance", "PPFI_harnessing",
                                              "SS_goal_achievement", "SS_practical_skills", "SS_ideal_performance")]

team_data_scales <- team_data[, c("PPFI_avoidance", "PPFI_acceptance", "PPFI_harnessing",
                                  "SS_goal_achievement", "SS_practical_skills", "SS_ideal_performance")]

# 個人競技のネットワーク分析
result_individual <- estimateNetwork(individual_data_scales, 
                                     default = "EBICglasso", 
                                     corMethod = "cor", 
                                     corArgs = list(method = "spearman"))

# 団体競技のネットワーク分析
result_team <- estimateNetwork(team_data_scales, 
                               default = "EBICglasso", 
                               corMethod = "cor", 
                               corArgs = list(method = "spearman"))

# 個人競技のネットワーク可視化
qgraph(result_individual$graph, 
       layout = "spring", 
       labels = colnames(individual_data_scales), 
       title = "個人競技 - 下位尺度レベルのネットワーク")

# 団体競技のネットワーク可視化
qgraph(result_team$graph, 
       layout = "spring", 
       labels = colnames(team_data_scales), 
       title = "団体競技 - 下位尺度レベルのネットワーク")

# 個人競技のネットワーク可視化
qgraph(result_individual$graph, 
       layout = "spring", 
       labels = colnames(individual_data_scales), 
       title = "個人競技 - 下位尺度レベルのネットワーク", 
       vsize = 10,            # ノードのサイズを大きく
       label.cex = 2,         # ラベルの文字サイズを大きく
       legend.cex = 1.5)      # 凡例の文字サイズを調整

# 団体競技のネットワーク可視化
qgraph(result_team$graph, 
       layout = "spring", 
       labels = colnames(team_data_scales), 
       title = "団体競技 - 下位尺度レベルのネットワーク", 
       vsize = 10,            # ノードのサイズを大きく
       label.cex = 2,         # ラベルの文字サイズを大きく
       legend.cex = 1.5)      # 凡例の文字サイズを調整

##################################################################
# 必要なパッケージの読み込み
library(qgraph)
library(bootnet)

# データの読み込み
data <- read.csv("filtered_data.csv")

# スポーツ名の統一
data$sport <- gsub("テニス|硬式テニス|軟式テニス", "テニス", data$sport)
data$sport <- gsub("バスケット|バスケ|バスケットボール部", "バスケットボール", data$sport)
data$sport <- gsub("陸上競技|陸上競技部", "陸上", data$sport)
data$sport <- gsub("サッカー|男子サッカー|女子サッカー", "サッカー", data$sport)
data$sport <- gsub("野球|硬式野球|軟式野球", "野球", data$sport)
data$sport <- gsub("水泳|競泳", "水泳", data$sport)
data$sport <- gsub("トライアスロン部", "トライアスロン", data$sport)
data$sport <- gsub("ダンス部", "ダンス", data$sport)
data$sport <- gsub("ボート部", "ボート", data$sport)
data$sport <- gsub("フットサル部", "フットサル", data$sport)
data$sport <- gsub("体操競技", "体操", data$sport)

# 個人競技と団体競技のリスト
individual_sports <- c("テニス", "水泳", "陸上", "ゴルフ", "バドミントン", 
                       "トライアスロン", "ランニング", "ダンス", "ボート", 
                       "体操", "卓球", "スキー", "スノーボード", "柔道", 
                       "剣道", "空手", "アーチェリー", "フィギュアスケート")

team_sports <- c("サッカー", "バスケットボール", "バレーボール", "ラグビー", 
                 "野球", "ハンドボール", "フットサル", "アイスホッケー", 
                 "アメリカンフットボール", "ラクロス", "ホッケー")

# 個人競技と団体競技に分ける
individual_data <- data[data$sport %in% individual_sports, ]
team_data <- data[data$sport %in% team_sports, ]

# 不要な列 ppfi_j00 を削除
individual_data <- subset(individual_data, select = -ppfi_j00)
team_data <- subset(team_data, select = -ppfi_j00)

# PPFI と SS の項目名
ppfi_columns <- paste0("ppfi_j", sprintf("%02d", 1:15))
ss_columns <- paste0("ss", sprintf("%02d", 1:12))
columns_to_convert <- c(ppfi_columns, ss_columns)

# 数値化
individual_data[, columns_to_convert] <- lapply(individual_data[, columns_to_convert], function(x) as.numeric(as.character(x)))
team_data[, columns_to_convert] <- lapply(team_data[, columns_to_convert], function(x) as.numeric(as.character(x)))

# PPFI の逆転項目の処理
# PPFI は 1～7 の Likert スケール
reverse_items <- paste0("ppfi_j", sprintf("%02d", 1:5))  # 逆転項目
individual_data[, reverse_items] <- 8 - individual_data[, reverse_items]
team_data[, reverse_items] <- 8 - team_data[, reverse_items]

# PPFI 下位尺度の計算
individual_data$PPFI_avoidance <- rowMeans(individual_data[, c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")], na.rm = TRUE)
individual_data$PPFI_acceptance <- rowMeans(individual_data[, c("ppfi_j06", "ppfi_j07", "ppfi_j08", "ppfi_j09", "ppfi_j10")], na.rm = TRUE)
individual_data$PPFI_harnessing <- rowMeans(individual_data[, c("ppfi_j11", "ppfi_j12", "ppfi_j13", "ppfi_j14", "ppfi_j15")], na.rm = TRUE)

team_data$PPFI_avoidance <- rowMeans(team_data[, c("ppfi_j01", "ppfi_j02", "ppfi_j03", "ppfi_j04", "ppfi_j05")], na.rm = TRUE)
team_data$PPFI_acceptance <- rowMeans(team_data[, c("ppfi_j06", "ppfi_j07", "ppfi_j08", "ppfi_j09", "ppfi_j10")], na.rm = TRUE)
team_data$PPFI_harnessing <- rowMeans(team_data[, c("ppfi_j11", "ppfi_j12", "ppfi_j13", "ppfi_j14", "ppfi_j15")], na.rm = TRUE)

# SS 下位尺度の計算
individual_data$SS_goal_achievement <- rowMeans(individual_data[, c("ss01", "ss02", "ss03", "ss04")], na.rm = TRUE)
individual_data$SS_practical_skills <- rowMeans(individual_data[, c("ss05", "ss06", "ss07", "ss08")], na.rm = TRUE)
individual_data$SS_ideal_performance <- rowMeans(individual_data[, c("ss09", "ss10", "ss11", "ss12")], na.rm = TRUE)

team_data$SS_goal_achievement <- rowMeans(team_data[, c("ss01", "ss02", "ss03", "ss04")], na.rm = TRUE)
team_data$SS_practical_skills <- rowMeans(team_data[, c("ss05", "ss06", "ss07", "ss08")], na.rm = TRUE)
team_data$SS_ideal_performance <- rowMeans(team_data[, c("ss09", "ss10", "ss11", "ss12")], na.rm = TRUE)

# 下位尺度データを抽出
individual_data_scales <- individual_data[, c(ppfi_columns, ss_columns)]
team_data_scales <- team_data[, c(ppfi_columns, ss_columns)]

# 個人競技のネットワーク分析
result_individual <- estimateNetwork(individual_data_scales, 
                                     default = "EBICglasso", 
                                     corMethod = "cor", 
                                     corArgs = list(method = "spearman"))

# 団体競技のネットワーク分析
result_team <- estimateNetwork(team_data_scales, 
                               default = "EBICglasso", 
                               corMethod = "cor", 
                               corArgs = list(method = "spearman"))

# 個人競技のネットワーク可視化
qgraph(result_individual$graph, 
       layout = "spring", 
       labels = colnames(individual_data_scales), 
       title = "個人競技 - 項目レベルのネットワーク", 
       vsize = 10,            # ノードのサイズを大きく
       label.cex = 2,         # ラベルの文字サイズを大きく
       legend.cex = 1.5
       threshold = 0.2)      # 凡例の文字サイズを調整

# 団体競技のネットワーク可視化
qgraph(result_team$graph, 
       layout = "spring", 
       labels = colnames(team_data_scales), 
       title = "団体競技 - 項目レベルのネットワーク", 
       vsize = 10,            # ノードのサイズを大きく
       label.cex = 2,         # ラベルの文字サイズを大きく
       legend.cex = 1.5)      # 凡例の文字サイズを調整


#ネットワークが出なかったので別の方法
result_individual <- estimateNetwork(individual_data_scales, 
                                     default = "glasso", 
                                     corMethod = "cor", 
                                     corArgs = list(method = "spearman"))
result_team <- estimateNetwork(team_data_scales, 
                               default = "glasso", 
                               corMethod = "cor", 
                               corArgs = list(method = "spearman"))
# 個人競技のネットワーク可視化
qgraph(result_individual$graph, 
       layout = "spring", 
       labels = colnames(individual_data_scales), 
       title = "個人競技 - 項目レベルのネットワーク", 
       vsize = 10, 
       label.cex = 2, 
       legend.cex = 1.5)qgraph(result_individual$graph, 
       layout = "spring", 
       labels = colnames(individual_data_scales), 
       title = "個人競技 - 項目レベルのネットワーク", 
       vsize = 10, 
       label.cex = 2, 
       legend.cex = 1.5, 
       threshold = 0.2)  # 相関が0.2以上のエッジのみ表示


# チーム競技のネットワーク可視化
qgraph(result_team$graph, 
       layout = "spring", 
       labels = colnames(team_data_scales), 
       title = "チーム競技 - 項目レベルのネットワーク", 
       vsize = 10, 
       label.cex = 2, 
       legend.cex = 1.5)


#ブリッジ中心性
library(qgraph)
library(bootnet)

# 個人競技と団体競技のデータセットをそれぞれ利用する
data_individual <- individual_data_scales  # 個人競技データ
data_team <- team_data_scales  # 団体競技データ

# 個人競技のネットワーク推定
result_individual <- estimateNetwork(data_individual, 
                                     default = "EBICglasso", 
                                     corMethod = "cor_auto")

# 団体競技のネットワーク推定
result_team <- estimateNetwork(data_team, 
                               default = "EBICglasso", 
                               corMethod = "cor_auto")

# ラベル設定
label <- c("SS1","SS2","SS3","SS4","SS5","SS6","SS7","SS8","SS9","SS10","SS11","SS12",
           "PPFI1","PPFI2","PPFI3","PPFI4","PPFI5","PPFI6","PPFI7","PPFI8","PPFI9","PPFI10",
           "PPFI11","PPFI12","PPFI13","PPFI14","PPFI15")

# グループ設定 (SS と PPFI)
group <- list("SS" = c(1:12), "PPFI" = c(13:27))

# 個人競技のネットワーク可視化
plot(result_individual, layout = "spring", groups = group, labels = label)

# 団体競技のネットワーク可視化
plot(result_team, layout = "spring", groups = group, labels = label)

#えらー 個人競技のブリッジ中心性計算
bridge_centrality_individual <- bridge(result_individual, 
                                       communities = c(rep("SS", 12), rep("PPFI", 15)))

# 団体競技のブリッジ中心性計算
bridge_centrality_team <- bridge(result_team, 
                                 communities = c(rep("SS", 12), rep("PPFI", 15)))

# 個人競技のブリッジ中心性を表示
print(bridge_centrality_individual)

# 団体競技のブリッジ中心性を表示
print(bridge_centrality_team)

# 個人競技のブリッジ中心性のプロット
plot(bridge_centrality_individual, main = "個人競技 - ブリッジ中心性")

# 団体競技のブリッジ中心性のプロット
plot(bridge_centrality_team, main = "団体競技 - ブリッジ中心性")
# 個人競技のネットワーク推定
result_individual <- estimateNetwork(data_individual, 
                                     default = "EBICglasso", 
                                     corMethod = "cor_auto")

# 団体競技のネットワーク推定
result_team <- estimateNetwork(data_team, 
                               default = "EBICglasso", 
                               corMethod = "cor_auto")

# 推定したネットワークを qgraph オブジェクトに変換
network_individual <- qgraph(result_individual$graph, layout = "spring")
network_team <- qgraph(result_team$graph, layout = "spring")

# ブリッジ中心性の計算 (個人競技)
bridge_centrality_individual <- bridge(network_individual, 
                                       communities = c(rep("SS", 12), rep("PPFI", 15)))

# ブリッジ中心性の計算 (団体競技)
bridge_centrality_team <- bridge(network_team, 
                                 communities = c(rep("SS", 12), rep("PPFI", 15)))

# 結果を表示
print(bridge_centrality_individual)
print(bridge_centrality_team)

# 可視化 (必要に応じて)
plot(bridge_centrality_individual, main = "個人競技 - ブリッジ中心性")
plot(bridge_centrality_team, main = "団体競技 - ブリッジ中心性")
# 個人競技のブリッジ中心性の計算
bridge_centrality_individual <- bridge(result_individual, 
                                       communities = c(rep("SS", 12), rep("PPFI", 15)))

# 団体競技のブリッジ中心性の計算
bridge_centrality_team <- bridge(result_team, 
                                 communities = c(rep("SS", 12), rep("PPFI", 15)))

# 結果を表示
print(bridge_centrality_individual)
print(bridge_centrality_team)

# 可視化 (必要に応じて)
plot(bridge_centrality_individual, main = "個人競技 - ブリッジ中心性")
plot(bridge_centrality_team, main = "団体競技 - ブリッジ中心性")
# 推定結果の中身を確認
str(result_individual)
str(result_team)


#ブリッジ中心性指標を算出
# 必要なパッケージを読み込む
library(dplyr)
library(networktools)
library(qgraph)

# データ準備 (個人競技用と団体競技用)
# 個人競技データ (individual_sports)、団体競技データ (team_sports) が既に分割されていると仮定
data_individual <- individual_sports %>%
  select(ppfi_j01:ppfi_j15, ss01:ss12)

data_team <- team_sports %>%
  select(ppfi_j01:ppfi_j15, ss01:ss12)

# ネットワーク推定
res_individual <- estimateNetwork(data_individual, default = "EBICglasso", corMethod = "cor_auto")
res_team <- estimateNetwork(data_team, default = "EBICglasso", corMethod = "cor_auto")

# プロット保存 (ブリッジ中心性指標計算に使用)
plot_res_individual <- plot(res_individual, layout = "spring")
plot_res_team <- plot(res_team, layout = "spring")

# コミュニティの設定
# 1: PPFI, 2: SS
communities <- c(rep("1", 15), rep("2", 12))

# 個人競技のブリッジ中心性指標
bridge_individual <- bridge(plot_res_individual, communities = communities)

# 団体競技のブリッジ中心性指標
bridge_team <- bridge(plot_res_team, communities = communities)

# ブリッジ中心性指標のプロット
# 個人競技
plot(bridge_individual, include = c("Bridge Strength", "Bridge Betweenness", "Bridge Closeness"))

# 団体競技
plot(bridge_team, include = c("Bridge Strength", "Bridge Betweenness", "Bridge Closeness"))
