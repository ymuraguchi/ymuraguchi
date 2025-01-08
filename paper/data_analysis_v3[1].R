
# 使用するパッケージ
library(tidyverse)
library(bootnet)
library(qgraph)

# 逆転項目の処理（PPFIとPANAS）
# PPFI-Jの逆転項目（1～5）
reverse_ppfi_items <- paste0("ppfi_j", sprintf("%02d", 1:5))

# PPFI-Jが1から7のLikertスケールの場合、逆転項目を7 - スコアで反転
data[, reverse_ppfi_items] <- 8 - data[, reverse_ppfi_items]

# PANASの逆転項目（1, 3, 5, 6, 9, 11, 15, 16, 18, 19）
panas_reverse_items <- c("panas01", "panas03", "panas05", "panas06", "panas09", 
                         "panas11", "panas15", "panas16", "panas18", "panas19")
data[, panas_reverse_items] <- 7 - data[, panas_reverse_items]  # PANASが1から5のLikertスケールの場合


######データの読み込み
data_ss <- data %>% 
  select(
    ss01, ss02, ss03, ss04, ss05, ss06, ss07, ss08, ss09, ss10,
    ss11, ss12)
data_ppfi <- data %>% 
  select(
    ppfi_j01, ppfi_j02, ppfi_j03, ppfi_j04, ppfi_j05,
    ppfi_j06, ppfi_j07, ppfi_j08, ppfi_j09, ppfi_j10,
    ppfi_j11, ppfi_j12, ppfi_j13, ppfi_j14, ppfi_j15)
data_ss_ppfi <- data %>% 
  select(
    ss01, ss02, ss03, ss04, ss05, ss06, ss07, ss08, ss09, ss10,
    ss11, ss12,
    ppfi_j01, ppfi_j02, ppfi_j03, ppfi_j04, ppfi_j05,
    ppfi_j06, ppfi_j07, ppfi_j08, ppfi_j09, ppfi_j10,
    ppfi_j11, ppfi_j12, ppfi_j13, ppfi_j14, ppfi_j15)
data <- read.csv("data.csv")
# データの選択
data_ss <- data 
  select(
    ss01, ss02, ss03, ss04, ss05, ss06, ss07, ss08, ss09, ss10,
    ss11, ss12)
data_ppfi <- data 
  select(
    ppfi_j01, ppfi_j02, ppfi_j03, ppfi_j04, ppfi_j05,
    ppfi_j06, ppfi_j07, ppfi_j08, ppfi_j09, ppfi_j10,
    ppfi_j11, ppfi_j12, ppfi_j13, ppfi_j14, ppfi_j15)
data_ss_ppfi <- data 
  select(
    ss01, ss02, ss03, ss04, ss05, ss06, ss07, ss08, ss09, ss10,
    ss11, ss12,
    ppfi_j01, ppfi_j02, ppfi_j03, ppfi_j04, ppfi_j05,
    ppfi_j06, ppfi_j07, ppfi_j08, ppfi_j09, ppfi_j10,
    ppfi_j11, ppfi_j12, ppfi_j13, ppfi_j14, ppfi_j15)
#ネットワーク推定とプロット
## ss
result_ss <- estimateNetwork(data_ss,default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"))
plot(result_ss, layout = "spring",labels = TRUE)
## ppfi
result_ppfi <- estimateNetwork(data_ppfi,default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"))
plot(result_ppfi, layout = "spring",labels = TRUE)
## ppfi
result_panas <- estimateNetwork(data_ppfi,default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"))
plot(result_panas, layout = "spring",labels = TRUE)
## ppfiとss
result_ss_ppfi <- estimateNetwork(data_ss_ppfi,default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"))
label <- c("SS1","SS2","SS3","SS4","SS5","SS6","SS7","SS8","SS9","SS10","SS11","SS12",
           "PPFI1","PPFI2","PPFI3","PPFI4","PPFI5","PPFI6","PPFI7","PPFI8","PPFI9","PPFI10",
           "PPFI11","PPFI12","PPFI13","PPFI14","PPFI15")
group <- list("SS"=c(1:12),"PHQ9"=c(13:27)) 
plot(result_ss_ppfi, layout = "spring", groups = group, labels = label)

#filter_data#
# filtered.data.csvを読み込む
data <- read.csv("filtered_data.csv")

# filtered_data.csvを読み込む
data <- read.csv("filtered_data.csv")

# データの選択
data_ss <- data %>% 
  select(
    ss01, ss02, ss03, ss04, ss05, ss06, ss07, ss08, ss09, ss10,
    ss11, ss12)

data_ppfi <- data %>% 
  select(
    ppfi_j01, ppfi_j02, ppfi_j03, ppfi_j04, ppfi_j05,
    ppfi_j06, ppfi_j07, ppfi_j08, ppfi_j09, ppfi_j10,
    ppfi_j11, ppfi_j12, ppfi_j13, ppfi_j14, ppfi_j15)

data_ss_ppfi <- data %>% 
  select(
    ss01, ss02, ss03, ss04, ss05, ss06, ss07, ss08, ss09, ss10,
    ss11, ss12,
    ppfi_j01, ppfi_j02, ppfi_j03, ppfi_j04, ppfi_j05,
    ppfi_j06, ppfi_j07, ppfi_j08, ppfi_j09, ppfi_j10,
    ppfi_j11, ppfi_j12, ppfi_j13, ppfi_j14, ppfi_j15)

# ネットワークの推定とプロット
## ss
result_ss <- estimateNetwork(data_ss, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"))
plot(result_ss, layout = "spring", labels = TRUE)

## ppfi
result_ppfi <- estimateNetwork(data_ppfi, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"))
plot(result_ppfi, layout = "spring", labels = TRUE)

## ssとppfi
result_ss_ppfi <- estimateNetwork(data_ss_ppfi, default = "EBICglasso", corMethod = "cor", corArgs = list(method = "spearman"))
label <- c("SS1","SS2","SS3","SS4","SS5","SS6","SS7","SS8","SS9","SS10","SS11","SS12",
           "PPFI1","PPFI2","PPFI3","PPFI4","PPFI5","PPFI6","PPFI7","PPFI8","PPFI9","PPFI10",
           "PPFI11","PPFI12","PPFI13","PPFI14","PPFI15")
group <- list("SS" = c(1:12), "PPFI" = c(13:27))
plot(result_ss_ppfi, layout = "spring", groups = group, labels = label)

##ブリッジ中心性
data_bridge <- data %>%
  select(
    ss01, ss02, ss03, ss04, ss05, ss06, ss07, ss08, ss09, ss10,
    ss11, ss12,
    ppfi_j01, ppfi_j02, ppfi_j03, ppfi_j04, ppfi_j05,
    ppfi_j06, ppfi_j07, ppfi_j08, ppfi_j09, ppfi_j10,
    ppfi_j11, ppfi_j12, ppfi_j13, ppfi_j14, ppfi_j15)
res_bridge <- estimateNetwork(data_bridge,default = "EBICglasso", corMethod = "cor_auto")

label <- c("SS1","SS2","SS3","SS4","SS5","SS6","SS7","SS8","SS9","SS10","SS11","SS12",
           "PPFI1","PPFI2","PPFI3","PPFI4","PPFI5","PPFI6","PPFI7","PPFI8","PPFI9","PPFI10",
           "PPFI11","PPFI12","PPFI13","PPFI14","PPFI15")
group <- list("SS"=c(1:12),"PHQ9"=c(13:27)) 
plot(res_bridge, layout = "spring", groups = group, labels = label)

bridge_centrality <- bridge(result_ss_ppfi, 
                            communities <- c(rep('SS', 12), rep('PPFI', 15))
##再チャレンジ
#データの選択と列名の変更
data_bridge <- data %>%
  select(
    SS1 = ss01, SS2 = ss02, SS3 = ss03, SS4 = ss04, SS5 = ss05, SS6 = ss06,
    SS7 = ss07, SS8 = ss08, SS9 = ss09, SS10 = ss10, SS11 = ss11, SS12 = ss12,
    PPFI1 = ppfi_j01, PPFI2 = ppfi_j02, PPFI3 = ppfi_j03, PPFI4 = ppfi_j04,
    PPFI5 = ppfi_j05, PPFI6 = ppfi_j06, PPFI7 = ppfi_j07, PPFI8 = ppfi_j08,
    PPFI9 = ppfi_j09, PPFI10 = ppfi_j10, PPFI11 = ppfi_j11, PPFI12 = ppfi_j12,
    PPFI13 = ppfi_j13, PPFI14 = ppfi_j14, PPFI15 = ppfi_j15
  )

# ネットワーク推定
res_bridge <- estimateNetwork(data_bridge, default = "EBICglasso", corMethod = "cor_auto")

# ブリッジ中心性の計算
data_bridge <- data %>%
  select(
    SS1 = ss01, SS2 = ss02, SS3 = ss03, SS4 = ss04, SS5 = ss05, SS6 = ss06,
    SS7 = ss07, SS8 = ss08, SS9 = ss09, SS10 = ss10, SS11 = ss11, SS12 = ss12,
    PPFI1 = ppfi_j01, PPFI2 = ppfi_j02, PPFI3 = ppfi_j03, PPFI4 = ppfi_j04,
    PPFI5 = ppfi_j05, PPFI6 = ppfi_j06, PPFI7 = ppfi_j07, PPFI8 = ppfi_j08,
    PPFI9 = ppfi_j09, PPFI10 = ppfi_j10, PPFI11 = ppfi_j11, PPFI12 = ppfi_j12,
    PPFI13 = ppfi_j13, PPFI14 = ppfi_j14, PPFI15 = ppfi_j15
  )

res_bridge <- estimateNetwork(data_bridge, default = "EBICglasso", corMethod = "cor_auto")


label <- c("SS1","SS2","SS3","SS4","SS5","SS6","SS7","SS8","SS9","SS10","SS11","SS12",
                      "PPFI1","PPFI2","PPFI3","PPFI4","PPFI5","PPFI6","PPFI7","PPFI8","PPFI9","PPFI10",
                      "PPFI11","PPFI12","PPFI13","PPFI14","PPFI15")
group <- list("SS"=c(1:12),"PHQ9"=c(13:27)) 
plot_res_bridge <- plot(res_bridge, layout = "spring", groups = group, labels = label)


##
library(networktools)
bridge_centrality <- bridge(plot_res_bridge, 
                            communities = c('1','1','1','1','1','1','1','1','1','1','1','1',
                                            '2','2','2','2','2','2','2','2','2','2','2','2','2','2','2'))

# ブリッジ中心性のプロット
plot(bridge_centrality, include = c("Bridge Strength", "Bridge Betweenness", "Bridge Closeness"))
