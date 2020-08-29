### 状態空間モデルによる指数分布族の母数の追跡
### tokyo_covid19_patients データを用いた例

## パッケージの読み込み
library(tidyverse)
library(scales) # 年月日表示
library(plotly) 
library(zoo)    # 移動平均のため
library(KFAS)   # 状態空間モデルの構成

## データの取得と整理 (東京都)
myData <-
    read.csv("https://stopcovid19.metro.tokyo.lg.jp/data/130001_tokyo_covid19_patients.csv") %>% 
    dplyr::select(公表_年月日) %>% 
    dplyr::rename(date=公表_年月日) %>% 
    dplyr::transmute(date=as.Date(date)) %>%
    dplyr::group_by(date) %>% 
    dplyr::summarize(patients = n()) # 陽性者数

## データの視覚化
p <-
  ggplot(data = myData,
	 mapping = aes(x = date,
		       y = patients)) +
  geom_point() + 
  geom_line(data = myData %>% # 7日移動平均
	      dplyr::mutate(patients =
			      zoo::rollmean(x = patients,
					    k = 7, 
					    align = "right",
					    fill = NA)),
	    color = "green", alpha = 0.5, size = 1,
	    na.rm = TRUE) +
  geom_line(data = myData %>% # 14日移動平均
	      dplyr::mutate(patients =
			      zoo::rollmean(x = patients,
					    k = 14, 
					    align = "right",
					    fill = NA)),
	    color = "orange", alpha = 0.5, size = 1,
	    na.rm = TRUE) +
  scale_x_date(labels = date_format("%y-%m-%d"), # 年月日表示
	       breaks = date_breaks("1 week")) + # 週毎
  theme(axis.text.x = element_text(angle = 90, 
				   vjust = 0.5, hjust=1)) +
  labs(title = "Tokyo COVID-19 patients",
       x = "date",
       y = "number of patients")
print(p) # グラフ出力
ggplotly() # plotly表示 (browser)

## 状態空間モデルの構成
myModel <-
  SSModel(data = myData, 
	  formula = patients ~ # 目的変数
	    -1 + # 定数項を持たない
	    SSMtrend(degree = 2, # トレンド成分の定義
		     Q = list(0,NA)) +
	    SSMcycle(period = 7, # 周期成分の定義
		     Q = 0.05),
	  distribution = "poisson") # 目的変数は Poisson 分布
stateName <- colnames(myModel$Z) # 状態変数の名称

## 母数推定 
fit <- fitSSM(myModel, 
		   inits = 0, # 初期値
		   method = "BFGS") # 最適化法
## 状態推定 (推定した母数を用いる)
out <- KFS(fit$model,
                filtering = c("state","mean"),
                smoothing = c("state","mean"))

alpha <- 0.05 # 有意水準 (信頼区間の準備)
zq <- qnorm(1-alpha/2) # 正規分布の (1-alpha/2) 分位点
tmp <- # 必要な状態変数を取り出す
  cbind(myData["date"],
	out$alphahat, # 状態変数の平均
	t(sqrt(apply(out$V,3,diag)))) # 標準偏差
names(tmp)[-1] <- # 名前を付与
  paste(rep(c("value","sd"), each = length(stateName)),
	rep(stateName, times = 2),
	sep = "_")
myState <- # tidy data 化
  tmp %>% 
  tidyr::pivot_longer(
    -date,
    names_to = c(".value", "name"), 
    names_pattern = "(.*)_(.*)") %>%
  dplyr::mutate_at("name", ~factor(., levels = unique(.)))
p <- 
  ggplot(data = myState, group = name,
	 mapping = aes(x = date,
		       y = value)) +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = value-zq*sd,
			    ymax = value+zq*sd),
	      fill = "blue", alpha = 0.2) +
  facet_grid(name ~ ., scale = "free_y") + 
  scale_x_date(labels = date_format("%y-%m-%d"), 
	       breaks = date_breaks("1 week")) + 
  theme(axis.text.x = element_text(angle = 90,
				   vjust = 0.5, hjust=1)) +
  labs(title = "Poisson model with trend and cycle",
       x = "date",
       y = "estimates")
print(p)
ggplotly()

## 状態空間モデルにもとづく平均の推定
tmp <- KFAS::signal(out, states = "trend")
tmpa <- tmp$signal
tmpb <- sqrt(tmp$variance[1,1,])
p <-
  ggplot(data = myData %>%
	   dplyr::mutate(mean = exp(tmpa),
			 lwr = exp(tmpa - zq*tmpb),
			 upr = exp(tmpa + zq*tmpb)),
	 mapping = aes(x = date,
		       y = patients)) +
  geom_point() +
  geom_line(mapping = aes(y = mean),
	    color = "red", alpha = 0.5, size = 1) +
  geom_ribbon(mapping = aes(ymin = lwr, ymax = upr),
	      fill = "red", alpha = 0.2) +
  scale_x_date(labels = date_format("%y-%m-%d"), 
	       breaks = date_breaks("1 week")) + 
  theme(axis.text.x = element_text(angle = 90, 
				   vjust = 0.5, hjust=1)) +
  labs(title = "Tokyo COVID-19 patients",
       x = "date",
       y = "number of patients")
print(p) # グラフ出力
ggplotly() # plotly表示 (browser)

### 以下，補遺ためのコード

## 簡単な設定で実験
Y <- log(myData$patients) # 陽性患者数の対数
tmp <- tibble(t = 1:length(Y),
              Y = Y)
p <- 
    ggplot(data = tmp, 
           mapping = aes(x = t,
                         y = Y)) +
    geom_line(color = "green", alpha = 0.8) +
    xlab(label = "time")
print(p)

## いくつかのモデルで状態の事前分布を確認
Qs <- fitModel$model$Q[2,2,1] # 推定されたQslopeを利用
tmpc <- tibble(t = 1,
               var = rep(c("level","slope"),each = 2),
               name = "V1",
               value = c(150,-150,1.5,-1.5))
for(lambda in c(0.3,1,3)){
    model <- # モデルを作成
        SSModel(formula = Y ~ # 目的変数
              	    -1 + # 定数項を持たない
               	    SSMtrend(degree = 2, # トレンド成分の定義
               	             Q = list(0,lambda*Qs)), H = 0.1)
    ## モデルにもとづいて状態を生成 (状態系列の事前分布)
    tmpa <- simulateSSM(model,
                        nsim = 16,
                        conditional=FALSE)
    tmpb <- 
        rbind(tibble(t = 1:length(Y),
                     var = "level",
                     as_tibble(tmpa[,1,])),
              tibble(t = 1:length(Y),
                     var = "slope",
                     as_tibble(tmpa[,2,]))) %>%
        tidyr::pivot_longer(-c(t,var)) 
    p <- 
        ggplot(data = tmpb, group = var,
               mapping = aes(x = t,
                             y = value,
                             color = name)) +
        geom_blank(data = tmpc) +
        geom_line(alpha = 0.5) +
        facet_grid(var ~ ., scale = "free_y") +
        theme(legend.position = "none") +
        xlab(label = "time")
    print(p)
}

## モデルの設定 (以降で使うモデル)
Qs <- fitModel$model$Q[2,2,1]
model <-
  SSModel(formula = Y ~ # 目的変数
              	    -1 + # 定数項を持たない
               	    SSMtrend(degree = 2, # トレンド成分の定義
               	             Q = list(0,Qs)), H = 0.1)
## 事前分布からサンプリングした level から観測値 Y と相関の高いものを取り出す
tmpa <- simulateSSM(model,
                    nsim = 30000,
                    conditional = FALSE)
tmpb <- rbind(
    tibble(t = 1:length(Y),
           var = "level",
           as_tibble(
               tmpa[,1,rank(apply(tmpa[,1,],2,
                                  function(x){var(x-Y)}))<17])) %>%
    tidyr::pivot_longer(-c(t,var)),
    tibble(t = 1:length(Y),
           var = "signal",
           name = "V17",
           value=Y))
## 図示
p <- 
    ggplot(data = tmpb, group = var,
           mapping = aes(x = t,
                         y = value,
                         color = name)) +
    geom_line(alpha = 0.5) +
    facet_grid(var ~ ., scale = "free_y") +
    theme(legend.position = "none") +
    xlab(label = "time")
print(p)

## モデルにもとづいて状態を生成 (状態系列の事前分布)
prior <- simulateSSM(model,
                     nsim = 16,
                     conditional = FALSE)
## 観測データで条件付けて状態を生成 (状態系列の事後分布)
postr <- simulateSSM(model,
                     nsim = 16,
                     conditional = TRUE)
## 図示
for(s in c("prior","postr")){
    tmpa <- eval(parse(text=s))
    tmpb <- 
        rbind(tibble(t = 1:length(Y),
                     var = "level",
                     as_tibble(tmpa[,1,])),
              tibble(t = 1:length(Y),
                     var = "slope",
                     as_tibble(tmpa[,2,]))) %>%
        tidyr::pivot_longer(-c(t,var)) 
    p <- 
        ggplot(data = tmpb, group = var,
               mapping = aes(x = t,
                             y = value,
                             color = name)) +
        geom_line(alpha = 0.5) +
        facet_grid(var ~ ., scale = "free_y") +
        theme(legend.position = "none") +
        xlab(label = "time")
    print(p)
}

## 周期成分の変動の分散の検討
Qc <- 10^seq(0,-2,length=32)/2
lL <- double(length(Qc))
for(i in 1:length(Qc)) {
    tmp <-
        SSModel(data = myData, 
                formula = patients ~ 
                    -1 + 
                    SSMtrend(degree = 2, 
                             Q = list(0,NA)) +
                    SSMcycle(period = 7, 
                             Q = Qc[i]), # 変更
                distribution = "poisson") 
    lL[i] <- logLik(fitSSM(tmp,
                           inits = 0,
                           method = "BFGS")$model)
}
p <-
    ggplot(data = data.frame(Q=Qc, logLik=lL),
           mapping = aes(x = Q, y = logLik)) +
    geom_line() +
    scale_x_log10() +
    labs(title = "assessment of variance",
       x = "Q_cycle",
       y = "log likelihood")
print(p)
print(Qc[which.max(lL)])

## データの取得と整理 (厚生労働省)
myData <- read.csv("https://www.mhlw.go.jp/content/pcr_positive_daily.csv")
names(myData) <- c("date","patients")
myData$date <- as.Date(myData$date)
