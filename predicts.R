### 基本的な時系列モデルによる予測
### 厚生労働省のCOVID-19の感染者数データを用いた例

## パッケージの読み込み
library(forecast)
library(tidyverse)
library(scales) # 年月日表示
library(plotly) 
library(zoo)    # 時系列表示
library(ggfortify)

## データの取得と整理 
patients <-
    read.csv("https://www.mhlw.go.jp/content/pcr_positive_daily.csv") %>%
    dplyr::rename(date=1, patients=2) %>% 
    dplyr::mutate(date=as.Date(date))
## 時系列データ(zooクラス)への変更
patients <- with(patients,
		   zoo(x=patients, order.by=date))

## データの視覚化
p <-
    ggplot(data = fortify(patients, melt = TRUE),
	   mapping = aes(x = Index,
			 y = Value)) +
    scale_x_date(labels = date_format("%y-%m-%d"), # 年月日表示
		 breaks = date_breaks("1 week")) + # 週毎
    theme(axis.text.x = element_text(angle = 90, 
				     vjust = 0.5, hjust=1)) +
    labs(title = "COVID-19 patients in Japan",
	 x = "date",
	 y = "number of patients")
## 棒グラフ
print(p + geom_col(fill="skyblue")) # グラフ出力
ggplotly() # plotly表示 (browser)
## 折れ線グラフ+常用対数表示
print(p + geom_line(colour="skyblue") + scale_y_log10())
ggplotly()

## 9月以降の第3波を対象とする
train <- window(patients,
		start="2020-09-15",
		end="2020-11-30")
test <- window(patients,
	       start="2020-12-01")

## 階差系列の性質
autoplot(diff(train)) +
  labs(x = "date",
       y = "D(patients)")
autoplot(acf(diff(train), plot = FALSE), # 自己相関
	 conf.int.fill = "royalblue",
	 conf.int.alpha =0.2,
	 conf.int.value = 0.7,
	 conf.int.type = "ma") +
  labs(title = "D(patients)")
autoplot(pacf(diff(train), plot = FALSE), # 偏自己相関
	 conf.int.fill = "royalblue",
	 conf.int.alpha =0.2,
	 conf.int.value = 0.7) +
  labs(title = "D(patients)",
       y = "PACF")

## 対数変換を確認する
ltrain <- log(train)
autoplot(diff(ltrain)) +
    labs(x = "date",
	 y = "D(log(patients))")
autoplot(acf(diff(ltrain), plot = FALSE), # 自己相関
	 conf.int.fill = "royalblue",
	 conf.int.alpha =0.2,
	 conf.int.value = 0.7,
	 conf.int.type = "ma") +
    labs(title = "D(log(patients))")
autoplot(pacf(diff(ltrain), plot = FALSE), # 偏自己相関
	 conf.int.fill = "royalblue",
	 conf.int.alpha =0.2,
	 conf.int.value = 0.7) +
    labs(title = "D(log(patients))",
	 y = "PACF")

## 基本統計量の確認
summary(as.numeric(diff(ltrain)))

## 7日の周期性を確認する
autoplot(diff(diff(ltrain), lag=7)) +
    labs(x = "date",
	 y = "D7*D(log(patients))")
autoplot(acf(diff(diff(ltrain), lag=7), plot = FALSE), # 自己相関
	 conf.int.fill = "royalblue",
	 conf.int.alpha =0.2,
	 conf.int.value = 0.7,
	 conf.int.type = "ma") +
    labs(title = "D7*D(log(patients))")
autoplot(pacf(diff(diff(ltrain), lag=7), plot = FALSE), # 偏自己相関
	 conf.int.fill = "royalblue",
	 conf.int.alpha =0.2,
	 conf.int.value = 0.7) +
    labs(title = "D7*D(log(patients))",
	 y = "PACF")

## drift付きのARIMAモデルの次数を自動推定
est.arima <- forecast::auto.arima(ltrain)
## 推定されたモデルを表示
print(est.arima)
## SARIMAモデルを当て嵌める場合は周期を指定する．
## frequency(ltrain) <- 7 # 7日周期の成分を仮定
## est.arima <- auto.arima(ltrain)
## このデータではモデルの推定はうまくいかない

## 診断プロット
tsdiag(est.arima)
## 残差に相関が残っているので，優れたモデルという訳ではない

## モデルによる当て嵌めの視覚化
p <- 
    ggplot(data = fortify(est.arima) %>%
	       dplyr::mutate(Index=as.Date(Index)),
	   mapping = aes(x = Index,
			 y = Data)) +
    geom_line(colour = "skyblue") +
    geom_line(mapping = aes(y = Fitted),
	      colour = "orange") +
    scale_x_date(labels = date_format("%y-%m-%d"), 
		 breaks = date_breaks("1 week")) + 
    theme(axis.text.x = element_text(angle = 90,
				     vjust = 0.5, hjust=1)) +
    labs(title = "Fitted by ARIMA model",
	 x = "date",
	 y = "log(patients)")
print(p)
ggplotly()

## 12月以降(最大60日)を予測してみる
p <- 
    ggplot(data = fortify(forecast(est.arima,
                                   h=min(length(test),60))) %>%
	       dplyr::mutate(Index=as.Date(Index)) %>%
	       left_join(fortify(test), by = "Index"), 
	   mapping = aes(x = Index,
			 y = exp(Data)),
	   na.rm = TRUE) +
    geom_line(colour = "skyblue",
	      na.rm = TRUE) +
    geom_line(mapping = aes(y = test),
	      colour = "red",
	      na.rm = TRUE) +
    geom_line(mapping = aes(y = exp(`Point Forecast`)),
	      colour = "royalblue",
	      na.rm = TRUE) +
    geom_ribbon(mapping = aes(ymin = exp(`Lo 80`),
			      ymax = exp(`Hi 80`)),
		fill = "royalblue", alpha = 0.3,
		na.rm = TRUE) +
    ## geom_ribbon(mapping = aes(ymin = exp(`Lo 95`),
    ##   			ymax = exp(`Hi 95`)),
    ##   	  fill = "royalblue", alpha = 0.1,
    ##   	  na.rm = TRUE) +
    scale_x_date(labels = date_format("%y-%m-%d"), 
		 breaks = date_breaks("1 week")) + 
    theme(axis.text.x = element_text(angle = 90,
				     vjust = 0.5, hjust=1)) +
    labs(title = "Prediction by ARIMA model",
	 x = "date",
	 y = "number of patients")
print(p)
ggplotly()

## StructTS による方法
est.sts <- StructTS(ltrain)
## 推定されたモデルを表示
print(est.sts)

## 診断プロット
tsdiag(est.sts)
## こちらも残差に相関が残っているので，優れたモデルという訳ではない

## StructTSによる時系列の分解結果
autoplot(est.sts)

## モデルによる当て嵌めの視覚化
p <- 
    ggplot(data = fortify(forecast(est.sts)) %>%
	       dplyr::mutate(Index=as.Date(Index)),
	   mapping = aes(x = Index,
			 y = Data),
	   na.rm = TRUE) +
    geom_line(colour = "skyblue",
	      na.rm = TRUE) +
    geom_line(mapping = aes(y = Fitted),
	      colour = "orange",
	      na.rm = TRUE) +
    scale_x_date(labels = date_format("%y-%m-%d"), 
		 breaks = date_breaks("1 week")) + 
    theme(axis.text.x = element_text(angle = 90,
				     vjust = 0.5, hjust=1)) +
    labs(title = "Fitted by Local Linear Structure model",
	 x = "date",
	 y = "log(patients)")
print(p)
ggplotly()

## 12月以降(最大60日)を予測してみる
p <- 
    ggplot(data = fortify(forecast(est.sts,
                                   h=min(length(test),60))) %>%
	       dplyr::mutate(Index=as.Date(Index)) %>%
	       left_join(fortify(test), by = "Index"), 
	   mapping = aes(x = Index,
			 y = exp(Data)),
	   na.rm = TRUE) +
    geom_line(colour = "skyblue",
	      na.rm = TRUE) +
    geom_line(mapping = aes(y = test),
	      colour = "red",
	      na.rm = TRUE) +
    geom_line(mapping = aes(y = exp(`Point Forecast`)),
	      colour = "royalblue",
	      na.rm = TRUE) +
    geom_ribbon(mapping = aes(ymin = exp(`Lo 80`),
			      ymax = exp(`Hi 80`)),
		fill = "royalblue", alpha = 0.3,
		na.rm = TRUE) +
    ## geom_ribbon(mapping = aes(ymin = exp(`Lo 95`),
    ##   			ymax = exp(`Hi 95`)),
    ##   	  fill = "royalblue", alpha = 0.1,
    ##   	  na.rm = TRUE) +
    scale_x_date(labels = date_format("%y-%m-%d"), 
		 breaks = date_breaks("1 week")) + 
    theme(axis.text.x = element_text(angle = 90,
				     vjust = 0.5, hjust=1)) +
    labs(title = "Prediction by Local Linear Structure model",
	 x = "date",
	 y = "number of patients")
print(p)
ggplotly()

## モデルの推定
est <- auto.arima(log(window(patients,start="2020-09-15")))
## 推定されたモデルの表示
print(est)
## 現在から60日先まで予測してみる
p <- 
    ggplot(data = fortify(forecast(est,h=60)) %>%
	       dplyr::mutate(Index=as.Date(Index)),
	   mapping = aes(x = Index,
			 y = exp(Data)),
	   na.rm = TRUE) +
    geom_line(colour = "skyblue",
	      na.rm = TRUE) +
    geom_line(mapping = aes(y = exp(`Point Forecast`)),
	      colour = "royalblue",
	      na.rm = TRUE) +
    geom_ribbon(mapping = aes(ymin = exp(`Lo 80`),
			      ymax = exp(`Hi 80`)),
		fill = "royalblue", alpha = 0.3,
		na.rm = TRUE) +
    geom_ribbon(mapping = aes(ymin = exp(`Lo 95`),
			      ymax = exp(`Hi 95`)),
		fill = "royalblue", alpha = 0.1,
		na.rm = TRUE) +
    scale_x_date(labels = date_format("%y-%m-%d"), 
		 breaks = date_breaks("1 week")) + 
    theme(axis.text.x = element_text(angle = 90,
				     vjust = 0.5, hjust=1)) +
    labs(title = "Prediction by ARIMA model (based on Sep.15 - present)",
	 x = "date",
	 y = "number of patients")
print(p)
ggplotly()
