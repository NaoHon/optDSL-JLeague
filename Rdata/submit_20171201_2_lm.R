#準備--------------------------------------------------------------------------------

#使用ライブラリ
library(dplyr)
library(stringr)
library(ggplot2)
library(randomForest)
library(knitr)
library(caret)

#データ読込
train<-read.csv("C:/study/JLeague/motodata/train.csv",
                header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
test<-read.csv("C:/study/JLeague/motodata/test.csv",
               header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
condition<-read.csv("C:/study/JLeague/motodata/condition.csv",
                    header=TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")
stadium<-read.csv("C:/study/JLeague/motodata/stadium.csv",
                  header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
sample<-read.csv("C:/study/JLeague/motodata/sample_submit.csv",
                 header=FALSE, stringsAsFactors=FALSE, fileEncoding="utf-8")
train_add<-read.csv("C:/study/JLeague/motodata/train_add.csv",
                    header=TRUE, stringsAsFactors=FALSE, fileEncoding="utf-8")
condition_add<-read.csv("C:/study/JLeague/motodata/condition_add.csv",
                        header=TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")

# #データ確認
# str(train)
# str(test)
# str(stadium)
# str(sample)
# str(train_add)
# #欠損有無
# anyNA(train)
# anyNA(test)
# anyNA(stadium)
# anyNA(train_add)

#前処理用関数--------------------------------------------------------------------------------
#train/testデータの前処理を実施します。
#train/test,stadium,conditionに対して変数加工を行い、それらを結合したデータを返却します。
#train/testとconditionはidで、train/testとstadiumはスタジアム名で結合しています。
#data:trainまたはtestデータを設定
#data_condition:conditionデータを設定
#data_stadium:stadiumデータを設定
F_pre <- function(data,data_condition,data_stadium,traindataflag) {
  #--------------train_all/test加工--------------
  #tmp作成
  data_tmp<-data
  #「match」の加工 節と日を分ける（数値のみにする）
  #節（数値のみ）の列「setu」追加 例：第１節第２日⇒1
  data_tmp<-data_tmp %>%
    #「match」の1字目から4字目を切り出し
    dplyr::mutate(setu=substring(data$match, 1, 4) %>%
                    #切り出した文字列の”第”を""に置き換え
                    gsub(pattern="第", replacement="", fixed = TRUE) %>%
                    #切り出した文字列の”節”を""に置き換え
                    gsub(pattern="節", replacement="", fixed = TRUE))
  
  #「setu」全角半角変換
  for (i in 1:nrow(data_tmp)) {
    data_tmp[i,"setu"] <- chartr("１２３４５６７８９０", 
                                 "1234567890",
                                 data_tmp[i,"setu"])
  }
  
  #日目（数値のみ）の列「setu_nitime」追加 例：第１節第２日⇒2
  data<-data_tmp %>%
    #「match」の5字目から6字目を切り出し
    dplyr::mutate(Snitime=substring(data_tmp$match, 5, 6) %>%
                    #切り出した文字列の”第”を""に置き換え
                    gsub(pattern="第", replacement="", fixed = TRUE) %>%
                    #切り出した文字列の”節”を""に置き換え
                    gsub(pattern="日", replacement="", fixed = TRUE))
  
  #「gameday」加工 月と日と曜日（祝日）を分ける　
  #「gameday」の月「gameM」を追加 例：03/10(土)⇒03
  data_tmp<-data_tmp %>%
    #「gameday」の1字目から2字目を切り出し
    dplyr::mutate(gameM=substring(data_tmp$gameday, 1, 2))
  
  #「gameday」の日「gameD」を追加 例：03/10(土)⇒10
  data_tmp<-data_tmp %>%
    #「gameday」の4字目から5字目を切り出し
    dplyr::mutate(gameD=substring(data_tmp$gameday, 4, 5))
  
  #「year」「gameM」「gameD」を結合「gameYMD」追加 例：2012 03/10(土)⇒20120310
  data_tmp<-data_tmp %>%
    dplyr::mutate(gameYMD= paste(data_tmp$year,data_tmp$gameM,data_tmp$gameD,sep=""))
  
  #gamedayの曜日「gameW」を追加  例：03/10(土)⇒土
  #「gameW」を作成（値はすべてブランク）
  data_tmp$gameW = ""
  #「gameday」に"日”の文字が含まれている行の「gameW」に"日"を設定　以下曜日分実施
  data_tmp$gameW[grep("日", data_tmp$gameday)] = "日"
  data_tmp$gameW[grep("月", data_tmp$gameday)] = "月"
  data_tmp$gameW[grep("火", data_tmp$gameday)] = "火"
  data_tmp$gameW[grep("水", data_tmp$gameday)] = "水"
  data_tmp$gameW[grep("木", data_tmp$gameday)] = "木"
  data_tmp$gameW[grep("金", data_tmp$gameday)] = "金"
  data_tmp$gameW[grep("土", data_tmp$gameday)] = "土"
  #「gameW」が更新されているか確認
  table(data_tmp$gameW)
  
  #gamedayの祝日フラグ「gameWS」を追加  例：03/10(土・祝)⇒祝あり:1、祝なし:0
  #「gameWS」を作成（値はすべて0）
  data_tmp$gameWS = "0"
  #「gameday」に"祝”の文字が含まれている行の「gameWS」に1を設定
  data_tmp$gameWS[grep("祝", data_tmp$gameday)] = 1
  #「gameWS」が更新されているか確認
  table(data_tmp$gameWS)
  
  #gamedayの休みの日（土日祝日）フラグ「kyuujitu」を追加
  #休みの日（土日祝日）を抽出
  data_tmp$kyuujitu = "0"
  data_tmp$kyuujitu <- ifelse(data_tmp$gameWS==1,1,0)
  data_tmp$kyuujitu[grep("土", data_tmp$gameday)] = 1
  data_tmp$kyuujitu[grep("日", data_tmp$gameday)] = 1
  
  # tv "/"で分割"放映社数とそれぞれで放映されたかどうか。 [作成中]
  # tv "/"で分割" したリストを作成
  tv_tmp <- strsplit(data_tmp$tv, "／")
  
  # 放映されたテレビ局の数「tv_num」を追加 例：スカパー！／スカパー！プレミアムサービス／テレ玉⇒3
  data_tmp<-data_tmp %>%
    #tv_tmpの各行の要素数を「tv_num」に設定
    dplyr::mutate(tv_num=lapply(tv_tmp,length) %>%
                    unlist())
  
  #テレビ局の種類数確認
  # a<-unlist(tv_tmp)
  # table(a)
  
  #timeをカテゴリ化
  #気象庁の定義
  #0時～3時 … 未明、3時～6時 … 明け方、6時～9時 … 朝
  #9時～12時 … 昼前、12時～15時 … 昼過ぎ、15時～18時 … 夕方
  #18時～21時 … 夜の初め頃、21時～24時 … 夜遅く
  data_tmp<-data_tmp %>%
    #「timezone」の1字目から2字目を切り出し
    dplyr::mutate(timezone=as.integer(substring(data_tmp$time, 1, 2)))
  
  for (i in 1:nrow(data_tmp)) {
    if(0 <= data_tmp[i,"timezone"] && data_tmp[i,"timezone"] < 15){
      data_tmp[i,"timezone"] <- "昼"
    }else if(15 <= data_tmp[i,"timezone"] && data_tmp[i,"timezone"] < 18){
      data_tmp[i,"timezone"] <- "夕方"
    }else if(18 <= data_tmp[i,"timezone"] && data_tmp[i,"timezone"] < 24){
      data_tmp[i,"timezone"] <- "夜"
    }
  }
  
  #timeの：削除
  data_tmp$timeInt <-""
  data_tmp$timeInt <- as.integer(gsub(data_tmp$time,pattern=":", replacement="", fixed = TRUE))
  
  #timeIntを時間単位に変換
  data_tmp<-data_tmp %>%
    dplyr::mutate(timeInt_c = floor(timeInt/100))
  
  #ザスパ草津をザスパクサツ群馬に変換
  data_tmp$home[grep("ザスパ草津", data_tmp$home)] = "ザスパクサツ群馬"
  data_tmp$away[grep("ザスパ草津", data_tmp$away)] = "ザスパクサツ群馬"
  
  #長崎県立総合運動公園陸上競技場を長崎市総合運動公園かきどまり陸上競技場に変換
  #data_tmp$stadium[grep("長崎県立総合運動公園陸上競技場", data_tmp$stadium)] = "長崎市総合運動公園かきどまり陸上競技場"
  
  #--------------stadium加工--------------
  #tmp作成
  data_stadium_tmp <- data_stadium
  #都道府県抽出「address_ken」
  #「address」の1文字目から「都道府県」のいずれかが出現するまでの文字列を切り出し
  data_stadium_tmp$address_ken=substring(data_stadium_tmp$address,
                                         1,
                                         regexpr("都|道|府|県",data_stadium$address,useBytes=F))
  #京都府だけ府にかかる前に都が現れるため修正
  data_stadium_tmp$address_ken[grep("京都", data_stadium_tmp$address_ken)] = "京都府"
  data_stadium_tmp$address_ken <- as.character(data_stadium_tmp$address_ken)
  
  #--------------condition加工--------------
  #tmp作成
  data_condition_tmp <- data_condition
  
  
  #勝ち点設定
  for (i in 1:nrow(data_condition_tmp)) {
    if(data_condition_tmp$home_score[i] > data_condition_tmp$away_score[i]){
      data_condition_tmp$home_katiten[i] <- 3
      data_condition_tmp$away_katiten[i] <- 0
      
    }else if(data_condition_tmp$home_score[i] < data_condition_tmp$away_score[i]){
      data_condition_tmp$home_katiten[i] <- 0
      data_condition_tmp$away_katiten[i] <- 3
      
    }else{
      data_condition_tmp$home_katiten[i] <- 1
      data_condition_tmp$away_katiten[i] <- 1
    }
  }
  
  
  #--------------データ結合--------------
  #data_tmpとconditionを対戦カードidを基準に結合
  data_tmp<-dplyr::left_join(data_tmp, data_condition_tmp, by="id")
  
  #data_tmpとstadiumを結合("stadium" = "name")
  data_new<-dplyr::left_join(data_tmp, data_stadium_tmp, by=c("stadium" = "name"))
  
  #traindataの場合
  if(traindataflag){
    #y/capaで収容率「y2」を算出
    data_new$y2<-data_new$y/data_new$capa
  }
  
  #型変換
  # len <- length(data_new)
  # for(count in 1:len){
  #   if(is.character(data_new[1,count])){
  #     data_new[count]<-lapply(data_new[count],as.factor)
  #   }
  # }

  data_new$setu<-as.integer(data_new$setu)
  data_new$gameM<-as.integer(data_new$gameM)
  data_new$gameD<-as.integer(data_new$gameD)
  data_new$gameYMD<-as.integer(data_new$gameYMD)
  data_new$gameWS<-as.integer(data_new$gameWS)
  
  return(data_new)
}

#改善------------------------------------------------------------------------------------------
#ここからは予測精度を向上させるための関数をつくります。

#指定したIDのデータを削除
F_outlier <- function(df,outlier) {
  data_new<-df %>%
    dplyr::filter(id!=outlier)
  return(data_new)
}

#setuの加工(setuをxで折り返してみる)
F_setu2 <- function(df,x) {
  df<-df %>%
    dplyr::mutate(setu2=abs(x-setu))  #absは絶対値
  return(df)
}

#timeInt_cの加工(timeIntをxで折り返して*-1)
F_timeInt_c2 <- function(df,x) {
  df<-df %>%
    dplyr::mutate(timeInt_c2=abs(x-timeInt_c)*-1)  #absは絶対値
  return(df)
}

#yearの加工(timeIntをxで折り返して*-1)
F_year2 <- function(df,x) {
  df<-df %>%
    dplyr::mutate(year2=abs(x-year)*-1)  #absは絶対値
  return(df)
}


#チームごとの外れ値を削除
F_outlier_all <- function(df) {
  df_tmp <- df
  uni <- unique(df_tmp$home)
  len <- length(uni)
  for(count in 1:len){
    out_tmp <-df_tmp %>%
      dplyr::filter(home==uni[count])
    
    x <- out_tmp$y
    Q1 <- quantile(x)[2]		# 第1四分位数
    Q3 <- quantile(x)[4]		# 第3四分位数
    IQRx <- IQR(x)			# 四分位範囲
    outlierU<- x[x < Q1-(IQRx*1.5)]		# 下側の外れ値
    outlierO<- x[x > Q3+(IQRx*1.5)]		# 上側の外れ値
    
    if(length(outlierO) != 0){
      out_tmp <- out_tmp %>%
        dplyr::filter(y>=min(outlierO))
      
      len2 <- nrow(out_tmp)
      for(count2 in 1:len2){
        df_tmp <-df_tmp %>%
          dplyr::filter(df_tmp$id != out_tmp$id[count2])
        
      }
    }
  }
  return(df_tmp)
}

#予測値の修正
F_pred_fix <- function(pred,tr,te) {
  min<-min(tr$y)
  print(min)
  pred2<-ifelse(pred < min, min,
                ifelse(pred > te$capa, te$capa, pred))
  return(pred2)
}

#main------------------------------------------------------------------------------------------
#ここから実際にデータを加工してモデルを作ります。
#addデータ追加
train_all <- dplyr::bind_rows(train, train_add)
condition_all <- dplyr::bind_rows(condition, condition_add)

#train/testの前処理実施
train_new <- F_pre(train_all,condition_all,stadium, traindataflag=TRUE)
test_new <- F_pre(test,condition_all,stadium, traindataflag=FALSE)

#########suiihyousakusei.R⇒add_lank.Rを実行#########

###CSV出力
# write.table(train_new, file="C:/study/JLeague/submit/train_new.csv",
#             quote=FALSE, sep=",", row.names=F, col.names=T)
# write.table(test_new, file="C:/study/JLeague/submit/test_new.csv",
#             quote=FALSE, sep=",", row.names=F, col.names=T)

#変数加工-----------------------------------

#無観客試合のデータ削除
train_new <- F_outlier(train_new,15699)
#マリノス戦の外れ値削除
train_new <- F_outlier(train_new,15127)

#チームごとの外れ値を削除
train_new<-F_outlier_all(train_new)

# J1/J2のデータ
lm_train_j1 <- train_new %>% filter(stage == "Ｊ１")
lm_train_j2 <- train_new %>% filter(stage == "Ｊ２")
lm_test_j1 <- test_new %>% filter(stage == "Ｊ１")
lm_test_j2 <- test_new %>% filter(stage == "Ｊ２")

#setuを加工
F_linearplot2(lm_train_j1,"setu") %>% plot()
F_linearplot2(lm_train_j2,"setu") %>% plot()
lm_train_j1 <- F_setu2(lm_train_j1,3)
lm_train_j2 <- F_setu2(lm_train_j2,3)
F_linearplot2(lm_train_j1,"setu2") %>% plot()
F_linearplot2(lm_train_j2,"setu2") %>% plot()

lm_test_j1 <- F_setu2(lm_test_j1,3)
lm_test_j2 <- F_setu2(lm_test_j2,3)

#timeInt_cを加工
F_linearplot(lm_train_j1,"timeInt_c") %>% plot()
F_linearplot(lm_train_j2,"timeInt_c") %>% plot()
lm_train_j1 <- F_timeInt_c2(lm_train_j1,18)
lm_train_j2 <- F_timeInt_c2(lm_train_j2,15)
F_linearplot2(lm_train_j1,"timeInt_c2") %>% plot()
F_linearplot2(lm_train_j2,"timeInt_c2") %>% plot()

lm_test_j1 <- F_timeInt_c2(lm_test_j1,18)
lm_test_j2 <- F_timeInt_c2(lm_test_j2,15)

#lankを加工
F_linearplot2(lm_train_j1,"home_lank") %>% plot()
F_linearplot2(lm_train_j1,"away_lank") %>% plot()
F_linearplot2(lm_train_j2,"home_lank") %>% plot()
F_linearplot2(lm_train_j2,"away_lank") %>% plot()

lm_train_j1<-lm_train_j1 %>% dplyr::mutate(home_lank2=home_lank*-1)  
lm_train_j1<-lm_train_j1 %>% dplyr::mutate(away_lank2=away_lank*-1)  
lm_train_j2<-lm_train_j2 %>% dplyr::mutate(home_lank2=home_lank*-1)  
lm_train_j2<-lm_train_j2 %>% dplyr::mutate(away_lank2=away_lank*-1)  

F_linearplot2(lm_train_j1,"home_lank2") %>% plot()
F_linearplot2(lm_train_j1,"away_lank2") %>% plot()
F_linearplot2(lm_train_j2,"home_lank2") %>% plot()
F_linearplot2(lm_train_j2,"away_lank2") %>% plot()

lm_test_j1<-lm_test_j1 %>% dplyr::mutate(home_lank2=home_lank*-1) 
lm_test_j1<-lm_test_j1 %>% dplyr::mutate(away_lank2=away_lank*-1)  
lm_test_j2<-lm_test_j2 %>% dplyr::mutate(home_lank2=home_lank*-1) 
lm_test_j2<-lm_test_j2 %>% dplyr::mutate(away_lank2=away_lank*-1)  

#yearを加工
# F_linearplot(lm_train_j1,"year") %>% plot()
# F_linearplot(lm_train_j2,"year") %>% plot()
# lm_train_j1 <- F_year2(lm_train_j1,2012)
# F_linearplot(lm_train_j1,"year2") %>% plot()
# 
# lm_test_j1 <- F_year2(lm_test_j1,2012)

#欠損確認
anyNA(lm_train_j1)
anyNA(lm_train_j2)
anyNA(lm_test_j1)
anyNA(lm_test_j2)

#モデル作成-----------------------------------
#収容率を予測した後、各スタジアムの収容人数をかけて
#予測観客動員数を算出する。

#変数選択
lm_train_j1_2<-dplyr::select(lm_train_j1, y2, tv_num, capa, setu2, home, kyuujitu,home_lank2,away_lank2)
lm_train_j2_2<-dplyr::select(lm_train_j2, y2, tv_num, capa, setu2, home, kyuujitu,home_lank2,away_lank2)
lm_test_j1_2<-dplyr::select(lm_test_j1, tv_num, capa, setu2, home, kyuujitu,home_lank2,away_lank2)
lm_test_j2_2<-dplyr::select(lm_test_j2, tv_num, capa, setu2, home, kyuujitu,home_lank2,away_lank2)

# ###重回帰分析
# #family:目的変数の確率分布とリンク関数の設定(今回は正規分布と恒等写像)
lm_j1<-glm(y2 ~ ., data=lm_train_j1_2, family=gaussian(link="identity"))
lm_j2<-glm(y2 ~ ., data=lm_train_j2_2, family=gaussian(link="identity"))

#testに当てはめ
pred_j1<-predict(lm_j1, lm_test_j1_2, type="response")
pred_j2<-predict(lm_j2, lm_test_j2_2, type="response")

#予測集客率*capaで予測集客人数を計算
pred_j1<-round(pred_j1*lm_test_j1_2$capa)
pred_j2<-round(pred_j2*lm_test_j2_2$capa)

#予測値補正
pred_j1<-F_pred_fix(pred_j1,lm_train_j1,lm_test_j1)
pred_j2<-F_pred_fix(pred_j2,lm_train_j2,lm_test_j2)

###submit形式変換
submit_j1<-data.frame(id=lm_test_j1[,"id"], pred=pred_j1)
submit_j2<-data.frame(id=lm_test_j2[,"id"], pred=pred_j2)

submit_all <- dplyr::bind_rows(submit_j1, submit_j2)

###CSV出力(ヘッダーなし)
write.table(submit_all, file="C:/study/JLeague/submit/submit_20171201_2_lm.csv",
            quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)

#########変数選択 rmseを確認したいとき用
#testに当てはめ
pred_j1<-predict(lm_j1, lm_train_j1_2, type="response")
pred_j2<-predict(lm_j2, lm_train_j2_2, type="response")

#予測集客率*capaで予測集客人数を計算
pred_j1<-round(pred_j1*lm_train_j1_2$capa)
pred_j2<-round(pred_j2*lm_train_j2_2$capa)

#予測値補正
pred_j1<-F_pred_fix(pred_j1,lm_train_j1,lm_train_j1_2)
pred_j2<-F_pred_fix(pred_j2,lm_train_j2,lm_train_j2_2)

sqrt(sum((lm_train_j1$y - pred_j1)^2)/nrow(lm_train_j1_2))
sqrt(sum((lm_train_j2$y - pred_j2)^2)/nrow(lm_train_j2_2))

########


#########変数選択 rmseを確認したいとき用２
lm_train_j1_2<-dplyr::select(lm_train_j1, y, tv_num, capa, setu2, home, kyuujitu,home_lank2,away_lank2)
lm_train_j2_2<-dplyr::select(lm_train_j2, y, tv_num, capa, setu2, home, kyuujitu,home_lank2,away_lank2)
# J1線形回帰、10分割交差検定
model_j1 <- train(
  y ~ .,
  data = lm_train_j1_2,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10))
# J1モデルのRMSE
model_j1$results$RMSE

# J2線形回帰、10分割交差検定
model_j2 <- train(
  y ~ .,
  data = lm_train_j2_2,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10))
# J2モデルのRMSE
model_j2$results$RMSE

# J1モデルとJ2モデルの結果を統合したときのRMSE
se <- model_j1$results$RMSE^2 * nrow(lm_train_j1_2) + model_j2$results$RMSE^2 * nrow(lm_train_j2_2)
n <- nrow(lm_train_j1_2) + nrow(lm_train_j2_2)
rmse <- sqrt(se / n)
rmse
###############################################################################################
#ここからはデータ内容を可視化・確認するためのコードです。--------------------------------------
#--------------箱ひげ図関数１（観客動員数）--------------
#df データフレーム
#dfx 上記dfから集計したい単位の列（チーム別の場合homeを指定）
F_boxplot <- function(df,dfx) {
  bp<-ggplot(df, aes(y=y, x=dfx,fill = dfx)) 
  bp<-bp+geom_boxplot(alpha=0.5,colour="gray30")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none")
  bp
}

#--------------箱ひげ図関数２（集客率）--------------
#df データフレーム
#dfx 上記dfから集計したい単位の列（ホームチーム別の場合homeを指定）
F_boxplot2 <- function(df,dfx) {
  bp<-ggplot(df, aes(y=(y/capa), x=dfx,fill = dfx)) 
  bp<-bp+geom_boxplot(alpha=0.5,colour="gray30")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none")
  bp
}
dev.off() 
#実行
#train_newをJ1、J2に分ける
tmpJ1 <- dplyr::filter(train_new,train_new$stage == "Ｊ１")
tmpJ2 <- dplyr::filter(train_new,train_new$stage == "Ｊ２")
#ホームチーム別観客動員数
F_boxplot(tmpJ1,tmpJ1$home)
F_boxplot(tmpJ2,tmpJ2$home)
#ホームチーム別集客率
F_boxplot2(tmpJ1,tmpJ1$home)
F_boxplot2(tmpJ2,tmpJ2$home)
#スタジアム別観客動員数
F_boxplot(tmpJ1,tmpJ1$stadium)
F_boxplot(tmpJ2,tmpJ2$stadium)
#スタジアム別集客率
F_boxplot2(tmpJ1,tmpJ1$stadium)
F_boxplot2(tmpJ2,tmpJ2$stadium)
#都道府県別観客動員数
F_boxplot(tmpJ1,tmpJ1$address_ken)
F_boxplot(tmpJ2,tmpJ2$address_ken)
#都道府県別集客率
F_boxplot2(tmpJ1,tmpJ1$address_ken)
F_boxplot2(tmpJ2,tmpJ2$address_ken)
#朝昼夜別観客動員数
F_boxplot(tmpJ1,tmpJ1$timezone)
F_boxplot(tmpJ2,tmpJ2$timezone)

#--------------線形性を確認--------------
#group_by_keyに確認したい列を指定
#集客人数を図示するためのデータを返却
F_linearplot <- function(df,group_by_key) {
  
  g_dat <- df %>%
    dplyr::group_by_(group_by_key) %>%
    dplyr::summarise(y_mean = mean(y)) %>%
    dplyr::ungroup(.) %>%
    ggplot(., aes_(x = as.name(group_by_key), y = as.name("y_mean"))) + geom_line()
  return(g_dat)
  
}
#group_by_keyに確認したい列を指定
#集客率を図示するためのデータを返却
F_linearplot2 <- function(df,group_by_key) {
  
  g_dat <- df %>%
    dplyr::group_by_(group_by_key) %>%
    dplyr::summarise(y_mean = mean(y2)) %>%
    dplyr::ungroup(.) %>%
    ggplot(., aes_(x = as.name(group_by_key), y = as.name("y_mean"))) + geom_line()
  return(g_dat)
  
}
#group_by_keyに確認したい列を指定
#集客率対数オッズを図示するためのデータを返却
F_linearplot3 <- function(df,group_by_key) {
  
  g_dat <- df %>%
    dplyr::group_by_(group_by_key) %>%
    dplyr::summarise(y_mean = mean(y2)) %>%
    dplyr::ungroup(.) %>%
    dplyr::mutate(log_odds=log(y_mean/(1-y_mean))) %>%
    ggplot(., aes_(x = as.name(group_by_key), y = as.name("log_odds"))) + geom_line()
  return(g_dat)
  
}

#集客人数
F_linearplot(train_new,"capa") %>% plot()
F_linearplot(train_new,"setu") %>% plot()
F_linearplot(train_new,"gameM") %>% plot() #3月から12月
F_linearplot(train_new,"gameD") %>% plot()
dplyr::filter(train_new,train_new$year == 2012) %>%
  F_linearplot("gameYMD") %>% plot()
dplyr::filter(train_new,train_new$year == 2013) %>%
  F_linearplot("gameYMD") %>% plot()
dplyr::filter(train_new,train_new$year == 2014) %>%
  F_linearplot("gameYMD") %>% plot()
F_linearplot(train_new,"tv_num") %>% plot()
F_linearplot(train_new,"temperature") %>% plot()

#集客率
F_linearplot2(train_new,"capa") %>% plot()
F_linearplot2(train_new,"setu") %>% plot()
F_linearplot2(train_new,"gameM") %>% plot() #3月から12月
F_linearplot2(train_new,"gameD") %>% plot()
dplyr::filter(train_new,train_new$year == 2012) %>%
  F_linearplot2("gameYMD") %>% plot()
dplyr::filter(train_new,train_new$year == 2013) %>%
  F_linearplot2("gameYMD") %>% plot()
dplyr::filter(train_new,train_new$year == 2014) %>%
  F_linearplot2("gameYMD") %>% plot()
F_linearplot2(train_new,"tv_num") %>% plot()
F_linearplot2(train_new,"temperature") %>% plot()


#train_newをJ1、J2に分ける
tmpJ1 <- dplyr::filter(train_new,train_new$stage == "Ｊ１")
tmpJ2 <- dplyr::filter(train_new,train_new$stage == "Ｊ２")

#月ごと
#J1J2共に４月が一番低く、１１月１２月が高い
#J1の６月は試合自体が少ない
F_linearplot2(tmpJ1,"gameM") %>% plot() #3月から12月
F_linearplot2(tmpJ2,"gameM") %>% plot() #3月から12月

table(tmpJ1$gameM)
table(tmpJ2$gameM)

#節ごと(リーグは３月はじめからスタート、１２月１週目あたりで終わる)
#リーグ始まりと終わりは予想通り集客が良い
F_linearplot2(tmpJ1,"setu") %>% plot()
F_linearplot2(tmpJ2,"setu") %>% plot()

table(tmpJ1$setu)
table(tmpJ2$setu)

#変数加工(setuを3で折り返してみる)
tmpJ1_2<-tmpJ1 %>%
  dplyr::mutate(setu2=abs(3-setu))  #absは絶対値
F_linearplot2(tmpJ1_2,"setu2") %>% plot()

#変数加工(setuを3で折り返してみる)
tmpJ2_2<-tmpJ2 %>%
  dplyr::mutate(setu2=abs(3-setu))  #absは絶対値
F_linearplot2(tmpJ2_2,"setu2") %>% plot()


#point
plot(train_new$temperature, train_new$y2)

#--------------平日祝日の線形性確認--------------

#仕事がある日（平日（祝日除く））を抽出
F_heijitu <- function(df) {
  df_g<-df %>%
    dplyr::filter(gameW=="月"|
                    gameW=="火"|
                    gameW=="水"|
                    gameW=="木"|
                    gameW=="金") %>%
    dplyr::filter(gameWS!=1)
  return(df_g)
}

#休みの日（土日祝日）を抽出
F_kyuujitu <- function(df) {
  df_g<-df %>%
    dplyr::filter(gameW=="土"|
                    gameW=="日"|
                    gameWS==1)
  
  return(df_g)
}

#train_newをJ1、J2に分ける
tmpJ1 <- dplyr::filter(train_new,train_new$stage == "Ｊ１")
tmpJ2 <- dplyr::filter(train_new,train_new$stage == "Ｊ２")

#曜日ごとの試合数を確認
#J1は平日は水曜多い、土日だと土曜日のほうが多い
table(tmpJ1$gameW) 
#J2も平日は水曜多い、、土日だと日曜日のほうが多い
table(tmpJ2$gameW) 

#j1のデータ数確認（少ない。。）
table(F_heijitu(tmpJ1)$gameW)
table(F_kyuujitu(tmpJ1)$gameW)
#j2のデータ数確認
table(F_heijitu(tmpJ2)$gameW)
table(F_kyuujitu(tmpJ2)$gameW)

#J1の平日の試合開始時間ごとの集客人数を確認
F_linearplot(F_heijitu(tmpJ1),"timeInt_c") %>% plot()
#J1の平日の試合開始時間ごとの集客率を確認
F_linearplot2(F_heijitu(tmpJ1),"timeInt_c") %>% plot()

#変数加工(timeIntを14で折り返して*-1)
tmpJ1_2<-tmpJ1 %>%
  dplyr::mutate(timeInt_c2=abs(14-timeInt_c)*-1)  #absは絶対値
F_linearplot2(F_heijitu(tmpJ1_2),"timeInt_c2") %>% plot()

#J2の休日の試合開始時間ごとの集客人数を確認
F_linearplot(F_kyuujitu(tmpJ2),"timeInt_c") %>% plot()
#J2の休日の試合開始時間ごとの集客率を確認
F_linearplot2(F_kyuujitu(tmpJ2),"timeInt_c") %>% plot()

#変数加工(timeIntを15で折り返して*-1)
tmpJ2_2<-tmpJ2 %>%
  dplyr::mutate(timeInt_c2=abs(15-timeInt_c)*-1)  #absは絶対値
F_linearplot2(F_heijitu(tmpJ2_2),"timeInt_c2") %>% plot()

#チームごと
g <- ggplot(lm_train_j1, aes(x = capa, y = y, colour = home)) + geom_point()
print(g)

#--------------浦和レッズ確認--------------
train_reds<-dplyr::filter(train_new,home=="浦和レッズ")
train_mari<-dplyr::filter(train_new,home=="横浜Ｆ・マリノス")
F_linearplot2(train_reds,"setu") %>% plot()
F_linearplot2(train_mari,"setu") %>% plot()

F_linearplot2(train_reds,"year") %>% plot()
F_linearplot2(train_mari,"year") %>% plot()

F_linearplot2(train_new,"year") %>% plot()

#--------------残差確認--------------
#モデル作成
# ###重回帰分析
# #family:目的変数の確率分布とリンク関数の設定(今回は正規分布と恒等写像)
lm_j1<-glm(y2 ~ ., data=lm_train_j1_2, family=gaussian(link="identity"))
lm_j2<-glm(y2 ~ ., data=lm_train_j2_2, family=gaussian(link="identity"))

#trainに当てはめ
pred_j1<-predict(lm_j1, lm_train_j1_2, type="response")
pred_j2<-predict(lm_j2, lm_train_j2_2, type="response")

#予測集客率*capaで予測集客人数を計算
pred_j1<-round(pred_j1*lm_train_j1_2$capa)
pred_j2<-round(pred_j2*lm_train_j2_2$capa)

###submit形式変換
submit_j1<-data.frame(id=lm_train_j1[,"id"], pred=pred_j1)
submit_j2<-data.frame(id=lm_train_j2[,"id"], pred=pred_j2)

submit_all <- dplyr::bind_rows(submit_j1, submit_j2)
lm_train_all <- dplyr::bind_rows(lm_train_j1, lm_train_j2)

#ホームチーム, アウェイチーム, スタジアム, 収容人数, 気温に注目
#残差の大きい順に並べる
zan<-data.frame(lm_train_all, Res=abs(lm_train_all$y-submit_all$pred), pred=submit_all$pred, 
                dif=lm_train_all$y-submit_all$pred) %>%
  dplyr::select(id, Res, y, pred, dif, stage, home, away, stadium, capa, setu, tv_num, timeInt_c,home_lank,away_lank) %>%
  dplyr::arrange(desc(Res))

#確認(残差の絶対値の大きい順)
#予測値が実測値より多い
kable(head(zan %>%
             dplyr::filter(dif<=0), n=25))

#予測値が実測値より少ない
kable(head(zan %>%
             dplyr::filter(dif>0), n=25))



#--------------その他確認用関数--------------
#一つの値しかない列を抽出
OnlyVariablePrint <- function(table){
  len <- length(names(table))
  for(VarCount in 1:len){
    var <- length(levels(factor(table[,names(table)[VarCount]])))
    if( var == 1 ){
      print( names(table)[VarCount])
    }
  }
}