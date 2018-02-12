#使用ライブラリ
library(randomForest)
library(dplyr)
library(pROC)

#データ読み込み
train<-dplyr::select(lm_train_j1, y2, tv_num, capa, setu2, home, kyuutu,address_ken,timeInt_c,gameW,away,year)
test<-lm_test_j1_2
train$y2 <- round(train$y2)
###Hold Out
#構築データの割合
rate<-0.7

#構築データ数(小数の切捨て)
num<-as.integer(nrow(train)*rate)

#再現性のため乱数シードを固定
set.seed(17)

#sample(ベクトル, ランダムに取得する個数, 復元抽出の有無)
row<-sample(1:nrow(train), num, replace=FALSE)

#構築データ
rf_train_train<-train[row,] %>%
  dplyr::select( -y2 )

#検証データ
rf_train_test<-train[-row,] %>%
  dplyr::select( -y2 )

#目的変数作成
y_train_train<- train[row,] %>%
  dplyr::select(y2)
y_train_test<- train[-row,] %>%
  dplyr::select(y2)

#再現性のため乱数シードを固定
set.seed(17)
tuneRF(y_train_train, as.factor(y_train_train$y),doBest=TRUE)

rf<-randomForest(rf_train_train, #学習データ(説明変数)
                 as.factor(y_train_train$y), #学習データ(目的変数)
                 mtry=4, #1本の木に使用する変数の数
                 sampsize=nrow(rf_train_train)*0.3, #モデル構築に使用するデータ数
                 nodesize=30, #生成する各決定木のノードが含むサンプル最小数
                 maxnodes=5, #生成する各決定木の終端ノードの最大数
                 ntree=500, #生成する決定木の数
                 imprtance=T #変数重要度の有無
)

##train_testのAUC
#prediction(予測結果,目的変数(1 or 0))
pred <-predict(rf, newdata=rf_train_test, type="prob")[,2]
auc<-roc(y_train_test$y2, pred)
print(auc)

##変数重要度
print(importance(rf))