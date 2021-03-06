#モデル作成-----------------------------------
#収容率を予測した後、各スタジアムの収容人数をかけて
#予測観客動員数を算出する。

#変数選択
lm_train_j1_2<-dplyr::select(lm_train_j1,y, y2, tv_num,  setu2, home, kyuujitu, capa)
lm_train_j2_2<-dplyr::select(lm_train_j2,y, y2, tv_num, setu2, home, kyuujitu, capa)
lm_test_j1_2<-dplyr::select(lm_test_j1, id,tv_num,  setu2, home, kyuujitu, capa)
lm_test_j2_2<-dplyr::select(lm_test_j2, id,tv_num,  setu2, home, kyuujitu, capa)

lm_train_all <- dplyr::bind_rows(lm_train_j1_2, lm_train_j2_2)
lm_test_all <- dplyr::bind_rows(lm_test_j1_2, lm_test_j2_2)

m_lm_train_1 <-lm_train_all%>%filter(home=="ＦＣ岐阜")
m_lm_train_2 <-lm_train_all%>%filter(home=="ＦＣ町田ゼルビア")
m_lm_train_3 <-lm_train_all%>%filter(home=="ＦＣ東京")
m_lm_train_4 <-lm_train_all%>%filter(home=="Ｖ・ファーレン長崎")
m_lm_train_5 <-lm_train_all%>%filter(home=="アビスパ福岡")
m_lm_train_6 <-lm_train_all%>%filter(home=="アルビレックス新潟")
m_lm_train_7 <-lm_train_all%>%filter(home=="ヴァンフォーレ甲府")
m_lm_train_8 <-lm_train_all%>%filter(home=="ヴィッセル神戸")
m_lm_train_9 <-lm_train_all%>%filter(home=="ガイナーレ鳥取")
m_lm_train_10 <-lm_train_all%>%filter(home=="カターレ富山")
m_lm_train_11 <-lm_train_all%>%filter(home=="カマタマーレ讃岐")
m_lm_train_12 <-lm_train_all%>%filter(home=="ガンバ大阪")
m_lm_train_13 <-lm_train_all%>%filter(home=="ギラヴァンツ北九州")
m_lm_train_14 <-lm_train_all%>%filter(home=="コンサドーレ札幌")
m_lm_train_15 <-lm_train_all%>%filter(home=="サガン鳥栖")
m_lm_train_16 <-lm_train_all%>%filter(home=="ザスパクサツ群馬")
m_lm_train_17 <-lm_train_all%>%filter(home=="サンフレッチェ広島")
m_lm_train_18 <-lm_train_all%>%filter(home=="ジェフユナイテッド千葉")
m_lm_train_19 <-lm_train_all%>%filter(home=="ジュビロ磐田")
m_lm_train_20 <-lm_train_all%>%filter(home=="セレッソ大阪")
m_lm_train_21 <-lm_train_all%>%filter(home=="ファジアーノ岡山")
m_lm_train_22 <-lm_train_all%>%filter(home=="ベガルタ仙台")
m_lm_train_23 <-lm_train_all%>%filter(home=="モンテディオ山形")
m_lm_train_24 <-lm_train_all%>%filter(home=="ロアッソ熊本")
m_lm_train_25 <-lm_train_all%>%filter(home=="愛媛ＦＣ")
m_lm_train_26 <-lm_train_all%>%filter(home=="浦和レッズ")
m_lm_train_27 <-lm_train_all%>%filter(home=="横浜Ｆ・マリノス")
m_lm_train_28 <-lm_train_all%>%filter(home=="横浜ＦＣ")
m_lm_train_29 <-lm_train_all%>%filter(home=="京都サンガF.C.")
m_lm_train_30 <-lm_train_all%>%filter(home=="鹿島アントラーズ")
m_lm_train_31 <-lm_train_all%>%filter(home=="松本山雅ＦＣ")
m_lm_train_32 <-lm_train_all%>%filter(home=="湘南ベルマーレ")
m_lm_train_33 <-lm_train_all%>%filter(home=="水戸ホーリーホック")
m_lm_train_34 <-lm_train_all%>%filter(home=="清水エスパルス")
m_lm_train_35 <-lm_train_all%>%filter(home=="川崎フロンターレ")
m_lm_train_36 <-lm_train_all%>%filter(home=="大宮アルディージャ")
m_lm_train_37 <-lm_train_all%>%filter(home=="大分トリニータ")
m_lm_train_38 <-lm_train_all%>%filter(home=="東京ヴェルディ")
m_lm_train_39 <-lm_train_all%>%filter(home=="徳島ヴォルティス")
m_lm_train_40 <-lm_train_all%>%filter(home=="栃木ＳＣ")
m_lm_train_41 <-lm_train_all%>%filter(home=="柏レイソル")
m_lm_train_42 <-lm_train_all%>%filter(home=="名古屋グランパス")


lm_train_1  <-m_lm_train_1   %>%  dplyr::select(-y, -home)
lm_train_2  <-m_lm_train_2   %>%  dplyr::select(-y, -home)
lm_train_3  <-m_lm_train_3   %>%  dplyr::select(-y, -home)
lm_train_4  <-m_lm_train_4   %>%  dplyr::select(-y, -home)
lm_train_5  <-m_lm_train_5   %>%  dplyr::select(-y, -home)
lm_train_6  <-m_lm_train_6   %>%  dplyr::select(-y, -home)
lm_train_7  <-m_lm_train_7   %>%  dplyr::select(-y, -home)
lm_train_8  <-m_lm_train_8   %>%  dplyr::select(-y, -home)
lm_train_9  <-m_lm_train_9   %>%  dplyr::select(-y, -home)
lm_train_10 <-m_lm_train_10  %>%  dplyr::select(-y, -home)
lm_train_11 <-m_lm_train_11  %>%  dplyr::select(-y, -home)
lm_train_12 <-m_lm_train_12  %>%  dplyr::select(-y, -home)
lm_train_13 <-m_lm_train_13  %>%  dplyr::select(-y, -home)
lm_train_14 <-m_lm_train_14  %>%  dplyr::select(-y, -home)
lm_train_15 <-m_lm_train_15  %>%  dplyr::select(-y, -home)
lm_train_16 <-m_lm_train_16  %>%  dplyr::select(-y, -home)
lm_train_17 <-m_lm_train_17  %>%  dplyr::select(-y, -home)
lm_train_18 <-m_lm_train_18  %>%  dplyr::select(-y, -home)
lm_train_19 <-m_lm_train_19  %>%  dplyr::select(-y, -home)
lm_train_20 <-m_lm_train_20  %>%  dplyr::select(-y, -home)
lm_train_21 <-m_lm_train_21  %>%  dplyr::select(-y, -home)
lm_train_22 <-m_lm_train_22  %>%  dplyr::select(-y, -home)
lm_train_23 <-m_lm_train_23  %>%  dplyr::select(-y, -home)
lm_train_24 <-m_lm_train_24  %>%  dplyr::select(-y, -home)
lm_train_25 <-m_lm_train_25  %>%  dplyr::select(-y, -home)
lm_train_26 <-m_lm_train_26  %>%  dplyr::select(-y, -home)
lm_train_27 <-m_lm_train_27  %>%  dplyr::select(-y, -home)
lm_train_28 <-m_lm_train_28  %>%  dplyr::select(-y, -home)
lm_train_29 <-m_lm_train_29  %>%  dplyr::select(-y, -home)
lm_train_30 <-m_lm_train_30  %>%  dplyr::select(-y, -home)
lm_train_31 <-m_lm_train_31  %>%  dplyr::select(-y, -home)
lm_train_32 <-m_lm_train_32  %>%  dplyr::select(-y, -home)
lm_train_33 <-m_lm_train_33  %>%  dplyr::select(-y, -home)
lm_train_34 <-m_lm_train_34  %>%  dplyr::select(-y, -home)
lm_train_35 <-m_lm_train_35  %>%  dplyr::select(-y, -home)
lm_train_36 <-m_lm_train_36  %>%  dplyr::select(-y, -home)
lm_train_37 <-m_lm_train_37  %>%  dplyr::select(-y, -home)
lm_train_38 <-m_lm_train_38  %>%  dplyr::select(-y, -home)
lm_train_39 <-m_lm_train_39  %>%  dplyr::select(-y, -home)
lm_train_40 <-m_lm_train_40  %>%  dplyr::select(-y, -home)
lm_train_41 <-m_lm_train_41  %>%  dplyr::select(-y, -home)
lm_train_42 <-m_lm_train_42  %>%  dplyr::select(-y, -home)


m_lm_test_1 <-lm_test_all%>%filter(home=="ＦＣ岐阜")
m_lm_test_2 <-lm_test_all%>%filter(home=="ＦＣ町田ゼルビア")
m_lm_test_3 <-lm_test_all%>%filter(home=="ＦＣ東京")
m_lm_test_4 <-lm_test_all%>%filter(home=="Ｖ・ファーレン長崎")
m_lm_test_5 <-lm_test_all%>%filter(home=="アビスパ福岡")
m_lm_test_6 <-lm_test_all%>%filter(home=="アルビレックス新潟")
m_lm_test_7 <-lm_test_all%>%filter(home=="ヴァンフォーレ甲府")
m_lm_test_8 <-lm_test_all%>%filter(home=="ヴィッセル神戸")
m_lm_test_9 <-lm_test_all%>%filter(home=="ガイナーレ鳥取")
m_lm_test_10 <-lm_test_all%>%filter(home=="カターレ富山")
m_lm_test_11 <-lm_test_all%>%filter(home=="カマタマーレ讃岐")
m_lm_test_12 <-lm_test_all%>%filter(home=="ガンバ大阪")
m_lm_test_13 <-lm_test_all%>%filter(home=="ギラヴァンツ北九州")
m_lm_test_14 <-lm_test_all%>%filter(home=="コンサドーレ札幌")
m_lm_test_15 <-lm_test_all%>%filter(home=="サガン鳥栖")
m_lm_test_16 <-lm_test_all%>%filter(home=="ザスパクサツ群馬")
m_lm_test_17 <-lm_test_all%>%filter(home=="サンフレッチェ広島")
m_lm_test_18 <-lm_test_all%>%filter(home=="ジェフユナイテッド千葉")
m_lm_test_19 <-lm_test_all%>%filter(home=="ジュビロ磐田")
m_lm_test_20 <-lm_test_all%>%filter(home=="セレッソ大阪")
m_lm_test_21 <-lm_test_all%>%filter(home=="ファジアーノ岡山")
m_lm_test_22 <-lm_test_all%>%filter(home=="ベガルタ仙台")
m_lm_test_23 <-lm_test_all%>%filter(home=="モンテディオ山形")
m_lm_test_24 <-lm_test_all%>%filter(home=="ロアッソ熊本")
m_lm_test_25 <-lm_test_all%>%filter(home=="愛媛ＦＣ")
m_lm_test_26 <-lm_test_all%>%filter(home=="浦和レッズ")
m_lm_test_27 <-lm_test_all%>%filter(home=="横浜Ｆ・マリノス")
m_lm_test_28 <-lm_test_all%>%filter(home=="横浜ＦＣ")
m_lm_test_29 <-lm_test_all%>%filter(home=="京都サンガF.C.")
m_lm_test_30 <-lm_test_all%>%filter(home=="鹿島アントラーズ")
m_lm_test_31 <-lm_test_all%>%filter(home=="松本山雅ＦＣ")
m_lm_test_32 <-lm_test_all%>%filter(home=="湘南ベルマーレ")
m_lm_test_33 <-lm_test_all%>%filter(home=="水戸ホーリーホック")
m_lm_test_34 <-lm_test_all%>%filter(home=="清水エスパルス")
m_lm_test_35 <-lm_test_all%>%filter(home=="川崎フロンターレ")
m_lm_test_36 <-lm_test_all%>%filter(home=="大宮アルディージャ")
m_lm_test_37 <-lm_test_all%>%filter(home=="大分トリニータ")
m_lm_test_38 <-lm_test_all%>%filter(home=="東京ヴェルディ")
m_lm_test_39 <-lm_test_all%>%filter(home=="徳島ヴォルティス")
m_lm_test_40 <-lm_test_all%>%filter(home=="栃木ＳＣ")
m_lm_test_41 <-lm_test_all%>%filter(home=="柏レイソル")
m_lm_test_42 <-lm_test_all%>%filter(home=="名古屋グランパス")

lm_test_1  <-m_lm_test_1   %>%  dplyr::select(-id, -home)
lm_test_2  <-m_lm_test_2   %>%  dplyr::select(-id, -home)
lm_test_3  <-m_lm_test_3   %>%  dplyr::select(-id, -home)
lm_test_4  <-m_lm_test_4   %>%  dplyr::select(-id, -home)
lm_test_5  <-m_lm_test_5   %>%  dplyr::select(-id, -home)
lm_test_6  <-m_lm_test_6   %>%  dplyr::select(-id, -home)
lm_test_7  <-m_lm_test_7   %>%  dplyr::select(-id, -home)
lm_test_8  <-m_lm_test_8   %>%  dplyr::select(-id, -home)
lm_test_9  <-m_lm_test_9   %>%  dplyr::select(-id, -home)
lm_test_10 <-m_lm_test_10  %>%  dplyr::select(-id, -home)
lm_test_11 <-m_lm_test_11  %>%  dplyr::select(-id, -home)
lm_test_12 <-m_lm_test_12  %>%  dplyr::select(-id, -home)
lm_test_13 <-m_lm_test_13  %>%  dplyr::select(-id, -home)
lm_test_14 <-m_lm_test_14  %>%  dplyr::select(-id, -home)
lm_test_15 <-m_lm_test_15  %>%  dplyr::select(-id, -home)
lm_test_16 <-m_lm_test_16  %>%  dplyr::select(-id, -home)
lm_test_17 <-m_lm_test_17  %>%  dplyr::select(-id, -home)
lm_test_18 <-m_lm_test_18  %>%  dplyr::select(-id, -home)
lm_test_19 <-m_lm_test_19  %>%  dplyr::select(-id, -home)
lm_test_20 <-m_lm_test_20  %>%  dplyr::select(-id, -home)
lm_test_21 <-m_lm_test_21  %>%  dplyr::select(-id, -home)
lm_test_22 <-m_lm_test_22  %>%  dplyr::select(-id, -home)
lm_test_23 <-m_lm_test_23  %>%  dplyr::select(-id, -home)
lm_test_24 <-m_lm_test_24  %>%  dplyr::select(-id, -home)
lm_test_25 <-m_lm_test_25  %>%  dplyr::select(-id, -home)
lm_test_26 <-m_lm_test_26  %>%  dplyr::select(-id, -home)
lm_test_27 <-m_lm_test_27  %>%  dplyr::select(-id, -home)
lm_test_28 <-m_lm_test_28  %>%  dplyr::select(-id, -home)
lm_test_29 <-m_lm_test_29  %>%  dplyr::select(-id, -home)
lm_test_30 <-m_lm_test_30  %>%  dplyr::select(-id, -home)
lm_test_31 <-m_lm_test_31  %>%  dplyr::select(-id, -home)
lm_test_32 <-m_lm_test_32  %>%  dplyr::select(-id, -home)
lm_test_33 <-m_lm_test_33  %>%  dplyr::select(-id, -home)
lm_test_34 <-m_lm_test_34  %>%  dplyr::select(-id, -home)
lm_test_35 <-m_lm_test_35  %>%  dplyr::select(-id, -home)
lm_test_36 <-m_lm_test_36  %>%  dplyr::select(-id, -home)
lm_test_37 <-m_lm_test_37  %>%  dplyr::select(-id, -home)
lm_test_38 <-m_lm_test_38  %>%  dplyr::select(-id, -home)
lm_test_39 <-m_lm_test_39  %>%  dplyr::select(-id, -home)
lm_test_40 <-m_lm_test_40  %>%  dplyr::select(-id, -home)
lm_test_41 <-m_lm_test_41  %>%  dplyr::select(-id, -home)
lm_test_42 <-m_lm_test_42  %>%  dplyr::select(-id, -home)


# ###重回帰分析
# #family:目的変数の確率分布とリンク関数の設定(今回は正規分布と恒等写像)
lm_1   <-  glm(y2 ~ ., data=lm_train_1 , family=gaussian(link="identity"))
lm_2   <-  glm(y2 ~ ., data=lm_train_2 , family=gaussian(link="identity"))
lm_3   <-  glm(y2 ~ ., data=lm_train_3 , family=gaussian(link="identity"))
lm_4   <-  glm(y2 ~ ., data=lm_train_4 , family=gaussian(link="identity"))
lm_5   <-  glm(y2 ~ ., data=lm_train_5 , family=gaussian(link="identity"))
lm_6   <-  glm(y2 ~ ., data=lm_train_6 , family=gaussian(link="identity"))
lm_7   <-  glm(y2 ~ ., data=lm_train_7 , family=gaussian(link="identity"))
lm_8   <-  glm(y2 ~ ., data=lm_train_8 , family=gaussian(link="identity"))
lm_9   <-  glm(y2 ~ ., data=lm_train_9 , family=gaussian(link="identity"))
lm_10  <-  glm(y2 ~ ., data=lm_train_10, family=gaussian(link="identity"))
lm_11  <-  glm(y2 ~ ., data=lm_train_11, family=gaussian(link="identity"))
lm_12  <-  glm(y2 ~ ., data=lm_train_12, family=gaussian(link="identity"))
lm_13  <-  glm(y2 ~ ., data=lm_train_13, family=gaussian(link="identity"))
lm_14  <-  glm(y2 ~ ., data=lm_train_14, family=gaussian(link="identity"))
lm_15  <-  glm(y2 ~ ., data=lm_train_15, family=gaussian(link="identity"))
lm_16  <-  glm(y2 ~ ., data=lm_train_16, family=gaussian(link="identity"))
lm_17  <-  glm(y2 ~ ., data=lm_train_17, family=gaussian(link="identity"))
lm_18  <-  glm(y2 ~ ., data=lm_train_18, family=gaussian(link="identity"))
lm_19  <-  glm(y2 ~ ., data=lm_train_19, family=gaussian(link="identity"))
lm_20  <-  glm(y2 ~ ., data=lm_train_20, family=gaussian(link="identity"))
lm_21  <-  glm(y2 ~ ., data=lm_train_21, family=gaussian(link="identity"))
lm_22  <-  glm(y2 ~ ., data=lm_train_22, family=gaussian(link="identity"))
lm_23  <-  glm(y2 ~ ., data=lm_train_23, family=gaussian(link="identity"))
lm_24  <-  glm(y2 ~ ., data=lm_train_24, family=gaussian(link="identity"))
lm_25  <-  glm(y2 ~ ., data=lm_train_25, family=gaussian(link="identity"))
lm_26  <-  glm(y2 ~ ., data=lm_train_26, family=gaussian(link="identity"))
lm_27  <-  glm(y2 ~ ., data=lm_train_27, family=gaussian(link="identity"))
lm_28  <-  glm(y2 ~ ., data=lm_train_28, family=gaussian(link="identity"))
lm_29  <-  glm(y2 ~ ., data=lm_train_29, family=gaussian(link="identity"))
lm_30  <-  glm(y2 ~ ., data=lm_train_30, family=gaussian(link="identity"))
lm_31  <-  glm(y2 ~ ., data=lm_train_31, family=gaussian(link="identity"))
lm_32  <-  glm(y2 ~ ., data=lm_train_32, family=gaussian(link="identity"))
lm_33  <-  glm(y2 ~ ., data=lm_train_33, family=gaussian(link="identity"))
lm_34  <-  glm(y2 ~ ., data=lm_train_34, family=gaussian(link="identity"))
lm_35  <-  glm(y2 ~ ., data=lm_train_35, family=gaussian(link="identity"))
lm_36  <-  glm(y2 ~ ., data=lm_train_36, family=gaussian(link="identity"))
lm_37  <-  glm(y2 ~ ., data=lm_train_37, family=gaussian(link="identity"))
lm_38  <-  glm(y2 ~ ., data=lm_train_38, family=gaussian(link="identity"))
lm_39  <-  glm(y2 ~ ., data=lm_train_39, family=gaussian(link="identity"))
lm_40  <-  glm(y2 ~ ., data=lm_train_40, family=gaussian(link="identity"))
lm_41  <-  glm(y2 ~ ., data=lm_train_41, family=gaussian(link="identity"))
lm_42  <-  glm(y2 ~ ., data=lm_train_42, family=gaussian(link="identity"))



#testに当てはめ
pred_1   <-  predict(lm_1   , lm_test_1   , type="response")
pred_2   <-  predict(lm_2   , lm_test_2   , type="response")
pred_3   <-  predict(lm_3   , lm_test_3   , type="response")
pred_4   <-  predict(lm_4   , lm_test_4   , type="response")
pred_5   <-  predict(lm_5   , lm_test_5   , type="response")
pred_6   <-  predict(lm_6   , lm_test_6   , type="response")
pred_7   <-  predict(lm_7   , lm_test_7   , type="response")
pred_8   <-  predict(lm_8   , lm_test_8   , type="response")
pred_9   <-  predict(lm_9   , lm_test_9   , type="response")
pred_10  <-  predict(lm_10  , lm_test_10  , type="response")
pred_11  <-  predict(lm_11  , lm_test_11  , type="response")
pred_12  <-  predict(lm_12  , lm_test_12  , type="response")
pred_13  <-  predict(lm_13  , lm_test_13  , type="response")
pred_14  <-  predict(lm_14  , lm_test_14  , type="response")
pred_15  <-  predict(lm_15  , lm_test_15  , type="response")
pred_16  <-  predict(lm_16  , lm_test_16  , type="response")
pred_17  <-  predict(lm_17  , lm_test_17  , type="response")
pred_18  <-  predict(lm_18  , lm_test_18  , type="response")
pred_19  <-  predict(lm_19  , lm_test_19  , type="response")
pred_20  <-  predict(lm_20  , lm_test_20  , type="response")
pred_21  <-  predict(lm_21  , lm_test_21  , type="response")
pred_22  <-  predict(lm_22  , lm_test_22  , type="response")
pred_23  <-  predict(lm_23  , lm_test_23  , type="response")
pred_24  <-  predict(lm_24  , lm_test_24  , type="response")
pred_25  <-  predict(lm_25  , lm_test_25  , type="response")
pred_26  <-  predict(lm_26  , lm_test_26  , type="response")
pred_27  <-  predict(lm_27  , lm_test_27  , type="response")
pred_28  <-  predict(lm_28  , lm_test_28  , type="response")
pred_29  <-  predict(lm_29  , lm_test_29  , type="response")
pred_30  <-  predict(lm_30  , lm_test_30  , type="response")
pred_31  <-  predict(lm_31  , lm_test_31  , type="response")
pred_32  <-  predict(lm_32  , lm_test_32  , type="response")
pred_33  <-  predict(lm_33  , lm_test_33  , type="response")
pred_34  <-  predict(lm_34  , lm_test_34  , type="response")
pred_35  <-  predict(lm_35  , lm_test_35  , type="response")
pred_36  <-  predict(lm_36  , lm_test_36  , type="response")
pred_37  <-  predict(lm_37  , lm_test_37  , type="response")
pred_38  <-  predict(lm_38  , lm_test_38  , type="response")
pred_39  <-  predict(lm_39  , lm_test_39  , type="response")
pred_40  <-  predict(lm_40  , lm_test_40  , type="response")
pred_41  <-  predict(lm_41  , lm_test_41  , type="response")
pred_42  <-  predict(lm_42  , lm_test_42  , type="response")

#予測集客率*capaで予測集客人数を計算
pred_1   <-round( pred_1   *  lm_test_1$capa)
pred_2   <-round( pred_2   *  lm_test_2$capa)
pred_3   <-round( pred_3   *  lm_test_3$capa)
pred_4   <-round( pred_4   *  lm_test_4$capa)
pred_5   <-round( pred_5   *  lm_test_5$capa)
pred_6   <-round( pred_6   *  lm_test_6$capa)
pred_7   <-round( pred_7   *  lm_test_7$capa)
pred_8   <-round( pred_8   *  lm_test_8$capa)
pred_9   <-round( pred_9   *  lm_test_9$capa)
pred_10  <-round( pred_10  *  lm_test_10$capa)
pred_11  <-round( pred_11  *  lm_test_11$capa)
pred_12  <-round( pred_12  *  lm_test_12$capa)
pred_13  <-round( pred_13  *  lm_test_13$capa)
pred_14  <-round( pred_14  *  lm_test_14$capa)
pred_15  <-round( pred_15  *  lm_test_15$capa)
pred_16  <-round( pred_16  *  lm_test_16$capa)
pred_17  <-round( pred_17  *  lm_test_17$capa)
pred_18  <-round( pred_18  *  lm_test_18$capa)
pred_19  <-round( pred_19  *  lm_test_19$capa)
pred_20  <-round( pred_20  *  lm_test_20$capa)
pred_21  <-round( pred_21  *  lm_test_21$capa)
pred_22  <-round( pred_22  *  lm_test_22$capa)
pred_23  <-round( pred_23  *  lm_test_23$capa)
pred_24  <-round( pred_24  *  lm_test_24$capa)
pred_25  <-round( pred_25  *  lm_test_25$capa)
pred_26  <-round( pred_26  *  lm_test_26$capa)
pred_27  <-round( pred_27  *  lm_test_27$capa)
pred_28  <-round( pred_28  *  lm_test_28$capa)
pred_29  <-round( pred_29  *  lm_test_29$capa)
pred_30  <-round( pred_30  *  lm_test_30$capa)
pred_31  <-round( pred_31  *  lm_test_31$capa)
pred_32  <-round( pred_32  *  lm_test_32$capa)
pred_33  <-round( pred_33  *  lm_test_33$capa)
pred_34  <-round( pred_34  *  lm_test_34$capa)
pred_35  <-round( pred_35  *  lm_test_35$capa)
pred_36  <-round( pred_36  *  lm_test_36$capa)
pred_37  <-round( pred_37  *  lm_test_37$capa)
pred_38  <-round( pred_38  *  lm_test_38$capa)
pred_39  <-round( pred_39  *  lm_test_39$capa)
pred_40  <-round( pred_40  *  lm_test_40$capa)
pred_41  <-round( pred_41  *  lm_test_41$capa)
pred_42  <-round( pred_42  *  lm_test_42$capa)


#予測値補正
pred_1 <-F_pred_fix(pred_1 ,m_lm_train_1 ,lm_test_1 )
pred_2 <-F_pred_fix(pred_2 ,m_lm_train_2 ,lm_test_2 )
pred_3 <-F_pred_fix(pred_3 ,m_lm_train_3 ,lm_test_3 )
pred_4 <-F_pred_fix(pred_4 ,m_lm_train_4 ,lm_test_4 )
pred_5 <-F_pred_fix(pred_5 ,m_lm_train_5 ,lm_test_5 )
pred_6 <-F_pred_fix(pred_6 ,m_lm_train_6 ,lm_test_6 )
pred_7 <-F_pred_fix(pred_7 ,m_lm_train_7 ,lm_test_7 )
pred_8 <-F_pred_fix(pred_8 ,m_lm_train_8 ,lm_test_8 )
pred_9 <-F_pred_fix(pred_9 ,m_lm_train_9 ,lm_test_9 )
pred_10<-F_pred_fix(pred_10,m_lm_train_10,lm_test_10)
pred_11<-F_pred_fix(pred_11,m_lm_train_11,lm_test_11)
pred_12<-F_pred_fix(pred_12,m_lm_train_12,lm_test_12)
pred_13<-F_pred_fix(pred_13,m_lm_train_13,lm_test_13)
pred_14<-F_pred_fix(pred_14,m_lm_train_14,lm_test_14)
pred_15<-F_pred_fix(pred_15,m_lm_train_15,lm_test_15)
pred_16<-F_pred_fix(pred_16,m_lm_train_16,lm_test_16)
pred_17<-F_pred_fix(pred_17,m_lm_train_17,lm_test_17)
pred_18<-F_pred_fix(pred_18,m_lm_train_18,lm_test_18)
pred_19<-F_pred_fix(pred_19,m_lm_train_19,lm_test_19)
pred_20<-F_pred_fix(pred_20,m_lm_train_20,lm_test_20)
pred_21<-F_pred_fix(pred_21,m_lm_train_21,lm_test_21)
pred_22<-F_pred_fix(pred_22,m_lm_train_22,lm_test_22)
pred_23<-F_pred_fix(pred_23,m_lm_train_23,lm_test_23)
pred_24<-F_pred_fix(pred_24,m_lm_train_24,lm_test_24)
pred_25<-F_pred_fix(pred_25,m_lm_train_25,lm_test_25)
pred_26<-F_pred_fix(pred_26,m_lm_train_26,lm_test_26)
pred_27<-F_pred_fix(pred_27,m_lm_train_27,lm_test_27)
pred_28<-F_pred_fix(pred_28,m_lm_train_28,lm_test_28)
pred_29<-F_pred_fix(pred_29,m_lm_train_29,lm_test_29)
pred_30<-F_pred_fix(pred_30,m_lm_train_30,lm_test_30)
pred_31<-F_pred_fix(pred_31,m_lm_train_31,lm_test_31)
pred_32<-F_pred_fix(pred_32,m_lm_train_32,lm_test_32)
pred_33<-F_pred_fix(pred_33,m_lm_train_33,lm_test_33)
pred_34<-F_pred_fix(pred_34,m_lm_train_34,lm_test_34)
pred_35<-F_pred_fix(pred_35,m_lm_train_35,lm_test_35)
pred_36<-F_pred_fix(pred_36,m_lm_train_36,lm_test_36)
pred_37<-F_pred_fix(pred_37,m_lm_train_37,lm_test_37)
pred_38<-F_pred_fix(pred_38,m_lm_train_38,lm_test_38)
pred_39<-F_pred_fix(pred_39,m_lm_train_39,lm_test_39)
pred_40<-F_pred_fix(pred_40,m_lm_train_40,lm_test_40)
pred_41<-F_pred_fix(pred_41,m_lm_train_41,lm_test_41)
pred_42<-F_pred_fix(pred_42,m_lm_train_42,lm_test_42)


###submit形式変換
submit_1   <-  data.frame(id=m_lm_test_1[,"id"], pred=pred_1  )
submit_2   <-  data.frame(id=m_lm_test_2[,"id"], pred=pred_2  )
submit_3   <-  data.frame(id=m_lm_test_3[,"id"], pred=pred_3  )
submit_4   <-  data.frame(id=m_lm_test_4[,"id"], pred=pred_4  )
submit_5   <-  data.frame(id=m_lm_test_5[,"id"], pred=pred_5  )
submit_6   <-  data.frame(id=m_lm_test_6[,"id"], pred=pred_6  )
submit_7   <-  data.frame(id=m_lm_test_7[,"id"], pred=pred_7  )
submit_8   <-  data.frame(id=m_lm_test_8[,"id"], pred=pred_8  )
submit_9   <-  data.frame(id=m_lm_test_9[,"id"], pred=pred_9  )
submit_10  <-  data.frame(id=m_lm_test_10[,"id"], pred=pred_10)
submit_11  <-  data.frame(id=m_lm_test_11[,"id"], pred=pred_11)
submit_12  <-  data.frame(id=m_lm_test_12[,"id"], pred=pred_12)
submit_13  <-  data.frame(id=m_lm_test_13[,"id"], pred=pred_13)
submit_14  <-  data.frame(id=m_lm_test_14[,"id"], pred=pred_14)
submit_15  <-  data.frame(id=m_lm_test_15[,"id"], pred=pred_15)
submit_16  <-  data.frame(id=m_lm_test_16[,"id"], pred=pred_16)
submit_17  <-  data.frame(id=m_lm_test_17[,"id"], pred=pred_17)
submit_18  <-  data.frame(id=m_lm_test_18[,"id"], pred=pred_18)
submit_19  <-  data.frame(id=m_lm_test_19[,"id"], pred=pred_19)
submit_20  <-  data.frame(id=m_lm_test_20[,"id"], pred=pred_20)
submit_21  <-  data.frame(id=m_lm_test_21[,"id"], pred=pred_21)
submit_22  <-  data.frame(id=m_lm_test_22[,"id"], pred=pred_22)
submit_23  <-  data.frame(id=m_lm_test_23[,"id"], pred=pred_23)
submit_24  <-  data.frame(id=m_lm_test_24[,"id"], pred=pred_24)
submit_25  <-  data.frame(id=m_lm_test_25[,"id"], pred=pred_25)
submit_26  <-  data.frame(id=m_lm_test_26[,"id"], pred=pred_26)
submit_27  <-  data.frame(id=m_lm_test_27[,"id"], pred=pred_27)
submit_28  <-  data.frame(id=m_lm_test_28[,"id"], pred=pred_28)
submit_29  <-  data.frame(id=m_lm_test_29[,"id"], pred=pred_29)
submit_30  <-  data.frame(id=m_lm_test_30[,"id"], pred=pred_30)
submit_31  <-  data.frame(id=m_lm_test_31[,"id"], pred=pred_31)
submit_32  <-  data.frame(id=m_lm_test_32[,"id"], pred=pred_32)
submit_33  <-  data.frame(id=m_lm_test_33[,"id"], pred=pred_33)
submit_34  <-  data.frame(id=m_lm_test_34[,"id"], pred=pred_34)
submit_35  <-  data.frame(id=m_lm_test_35[,"id"], pred=pred_35)
submit_36  <-  data.frame(id=m_lm_test_36[,"id"], pred=pred_36)
submit_37  <-  data.frame(id=m_lm_test_37[,"id"], pred=pred_37)
submit_38  <-  data.frame(id=m_lm_test_38[,"id"], pred=pred_38)
submit_39  <-  data.frame(id=m_lm_test_39[,"id"], pred=pred_39)
submit_40  <-  data.frame(id=m_lm_test_40[,"id"], pred=pred_40)
submit_41  <-  data.frame(id=m_lm_test_41[,"id"], pred=pred_41)
submit_42  <-  data.frame(id=m_lm_test_42[,"id"], pred=pred_42)


submit_all <- dplyr::bind_rows(submit_1 ,
                               submit_2 ,
                               submit_3 ,
                               submit_4 ,
                               submit_5 ,
                               submit_6 ,
                               submit_7 ,
                               submit_8 ,
                               submit_9 ,
                               submit_10,
                               submit_11,
                               submit_12,
                               submit_13,
                               submit_14,
                               submit_15,
                               submit_16,
                               submit_17,
                               submit_18,
                               submit_19,
                               submit_20,
                               submit_21,
                               submit_22,
                               submit_23,
                               submit_24,
                               submit_25,
                               submit_26,
                               submit_27,
                               submit_28,
                               submit_29,
                               submit_30,
                               submit_31,
                               submit_32,
                               submit_33,
                               submit_34,
                               submit_35,
                               submit_36,
                               submit_37,
                               submit_38,
                               submit_39,
                               submit_40,
                               submit_41,
                               submit_42,)

sqrt(sum((lm_train_1 $y - pred_1 )^2)/nrow(lm_train_1 ))
sqrt(sum((lm_train_2 $y - pred_2 )^2)/nrow(lm_train_2 ))
sqrt(sum((lm_train_3 $y - pred_3 )^2)/nrow(lm_train_3 ))
sqrt(sum((lm_train_4 $y - pred_4 )^2)/nrow(lm_train_4 ))
sqrt(sum((lm_train_5 $y - pred_5 )^2)/nrow(lm_train_5 ))
sqrt(sum((lm_train_6 $y - pred_6 )^2)/nrow(lm_train_6 ))
sqrt(sum((lm_train_7 $y - pred_7 )^2)/nrow(lm_train_7 ))
sqrt(sum((lm_train_8 $y - pred_8 )^2)/nrow(lm_train_8 ))
sqrt(sum((lm_train_9 $y - pred_9 )^2)/nrow(lm_train_9 ))
sqrt(sum((lm_train_10$y - pred_10)^2)/nrow(lm_train_10))
sqrt(sum((lm_train_11$y - pred_11)^2)/nrow(lm_train_11))
sqrt(sum((lm_train_12$y - pred_12)^2)/nrow(lm_train_12))
sqrt(sum((lm_train_13$y - pred_13)^2)/nrow(lm_train_13))
sqrt(sum((lm_train_14$y - pred_14)^2)/nrow(lm_train_14))
sqrt(sum((lm_train_15$y - pred_15)^2)/nrow(lm_train_15))
sqrt(sum((lm_train_16$y - pred_16)^2)/nrow(lm_train_16))
sqrt(sum((lm_train_17$y - pred_17)^2)/nrow(lm_train_17))
sqrt(sum((lm_train_18$y - pred_18)^2)/nrow(lm_train_18))
sqrt(sum((lm_train_19$y - pred_19)^2)/nrow(lm_train_19))
sqrt(sum((lm_train_20$y - pred_20)^2)/nrow(lm_train_20))
sqrt(sum((lm_train_21$y - pred_21)^2)/nrow(lm_train_21))
sqrt(sum((lm_train_22$y - pred_22)^2)/nrow(lm_train_22))
sqrt(sum((lm_train_23$y - pred_23)^2)/nrow(lm_train_23))
sqrt(sum((lm_train_24$y - pred_24)^2)/nrow(lm_train_24))
sqrt(sum((lm_train_25$y - pred_25)^2)/nrow(lm_train_25))
sqrt(sum((lm_train_26$y - pred_26)^2)/nrow(lm_train_26))
sqrt(sum((lm_train_27$y - pred_27)^2)/nrow(lm_train_27))
sqrt(sum((lm_train_28$y - pred_28)^2)/nrow(lm_train_28))
sqrt(sum((lm_train_29$y - pred_29)^2)/nrow(lm_train_29))
sqrt(sum((lm_train_30$y - pred_30)^2)/nrow(lm_train_30))
sqrt(sum((lm_train_31$y - pred_31)^2)/nrow(lm_train_31))
sqrt(sum((lm_train_32$y - pred_32)^2)/nrow(lm_train_32))
sqrt(sum((lm_train_33$y - pred_33)^2)/nrow(lm_train_33))
sqrt(sum((lm_train_34$y - pred_34)^2)/nrow(lm_train_34))
sqrt(sum((lm_train_35$y - pred_35)^2)/nrow(lm_train_35))
sqrt(sum((lm_train_36$y - pred_36)^2)/nrow(lm_train_36))
sqrt(sum((lm_train_37$y - pred_37)^2)/nrow(lm_train_37))
sqrt(sum((lm_train_38$y - pred_38)^2)/nrow(lm_train_38))
sqrt(sum((lm_train_39$y - pred_39)^2)/nrow(lm_train_39))
sqrt(sum((lm_train_40$y - pred_40)^2)/nrow(lm_train_40))
sqrt(sum((lm_train_41$y - pred_41)^2)/nrow(lm_train_41))
sqrt(sum((lm_train_42$y - pred_42)^2)/nrow(lm_train_42))
###CSV出力(ヘッダーなし)
write.table(submit_all, file="C:/study/JLeague/submit/submit_20171125_2_lm.csv",
            quote=FALSE, sep=",", row.names=FALSE, col.names=FALSE)