
suii_J1_all <- dplyr::bind_rows(suii_J1_2012, suii_J1_2013,suii_J1_2014)
suii_J2_all <- dplyr::bind_rows(suii_J2_2012, suii_J2_2013,suii_J2_2014)


train_new$home_lank<-0
train_new$away_lank<-0
test_new$home_lank<-0
test_new$away_lank<-0

train_new<-F_suiiketugou(train_new,suii_J1_all,1)
train_new<-F_suiiketugou(train_new,suii_J2_all,2)

test_new<-F_suiiketugou(test_new,suii_J1_all,1)
test_new<-F_suiiketugou(test_new,suii_J2_all,2)

table(train_new$home_lank)
table(train_new$away_lank)

table(test_new$home_lank)
table(test_new$away_lank)


F_suiiketugou<- function(df, df_suii, df_stage) {
  

  
  for(i in 1:nrow(df)){
    
    t_year<-df$year[i]
    t_stage<-df$stage[i]
    t_home<-df$home[i]
    t_away<-df$away[i]
    t_setu<-df$setu[i]
    
    for(j in 1:nrow(df_suii)){
      
      s_year<-df_suii$year[j]
      s_stage<-df_suii$stage[j]
      s_team<-df_suii$team[j]
      

      if(df_stage==1){
        max_setu<-34
      }else{
        max_setu<-42
      }
      
      for(s_setu in 1:max_setu){
        if(t_year==s_year &&
           t_stage==s_stage &&
           (t_home==s_team || t_away==s_team) &&
           t_setu==s_setu){
          if(t_home==s_team ){
            df$home_lank[i]<-df_suii[j , 3+max_setu+t_setu]
          }else if(t_away==s_team){
            df$away_lank[i]<-df_suii[j , 3+max_setu+t_setu]
          }
        }
      }
    }
  }
  return(df)
}


lank_i <- paste("lank_",i,sep="")