#======================== load dataset =================================
library(readxl)
df <- read_excel("df.xlsx")
summary(df)

#===================== preprocess features =========================================
hist(df$T1_bby_NBON^0.33)
colnames(df)

df_st<-df[,c(1:2)]
summary(df_st)
colnames(df)

    #=============== check distribution normality and standardize if necessary====

#standardize data - bby
df_st["T1_bby_NBON_SCL"]<-df$T1_bby_NBON^0.33
df_st["T1_bby_MOPN_SCL"]<-df$T1_bby_MOPN^0.33
df_st["T1_bby_CA_SCL"]<-df$T1_bby_CA^0.33


#standardize data - cosm
df_st["T1_COSM_NBON_SCL"]<-df$T1_COSM_NBON^0.33
df_st["T1_COSM_MOPN_SCL"]<-df$T1_COSM_MOPN^0.33
df_st["T1_COSM_CA_SCL"]<-df$T1_COSM_CA^0.33

#standardize data  - derm
df_st["T1_DERM_NBON_SCL"]<-df$T1_DERM_NBON^0.33
df_st["T1_DERM_MOPN_SCL"]<-df$T1_DERM_MOPN^0.33
df_st["T1_DERM_CA_SCL"]<-df$T1_DERM_CA^0.33


#standardize data  - ma
df_st["T1_MA_NBON_SCL"]<-df$T1_MA_NBON^0.33
df_st["T1_MA_MOPN_SCL"]<-df$T1_MA_MOPN^0.33
df_st["T1_MA_CA_SCL"]<-df$T1_MA_CA^0.33


#standardize otc

df_st["T1_OTC_NBON_SCL"]<-df$T1_OTC_NBON^0.33
df_st["T1_OTC_MOPN_SCL"]<-df$T1_OTC_MOPN^0.33
df_st["T1_OTC_CA_SCL"]<-df$T1_OTC_CA^0.33


#standardize med
df_st["T1_med_NBON_SCL"]<-df$T1_med_NBON^0.33
df_st["T1_med_MOPN_SCL"]<-df$T1_med_MOPN^0.33
df_st["T1_med_CA_SCL"]<-df$T1_med_CA^0.33


#normalize/scale  data

library(caret)
df_scl<-df_st
summary(df_scl)
summary(df_st)


featureNormalize<-function(x){
  (x-min(x))/(max(x)-min(x))
}

df_scl[3:74]<-lapply(df_scl[3:74], FUN = featureNormalize)
summary(df_scl[,70:74])

#============================ dimension. reduction (t-sne) ========================

library(Rtsne)
tsne <- Rtsne(df_scl[3:74],dims=2, perplexity=20, verbose=TRUE, max_iter = 100)
summary(tsne)
df_tsne<-as.data.frame(tsne$Y)
df_final<-df_scl
df_final["tsne_v1"]<-df_tsne$V1
df_final["tsne_v2"]<-df_tsne$V2

#======================== export & save  ===========================================

#export data for embeddings.projector visualizations
str(df_scl)
df_projector<-df_scl[,3:74]

write.table(df_projector, file='unitsknn.tsv', quote=FALSE, sep="\t", 
            col.names = FALSE, row.names = FALSE)

#import/export mtdt

mtdt <- read_excel("mtdt.xlsx")
write.table(mtdt, file = "mtdt.tsv", quote = FALSE, sep = "\t", 
            col.names = TRUE, row.names = FALSE)
write.csv(df_final, "df_final.csv")













