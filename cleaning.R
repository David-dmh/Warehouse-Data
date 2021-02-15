df_old <- read.csv("~/data/ValueAddedActivities.csv")
head(df_old)
colnames(df_old)
rownames(df_old)
df_c <- data.frame(df_old[2])
df_c_units <- df_old[3]

for(i in 1:dim(new_cols)[1]){
  if(df_c$X.1[i] != ""){
    df_c$X.1[i] <- paste(df_c$X.1[i], " (", df_c_units$X.2[i],  ")", sep="")
  }
}

df_c <- df_c[!(df_c$X.1 == ""), ]
sbset <- df_old[match("YEAR.2010", colnames(df_old)):match("X.51", colnames(df_old))][1, ]
df_r <- as.character(sbset)
df <- data.frame(matrix(ncol=length(df_c), nrow=length(df_r)))
colnames(df) <- df_c
rownames(df) <- df_r
df <- df[-1]
head(df)

# populate df
for(i in 1:dim(df)[2]){
  # assign to its rows: all relevant col on df_old 2:54
  df[i] <- as.numeric(df_old[4:54][i+1, ]) 
}

head(df)
write.csv(df, file="plot.csv")
