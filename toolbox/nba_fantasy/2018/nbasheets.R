
# Define functions
getZdf <- function(df) {
  
  n <- dim(df)[1]
  d <- dim(df)[2]

  # Indices of the first stat, and the last. Should only include 9-cat
  fs <- 6
  ls <- 14 

  stats <- colnames(df)[fs:ls]

  # We will add the Z_AVG column
  zdf <- data.frame(matrix(nrow=n, ncol=(ls+1)))
  colnames(zdf) = colnames(df)
  colnames(zdf)[fs:ls] = paste("Z", stats, sep ="_")
  colnames(zdf)[ls+1] = paste("Z_AVG")
  #colnames(zdf)[ls+2] = paste("Tier")

  # Copy over all the non-stat columns
  zdf[,1:(fs-1)] <- df[,1:(fs-1)]

  # Compute the z-score for each stat
  for (j in fs:ls) {
    zdf[,j] <- (df[,j] - mean(df[,j])) / sd(df[,j])
  }

  # Reverse the zscore for TO
  zdf[,ls] <- (mean(df[,ls]) - df[,ls]) / sd(df[,ls])

  # Add the Z_AVG column
  for (i in 1:n){
    zdf[i,(ls+1)] = mean(as.numeric(zdf[i,fs:ls]))
  }

  # Round to 3 places
  zdf[,fs:(ls+1)] <- round(zdf[,fs:(ls+1)],3)

  # Sort by average zscore
  zdf <- zdf[order(zdf$Z_AVG, decreasing = TRUE),]
  return(zdf[1:250,])

}

addTiers <- function(df, tiers) {

  n <- dim(tiers)[1]
  d <- dim(df)[2]

  for (i in 1:n) {
    df[grep(tiers[i,2], as.vector(df[,1])),d] <- tiers[i,1]
  }

  df <- df[order(df$Tier),]
  return(df)

} 

# START
df <- read.csv("2018raw.csv", header=TRUE)
zdf <- getZdf(df)

pg_zdf <- getZdf(df[grep("PG",df[,2]),])
sg_zdf <- getZdf(df[grep("SG",df[,2]),])
sf_zdf <- getZdf(df[grep("SF",df[,2]),])
pf_zdf <- getZdf(df[grep("PF",df[,2]),])
c_zdf <- getZdf(df[grep("C",df[,2]),])

#pg_tiers <- read.csv("pg_tiers.csv", header=FALSE)
#pg_zdf <- addTiers(pg_zdf,pg_tiers)

#sg_tiers <- read.csv("sg_tiers.csv", header=FALSE)
#sg_zdf <- addTiers(sg_zdf,sg_tiers)

#sf_tiers <- read.csv("sf_tiers.csv", header=FALSE)
#sf_zdf <- addTiers(sf_zdf,sf_tiers)

#pf_tiers <- read.csv("pf_tiers.csv", header=FALSE)
#pf_zdf <- addTiers(pf_zdf,pf_tiers)

#c_tiers <- read.csv("c_tiers.csv", header=FALSE)
#c_zdf <- addTiers(c_zdf,c_tiers)

write.csv(zdf, "./zdf.csv", quote=FALSE, row.names=FALSE)
write.csv(pg_zdf, "./pg_zdf.csv",  quote=FALSE, row.names=FALSE)
write.csv(sg_zdf, "./sg_zdf.csv",quote=FALSE, row.names=FALSE)
write.csv(sf_zdf, "./sf_zdf.csv",quote=FALSE, row.names=FALSE)
write.csv(pf_zdf, "./pf_zdf.csv",quote=FALSE, row.names=FALSE)
write.csv(c_zdf, "./c_zdf.csv",quote=FALSE, row.names=FALSE)
