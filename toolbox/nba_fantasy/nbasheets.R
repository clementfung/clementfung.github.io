
# Define functions
getZdf <- function(df) {
  
  n <- dim(df)[1]
  d <- dim(df)[2]

  stats <- colnames(df)[6:d]

  zdf <- data.frame(matrix(nrow=n,ncol=(d+2)))
  colnames(zdf) = colnames(df)
  colnames(zdf)[6:d] = paste("Z", stats, sep ="_")
  colnames(zdf)[d+1] = paste("Z_AVG")
  colnames(zdf)[d+2] = paste("Tier")

  zdf[,1:5] <- df[,1:5]
  for (j in 6:(d-1)) {
    zdf[,j] <- (df[,j] - mean(df[,j])) / sd(df[,j])
  }

  # Reverse the zscore for TO
  zdf[,d] <- (mean(df[,d]) - df[,d]) / sd(df[,d])

  for (i in 1:n){
    zdf[i,(d+1)] = mean(as.numeric(zdf[i,6:d]))
  }

  # Round to 3 places
  zdf[,6:(d+1)] <- round(zdf[,6:(d+1)],3)

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
df <- read.csv("rawstats.csv", header=TRUE)
zdf <- getZdf(df)

pg_zdf <- getZdf(df[grep("PG",df[,2]),])
sg_zdf <- getZdf(df[grep("SG",df[,2]),])
sf_zdf <- getZdf(df[grep("SF",df[,2]),])
pf_zdf <- getZdf(df[grep("PF",df[,2]),])
c_zdf <- getZdf(df[grep("C",df[,2]),])

pg_tiers <- read.csv("pg_tiers.csv", header=FALSE)
pg_zdf <- addTiers(pg_zdf,pg_tiers)

sg_tiers <- read.csv("sg_tiers.csv", header=FALSE)
sg_zdf <- addTiers(sg_zdf,sg_tiers)

sf_tiers <- read.csv("sf_tiers.csv", header=FALSE)
sf_zdf <- addTiers(sf_zdf,sf_tiers)

pf_tiers <- read.csv("pf_tiers.csv", header=FALSE)
pf_zdf <- addTiers(pf_zdf,pf_tiers)

c_tiers <- read.csv("c_tiers.csv", header=FALSE)
c_zdf <- addTiers(c_zdf,c_tiers)

write.csv(zdf, "./zdf.csv", quote=FALSE, row.names=FALSE)
write.csv(pg_zdf, "./pg_zdf.csv",  quote=FALSE, row.names=FALSE)
write.csv(sg_zdf, "./sg_zdf.csv",quote=FALSE, row.names=FALSE)
write.csv(sf_zdf, "./sf_zdf.csv",quote=FALSE, row.names=FALSE)
write.csv(pf_zdf, "./pf_zdf.csv",quote=FALSE, row.names=FALSE)
write.csv(c_zdf, "./c_zdf.csv",quote=FALSE, row.names=FALSE)
