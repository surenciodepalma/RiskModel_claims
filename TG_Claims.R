ddc <- read.csv('full_claims.csv', header = TRUE, sep = ',')



ddc$Boro <- as.character(ddc$Boro) # change BORO class/mode type from factor/numeric to character
ddc['Boro.adj'] <- ddc$Boro # copy BORO column and rename

table(ddc$Boro.adj)
head(ddc)
tail(ddc)
dim(ddc)
colnames(ddc)

sapply(ddc, mode)
sapply(ddc, class)

summary(ddc$Year)

which(ddc$Boro == "") # identify missing Boro rows
ddc$Boro[ddc$Boro == ""] <- NA  # replace "" with NA

which(ddc$Boro.adj == "")  # find blank spaces
ddc$Boro.adj[ddc$Boro.adj == ""] <- 'Other' # replace blank space with Other
ddc$Boro.adj[ddc$Boro.adj == 'MANHATTAN (NEW YORK)'] <- 'MANHATTAN'  # rename Manhattan
ddc$Boro.adj[ddc$Boro.adj == 'BROOKLYN (KINGS)'] <- 'BROOKLYN'  # rename
ddc$Boro.adj[ddc$Boro.adj == 'STATEN ISLAND (RICHMOND)'] <- 'STATEN ISLAND' 
ddc$Boro.adj[ddc$Boro.adj == 'PUTNAM'] <- 'Other'
ddc$Boro.adj[ddc$Boro.adj == 'ORANGE'] <- 'Other'
ddc$Boro.adj[ddc$Boro.adj == 'DUTCHESS'] <- 'Other'
ddc$Boro.adj[ddc$Boro.adj == 'WESTCHESTER'] <- 'Other'
ddc$Boro.adj[ddc$Boro.adj == 'SULLIVAN COUNTY'] <- 'Other'
ddc$Boro.adj[ddc$Boro.adj == 'ULSTER'] <- 'Other'

table(ddc$Boro.adj)

temp <- ddc # create temp variable in case of mistake

# create adjusted Agency column 
temp['Agency.adj'] <- temp$DeptDesc
table(temp$Agency.adj)
which(temp$Agency.adj == "") # identify missing Agency rows
temp$Agency.adj[temp$Agency.adj == ""] <- NA  # replace "" with NA
temp$Agency.adj <- as.character(temp$Agency.adj)
agency <- c('DEPT OF ENVIRONMENTAL PROTECTION', 'DEPT OF EDUCATION', 'DEPT OF TRANSPORTATION', 'NYC TRANSIT AUTHORITY', 
            'DEPT OF DESIGN AND CONSTRUCTION', 'DEPT OF PARKS & RECREATION')

temp$Agency.adj[!(temp$Agency.adj %in% agency)] <- 'Other'
table(temp$Agency.adj)
ddc <- temp
colnames(ddc)
# show claim amounts per $1M
ddc['Claim_Amt.1M'] <- ddc$CLAIMED_AMOUNT/1000000 
head(ddc$Claim_Amt.1M)

# boxplot of claim amounts
boxplot(ddc$Claim_Amt.1M, las=TRUE, col='cadetblue', density=30, angle=50)

# remove outliers
rm.out <- ddc$Claim_Amt.1M[!ddc$Claim_Amt.1M %in% boxplot.stats(ddc$Claim_Amt.1M)$out]
par(mfrow=c(1,1))
boxplot(ddc$Claim_Amt.1M ~ ddc$Boro.adj + ddc$Agency.adj, las=TRUE, col='cadetblue', density=30, angle=50, ylab = "Claim Amount x $1M")
hist(rm.out/100000, las = TRUE, col = 'cadetblue', density = 30, angle = 50, xlab = 'Claim Amount x $1M')

par(mar=c(4,8,2,1)) # adjust plot margins. 4 is bottom, 8 is left, 4 is top, 1 is right 
# boxplot of Claim Amt per $1M for each Borough
boxplot(ddc$Claim_Amt.1M[ddc$Claim_Amt.1M < 4000]~ddc$Boro.adj[ddc$Claim_Amt.1M < 4000], horizontal = TRUE, las=TRUE, 
        main = 'Claim Amt ($1M)', xlab = 'Claim Amount per $1M')
par(mar=c(5,12,4,2)) 
# sqrt transformation of claim amount
boxplot((sqrt(ddc$Claim_Amt.1M))[ddc$Claim_Amt.1M < 4000]~(sqrt(ddc$Boro.adj))[ddc$Claim_Amt.1M < 4000], 
        horizontal = TRUE, las=TRUE)


# setup contingency table for TG Claims by Borough
table(ddc$Boro.adj, ddc$Agency.adj)
# contingency table by Borough and Agency
table(ddc$Boro.adj)
table(ddc$Agency.adj)

# linear model for claim amount vs borough and agency
claim.lm <- lm(Claim_Amt.1M ~ Boro.adj+Agency.adj, data = ddc)
summary(claim.lm)
# linear model for claim amount vs agency
claim.ag <- lm(Claim_Amt.1M ~ Agency.adj, data = ddc)
summary(claim.ag)

# Boxplot of claim amount for each year
table(ddc$Year) # contingency table to ensure values are represented for each year
par(mar=c(4,4,2,2)) 
boxplot(ddc$Claim_Amt.1M[ddc$Claim_Amt.1M < 4000]~ddc$Year[ddc$Claim_Amt.1M < 4000], horizontal = TRUE, las=TRUE, 
        main = 'Claim Amount($1M) by Year', xlab = 'Claim Amount per $1M', pch=19, cex.axis=0.9)
abline(v = 10, col="firebrick", lwd=.5)
abline(v=5, col='purple', lwd=0.5)

# Boxplot of claim amount for each agency
par(mar=c(5,8,4,2)) 
boxplot(ddc$Claim_Amt.1M[ddc$Claim_Amt.1M < 4000]~ddc$Agency.adj[ddc$Claim_Amt.1M < 4000], horizontal = TRUE, las=TRUE, 
        main = 'Claim Amount Per Agency', xlab = 'Claim Amount per $1M', pch=19, cex.axis=0.6)


# write clean file
ddc.clean <- subset(ddc, !is.na(RIS))
write.table(ddc.clean, "full_claims.csv", sep=',', append = FALSE, row.names=FALSE, col.names = TRUE)
x <- read.table('full_claims.csv', sep = ",", header = TRUE)
head(x)
