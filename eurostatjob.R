library(eurostat)
library(ggplot2)

toc <- get_eurostat_toc()

# filter toc
tocgas <- as.data.frame(filter(toc,grepl("gas",toc$title)))
tocgas <- toc[grepl(c("gas"),toc$title, ignore.case = TRUE), ]
tocgas <- tocgas[grepl(c("price"),tocgas$title, ignore.case = TRUE), ]


#### gasprices

dfgasp <- get_eurostat(id="nrg_pc_202")
dfgasp2 <- subset(dfgasp,dfgasp$currency=="EUR" &
                    dfgasp$unit=="KWH" &
                    dfgasp$tax=="I_TAX" &
                    dfgasp$consom=="4141901"
                    )
dfgasp3 <- dfgasp2[,-c(1,2,3,4,5)]

givevals <- function(geoc) {
  dfretval <- subset(dfgasp3,
                     dfgasp3$geo %in% geoc
                       )
  return(dfretval)
}

dfp1 <- givevals(c("DK","DE","HU"))

ggplot(dfp1,aes(x=dfp1$time,y=dfp1$values,group=geo,color=geo))+
  geom_line() + labs(x="year",y="gas price euro/kwh", title="Gas prices in Europe")

#### job vacancies
dfjvs <- get_eurostat(id="jvs_q_nace2")
# remove if na in values in two ways
dfjvsclean <- dfjvs[!is.na(dfjvs$values),]
dfhjobs2 <- subset(dfjvs,!is.na(dfjvs$values))

# jobs for dk, es and hu. Pick Season Adjusted, Total and JOBVAC
dfhjobs2sub <- subset(dfhjobs2,
                        dfhjobs2$indic_em =="CH_Q_Q"&
                        dfhjobs2$s_adj =="SA"&
                        dfhjobs2$sizeclas =="TOTAL") 

dfjobclean <- dfhjobs2sub[-c(1,3,4)]


pldf <- giveoccuppation(c("L"),c("DE","DK"))
pldfn <- giveoccuppation(c("L","C"),c("DK"))
pldf <- pldf[,-c(1)]
pldfn <- pldfn[,-c(2)]

ggplot(data = pldf, aes(x=time, y=values, group=geo,color=geo))+
  geom_line() + labs(title = "Real Estate activity", 
                                    y="change rate",
                                    x="year")

ggplot(data = pldfn, aes(x=time, y=values, group=nace_r2,color=nace_r2))+
  geom_point() + geom_line() + labs(title = "Real Estate activity vs Manufacturing", 
                                    y="change rate",
                                    x="year")
# function to subset main-df
giveoccuppation <- function(occode,geoc) {
  dfretval <- subset(dfjobclean,
                     dfjobclean$nace_r2 %in% occode &
                     dfjobclean$geo %in% geoc
                       )
  return(dfretval)
}


#### TRASH #####
mydiff <- function(x,y) {
  retval=100*((x-y)/y)
  return(retval)
}
# diff
#for (i in (1:length(pldf$geo)-1)) {

for (i in (2:length(pldfEs$time))) {
  pldfEs[[5]][[i]] <- mydiff(pldfEs[[4]][[i]],pldfEs[[4]][[i-1]])
}
for (i in (2:length(pldf$time))) {
  pldf[[5]][[i]] <- mydiff(pldf[[4]][[i]],pldf[[4]][[i-1]])
}

colnames(pldf)[5] = c("perc")
colnames(pldfEs)[5] = c("perc")

