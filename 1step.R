#### install.packages $ require ####

suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))
# install.packages('RODBC')
suppressMessages(library(RODBC))
suppressMessages(library(ROracle))
suppressMessages(library(lubridate))
suppressMessages(library(outliers))
# install.packages('arulesViz')
suppressMessages(library(arulesViz))
# install.packages("rgdal")
suppressMessages(library(rgdal))
# install.packages("pbapply")
suppressMessages(library(pbapply))
# install.packages('imputeTS')
suppressMessages(library(imputeTS))
# install.packages('lubridate')
suppressMessages(library('lubridate'))
suppressMessages(library(neuralnet))
suppressMessages(library(nnet))
# install.packages('devtools')
suppressMessages(library(devtools))
# install.packages('NeuralNetTools')
suppressMessages(library(NeuralNetTools))
# install.packages('lubridate')
suppressMessages(library(lubridate))
suppressMessages(library(rCharts))
# install.packages('reshape')
suppressMessages(library(reshape))
suppressMessages(library(reshape2))
# https://github.com/stascorp/rdpwrap/releases ## multi-session
# install.packages('mlr')
suppressMessages(library(mlr))
# install.packages('dummies')
suppressMessages(library(dummies))
suppressMessages(library(tm))
suppressMessages(library(arules))
#install.packages("data.table")
suppressMessages(library(data.table))
suppressMessages(library(stringr))
suppressMessages(library(AppliedPredictiveModeling))
transparentTheme(trans = .4) # color 명암 조절
# install.packages('caret')
suppressMessages(library(caret))
suppressMessages(library(plotly))
# install.packages("devtools")
# devtools::install_github("twitter/AnomalyDetection")
# install.packages("seasonalview")
suppressMessages(library(seasonalview))
suppressMessages(library(zoo))
# install.packages('outliers')
require(outliers)
# install.packages('forecast')
suppressMessages(library(forecast))
rm(list = ls())
#### data ####
driver<- Oracle()

driver <- dbDriver("Oracle") ; host <- "10.129.31.72" ; port <- 1527 ; sid <- "WMSB2C"

connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SERVICE_NAME=", sid, ")))", sep = "")

con <- dbConnect(driver, username = "AP492001", password = "qkrwjdrms$492001", dbname = connect.string)

test_if <- dbGetQuery(con, "SELECT DISTINCT A.STORER_CD as 화주코드
                      ,B.CREATE_NO as 생성번호
                      ,B.ORD_SYSTEM_NO as 주문관리번호
                      ,B.ORD_SEQ as 주문순번
                      ,A.CUST_CD as 거래처코드
                      ,A.OPER_CD as 사업부코드
                      ,D.OPER_NM as 사업부명
                      ,A.OWHS_TYPE_CD as  출고유형
                      ,A.OWHS_TYPE_DTL_CD as 출고유형상세
                      ,A.STATE_CD as 진행상태코드a
                      ,A.INS_DATETIME as 등록일시
                      ,A.ORD_DATE as 주문일자a
                      ,A.ORD_TIME as 주문시간
                      ,A.ORD_CREATE_DATE as 주문생성일자
                      ,A.ORD_CREATE_TIME as 주문생성시간
                      ,B.KEY_ORD_SYSTEM_NO as 대표주문관리번호
                      ,A.ORD_SYSTEM_NO as 내부주문번호
                      ,A.ORG_ORD_NO as 웹주문번호a
                      ,B.APPLY_EVENT_NO as 적용이벤트번호
                      ,B.APPLY_EVENT_YN as 적용이벤트여부
                      ,B.ITEM_CD as 제품코드a
                      ,E.ORD_ITEM_CD as 주문제품코드
                      ,E.WEBPRDCODE as WEBPRD
                      ,B.ORG_ITEM_CD as 원제품코드
                      ,B.ORD_QTY as 주문수량
                      ,B.ITEM_CD || '/' || B.ORD_QTY AS ITEMCDQTY
                      FROM OMSUSER.OM_ORG_ORD_INFO A
                      ,OMSUSER.OM_ORD_DTL_INFO B
                      ,WMSUSER.CO_OPER_INFO D
                      ,OMSUSER.OM_ORG_ORD_DTL_INFO E
                      WHERE A.ORD_SYSTEM_NO = B.ORD_SYSTEM_NO
                      AND A.CREATE_NO = B.CREATE_NO
                      AND A.ORD_SYSTEM_NO = E.ORD_SYSTEM_NO
                      AND A.CREATE_NO = E.CREATE_NO
                      AND B.ORD_SYSTEM_NO = E.ORD_SYSTEM_NO
                      AND B.CREATE_NO = E.CREATE_NO
                      AND B.ORD_SEQ = E.ORD_SEQ
                      AND A.STORER_CD in ('1300', 'TPIN')
                      AND A.OPER_CD  = 'IS01'
                      AND A.STORER_CD = D.STORER_CD
                      AND A.OPER_CD = D.OPER_CD
                      AND A.OWHS_TYPE_CD = '10'
                      AND A.OWHS_TYPE_DTL_CD = '11'")

write.csv(test_if, file = "D:/Final_IF/IF_MAll.csv", row.names = FALSE)
rm(list = ls())

if_mall <- data.frame(fread("D:/Final_IF/IF_MAll.csv", header=T, sep=','))
ad_code <- data.frame(fread("D:/Final_IF/ad_code.csv", header=T, sep=','))
ad_code$제품코드a <- as.character(ad_code$제품코드a)
colnames(ad_code) <- c("대표상품코드", "제품코드A", "year", "month")

# if_mall$제품코드A <- ifelse(if_mall$제품코드A %in% ad_code$제품코드A, ad_code$대표상품코드, if_mall$제품코드A)
if_mall$제품코드A <- ifelse(if_mall$제품코드A == ad_code$제품코드A, ad_code$대표상품코드, if_mall$제품코드A)

#### 주문일자A 결측치 제거####
if_mall$주문일자A <- as.Date(if_mall$주문일자A, '%Y%m%d')
if_mall_na <- if_mall[if_mall$내부주문번호 %in% unique(if_mall[is.na(if_mall$주문일자A) == T, '내부주문번호']),]
write.csv(if_mall_na, file = "D:/Final_IF/prepro/Nadate.csv", row.names = FALSE)
if_mall <- if_mall[!(if_mall$내부주문번호 %in% unique(if_mall[is.na(if_mall$주문일자A) == T, '내부주문번호'])),]

### 거래처(직영몰/입점몰) 선택 ###
cust_if <- data.frame(fread("D:/Final_IF/ifmall_custcd.csv", header=T, sep=','))

cust_if_y <- cust_if[cust_if$거래처구분 == 'Y','거래처코드']

if_mall <- if_mall[if_mall$거래처코드 %in% cust_if$거래처코드,]
if_mall$cust_yn <- ifelse(if_mall$거래처코드 %in% cust_if_y, 'Y', 'N')

#### 일자 결측치 확인 (등록일시, 주문일자A) ####
if_mall$등록일시 <- as.Date(if_mall$등록일시, '%Y-%m-%d %H:%M:%S')
if_mall$dis_date_NY <- ifelse(if_mall$등록일시 >= if_mall$주문일자A, 1, 0)

write.csv(if_mall[if_mall$내부주문번호 %in% unique(if_mall[if_mall$dis_date_NY == 0, '내부주문번호']),], file = "D:/Final_IF/prepro/error.csv", row.names = FALSE)
if_mall <- if_mall[!(if_mall$내부주문번호 %in% unique(if_mall[if_mall$dis_date_NY == 0, '내부주문번호'])),]

if_mall$year <- year(if_mall$주문일자A)
if_mall$month <- month(if_mall$주문일자A)
if_mall$weekday <- weekdays(if_mall$주문일자A)

#### 주문수량 0 제거 ####
write.csv(if_mall[if_mall$주문수량 == 0,], file = "D:/Final_IF/prepro/zero_volumes.csv", row.names = FALSE)
if_mall <- if_mall[!(if_mall$내부주문번호 %in% unique(if_mall[if_mall$주문수량 == 0, '내부주문번호'])),]

if_mall_ch <- if_mall

write.csv(if_mall_ch, file = "D:/Final_IF/IF_MAll_chan.csv", row.names = FALSE)
rm(list = ls())

if_mall <- data.frame(fread("D:/Final_IF/IF_MAll_chan.csv", header=T, sep=','))

date <- data.frame(date1 = sort(unique(substr(as.character(if_mall$주문일자A), 1, 10))))

if_mall <- if_mall[between(if_mall$주문일자A,'2014-01-01', '2017-12-31'), ] 

#### day base combine Data creating ####
# test_all 
if_all_org_demand_data <- data.frame(data.table(if_mall)[, list( s_order_volume = sum(주문수량),
                                                                 l_order_count = length(unique(내부주문번호)),
                                                                 l_product_count = length(unique(제품코드A)))
                                                         , by = list(as.character(주문일자A))])

write.csv(if_all_org_demand_data, file = "D:/Final_IF/if_all_org_demand_data.csv", row.names = FALSE)
write.csv(if_mall, file = "D:/Final_IF/if_all_association_data.csv", row.names = FALSE)
rm(list = ls())

#### org_demand model ####
if_org_demand_data <- data.frame(fread("D:/Final_IF/if_all_org_demand_data.csv", header=T, sep=','))

if_event <- data.frame(fread("D:/Final_IF/if_mall_eventdate.csv", header=T, sep=','))

ttt <- if_org_demand_data
colnames(ttt) <- c("date", "s_order_volume", "l_order_count", "l_product_count")
ttt$date <- as.Date(ttt$date, "%Y-%m-%d")

# str(if_event)
if_event$시작일 <- as.Date(if_event$시작일, "%Y-%m-%d")
if_event$종료일 <- as.Date(if_event$종료일, "%Y-%m-%d")

if_event$len <- if_event$종료일-if_event$시작일+1
# sum(if_event$len)

ttt$event <- 0
ttt$event_cl <- 0

for(i in 1:nrow(if_event)){
  for(j in 1:nrow(ttt)){
    ttt[j,'event'] <- ifelse(between(ttt[j,'date'], if_event[i,'시작일'], if_event[i,'종료일']),
                             if_event[i,'구분1'], ttt[j,'event'])
    ttt[j,'event_cl'] <- ifelse(between(ttt[j,'date'], if_event[i,'시작일'], if_event[i,'종료일']),
                                if_event[i,'구분2'], ttt[j,'event_cl'])
  }
}

# table(ttt$event)
# table(ttt$event_cl)

ttt <- ttt[order(ttt$date), ]

if_eventdate <- ttt[, c('date', "event", "event_cl")]

write.csv(if_eventdate, file = "D:/Final_IF/if_eventdate.csv", row.names = FALSE)

#### rules ####
rm(list = ls())

if_all_association_data <- data.frame(fread("D:/Final_IF/if_all_association_data.csv", header=T, sep=','))
if_all_association_data$주문일자A <- as.Date(if_all_association_data$주문일자A, "%Y-%m-%d")

### event data join###
if_eventdate <- data.frame(fread("D:/Final_IF/if_eventdate.csv", header=T, sep=','))
# str(if_eventdate)
if_eventdate$date <- as.Date(if_eventdate$date, "%Y-%m-%d")
# summary(if_eventdate)
colnames(if_eventdate) <- c('주문일자A', 'event', 'event_cl')

if_all_association_data <- join(if_all_association_data, if_eventdate)
if_all_association_data$weeknum <- as.numeric( format(if_all_association_data$주문일자A, "%U"))

write.csv(if_all_association_data, file = "D:/Final_IF/if_all_change_association_data.csv", row.names = FALSE)

rm(list = ls())

if_all_association_data <- data.frame(fread("D:/Final_IF/if_all_change_association_data.csv", header=T, sep=','))
if_all_association_data$주문일자A <- as.Date(if_all_association_data$주문일자A, "%Y-%m-%d")

if_all_association_data <- if_all_association_data[!(if_all_association_data$내부주문번호 %in% unique(if_all_association_data[if_all_association_data$주문수량 > 100, '내부주문번호'])),]
# summary(if_all_association_data$주문수량)

## direct
y_association_data <- if_all_association_data[if_all_association_data$내부주문번호 %in% unique(if_all_association_data[if_all_association_data$cust_yn == 'Y', '내부주문번호']),]
direct_association_data <- y_association_data[!(y_association_data$내부주문번호 %in% unique(y_association_data[y_association_data$주문수량 >= 10, '내부주문번호'])),]
# summary(direct_association_data$주문수량)

## store
n_association_data <- if_all_association_data[if_all_association_data$내부주문번호 %in% unique(if_all_association_data[if_all_association_data$cust_yn == 'N', '내부주문번호']),]
store_association_data <- n_association_data[!(n_association_data$내부주문번호 %in% unique(n_association_data[n_association_data$주문수량 >= 10, '내부주문번호'])),]
# summary(store_association_data$주문수량)

## direct_mask
direct_mask_association_data <- y_association_data[!(y_association_data$내부주문번호 %in% unique(y_association_data[y_association_data$주문수량 < 10, '내부주문번호'])),]
# summary(direct_mask_association_data$주문수량)

## store_mask
store_mask_association_data <- n_association_data[!(n_association_data$내부주문번호 %in% unique(n_association_data[n_association_data$주문수량 < 10, '내부주문번호'])),]
# summary(store_mask_association_data$주문수량)

write.csv(direct_association_data, file = "D:/Final_IF/direct/direct_association_data.csv", row.names = FALSE)
write.csv(store_association_data, file = "D:/Final_IF/store/store_association_data.csv", row.names = FALSE)
write.csv(direct_mask_association_data, file = "D:/Final_IF/direct_mask/direct_mask_association_data.csv", row.names = FALSE)
write.csv(store_mask_association_data, file = "D:/Final_IF/store_mask/store_mask_association_data.csv", row.names = FALSE)

rm(list = ls())



#### 2.direct association ####
### rules auto ####
direct <- data.frame(fread("D:/Final_IF/direct/direct_association_data.csv", header=T, sep=','))
direct[is.na(direct$WEBPRD) == T,'WEBPRD'] <- 'NNNN'
direct$ITEMCDQTY <- paste(direct$제품코드A, direct$주문수량 , sep = '/')
direct$ITEMCDQTY2 <- paste(direct$WEBPRD, direct$ITEMCDQTY , sep = '/')

raw <- direct

raw1 <- raw
raw1 <- raw1[, c(17, 21, 29, 30, 34, 35)]
colnames(raw1) <- c("incode", "code" , "year", "month", "weeknum", "name" )
raw1 <- data.table(unique(raw1))

raw1_1 <- data.frame(raw1[, list(volume = length(name)), by = list(incode, year, month, weeknum)])
raw1_2 <- unique(raw1_1[,-1])
raw1_3 <- raw1_2[raw1_2$volume ==1 ,]
colnames(raw1_3) <- c("year", "month", "weeknum", "check_one")
raw_one <- join(raw, raw1_3)
raw_one1 <- raw_one[is.na(raw_one$check_one) == F,]

#### all_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw # all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_rules/all/if_rule_all_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/proall/if_month_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_rules/month_all_rules_merge.csv", sep=''), row.names = FALSE)

### all_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw #all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/all/if_rule_all_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/direct/proall/if_month_week_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/month_week_all_rules_merge.csv", sep=''), row.names = FALSE)

#### all_single auto month relus data creat code ####

for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw_one1 # all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_rules/all/if_rule_all_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/pro10/if_month_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_rules/month_all_rules_merge.csv", sep=''), row.names = FALSE)

#### all_single auto week relus data creat code ####
for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1 #all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/all/if_rule_all_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/direct/pro10/if_month_week_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/month_week_all_rules_merge.csv", sep=''), row.names = FALSE)

#### notevent_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event == 0, ] # not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_rules/notevent/if_rule_notevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/proall/if_month_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_rules/month_notevent_rules_merge.csv", sep=''), row.names = FALSE)

### notevent_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event == 0, ] #not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/notevent/if_rule_notevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####
file.path <- 'D:/Final_IF/direct/proall/if_month_week_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/month_week_notevent_rules_merge.csv", sep=''), row.names = FALSE)

#### notevent_single auto month relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 0, ] # not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_rules/notevent/if_rule_notevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)  
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/pro10/if_month_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_rules/month_notevent_rules_merge.csv", sep=''), row.names = FALSE)

#### notevent_single auto week relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 0, ] #not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      if(nrow(if.mall.train.rule) > 0){
        for(k in 1:length(unique(if.mall.train.rule$volume))){
          volume <- unique(if.mall.train.rule$volume)[k]
          if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
          order_volume <- length(unique(if.mall.train$incode))
          
          if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
          
          if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
          
          if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
          
          if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
          
          ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
          if_rule <- data.frame(ifmall$x$data)
          
          # Remove curly brackets around rules
          if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
          if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
          
          if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
          for(l in 1:nrow(if_rule)){
            if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
          }
          if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
          if_rule$year <- year
          if_rule$month <- month
          if_rule$weeknumber <- weeknumber
          if_rule$all_order_volume <- order_volume
          if_rule$text_volume <- volume
          if_rule$order_volume <- round(if_rule$support * order_volume)
          if_rule <- if_rule[if_rule$order_volume >= 1, ]   
          write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/notevent/if_rule_notevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
        }
      } else {
        if_rule <- data.frame(rule = NA, support = NA , confidence = NA, lift = NA,
                              year = NA, month = NA, weeknumber = NA, all_order_volume = NA, text_volume = NA, order_volume = NA)
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(0 * order_volume)
        write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/notevent/if_rule_notevent_", year, "_",month, "_", weeknumber, "_1.csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####
file.path <- 'D:/Final_IF/direct/pro10/if_month_week_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/month_week_notevent_rules_merge.csv", sep=''), row.names = FALSE)

#### event_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  if_train_10_notevent <- raw[raw$event == 1 & raw$event_cl == 0, ] # event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_rules/event/if_rule_event_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/proall/if_month_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_rules/month_event_rules_merge.csv", sep=''), row.names = FALSE)

#### event_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event == 1 & raw$event_cl == 0, ] #event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/event/if_rule_event_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
        
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/direct/proall/if_month_week_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/month_week_event_rules_merge.csv", sep=''), row.names = FALSE)


#### event_single auto month relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 1 & raw_one1$event_cl == 0, ] # event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_rules/event/if_rule_event_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/pro10/if_month_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_rules/month_event_rules_merge.csv", sep=''), row.names = FALSE)


#### event_single auto week relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 1 & raw_one1$event_cl == 0, ] #event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/event/if_rule_event_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
        
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/direct/pro10/if_month_week_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/month_week_event_rules_merge.csv", sep=''), row.names = FALSE)

#### big event_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event_cl == 1, ] # big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/proall/if_month_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_rules/month_bigevent_rules_merge.csv", sep=''), row.names = FALSE)

#### big event_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event_cl == 1, ] #big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/direct/proall/if_month_week_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/proall/if_month_week_rules/month_week_bigevent_rules_merge.csv", sep=''), row.names = FALSE)


#### big event_sigle auto month relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event_cl == 1, ] # big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/direct/pro10/if_month_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_rules/month_bigevent_rules_merge.csv", sep=''), row.names = FALSE)


#### big event_sigle auto week relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event_cl == 1, ] #big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/direct/pro10/if_month_week_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',', '', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/direct/pro10/if_month_week_rules/month_week_bigevent_rules_merge.csv", sep=''), row.names = FALSE)


#### 3.store association ####
#### install.packages $ require ####
rm(list = ls())

### rules auto ####
store <- data.frame(fread("D:/Final_IF/store/store_association_data.csv", header=T, sep=','))
store[is.na(store$WEBPRD) == T,'WEBPRD'] <- 'NNNN'
store$ITEMCDQTY <- paste(store$제품코드A, store$주문수량 , sep = '/')
store$ITEMCDQTY2 <- paste(store$WEBPRD, store$ITEMCDQTY , sep = '/')

raw <- store

raw1 <- raw
raw1 <- raw1[, c(17, 21, 29, 30, 34, 35)]
colnames(raw1) <- c("incode", "code" , "year", "month", "weeknum", "name" )
raw1 <- data.table(unique(raw1))

raw1_1 <- data.frame(raw1[, list(volume = length(name)), by = list(incode,year,month,weeknum)])
raw1_2 <- unique(raw1_1[,-1])
raw1_3 <- raw1_2[raw1_2$volume ==1 ,]
colnames(raw1_3) <- c("year", "month", "weeknum", "check_one")
raw_one <- join(raw, raw1_3)
raw_one1 <- raw_one[is.na(raw_one$check_one) == F,]

#### all_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw # all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_rules/all/if_rule_all_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/proall/if_month_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_rules/month_all_rules_merge.csv", sep=''), row.names = FALSE)

### all_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw #all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_week_rules/all/if_rule_all_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/store/proall/if_month_week_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_week_rules/month_week_all_rules_merge.csv", sep=''), row.names = FALSE)

#### all_single auto month relus data creat code ####

for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw_one1 # all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_rules/all/if_rule_all_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/pro10/if_month_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_rules/month_all_rules_merge.csv", sep=''), row.names = FALSE)

#### all_single auto week relus data creat code ####
for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1 #all
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/all/if_rule_all_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/store/pro10/if_month_week_rules/all'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/month_week_all_rules_merge.csv", sep=''), row.names = FALSE)

#### notevent_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event == 0, ] # not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_rules/notevent/if_rule_notevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/proall/if_month_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_rules/month_notevent_rules_merge.csv", sep=''), row.names = FALSE)

### notevent_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event == 0, ] #not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_week_rules/notevent/if_rule_notevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####
file.path <- 'D:/Final_IF/store/proall/if_month_week_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_week_rules/month_week_notevent_rules_merge.csv", sep=''), row.names = FALSE)

#### notevent_single auto month relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 0, ] # not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_rules/notevent/if_rule_notevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)  
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/pro10/if_month_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_rules/month_notevent_rules_merge.csv", sep=''), row.names = FALSE)

#### notevent_single auto week relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 0, ] #not event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      if(nrow(if.mall.train.rule) > 0){
        for(k in 1:length(unique(if.mall.train.rule$volume))){
          volume <- unique(if.mall.train.rule$volume)[k]
          if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
          order_volume <- length(unique(if.mall.train$incode))
          
          if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
          
          if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
          
          if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
          
          if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
          
          ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
          if_rule <- data.frame(ifmall$x$data)
          
          # Remove curly brackets around rules
          if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
          if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
          
          if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
          for(l in 1:nrow(if_rule)){
            if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
          }
          if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
          if_rule$year <- year
          if_rule$month <- month
          if_rule$weeknumber <- weeknumber
          if_rule$all_order_volume <- order_volume
          if_rule$text_volume <- volume
          if_rule$order_volume <- round(if_rule$support * order_volume)
          if_rule <- if_rule[if_rule$order_volume >= 1, ]   
          write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/notevent/if_rule_notevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
        }
      } else {
        if_rule <- data.frame(rule = NA, support = NA , confidence = NA, lift = NA,
                              year = NA, month = NA, weeknumber = NA, all_order_volume = NA, text_volume = NA, order_volume = NA)
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(0 * order_volume)
        write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/notevent/if_rule_notevent_", year, "_",month, "_", weeknumber, "_1.csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####
file.path <- 'D:/Final_IF/store/pro10/if_month_week_rules/notevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/month_week_notevent_rules_merge.csv", sep=''), row.names = FALSE)

#### event_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  if_train_10_notevent <- raw[raw$event == 1 & raw$event_cl == 0, ] # event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_rules/event/if_rule_event_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/proall/if_month_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_rules/month_event_rules_merge.csv", sep=''), row.names = FALSE)

#### event_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event == 1 & raw$event_cl == 0, ] #event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_week_rules/event/if_rule_event_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
        
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/store/proall/if_month_week_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_week_rules/month_week_event_rules_merge.csv", sep=''), row.names = FALSE)


#### event_single auto month relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 1 & raw_one1$event_cl == 0, ] # event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_rules/event/if_rule_event_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/pro10/if_month_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_rules/month_event_rules_merge.csv", sep=''), row.names = FALSE)


#### event_single auto week relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event == 1 & raw_one1$event_cl == 0, ] #event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/event/if_rule_event_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
        
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/store/pro10/if_month_week_rules/event'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/month_week_event_rules_merge.csv", sep=''), row.names = FALSE)

#### big event_all auto month relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event_cl == 1, ] # big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check <- volume_check[volume_check$Var1 != 1,]
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    ## select volume ##
    chisq <- chisq.test(volume_check[, 3])
    # 
    x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
    xx <- cbind(volume_check[, 3], x)
    colnames(xx) <- c('주문수량', 'x')
    
    text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/proall/if_month_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_rules/month_bigevent_rules_merge.csv", sep=''), row.names = FALSE)

#### big event_all auto week relus data creat code ####
for(i in 1:length(unique(raw$year))){
  
  if_train_10_notevent <- raw[raw$event_cl == 1, ] #big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check <- volume_check[volume_check$Var1 != 1,]
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      ## select volume ##
      chisq <- chisq.test(volume_check[, 3])
      # 
      x <- data.frame(scores(volume_check[, 3], type = "chisq", prob = 0.95))
      xx <- cbind(volume_check[, 3], x)
      colnames(xx) <- c('주문수량', 'x')
      
      text_volume <- ifelse(sum(xx$x == T)==0, 5, as.numeric(volume_check[volume_check$rate == min(xx[xx$x == T, '주문수량']), 'Var1']) + 1)
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume != 1 & if.mall.train.rule$volume <= text_volume,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        if_rule <- if_rule[nchar(if_rule$LHS) != 2 & nchar(if_rule$RHS) != 2 ,]
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/store/proall/if_month_week_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/store/proall/if_month_week_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/proall/if_month_week_rules/month_week_bigevent_rules_merge.csv", sep=''), row.names = FALSE)


#### big event_sigle auto month relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event_cl == 1, ] # big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    
    if_train_10 <- if_train_10_year[if_train_10_year$month == month, ]
    
    # order volume outlier test #
    if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY2))
    if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
    
    if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
    
    colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
    
    if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
    if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list( volume = length(name)), by = list(incode)]
    
    if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
    
    volume_check <- unique(if.mall.train.rule[, c(1, 4)])
    volume_check <- data.frame(table(volume_check$volume))
    volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
    
    if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
    
    for(k in 1:length(unique(if.mall.train.rule$volume))){
      volume <- unique(if.mall.train.rule$volume)[k]
      if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
      order_volume <- length(unique(if.mall.train$incode))
      
      if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
      
      if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
      
      if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
      
      if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
      
      ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
      if_rule <- data.frame(ifmall$x$data)
      
      
      # Remove curly brackets around rules
      if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
      if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
      
      if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep = ',')
      for(l in 1:nrow(if_rule)){
        if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l],","))))), 2, paste, collapse = ",")
      }
      if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
      if_rule$year <- year
      if_rule$month <- month
      if_rule$all_order_volume <- order_volume
      if_rule$text_volume <- volume
      if_rule$order_volume <- round(if_rule$support * order_volume)
      if_rule <- if_rule[if_rule$order_volume >= 1, ] 
      
      write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      
    } 
  }
}

# auto month rules data join code ####
file.path <- 'D:/Final_IF/store/pro10/if_month_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')

rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_rules/month_bigevent_rules_merge.csv", sep=''), row.names = FALSE)


#### big event_sigle auto week relus data creat code ####

for(i in 1:length(unique(raw_one1$year))){
  
  if_train_10_notevent <- raw_one1[raw_one1$event_cl == 1, ] #big event
  
  year <- as.character(sort(as.numeric(unique(if_train_10_notevent$year)))[i])
  if_train_10_year <- if_train_10_notevent[if_train_10_notevent$year == year, ]
  
  for(j in 1:length(unique(if_train_10_year$month))){
    month <- as.character(sort(as.numeric(unique(if_train_10_year$month)))[j])
    if_train_10_month <- if_train_10_year[if_train_10_year$month == month, ]
    
    for(g in 1:length(unique(if_train_10_month$weeknum))){
      weeknumber <- as.character(sort(as.numeric(unique(if_train_10_month$weeknum)))[g])
      if_train_10 <- if_train_10_month[if_train_10_month$weeknum == weeknumber, ]
      
      # order volume outlier test #
      if_train_10_names <- data.frame(u_name = unique(if_train_10$ITEMCDQTY))
      if_train_10_names$ord <- seq(1,nrow(if_train_10_names))
      
      if.mall.train.rule <- if_train_10[, c(17, 21, 35)]
      
      colnames(if.mall.train.rule) <- c("incode", "code" , "name" )
      
      if.mall.train.orderuniqu <- data.table(unique(if.mall.train.rule))
      if.mall.train.ordervolume <- if.mall.train.orderuniqu[, list(volume = length(name)), by = list(incode)]
      
      if.mall.train.rule <- join(data.frame(if.mall.train.orderuniqu), data.frame(if.mall.train.ordervolume))
      
      volume_check <- unique(if.mall.train.rule[, c(1, 4)])
      volume_check <- data.frame(table(volume_check$volume))
      volume_check$rate <- (volume_check$Freq/sum(volume_check$Freq))*100
      
      if.mall.train.rule <- if.mall.train.rule[if.mall.train.rule$volume == 1,]
      
      for(k in 1:length(unique(if.mall.train.rule$volume))){
        volume <- unique(if.mall.train.rule$volume)[k]
        if.mall.train <- if.mall.train.rule[if.mall.train.rule$volume == volume,]
        order_volume <- length(unique(if.mall.train$incode))
        
        if.mall.rule.list <- split(if.mall.train$name, if.mall.train$incode)
        
        if.mall.rule.list.trans <- as(if.mall.rule.list, "transactions")
        
        if.mall.rule.rules <- apriori(if.mall.rule.list.trans, parameter = list(support = 0.001, confidence = 0.001, target = "rules"))
        
        if.mall.rule.rules_sort <- sort(if.mall.rule.rules, decreasing = TRUE, by = "confidence")
        
        ifmall <- inspectDT(sort(if.mall.rule.rules_sort, by = "lift"))
        if_rule <- data.frame(ifmall$x$data)
        
        # Remove curly brackets around rules
        if_rule$LHS <- gsub("[:{}:]", "", if_rule$LHS)
        if_rule$RHS <- gsub("[:{}:]", "", if_rule$RHS)
        
        if_rule$rule <- paste(if_rule$LHS, if_rule$RHS, sep=',')
        for(l in 1:nrow(if_rule)){
          if_rule$rule[l] <- apply(data.frame(rules = as.character(sort(unlist(strsplit(if_rule$rule[l], ","))))), 2, paste, collapse = ",")
        }
        if_rule <- data.frame(data.table(if_rule)[, list( support = mean(support), confidence = mean(confidence), lift = mean(lift)), by = list(rule)])
        if_rule$year <- year
        if_rule$month <- month
        if_rule$weeknumber <- weeknumber
        if_rule$all_order_volume <- order_volume
        if_rule$text_volume <- volume
        if_rule$order_volume <- round(if_rule$support * order_volume)
        if_rule <- if_rule[if_rule$order_volume >= 1, ] 
        
        write.csv(if_rule, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/bigevent/if_rule_bigevent_", year, "_",month, "_", weeknumber, "_", unique(if.mall.train.rule$volume)[k], ".csv", sep=''), row.names = FALSE)
      }
    } 
  }
}

# auto week relus data join code ####

file.path <- 'D:/Final_IF/store/pro10/if_month_week_rules/bigevent'

rules.list <- list.files(path = file.path, all.files = T, no.. = T)
file.list <- paste(file.path, rules.list, sep = '/')

data = list()
for (n in 1:length(rules.list)) {
  data[[n]] = data.frame(fread(file.list[n], header = T, sep = ','))}

# rbind
rules.list.merge <- data.frame(rbindlist(data))
rules.list.merge$rule <- gsub(',','', rules.list.merge$rule)

a <- str_split(rules.list.merge$rule, ',')
rules.list.merge$rules_volume <- unlist(lapply(a, length))

write.csv(rules.list.merge, file = paste("D:/Final_IF/store/pro10/if_month_week_rules/month_week_bigevent_rules_merge.csv", sep=''), row.names = FALSE)

