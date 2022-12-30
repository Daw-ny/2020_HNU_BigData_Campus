# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("tidyverse")
# install.packages("ggmap")
library(tidyverse)
library(ggmap)
library(dplyr)
library(reshape2)
library(stringr)

###서울교통공사_임대상가정보_20200601.csv
datacho <- file.choose()
imdae <- read.csv(datacho,header=T)

### 필요없는 열 제거
imdae <- imdae[,c(-1,-8)]

### 변수 영문으로 바꾸기
colnames(imdae)<- c("buildtype","hosun","name","number",
                          "square","opentype","startdate","enddate","borrow")

### 변수확인
imdae %>% str()
imdae %>% summary()
### 매출액에서 콤마 제거
imdae$borrow <- gsub(",","",imdae$borrow)

### 평당 매출액 환산
imdae$borrow <- imdae$borrow %>% as.numeric()
#imdae$borrow[which(is.na(imdae$borrow))] <- 0
imdae$square[which(is.na(imdae$square))] <- 1
imdae <- imdae %>% mutate(price = borrow/square)

### 데이터 확인
imdae %>% head()

### 공실률 구하기
imdae$n <- 0
imdae$n[which(imdae$buildtype != "공실" & imdae$buildtype != "GS")] <- 1
imdae %>% str()

mean(imdae$price, na.rm=T)

################################################
### 면적 33 더미변수 저장
imdae$size <- 1
imdae$size[which(imdae$square<33)] <- 0
imdae %>% head()

### 필요한 변수만 뽑는 과정
data <- imdae %>% group_by(hosun,name) %>%
  summarise(avgprice = mean(price, na.rm=T),gongsil=round(1-(sum(n)/n()),3),
            size_under=length(which(size==0)),size_over=length(which(size==1)))
data <- data %>% as.data.frame()

data <- data[-which(is.na(data$avgprice)), ]
data %>% summary()



### 공실률 데이터 0.5를 기준으로 구분
data$yn <- 1
data$yn[which(data$gongsil<0.25)] <- 0
data %>% head()

### 데이터 융합을 위한 이름 통일
locater <-str_locate(data$name,"\\(")
num <- locater[,1]
num[which(is.na(num))] <- str_length(data$name[which(is.na(num))])
num
a=1
for(i in num){
  data$name[a] <- substr(data$name[a],1,(i-1))
  a=a+1
}
data$name

###############################################################
### 서울교통공사_1_8호선일별역별시간대별승하차인원_202001_202005.csv
meand <- file.choose()
sh <- read.csv(meand,header=T)
sh %>% head()
sh <- sh[,c(1,2,3,4,5,26)]

### 이름 통일
locater2 <-str_locate(sh$역명,"\\(")
num2 <- locater2[,1]
num2[which(is.na(num2))] <- str_length(sh$역명[which(is.na(num2))])+1
num2
b=1
for(i in num2){
  sh$역명[b] <- substr(sh$역명[b],1,(i-1))
  b=b+1
}

### 필요한 변수만 선정하여 선출
sh_use <- sh %>% group_by(호선,역번호,역명,구분) %>%
 summarise(mean = mean(합.계))
sh_use <- sh_use %>% as.data.frame()
sh_use %>% str()

### 데이터 형식 재 가공
sh_made <- dcast(sh_use,호선+역번호+역명 ~ 구분)
sh_made %>% head()
sh_made %>% str()

########################################################
### 다르게 표시 된 역명 수정
sh_made$역명[which(sh_made$역명=="서울역")] = "서울" 
sh_made$역명[which(sh_made$역명=="구로디지털단지")] = "구로디지털"

########################################################
### subway.csv
sub <- file.choose()
subdata <- read.csv(sub,header=T)

### 데이터 합치기
usedata <- data %>% left_join(sh_made,by=c(hosun="호선",name="역명"))
usedata2 <- usedata %>% left_join(subdata,by=c(hosun="line",name="station"))
usedata2 %>% View()

### 데이터 저장
write.csv(usedata2,file="datause.csv",row.names = F)
