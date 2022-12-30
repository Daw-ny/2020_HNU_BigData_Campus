### 패키지 설치
# install.packages("Rtsne")
# install.packages('plotly')
# install.packages("vcd")
library(vcd)
library(knitr)
library(dplyr)
library(stringr)
library(cluster)
library(Rtsne)
library(ggplot2)
library(plotly)

### datause.csv
loadfile <- file.choose()
datause <- read.csv(loadfile,header=T)

### Factor요소로 바꾸기
datause$yn <- datause$yn %>% as.factor()
datause$hosun <- datause$hosun %>% as.factor()
datause %>% str()

### 구분할 수 있는 행 이름으로 바꾸기
rownames(datause)<-paste0(datause$hosun,datause$name)
rn <- rownames(datause)

#####################################################
### 클러스터링
gower_distance <- daisy(datause[,c(1,3:4,7:8)],metric=c("gower"))
class(gower_distance) # gower방법을 통해 각 변수들간의 거리 생성
datause %>% str
### Agglomerative Hierachical Clustering 알고리즘
agg_clust_c <- hclust(gower_distance,method="complete")
plot(agg_clust_c,main="Agglomerative, complete linkages")

### Divisive Hierachical Clustering 알고리즘
divisive_clust <- diana(as.matrix(gower_distance), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive_clust, main = "Divisive")

### 클러스터의 수 결정
sil_width <- c(NA) # 빈공간 생성
for(i in 2:15) {
  pam_fit <- pam(gower_distance, diss = TRUE, k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
} # 클러스터의 수 2~15개로 나누었을 때의 결과 합치기
plot(1:15, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:15, sil_width) # 결과를 그래프로 시각화

######################################################
### Summary
set.seed(123)
k <-  which.max(sil_width) # 값이 가장 큰 집단의 수 채택
pam_fit <- pam(gower_distance, diss = TRUE, k) # pam함수를 통해 객체 분류 
pam_results <- datause %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.)) # 각 결과를 요약해서 출력

pam_results$the_summary # 요약된 것

### T-sne 방법을 통한 군집 묶기
tsne_obj <- Rtsne(gower_distance, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering)) # 데이터 재 추출
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) # 그래프로 데이터 표현

######################################################
### Summary
set.seed(123)
k <- 5 # 값이 가장 큰 집단의 수 채택
pam_fit <- pam(gower_distance, diss = TRUE, k) # pam함수를 통해 객체 분류 
pam_results <- datause %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.)) # 각 결과를 요약해서 출력

pam_results$the_summary # 요약된 것

### T-sne 방법을 통한 군집 묶기
tsne_obj <- Rtsne(gower_distance, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering)) # 데이터 재 추출
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster)) # 그래프로 데이터 표현

######################################################
### 클러스터링 집단 결과를 추출
clust <- pam_fit$clustering %>% as.vector()
dataresult <- datause %>% cbind(clust)
dr <- dataresult %>% group_by(clust) %>%
  summarise(O=length(which(yn==1)),X=length(which(yn==0)))
dr$view <- ifelse(dr$O>dr$X,T,F)
view <- which(dr$view==T)
view

### 집단 선정 표시
group <- dataresult[,c(1,7,14)]
group %>% str()
group$clust <- group$clust %>% as.factor()
group$yn2 <- ifelse(group$yn==1,"공실25%이상","공실25%미만")
ggplot(group) +
  geom_bar(aes(x=clust,fill=yn2))

### 공실이 많은 역 표시
clu <- data.frame()
for(k in view){
  temp <- dataresult %>% filter(clust==k)
  clu <- rbind(clu,temp)
}
clu$clust <- clu$clust %>% as.factor()

clu <- clu %>% arrange(역번호)
dataresult <- dataresult %>% arrange(역번호)

### 지도에 역 위치 띄우기
fig <- plot_ly(clu,lat = ~lat,lon = ~lon,
               type="scattermapbox",
               mode="markers",
               marker=list(size=9)) %>%
  add_trace(data=dataresult,lat=~lat,lon=~lon,split=~hosun,mode='lines',marker=list(size=5))
fig <- fig %>%
  layout(
    mapbox = list(
      style = "carto-positron",
      center = list(lat=37.55,lon=126.97),
      zoom=10
    )
  )
fig

####################################################
### 필요한 결과 도출
## 공실이 더 많은 집단 확인
view

## 호선별로 공실이 많은 집단
empty <- clu %>% group_by(hosun) %>%
  summarise(count=n())

all <- dataresult %>% group_by(hosun) %>%
  summarise(count=n())

res <- empty$count / all$count
result <- data.frame(all$hosun,res)
plot(result$res,xlab="호선",ylab="공실비율",main="호선별 공실비율율")
