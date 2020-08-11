library('foreign')
library('plm')
library('ggplot2')


sample <- read.dta("car_fatalities.dta")
summary(sample)
p.sample <- pdata.frame(sample,index = c("state","year"))
summary(p.sample)

######### Line graph for perinc and unemployment ##################
agg <- aggregate(sample, by=list(sample$year) ,FUN = mean)

p1 <- ggplot(data=agg, aes(x=Group.1, y=unrate     , group=1)) +
  geom_line(linetype='dashed', color="red")+ xlab("Years") + ylab("Unemployment rate")+
  geom_point()
p2 <- ggplot(data=agg, aes(x=Group.1, y=perinc     , group=1)) +
  geom_line(linetype='dashed', color="red")+ xlab("Years") + ylab("Per Capita income")+
  geom_point()

plot_grid(p1,p2)

#############Sctter plot religion############             
ggplot(sample, aes(x=sobapt, y=mormon)) +
  geom_point(size=2, shape=20) + geom_smooth(method=lm)

############Bar plot########################
c <- sample %>% select(state,comserd,mraidall)
c <- na.omit(c,cols=c(state,comserd,mraidall))
b <- c %>% group_by(state,comserd) %>%
  summarise(
    avgvrf = mean(mraidall))

ggplot(data=b, aes(b$state, b$avgvrf)) + geom_bar(aes(fill = comserd),
                                                  position = "dodge", stat="identity")+xlab("State") + ylab("Average fatality rate")


e <- sample %>% select(state,jaild,mraidall)
e <- na.omit(c,cols=c(state,jail,mraidall))
class(c$mrall)
d <- e %>% group_by(state,jaild) %>%
  summarise(
    avgvrf = mean(mraidall))
ggplot(data=d, aes(d$state, d$avgvrf)) + geom_bar(aes(fill = d$jaild),
                                                  position = "dodge", stat="identity")+xlab("State") + ylab("Average fatality rate")

###########US map for VFR##############

library(maps)
library(usmap)
library(ggplot2)
library(dplyr)

agg1 <- aggregate(sample, by=list(sample$state) ,FUN = mean)

us_states <- map_data("state")
f <- state.name
f <- f[-c(2,11)]
f
States <- agg1$Group.1
Fatality_rate <- agg1$aidall
pdata.df <- data.frame(f,Fatality_rate)
pdata.df$region <- tolower(pdata.df$f) 
us_mapplot <- left_join(us_states, pdata.df)

p <- ggplot(data = us_mapplot,
            aes(x = long, y = lat,
                group = group, fill = Fatality_rate))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

############Descriptive stats######################

library(psych)

final.df <- sample[,c(3,4,5,6,8,10,11,13,14,32,38)]
cbind(p.sample$unrate,p.sample$beertax,p.sample$mormon,p.sample$miles,p.sample$comserd,p.sample$jaild,p.sample$spircons,p.sample$dryp.sample$perinc)
final.df <- colnames(unrate,beertax,mormon,miles,comserd,jaild,spircons,dry,perinc)
final <- final.df[,-c(8,9)]
summary(final.df)

pairs.panels(final,col="red")

###############Model####################

p.sample$jaild[is.na(p.sample$jaild)]=0
p.sample$comserd[is.na(p.sample$comserd)]=0
p.sample$jaild <- as.factor(p.sample$jaild)
p.sample$comserd <- as.factor(p.sample$comserd)
p.sample$mlda <- as.factor(p.sample$mlda)

#######pooled model#################
c <- plm(log(aidall) ~  unrate +  sobapt  + log(miles) 
         + jaild +
           log(spircons) +log(perinc)  ,data = p.sample , model = "pooling")##r2=42%

summary(c)

library(lmtest)
bptest(c) ##### Breusch -pagan test
pbgtest(c) ##### Serial correlation test
coeftest(c, vcov = vcovHC(c, type = "HC0")) ####Robust standard errors

########Fixed effect model########

model1 <- plm(log(aidall) ~  log(perinc)    +sobapt   
              +log(spircons)   ,data = p.sample , model = "within")##r2=42%

summary(model1)

model2 <- plm(log(aidall) ~  log(perinc)    +sobapt   
              +log(spircons) + mlda  ,data = p.sample , model = "within")##r2=42%

summary(model2)

model3 <- plm(log(aidall) ~  unrate    +sobapt  + log(beertax)  
              +log(spircons) +jaild + comserd + mlda    ,data = p.sample , model = "within")##r2=42%

summary(model3)

model4 <- plm(log(aidall) ~  log(perinc)    +sobapt  
              +log(spircons) +jaild + comserd + mlda    ,data = p.sample , model = "within")##r2=42%

summary(model4)

model4r <- plm(log(aidall) ~  log(perinc)    +sobapt  
               +log(spircons) +jaild + comserd + mlda    ,data = p.sample , model = "random")##r2=42%

summary(model4r)

phtest(model4,model4r) #Hausman test

#################The end############################



