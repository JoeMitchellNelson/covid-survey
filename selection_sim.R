require(pacman)
p_load(MASS,evd,tidyverse,survival,sampleSelection)


set.seed(301541)

# number of "survey invitations" within each iteration
n = 5000
m = 5000
# 2 utility functions
u1 <- function (type,a,c,e1,statquo) {
  ifelse(type=="I",
         1*a - 1*c + e1 - 1*statquo, # utility function for type I agents
         1*a - 1*c + e1 - 1*statquo) # utility function for type II agents
}




# initialize df for results of simulation
dat <- data.frame(everyone=rep(NA,m),respondersonly=NA,fixed=NA)


for (i in 1:m) {
  
  sim <- data.frame(type = rep(c("I","II"),each=2),  # agents are one of two types, with different utility functions
                    choiceid=rep(1:n,each=2),        # choice index
                    altid=1:(2*n),                   # alternative index
                    a=rnorm(2*n,5,1),                # a is level of some desirable alternative
                    c=rnorm(2*n,5,1),                # c is cost
                    e1=rgumbel(2*n,-0.57721,1),             # e1 is type 1 extreme value error for logit estimation
                    e2=rep(rlogis(n,0,sqrt(3)/pi),each=2),     # e2 is logistic error for rp estimation
                    z=rep(rnorm(n,0,1),each=2))      # z is an observable variable correlated with rp (and maybe also type)
  
  # force one option in each pair to be "status quo"
  # with a=0, c=0 and statquo=1
  sim$a <- ifelse(sim$altid %% 2 == 0,0,sim$a)
  sim$c <- ifelse(sim$altid %% 2 == 0,0,sim$c)
  sim$statquo <- ifelse(sim$altid %% 2 == 0,1,0)
  
  # calculate utility for each alternative
  sim <- sim %>% mutate(u = u1(type,a,c,e1,statquo))
  
  # agent selections whichever alt gives more utility
  sim <- sim %>% group_by(choiceid) %>% mutate(best = ifelse(u==max(u),1,0)) %>% ungroup()
  
  
  # RP correlated with type?
  sim$z <- ifelse(sim$type=="I",sim$z+1,sim$z-1)
  
  # probability of response
  sim$rp <- pnorm(sim$z + sim$e2)
  
  sim$respond <- rbinom(nrow(sim),1,sim$rp)
  
  sim <- sim %>% group_by(choiceid) %>% mutate(respond=first(respond)) %>% ungroup()
  
  # estimate WTP for attribute "a" if we could observe everyone (benchmark case)
  res <- clogit(best ~ a + c  + statquo + strata(choiceid),data=sim[which(sim$choiceid %in% sample(max(sim$choiceid),sum(sim$respond)/2)),])
  t1 <- -1*res$coefficients[["a"]]/res$coefficients[["c"]]
  
  # estimate WTP using only responders (should be higher than true on average)
  res2 <- clogit(best ~ a + c + statquo + strata(choiceid),data=sim[which(sim$respond==1),])
  t2 <- -1*res2$coefficients[["a"]]/res2$coefficients[["c"]]
  
  # run first stage for selection correction
  sim2 <- sim %>% dplyr::select(choiceid,respond,z) %>% unique()
  fix <- glm(respond ~ z, data = sim2, family = binomial(link="logit"))
  sim2$fitted <- predict(fix)
  sim2$fitted <- sim2$fitted - mean(sim2$fitted)
  
  sim <- left_join(sim,sim2,by = c("choiceid", "z", "respond"))
  
  # selection corrected WTP
  res3 <- clogit(best ~ a*fitted + c*fitted + statquo*fitted + strata(choiceid),data=sim[which(sim$respond==1),])
  t3 <- -1*res3$coefficients[["a"]]/res3$coefficients[["c"]]

 # clogit(best ~ a + c + statquo + strata(choiceid), data=sim[which(sim$type=="I"),])
  
  # store results in a dataframe 
  dat$everyone[i] <- t1
  dat$respondersonly[i] <- t2
  dat$fixed[i] <- t3
  dat$proptype1[i] <- sum(sim$respond=="1" & sim$type=="I")/sum(sim$respond)

  
}

mean(dat$everyone,na.rm=T)
mean(dat$respondersonly,na.rm=T)
mean(dat$fixed,na.rm=T)

ggplot(dat) + 
  geom_density(aes(x=everyone),fill="green",alpha=.5) + 
  geom_density(aes(x=respondersonly),fill="blue",alpha=0.5) +
  geom_density(aes(x=fixed),fill="red",alpha=0.5) +
  geom_vline(xintercept = mean(dat$everyone),color="green") +
  geom_vline(xintercept = mean(dat$respondersonly),color="blue") +
  geom_vline(xintercept = mean(dat$fixed),color="red") 


dat <- dat %>% mutate(uncorrected = respondersonly - everyone)
dat <- dat %>% mutate(corrected = fixed - everyone)


ggplot(dat) +
  geom_density(aes(x=uncorrected),color="blue") +
  geom_density(aes(x=corrected),color="red") +
  geom_vline(xintercept=0,linetype="dashed",color="grey50",alpha=0.5)



mean(dat$corrected,na.rm=T)
mean(dat$uncorrected,na.rm=T)
sd(dat$corrected,na.rm=T)/sqrt(m)
sd(dat$uncorrected,na.rm=T)/sqrt(m)
1 - abs(mean(dat$corrected,na.rm=T)/mean(dat$uncorrected,na.rm=T))

sum(abs(dat$corrected)<abs(dat$uncorrected),na.rm=T)/m


########## reconfigure for probit ###############

n = 10000000

sim2 <- data.frame(type=rep(c("I","II"),n),
                   a = rnorm(n,5,1),
                   c = rnorm(n,5,1),
                   e1= rnorm(n,0,1),
                   e2= rnorm(n,0,1),
                   z = rnorm(n,0,1),
                   statquo = -1)

sim2 <- sim2 %>% mutate(u = u1(type,a,c,e1,statquo))
sim2$best <- ifelse(sim2$u > 0,1,0)
sim2$z <- ifelse(sim2$type == "I",sim2$z+1,sim2$z-1)
sim2$rp <- pnorm(sim2$z + sim2$e2)
sim2$respond <- rbinom(nrow(sim2),1,sim2$rp)
head(sim2)
summary(sim2)

fix <- glm(respond ~ z, family = binomial(link = "probit"), 
    data = sim2)
sim2$fitted <- predict(fix)
sim2$fitted <- sim2$fitted - mean(sim2$fitted)

ggplot(sim2) +
  geom_density(aes(x=fitted,group=type,color=type))

summary(p1 <- glm(best ~ a + c + statquo + 0,family = binomial(link = "probit"), 
            data = sim2))

summary(p2 <- glm(best ~ a + c + statquo + 0,family = binomial(link = "probit"), 
            data = sim2[which(sim2$respond==1),]))

summary(p3 <- glm(best ~ a:fitted + c:fitted + statquo:fitted + a + c + statquo + 0,family = binomial(link = "probit"), 
            data = sim2[which(sim2$respond==1),]))

-1 * p1$coefficients[["a"]]/p1$coefficients[["c"]]
-1 * p2$coefficients[["a"]]/p2$coefficients[["c"]]
-1 * p3$coefficients[["a"]]/p3$coefficients[["c"]]
