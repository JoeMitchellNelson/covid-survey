require(pacman)
p_load(MASS,evd)

# number of "survey invitations" within each iteration
n = 1000

# 2 utility functions
u1 <- function (type,a,c,e1,statquo) {
  ifelse(type=="I",
         2*a - 1*c + e1 + statquo, # utility function for type I agents
         1*a - 1*c + e1 + statquo) # utility function for type II agents
}




# initialize df for results of simulation
dat <- data.frame(everyone=rep(NA,1000),respondersonly=NA,fixed=NA)


for (i in 1:1000) {
  
  sim <- data.frame(type = rep(c("I","II"),each=2),  # agents are one of two types, with different utilty functions
                    choiceid=rep(1:n,each=2),        # choice index
                    altid=1:(2*n),                   # alternative index
                    a=rnorm(2*n,5,1),                # a is level of some desirable alternative
                    c=rnorm(2*n,5,1),                # c is cost
                    e1=rgumbel(2*n,0,1),             # e1 is type 1 extreme value error for logit estimation
                    e2=rep(rnorm(n,0,1),each=2),     # e2 is normal error for rp estimation
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
  sim$z <- ifelse(sim$type=="I",sim$z+0.5,sim$z-0.5)
  
  # probability of response
  sim$rp <- pnorm(sim$z + sim$e2)
  
  
  # binomial draw to determine if each person actually responds to survey
  sim$respond <- NA
  for (j in 1:nrow(sim)) {
    sim$respond[j] <- rbinom(1,1,sim$rp[j])
  }
  
  sim <- sim %>% group_by(choiceid) %>% mutate(respond=first(respond)) %>% ungroup()
  
  # estimate WTP for attribute "a" if we could observe everyone (benchmark case)
  res <- clogit(best ~ a + c  + statquo + strata(choiceid),data=sim)
  t1 <- -1*res$coefficients[["a"]]/res$coefficients[["c"]]
  
  # estimate WTP using only responders (should be higher than true on average)
  res2 <- clogit(best ~ a + c + statquo + strata(choiceid),data=sim[which(sim$respond==1),])
  t2 <- -1*res2$coefficients[["a"]]/res2$coefficients[["c"]]
  
  # run first stage for selection correction
  sim2 <- sim %>% dplyr::select(choiceid,respond,z) %>% unique()
  fix <- glm(respond ~ z, data = sim2, family = binomial(link="logit"))
  sim2$fitted <- predict(fix)
  sim2$fitted <- sim2$fitted - mean(sim2$fitted)
  
  sim <- left_join(sim,sim2)
  
  # selection corrected WTP
  res3 <- clogit(best ~ a + c + statquo + a:fitted + c:fitted + statquo:fitted + strata(choiceid),data=sim[which(sim$respond==1),])
  t3 <- -1*res3$coefficients[["a"]]/res3$coefficients[["c"]]
  
  # store results in a dataframe 
  dat$everyone[i] <- t1
  dat$respondersonly[i] <- t2
  dat$fixed[i] <- t3
  
}


ggplot(dat) + 
  geom_density(aes(x=everyone),fill="green",alpha=.5) + 
  geom_density(aes(x=respondersonly),fill="blue",alpha=0.5) +
  geom_density(aes(x=fixed),fill="red",alpha=0.5)


ggplot(dat) + 
  geom_density(aes(x=everyone),fill="green",alpha=.5) + 
  geom_density(aes(x=respondersonly),fill="blue",alpha=0.5) +
  geom_density(aes(x=fixed),fill="red",alpha=0.5)

dat <- dat %>% mutate(uncorrected = respondersonly - everyone)
dat <- dat %>% mutate(corrected = fixed - everyone)


ggplot(dat) +
  geom_density(aes(x=uncorrected),color="blue") +
  geom_density(aes(x=corrected),color="red") +
  geom_vline(xintercept=0,linetype="dashed",color="grey50",alpha=0.5)


summary(dat$corrected)
summary(dat$uncorrected)
sd(dat$corrected)/sqrt(500)
sd(dat$uncorrected)/sqrt(500)
