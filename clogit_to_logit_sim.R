### clogit with two alternatives to logit simulation ####
### dat is for clogit
### dat2 is for logit, with no sorting by best

# if you get an error on line 8, uncomment the line below and run it first
# install.packages("pacman")

require(pacman)

p_load(evd,stargazer,patchwork)

set.seed(100)

n <- 10001 # must be odd

dat <- data.frame(choice=rep(1:n,2),alt=rep(1:2,n),x1=rnorm(2*n),x2=rnorm(2*n),x3=runif(2*n),e=rgumbel(2*n))
dat <- dat[order(dat$choice,dat$alt),]

# pick true parameters for utility function
beta <- c(1,2,-1) # x3 is "cost"

# calculate utility for each alt. e is unobserved idiosyncratic component.
dat <- dat %>% mutate(u = beta[1]*x1 + beta[2]*x2 + beta[3]*x3 + e)

# person chooses highest utility alternative
dat <- dat %>% group_by(choice) %>% mutate(best = ifelse(u == max(u),1,0))

# run clogit
summary(a <- clogit(best ~ x1 + x2 + x3 + strata(choice),data=dat))

# turn data into logit data
dat2 <- dat %>% group_by(choice) %>% summarise(best=(first(best)-last(best)+1)/2,
                                               x1 = first(x1)-last(x1),
                                               x2 = first(x2)-last(x2),
                                               x3 = first(x3)-last(x3))

# run logit
summary(b <- glm(best ~x1 + x2 + x3, data = dat2, family = "binomial"))

# print pretty side-by-side results
stargazer(a,b,type="text")

#### now do a bunch of different mixes of 0 and 1 outcomes ####
# the for-loop takes a few minutes

resdf <- data.frame(j = (1:1000)*10,mix=NA,wtp1=NA,wtp2=NA)

beta <- c(1,2,-1) # x3 is "cost"

for (i in 1:1000) {
  
  dat <- data.frame(choice=rep(1:n,2),alt=rep(1:2,n),x1=rnorm(2*n),x2=rnorm(2*n),x3=rnorm(2*n),e=rgumbel(2*n))
  dat <- dat[order(dat$choice,dat$alt),]
  
  
  dat <- dat %>% mutate(u = beta[1]*x1 + beta[2]*x2 + beta[3]*x3 + e)
  dat <- dat %>% group_by(choice) %>% mutate(best = ifelse(u == max(u),1,0))
  quiet({
    dat3 <- dat %>% group_by(choice) %>% arrange(-best) %>% summarise(best=(first(best)-last(best)+1)/2,
                                                                      x1 = first(x1)-last(x1),
                                                                      x2 = first(x2)-last(x2),
                                                                      x3 = first(x3)-last(x3))
  })
  
  swap <- resdf$j[i]
  dat4 <- dat3
  dat4[1:swap,3:5] <- -1*dat4[1:swap,3:5]
  dat4$best[1:swap] <- 0
  
  c <- glm(best ~ x1 + x2 + x3, data = dat4, family = "binomial")
  c <- tidy(c)
  
  resdf$mix[i] = swap/nrow(dat4)
  resdf$wtp1[i] <- -c$estimate[2]/c$estimate[4]
  resdf$wtp2[i] <- -c$estimate[3]/c$estimate[4]
}

resdf$sqerr1 <- (resdf$wtp1 - -beta[1]/beta[3])^2
resdf$sqerr2 <- (resdf$wtp2 - -beta[2]/beta[3])^2


# make the graphs

p1 <- ggplot(resdf[3:998,]) +
  geom_point(aes(x=mix,y=wtp2)) +
  labs(x="Mix",y="WTP for x2") +
  geom_hline(yintercept=2,color="red") +
  theme_minimal()

p2 <- ggplot(resdf[3:998,]) +
  geom_point(aes(x=mix,y=wtp1)) +
  labs(x="Mix",y="WTP for x1") +
  geom_hline(yintercept=1,color="red") +
  theme_minimal()

p3 <- ggplot(resdf[3:998,]) +
  geom_smooth(aes(x=mix,y=sqerr1),se=F) +
  labs(x="Mix",y="Sqared error, estimated WTP for x1") +
  theme_minimal()

p4 <- ggplot(resdf[3:998,]) +
  geom_smooth(aes(x=mix,y=sqerr2),se=F) +
  labs(x="Mix",y="Sqared error, estimated WTP for x2") +
  theme_minimal()

# facet the graphs and output (this line uses the patchwork package, which is much better than ggplot's facet functions)

(p2 + p1) / (p3 + p4)
