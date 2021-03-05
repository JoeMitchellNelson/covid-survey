makedum <- function (a,dummies,stem="") {

b <- a[,which(names(a) %in% dummies)]

check <- rowSums(b) %in% c(0,1,NA)

if (sum(check==F)) {cat("something is wrong")}

c <- rep("",nrow(b))

for (j in 1:ncol(b)) {

for (i in 1:nrow(b)) {
  c[i] <- paste0(c[i],rep(names(b)[j],b[i,j]))
}

}

c <- str_remove_all(c,paste0("^",stem))

c <- str_replace_all(c,"^$","MISSING")

c
}


stem2vars <- function (a,stem) {
  names(a)[which(str_detect(names(a),stem))]
}

