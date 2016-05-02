rm(list = ls())

train <- read_csv("C:/Users/amulya/Desktop/train_amazon.csv")

test <- read_csv("C:/Users/amulya/Desktop/test_amazon.csv")

train$id <- 100000+c(1:dim(train)[1])

test$ACTION <- -1

tmp <- rbind(train, test) 

tmp$ACTION0 <- tmp$ACTION

tmp$ACTION <- (1-tmp$ACTION0)

tmp$y <- tmp$ACTION

tmp$y[tmp$y > 1] <- 0

tmp$ws <- 1

tmp$ws[tmp$ACTION0 < 0] <- 0

tmp$dummy <- 'A'

tmp$split1 <- 0

tmp$split1[tmp$ACTION0 < 0] <- 2

#convert variables into sequential ids

tmp$res_f <- factor(tmp$RESOURCE)

tmp$res_f <- as.integer(tmp$res_f)

tmp$mgr_f<-factor(tmp$MGR_ID)

tmp$mgr_f_id<-as.integer(tmp$mgr_f)

tmp$rocd_f<-factor(tmp$ROLE_CODE)

tmp$rocd_f_id<-as.integer(tmp$rocd_f)

tmp$rr1_f<-factor(tmp$ROLE_ROLLUP_1)

tmp$rr1_f_id<-as.integer(tmp$rr1_f)

tmp$rr2_f<-factor(tmp$ROLE_ROLLUP_2)

tmp$rr2_f_id<-as.integer(tmp$rr2_f)

tmp$rt_f<-factor(tmp$ROLE_TITLE)

tmp$rt_f_id<-as.integer(tmp$rt_f)

tmp$rf_f<-factor(tmp$ROLE_FAMILY)

tmp$rf_f_id<-as.integer(tmp$rf_f)

tmp$rd_f<-factor(tmp$ROLE_DEPTNAME)

tmp$rd_f_id<-as.integer(tmp$rd_f)

tmp$rfd_f<-factor(tmp$ROLE_FAMILY_DESC)

tmp$rfd_f_id<-as.integer(tmp$rfd_f)









