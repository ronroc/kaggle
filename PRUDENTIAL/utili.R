require(sqldf)

my.f2cnt <- function(th2, vn1, vn2, filter=TRUE) {
  
  df <- data.frame(f1=th2[,vn1], f2=th2[,vn2], filter=filter)
  
  sum1 <- sqldf("select f1, f2, count(*) as cnt 
                
                from df 
                
                where filter=1 
                
                group by 1,2")
  
  tmp <- sqldf("select b.cnt 
               
               from df a left join sum1 b 
               
               on a.f1=b.f1 and a.f2=b.f2")
  
  tmp$cnt[is.na(tmp$cnt)] <- 0
  
  return(tmp$cnt)
  
}


#three way count


my.f3cnt<-function(th2, vn1, vn2, vn3, filter=TRUE) {
  
  df<-data.frame(f1=th2[,vn1], f2=th2[,vn2], f3=th2[, vn3], filter=filter)
  
  sum1<-sqldf("select f1, f2, f3, count(*) as cnt 
              
              from df 
              
              where filter=1 
              
              group by 1,2, 3")
  
  tmp<-sqldf("select b.cnt 
             
             from df a left join sum1 b 
             
             on a.f1=b.f1 and a.f2=b.f2 and a.f3=b.f3")
  
  tmp$cnt[is.na(tmp$cnt)]<-0
  
  return(tmp$cnt)
  
}
