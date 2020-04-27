# functional programming
failrate<- function(df,col1,col2){
  t <- as.data.frame.matrix(table(df[[col1]],df[[col2]]))
  t$var <- rownames(t)
  rownames(t) <- c()
  colnames(t) <- c('no_fail','fail','var')
  t <- t %>% mutate(fp = round((t$fail/(t$fail + t$no_fail))*100,2)) %>% 
    arrange(desc(fp))
  return(t)
}