w <- t(smaller_svm$coefs) %*% smaller_svm$SV

  #calculate slope
slope_1 <- -w[1]/w[2]
slope_1

#calculate intercept
intercept_1 <- smaller_svm$rho/w[2]

#sv's
culled_sv <- sub[smaller_svm$index,]

p <- ggplot(data=sub, aes(x=dialogue_freq,y=consecutive_repeat_freq_vec,color=label2)) + geom_point()+ scale_colour_manual(values=c("red","blue"))
p <- p + geom_point(data=culled_sv,aes(x=dialogue_freq,y=consecutive_repeat_freq_vec),colour="purple",size = 4,alpha=0.5)
p<- p + geom_abline(slope=slope_1,intercept = intercept_1-1/w[2] + 2*intercept_1-1/w[1], linetype="solid")+
  geom_abline(slope=slope_1,intercept = intercept_1-1/w[2], linetype="dashed")
p
