mod = glm(as.factor(am)~hp+disp,data=mtcars,family='binomial')
head(mtcars)
round(predict(mod,head(mtcars),type='response'),3)

0.12170 *110
-0.09518*160


1/(
  1+ exp(abs(1.40342+ 13.387+ -15.2288))
)


sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}


