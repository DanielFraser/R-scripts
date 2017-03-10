#--------------------------------------------------------------
PTest <- function(df, c1, c2, e1, e2)
{
  p = PermutationTestSecond::Permutation(df, c1, c2, 10000, e1, e2)
  p2 = Ztest(df, c1, c2, e1, e2)
  cat(paste('Permutation test: P =',p, '\nZ-test:',p2))
}

Ztest <- function(df, c1, c2, e1, e2)
{
  #data clean and subset, either
  e1.data <- subset(df, grepl(e1,df[[c1]]))
  e2.data <- subset(df, grepl(e2,df[[c1]]))
  #------------
  e1.num <- e1.data[[c2]]
  e2.num <- e2.data[[c2]]
  #----------------------------------------
  sd.e1 <- sd(e1.num)
  sd.e2 <- sd(e2.num)
  # means of two samples
  mean.e1 <- mean(e1.num)
  mean.e2 <- mean(e2.num)
  #-----------------------------
  len_e1 <- length(e1.num)
  len_e2 <- length(e2.num)
  #standard deviation of difference population
  sd.e1.e2 <- sqrt(sd.e1^2/len_e1 + sd.e2^2/len_e2)
  #z score
  zeta <- (mean.e1 - mean.e2)/sd.e1.e2
  return (1 - pnorm(zeta))
}
PTest(CENSUSNEW, "ELEMENTS", "CAPITALGAINS", "Fire", "Metal")
PTest(CENSUSNEW, "ELEMENTS","CAPITALLOSS", "Wood", "Metal")

