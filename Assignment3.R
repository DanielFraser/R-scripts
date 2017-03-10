Fire <- subset(CENSUSNEW, grepl("Fire",ELEMENTS))
Earth <- subset(CENSUSNEW, grepl("Earth",ELEMENTS))
Metal <- subset(CENSUSNEW, grepl("Metal",ELEMENTS))
Wood <- subset(CENSUSNEW, grepl("Wood",ELEMENTS))
ColorsE = rainbow(4)
CENSUSNEW[CENSUSNEW=="?"] <- NA #wipes out ? and replaces with NA
#-----end of declarations----------------------

#misc plots
boxplot(CENSUSNEW$AGE ~ CENSUSNEW$ELEMENTS, col = ColorsE,xlab = "Elements", ylab = "Age (in years)",
        main = "Ages based on elements")

subset2 <- subset(CENSUSNEW, !grepl("Adm-clerical|NA|Craft-repair|Exec-managerial|Prof-specialty|Sales|
                                    Other-service|Transport-moving",PROFESSION))
mosaicplot(subset2$PROFESSION ~ subset2$ELEMENTS, col = ColorsE,xlab = "Elements", ylab = "Profession",
        main = "Profession based on elements", cex.axis = .5)

boxplot(CENSUSNEW$YEARS ~ CENSUSNEW$ELEMENTS, col = ColorsE,xlab = "Elements", ylab = "Years)",
        main = "Years based on elements")

subset3 <- subset(CENSUSNEW, !grepl("Bachelors|NA|HS-grad|Some-college|10th|Assoc-voc",EDUCATION))
mosaicplot(subset3$EDUCATION ~ subset3$ELEMENTS, col = ColorsE,xlab = "Elements", ylab = "EDUCATION",
           main = "EDUCATION based on elements")

#---------------------end of misc plots--------------------------------

#earth has lowest mean of capitalloss
#Fire has highest mean capitalgains
Elements = c("Fire","Earth","Metal","Wood")#the order we call subsets in

means = c(with(Fire, mean(CAPITALLOSS)),with(Earth, mean(CAPITALLOSS)),
          with(Metal, mean(CAPITALLOSS)),with(Wood, mean(CAPITALLOSS)))

meansG = c(with(Fire, mean(CAPITALGAINS)),with(Earth, mean(CAPITALGAINS)),
           with(Metal, mean(CAPITALGAINS)),with(Wood, mean(CAPITALGAINS)))

bp = barplot(means,names.arg = Elements, col = ColorsE, main = "Capital Losses Based on Elements",
             xlab = "Elements",ylab = "Average capital Loss")
text(x=bp, y=means, labels=round(means,0), pos=3, xpd=NA)

bp1 = barplot(meansG,names.arg = Elements, col = ColorsE, main = "Capital Gains Based on Elements",
              xlab = "Elements",ylab = "Average capital Gain")
text(x=bp1, y=meansG, labels=round(meansG,0), pos=3, xpd=NA)
