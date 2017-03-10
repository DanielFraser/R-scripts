#rm(list = ls()) - clears all variables
HAPPINESS2017["Region"] <- NA

#seperate country by region
Asia = "Afghanistan|Bahrain|Bangladesh|Cambodia|China|India|Indonesia|Iran|Iraq|Israel|Japan|Jordan|Kazakhstan|South Korea|
        Kuwait|Kyrgyzstan|Lebanon|Malaysia|Mongolia|Nepal|Oman|Pakistan|Philippines|Qatar|Russia|Saudi Arabia|Singapore|Sri Lanka|
        Syria|Tajikistan|Thailand|Turkey|Turkmenistan|United Arab Emirates|Uzbekistan|Vietnam|Yemen|Hong Kong|Palestinian Territories|
        Kuwait|Laos|Taiwan|Syria|Myanmar|Kuwait"

Africa = "Algeria|Angola|Benin|Botswana|Burkina Faso|Burundi|Cameroon|Central African Republic|Chad|Comoros|^Congo|Djibouti|
          Egypt|Ethiopia|Gabon|Ghana|Guinea|Kenya|Lesotho|Liberia|Libya|Madagascar|Malawi|Mali|Mauritania|Mauritania|
          Morocco|Mozambique|Niger|Nigeria|Rwand2a4|Senegal|South Africa|Sudan|Swaziland|Tanzania|Togo|Tunisia|Uganda|Zambia|
          Zimbabwe|Mauritius|Senegal|Sierra Leone|Egypt|Zimbabwe|Morocco"

SouthAmerica = "Argentina|Bolivia|Brazil|Chile|Colombia|Ecuador|Paraguay|Peru|Uruguay|Venezuela|Suriname" 

Europe = "Albania|Armenia|Austria|Azerbaijan|Belarus|Belgium|Bosnia and Herzegovina|Bulgaria|Croatia|Cyprus|Czech Republic|
          Denmark|Estonia|Finland|France|Georgia|Germany|Greece|Hungary|Iceland|Ireland|Italy|Latvia|Lithuania|Luxembourg|
          Macedonia|Malta|Moldova|Montenegro|Netherlands|Norway|Poland|Portugal|Serbia|Slovakia|Slovenia|Spain|Switzerland|
          Sweden|Ukraine|United Kingdom|Denmark|Kosovo|Macedonia|sweden"

NorthAmerica = "Canada|United States|Mexico|Panama|Costa rica|El Salvador|Guatemala|Haiti|Honduras|Jamaica|Nicaragua|
            Trinidad and Tobago|Dominican Republic|Trinidad and Tobago"

Oceania = "Australia|New Zealand"

AsiaSet = subset(HAPPINESS2017,grepl(Asia,COUNTRY))
AfricaSet= subset(HAPPINESS2017,grepl(Africa,COUNTRY))
EuropeSet= subset(HAPPINESS2017,grepl(Europe,COUNTRY))
NAmSet = subset(HAPPINESS2017,grepl(NorthAmerica,COUNTRY))
SAmSet = subset(HAPPINESS2017,grepl(SouthAmerica,COUNTRY))
OceanSet = subset(HAPPINESS2017,grepl(Oceania,COUNTRY)) 
summary(AsiaSet$HAPPINESS)
summary(AfricaSet$HAPPINESS)
summary(NAmSet$HAPPINESS)
summary(SAmSet$HAPPINESS)
summary(OceanSet$HAPPINESS)
#----------------------------------------------------------

#happieness by region, status, and gender
boxplotG <- function()
{
  colors = rainbow(6)
  boxplotF(AsiaSet, "Asia", colors[1])
  boxplotF(AfricaSet, "Africa",colors[2])
  boxplotF(EuropeSet, "Europe",colors[3])
  boxplotF(NAmSet, "North America",colors[4])
  boxplotF(SAmSet, "South America",colors[5])
  boxplotF(OceanSet, "Oceania",colors[6])
  
}
boxplotF <- function(SetC, s, col1)
{
  par(mfrow=c(1,2), oma = c(0,0,2,0))
  tempImmigrant = subset(SetC, IMMIGRANT == 0)
  tempNon = subset(SetC, IMMIGRANT == 1)
  boxplotE(tempImmigrant, "Native",col1)
  boxplotE(tempNon, "Immigrant",col1)
  mtext(paste("Happiness vs Gender in", s), outer = TRUE, cex = 1.5)
}
boxplotE <- function(setC, s, colors1)
{
  boxplot(setC$HAPPINESS ~ setC$GENDER, main = s, 
          ylab = "Happiness", xlab = "Gender", col = colors1)
}
summary(subset(HAPPINESS2017, grepl("Male",GENDER))$HAPPINESS)
boxplot(OceanSet$HAPPINESS ~ OceanSet$GENDER)
#------------------------------------------------------------------

#income vs happiness by gender
male = subset(HAPPINESS2017, grepl("Male",GENDER))
female = subset(HAPPINESS2017, grepl("Female",GENDER))
plot(male$INCOME ~ male$HAPPINESS, main = "Happiness vs Income (Male)", xlab = "Happiness", ylab = "Income", col = "Blue")
plot(female$INCOME ~ female$HAPPINESS, main = "Happiness vs Income (Female)", xlab = "Happiness", ylab = "Income",col = "Red")
#------------------------------------------------------------------

#Income Vs Happiness vs Age vs IDN
library(gclus)
temp <- HAPPINESS2017[c(1,2,6,7)]
temp.r = abs(cor(temp))
temp.col <- dmat.color(temp.r)
temp.o <- order.single(temp.r)
cpairs(temp, temp.o, panel.colors=temp.col, gap=.5, main = "Income Vs Happiness vs Age vs IDN")
#------------------------------------------------------------------
