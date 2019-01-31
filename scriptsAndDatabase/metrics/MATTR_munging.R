# MATTR work
library(dplyr)
big_boy <- read.csv('starts.csv', stringsAsFactors = FALSE)

python_list <- paste0(big_boy$titles_vec, collapse=",")
python_names <- paste0("'",big_boy$titles_vec, "'", collapse=",")

mattr <- read.csv('Python_Scripts/MATTR/mattr_t.csv', stringsAsFactors = FALSE)
#df.new <- read.csv('starts.csv', stringsAsFactors = FALSE)

colnames(mattr) <- c('heartOfDarkness','theRoad','theGreatGatsby','womenInLove','portraitOfTheArtist','lolita','theRainbow','mobyDick','theSerpent','pym','underTheVolcano','orlando','toTheLighthouse','eureka','paleFire','billyBudd','theSoundAndTheFury','thePedersenKid','theAngelOfTerror','lifeAndTimesOfMichaelK','absalomAbsalom','bloodMeridian','mrsDalloway','somethingHappened','theMoonstone','theSecretAdversary','theScarhavenKeep','theRedThumbMark','theParadiseMystery','theRaynerSladeAmalgamation','theLeavenworthCase','theOldManInTheCorner','theMoonRock','theHandInTheDark','theDaffodilMystery','theInnocenceOfFatherBrown','theBrandOfSilence','theCircularStaircase','theAshielMystery','theMysteryOfRoom75','theLadyInBlue','theMaxwellMystery','aStudyInScarlet','theBigSleep','theShriekingPit','thePictureOfDorianGray','theSignOfFour','wideSargassoSea','gravitysRainbow','theSpiralStaircase')
colnames(mattr) <- big_boy$titles_vec
# order it

#indexes <- c(6, )
mattr_dattr<- mattr %>% summary(funs(summary))

mattr_data<- as.matrix.data.frame(mattr_dattr, stringsAsFactors=FALSE)
mattr_data <- as.numeric(substr(mattr_dattr[3,], 9, 13))
cbind(big_boy$titles_vec, mattr_data)
median(mattr_data[1:26])
median(mattr_data[27:50])

big_boy <- as.data.frame(cbind(big_boy, mattr_data), stringsAsFactors = FALSE)
write.csv(big_boy,'starts.csv')








