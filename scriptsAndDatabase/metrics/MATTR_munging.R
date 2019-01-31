# MATTR work
library(dplyr)
mattr <- read.csv('Python_Scripts/MATTR/mattr_t.csv', stringsAsFactors = FALSE)
df.new <- read.csv('starts.csv', stringsAsFactors = FALSE)


colnames(mattr) <- c('heartOfDarkness','theRoad','theGreatGatsby','womenInLove','portraitOfTheArtist','lolita','theRainbow','mobyDick','theSerpent','pym','underTheVolcano','orlando','toTheLighthouse','eureka','paleFire','billyBudd','theSoundAndTheFury','thePedersenKid','theAngelOfTerror','lifeAndTimesOfMichaelK','absalomAbsalom','bloodMeridian','mrsDalloway','somethingHappened','theMoonstone','theSecretAdversary','theScarhavenKeep','theRedThumbMark','theParadiseMystery','theRaynerSladeAmalgamation','theLeavenworthCase','theOldManInTheCorner','theMoonRock','theHandInTheDark','theDaffodilMystery','theInnocenceOfFatherBrown','theBrandOfSilence','theCircularStaircase','theAshielMystery','theMysteryOfRoom75','theLadyInBlue','theMaxwellMystery','aStudyInScarlet','theBigSleep','theShriekingPit','thePictureOfDorianGray','theSignOfFour','wideSargassoSea','gravitysRainbow','theSpiralStaircase')
colnames(mattr)
# order it


mattr_dattr<- mattr %>% summary(funs(summary))

mattr_dattr$theGreatGatsby

mattr_data<- as.matrix.data.frame(mattr_dattr, stringsAsFactors=FALSE)
df.new = as.numeric(substr(mattr_dattr[3,], 9, 13))
cat(df.new)
df.sorted <- c(
df.new[6],
df.new[20],
df.new[16],
df.new[26],
df.new[13],
df.new[8],
df.new[19],
df.new[9],
df.new[21],
df.new[14],
df.new[24],
df.new[11],
df.new[23],
df.new[4],
df.new[12],
df.new[2],
df.new[22],
df.new[17],
df.new[28],
df.new[7],
df.new[1],
df.new[3],
df.new[10],
df.new[15],
df.new[40],
df.new[47],
df.new[46],
df.new[45],
df.new[43],
df.new[44], 
df.new[37],
df.new[42],
df.new[39],
df.new[34],
df.new[33],
df.new[35],
df.new[31],
df.new[32],
df.new[29],
df.new[41],
df.new[36],
df.new[38],
df.new[27],
df.new[30],
df.new[48],
df.new[18],
df.new[49],
df.new[25],
df.new[5],
df.new[50])

# -> add to csv 
x <- data.frame(sequence1, sequence2)
write.table(x, file = 'test.csv', row.names=FALSE,col.names=FALSE)
