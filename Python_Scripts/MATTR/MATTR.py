import os.path
from itertools import izip
import sqlite3
import csv

# MATTR Python Implementation


dir_path = os.path.dirname(os.path.realpath(__file__))

conn = sqlite3.connect('../../textTable.sqlite')
cursor = conn.cursor()

words = []

for row in cursor.execute('''SELECT Unit FROM textTable WHERE Type='word' '''):
    words.append((row[0]).encode('utf-8').strip())

conn.close()

# indices of each book

heartOfDarkness = words[0:39084]
theRoad = words[39085:97786]
theGreatGatsby = words[97787:146604]
womenInLove = words[146605:329261]
portraitOfTheArtist = words[329262:414188]
lolita = words[414189:526381]
theRainbow = words[526382:713940]
mobyDick = words[713941:928123]
theSerpent = words[928124:1100808]
pym = words[1100809:1201762]
underTheVolcano = words[1201763:1340374]
orlando = words[1340375:1419921]
toTheLighthouse = words[1419922:1489826]
eureka = words[1489827:1528392]
paleFire = words[1528393:1596524]
billyBudd = words[1596525:1627267]
theSoundAndTheFury = words[1627268:1723739]
thePedersenKid = words[1723740:1747441]
theAngelOfTerror = words[1747442:1810705]
lifeAndTimesOfMichaelK = words[1810706:1877059]
absalomAbsalom = words[1877060: 2009727]
bloodMeridian = words[2009728:2124927]
mrsDalloway = words[2124928:2189194]
somethingHappened = words[2189195:2379429]
theMoonstone = words[2379430: 2576798]
theSecretAdversary= words[2576799:2652844]
theScarhavenKeep = words[2652845:2727453]
theRedThumbMark = words[2727454:2802062]
theParadiseMystery = words[2802063:2878931]
theRaynerSladeAmalgamation = words[2878932:2958661]
theLeavenworthCase = words[2958662: 3068879]
theOldManInTheCorner=words[3068880:3138823]
theMoonRock = words[3138824:3246627]
theHandInTheDark = words[3246628: 3354187]
theDaffodilMystery = words[3354188:3422566]
theInnocenceOfFatherBrown = words[3422567:3502004]
theBrandOfSilence = words[3502005:3561098]
theCircularStaircase = words[3561099:3631729]
theAshielMystery = words[3631730:3719130]
theMysteryOfRoom75 = words[3719131:3767884]
theLadyInBlue = words[3767885:3846509]
theMaxwellMystery = words[3846510:3906798]
aStudyInScarlet=words[3906799:3950699]
theBigSleep=words[3950700:4017610]
theShriekingPit=words[4017611:4117616]
thePictureOfDorianGray = words[4117617:4196905]
theSignOfFour = words[4196906:4240350]
wideSargassoSea = words[4240351:4287699]
gravitysRainbow = words[4287700: 4618050]
theSpiralStaircase = words[4618051:4689006]

# thank you paste0 -- > with love, from R! paste0(titles, collapse=",")
[1]
# big_list = [heartOfDarkness,theRoad,theGreatGatsby,womenInLove,portraitOfTheArtist,lolita,theRainbow,mobyDick,theSerpent,pym,underTheVolcano,orlando,toTheLighthouse,eureka,paleFire,billyBudd,theSoundAndTheFury,thePedersenKid,theAngelOfTerror,lifeAndTimesOfMichaelK,absalomAbsalom,bloodMeridian,mrsDalloway,somethingHappened,theMoonstone,theSecretAdversary,theScarhavenKeep,theRedThumbMark,theParadiseMystery,theRaynerSladeAmalgamation,theLeavenworthCase,theOldManInTheCorner,theMoonRock,theHandInTheDark,theDaffodilMystery,theInnocenceOfFatherBrown,theBrandOfSilence,theCircularStaircase,theAshielMystery,theMysteryOfRoom75,theLadyInBlue,theMaxwellMystery,aStudyInScarlet,theBigSleep,theShriekingPit,thePictureOfDorianGray,theSignOfFour,wideSargassoSea,gravitysRainbow,theSpiralStaircase]
big_list = [absalomAbsalom, billyBudd, bloodMeridian, eureka, gravitysRainbow, heartOfDarkness, lifeAndTimesOfMichaelK,
            lolita, mobyDick, mrsDalloway, orlando, paleFire, portraitOfTheArtist, pym, somethingHappened,
            theGreatGatsby, thePedersenKid, thePictureOfDorianGray, theRainbow, theRoad, theSerpent, theSoundAndTheFury,
            toTheLighthouse, underTheVolcano, wideSargassoSea, womenInLove, aStudyInScarlet, theAngelOfTerror,
            theAshielMystery, theBigSleep, theBrandOfSilence, theCircularStaircase, theDaffodilMystery,
            theHandInTheDark, theInnocenceOfFatherBrown, theLadyInBlue, theLeavenworthCase, theMaxwellMystery,
            theMoonRock, theMoonstone, theMysteryOfRoom75, theOldManInTheCorner, theParadiseMystery,
            theRaynerSladeAmalgamation, theRedThumbMark, theScarhavenKeep, theSecretAdversary, theShriekingPit,
            theSignOfFour, theSpiralStaircase]

print(len(big_list[0]))


unique_container = []

for j in range(0, 50):
     unique_counts = []

     for i in range(0, len(big_list[j]), 64):

          unique=set(big_list[j][i: i+63])
          unique_counts.append(len(unique))

     unique_container.append(unique_counts)

print(len(unique_container))

moving_aves_container = []

N = 16
cumsum, moving_aves = [0.0], []

# https://stackoverflow.com/questions/13728392/moving-average-or-running-mean


for j in range(0,50):

     for i, x in enumerate(unique_container[j], 1):
          print(j)
          cumsum.append(cumsum[i - 1] + x)

          if i >= N:
               moving_ave = (cumsum[i] - cumsum[i - N]) / N
               moving_aves.append(moving_ave)

     moving_aves_container.append(moving_aves)
     moving_aves = []
     cumsum, moving_aves = [0.0], []


# https://stackoverflow.com/questions/14037540/writing-a-python-list-of-lists-to-a-csv-file

with open("mattr.csv", "wb") as f:
     writer = csv.writer(f)
     writer.writerows(moving_aves_container)


with open("unique.csv", "wb") as f:
     writer = csv.writer(f)
     writer.writerows(unique_container)

a = izip(*csv.reader(open("mattr.csv", "rb")))
csv.writer(open("mattr_t.csv", "wb")).writerows(a)

a = izip(*csv.reader(open("unique.csv", "rb")))
csv.writer(open("unique_t.csv", "wb")).writerows(a)
