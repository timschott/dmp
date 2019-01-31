import numpy as np
import matplotlib.pylab as plt

dummy = []
container = []
file_names = []
count = 0
with open("mattr.csv") as f:
    lis = [line.split() for line in f]  # create a list of lists
    for i, x in enumerate(lis):  # print the list items
        for row in x:
            count+=1
            dummy = []
            # the line
            #dummy.append(row)
            file_names.append("file_" + str(i))
            #dummy = [x.strip() for x in dummy.split(',')]
            for x in row.split(","):
                dummy.append(x)

            container.append(dummy)

titles = ['absalomAbsalom', 'billyBudd', 'bloodMeridian', 'eureka', 'gravitysRainbow', 'heartOfDarkness',
          'lifeAndTimesOfMichaelK', 'lolita', 'mobyDick', 'mrsDalloway', 'orlando', 'paleFire', 'portraitOfTheArtist',
          'pym', 'somethingHappened', 'theGreatGatsby', 'thePedersenKid', 'thePictureOfDorianGray', 'theRainbow',
          'theRoad', 'theSerpent', 'theSoundAndTheFury', 'toTheLighthouse', 'underTheVolcano', 'wideSargassoSea',
          'womenInLove', 'aStudyInScarlet', 'theAngelOfTerror', 'theAshielMystery', 'theBigSleep', 'theBrandOfSilence',
          'theCircularStaircase', 'theDaffodilMystery', 'theHandInTheDark', 'theInnocenceOfFatherBrown',
          'theLadyInBlue', 'theLeavenworthCase', 'theMaxwellMystery', 'theMoonRock', 'theMoonstone',
          'theMysteryOfRoom75', 'theOldManInTheCorner', 'theParadiseMystery', 'theRaynerSladeAmalgamation',
          'theRedThumbMark', 'theScarhavenKeep', 'theSecretAdversary', 'theShriekingPit', 'theSignOfFour',
          'theSpiralStaircase']

for i,x in enumerate(container):
    row = np.array(x)
    row = row.astype(float)
    y = np.arange(1, len(row)+1, 1)
    plt.figure(figsize=(9,9))

    plt.plot(y, row)
    plt.title(titles[i])

    plt.savefig(titles[i] + '_MATTR.png', dpi=500)
    plt.gcf().clear()
    plt.show()

