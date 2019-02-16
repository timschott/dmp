# General imports
import numpy as np
from string import maketrans
import matplotlib.pyplot as plt
from sklearn.utils import shuffle
import sys
import pandas as pd
import os
import sqlite3
# Keras
import keras
from keras.models import Sequential
from keras.preprocessing.text import Tokenizer
import keras.preprocessing.text
from keras.layers import Dense, Conv2D, MaxPooling2D, Dropout, Flatten


def get_data():
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
    theSecretAdversary = words[2576799:2652844]
    theScarhavenKeep = words[2652845:2727453]
    theRedThumbMark = words[2727454:2802062]
    theParadiseMystery = words[2802063:2878931]
    theRaynerSladeAmalgamation = words[2878932:2958661]
    theLeavenworthCase = words[2958662: 3068879]
    theOldManInTheCorner = words[3068880:3138823]
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
    aStudyInScarlet = words[3906799:3950699]
    theBigSleep = words[3950700:4017610]
    theShriekingPit = words[4017611:4117616]
    thePictureOfDorianGray = words[4117617:4196905]
    theSignOfFour = words[4196906:4240350]
    wideSargassoSea = words[4240351:4287699]
    gravitysRainbow = words[4287700: 4618050]
    theSpiralStaircase = words[4618051:4689006]

    # thank you paste0 -- > with love, from R! paste0(titles, collapse=",")
    [1]

    # thanks R
    big_string_list = [str(absalomAbsalom), str(billyBudd), str(bloodMeridian), str(eureka), str(gravitysRainbow),
                       str(heartOfDarkness), str(lifeAndTimesOfMichaelK), str(lolita), str(mobyDick), str(mrsDalloway),
                       str(orlando), str(paleFire), str(portraitOfTheArtist), str(pym), str(somethingHappened),
                       str(theGreatGatsby), str(thePedersenKid), str(thePictureOfDorianGray), str(theRainbow),
                       str(theRoad), str(theSerpent), str(theSoundAndTheFury), str(toTheLighthouse),
                       str(underTheVolcano), str(wideSargassoSea), str(womenInLove), str(aStudyInScarlet),
                       str(theAngelOfTerror), str(theAshielMystery), str(theBigSleep), str(theBrandOfSilence),
                       str(theCircularStaircase), str(theDaffodilMystery), str(theHandInTheDark),
                       str(theInnocenceOfFatherBrown), str(theLadyInBlue), str(theLeavenworthCase),
                       str(theMaxwellMystery), str(theMoonRock), str(theMoonstone), str(theMysteryOfRoom75),
                       str(theOldManInTheCorner), str(theParadiseMystery), str(theRaynerSladeAmalgamation),
                       str(theRedThumbMark), str(theScarhavenKeep), str(theSecretAdversary), str(theShriekingPit),
                       str(theSignOfFour), str(theSpiralStaircase)]

    return big_string_list

# https://github.com/keras-team/keras/issues/1072
def text_to_word_sequence(text, filters='!"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n', split=" "):
    if type(text) == unicode:
        translate_table = {ord(c): ord(t) for c, t in zip(filters, split * len(filters))}
    else:
        translate_table = maketrans(filters, split * len(filters))
    text = text.translate(translate_table)
    seq = text.split(split)
    return [i for i in seq if i]

def encode(encoded_list):
    tokenizer = Tokenizer(lower=True, split=' ')
    tokenizer.fit_on_texts(encoded_list)
    # print(tokenizer.word_index)  # To see the dictionary
    X = tokenizer.texts_to_sequences(encoded_list)
    # tune to longest book being gravitys rainbow
    print(len(X))
    # sequences: List of lists, where each element is a sequence. --> do at the end
    #X = keras.preprocessing.sequence.pad_sequences(X, maxlen=330351)
    print(len(X))
    return X

if __name__ == '__main__':
    print 'hello world'
    stringed_words = get_data()

    encoded_bucket = []

    for text in stringed_words:
        clean = text_to_word_sequence(text)
        numbers_now = encode(clean)
        encoded_bucket.append(numbers_now)

    padded = keras.preprocessing.sequence.pad_sequences(encoded_bucket, maxlen=330351)
    print(np.shape(padded))

    np.save('padded_keras_list', padded)



