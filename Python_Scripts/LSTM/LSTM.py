# General imports
import numpy as np
from string import maketrans
import matplotlib.pyplot as plt
from sklearn.utils import shuffle
import sys
import pandas as pd
import os
import sqlite3
from sklearn.model_selection import train_test_split
# Keras
import keras
from keras.layers import *
from keras.models import *
from keras_preprocessing.text import *


# https://towardsdatascience.com/understanding-lstm-and-its-quick-implementation-in-keras-for-sentiment-analysis-af410fd85b47
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
    print(len(X))
    # sequences: List of lists, where each element is a sequence. --> do at the end

    print(len(X))
    return X

def make_padded_list(word_strings):
    encoded_bucket = []

    for text in word_strings:
        clean = text_to_word_sequence(text)
        numbers_now = encode(clean)
        encoded_bucket.append(numbers_now)

    # pad 0's up to longest book length, gravity's rainbow
    padded = keras.preprocessing.sequence.pad_sequences(encoded_bucket, maxlen=330351)
    np.save('padded_keras_list')

# a vanilla LSTM
def create_LSTM():

    embed_dim = 128
    lstm_out = 200
    batch_size = 32
    model = Sequential()
    model.add(Embedding(69230, embed_dim, input_length=330351, mask_zero=True))
    model.add(keras.layers.SpatialDropout1D(.2))
    model.add(LSTM(units=128))
    model.add(Dense(units=330351))
    model.add(Activation('softmax'))
    model.compile(optimizer='adam', loss='sparse_categorical_crossentropy')

    # print(model.summary())

    return model

# https://keras.io/layers/embeddings/
# We want to print out some of the relevant data inside embeddings
# https://stackoverflow.com/questions/51477977/highlighting-important-words-in-a-sentence-using-deep-learning
def important_lstm():
    inp = Input((None,))
    # Embed words into vectors of size 10 each:
    # Output shape is (None,10)
    # 69230 is my vocab size
    embs = Embedding(69230, 128)(inp)
    # Run LSTM on these vectors and return output on each timestep
    # Output shape is (None,5)
    lstm = LSTM(units=128, return_sequences=True)(embs)
    ##Attention Block
    # Transform each timestep into 1 value (attention_value)
    # Output shape is (None,1)
    attention = TimeDistributed(Dense(1))(lstm)
    # By running softmax on axis 1 we force attention_values
    # to sum up to 1. We are effectively assigning a "weight" to each timestep
    # Output shape is still (None,1) but each value changes
    attention_vals = Softmax(axis=1)(attention)
    # Multiply the encoded timestep by the respective weight
    # I.e. we are scaling each timestep based on its weight
    # Output shape is (None,5): (None,5)*(None,1)=(None,5)
    scaled_vecs = Multiply()([lstm, attention_vals])
    # Sum up all scaled timesteps into 1 vector
    # i.e. obtain a weighted sum of timesteps
    # Output shape is (5,) : Observe the time dimension got collapsed
    context_vector = Lambda(lambda x: K.sum(x, axis=1))(scaled_vecs)
    ##Attention Block over
    # Get the output out
    out = Dense(1, activation='sigmoid')(context_vector)
    model = Model(inp, out)
    model_with_attention_output = Model(inp, [out, attention_vals])
    model.compile(optimizer='adam', loss='binary_crossentropy')

    # print(model.summary())
    # print(model_with_attention_output.summary())
    return model, model_with_attention_output

def train_test_division(padded_list, y_labels):

    mat = np.matrix(padded_list)

    df = pd.DataFrame(data=mat)
    X_train, X_test, Y_train, Y_test = train_test_split(df, y_labels, test_size=0.20, random_state=21)

    # Here we train the Network.

    return X_train, X_test, Y_train, Y_test


def train_and_test_vanilla_model(X_train, X_test, Y_train, Y_test, model):

    model.fit(X_train, Y_train, batch_size=32, nb_epoch=1, verbose=5)

    predictions = model.predict_classes(X_test)

    score = model.evaluate(X_test, Y_test, verbose=0)

    print score

    return 0

def train_and_test_attentive_model(X_train, X_test, Y_train, Y_test, model):
    model.fit(X_train, Y_train, batch_size=32, nb_epoch=1, verbose=5)

    predictions = model.predict_classes(X_test)

    score = model.evaluate(X_test, Y_test, verbose=0)

    print score

    return 0

if __name__ == '__main__':
    print 'hello world'

    # stringed_words = get_data()

    # make_padded_list(stringed_words)

    pads = np.load('padded_keras_list.npy')
    y_labels = np.asarray(
        [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])

    x_train, x_test, y_train, y_test = train_test_division(pads, y_labels)

    lstm = create_LSTM()
    lstm_2, attentive = important_lstm()

    train_and_test_vanilla_model(x_train, x_test, y_train, y_test, lstm)
    # train_and_test_attentive_model(x_train, x_test, y_train, y_test, attentive)