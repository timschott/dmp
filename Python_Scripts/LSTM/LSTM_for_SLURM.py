# General imports

import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split
# Keras
import keras
from keras.layers import *
from keras.models import *
from keras_preprocessing.text import *


# LSTM implementation
# In this file I will construct a LSTM with the goal of identifying particularly
# Consequential and important words across my corpus of 50 novels.

# This function pulls all the words out of my database and
# Returns them in a 50 element list with each element containing all the words of a single book
# So, element 2 in this list is all the words from Blood Meridian as a single (enormous!) string

# Instantiates a vanilla LSTM
# Takes advantage of word embeddings
# https://keras.io/layers/embeddings/
# https://towardsdatascience.com/understanding-lstm-and-its-quick-implementation-in-keras-for-sentiment-analysis-af410fd85b47
#
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


# We want to print out some of the relevant data inside embeddings
# This func is lifted from a stack overflow post about highlighting important words in sentences
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


# Splits and shuffles my training and testing data

def train_test_division(padded_list, y_labels):
    mat = np.matrix(padded_list)
    df = pd.DataFrame(data=mat)
    X_train, X_test, Y_train, Y_test = train_test_split(df, y_labels, test_size=0.25, random_state=21)

    return X_train, X_test, Y_train, Y_test


# Runs and evaluates my vanilla LSTM

def train_and_test_vanilla_model(X_train, X_test, Y_train, Y_test, model):
    model_history = model.fit(X_train, Y_train, batch_size=256, epochs=3, verbose=1)

    predictions = model.predict(X_test)

    score = model.evaluate(X_test, Y_test, verbose=0)
    print model.summary()

    val_loss_history = model_history.history['val_loss']
    val_acc_history = model_history.history['val_acc']

    print('Val loss: ', sum(val_loss_history) / len(val_loss_history))
    print('Val accuracy: ', sum(val_acc_history) / len(val_acc_history))
    print('Vanilla Model Score: ', score)

    return model_history, predictions

# Runs and evaluates my Attentive LSTM

def train_and_test_attentive_model(X_train, X_test, Y_train, Y_test, model):
    model_history = model.fit(X_train, Y_train, batch_size=256, epochs=3, verbose=1)

    # predictions = model.predict_classes(X_test)

    score = model.evaluate(X_test, Y_test, verbose=0)
    attentions = model.predict(X_test, batch_size=256)

    np.save('attentions_list', attentions)

    print model.summary()

    val_loss_history = model_history.history['val_loss']
    val_acc_history = model_history.history['val_acc']

    print('Val loss: ', sum(val_loss_history) / len(val_loss_history))
    print('Val accuracy: ', sum(val_acc_history) / len(val_acc_history))
    print('Vanilla Model Score: ', score)

    with open('hist.json', 'w') as f:
        json.dump(model_history.history, f)

    return model_history


if __name__ == '__main__':
    pads = np.load(sys.argv[1])

    y_labels = np.asarray(
       [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])

    # Split Data

    x_train, x_test, y_train, y_test = train_test_division(pads, y_labels)

    # Create Models

    # lstm = create_LSTM()

    lstm_2, attentive = important_lstm()

    # Small Models

    # Learn and Test (This is for big models)

    hist = train_and_test_attentive_model(x_train, x_test, y_train, y_test, lstm_2)