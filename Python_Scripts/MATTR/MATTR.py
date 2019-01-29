import numpy as np
import regex as re
import os.path
import pandas as pd
import sqlite3

## in this file i'm going to attempt to get the moving average type token ratio per book going.


dir_path = os.path.dirname(os.path.realpath(__file__))

conn = sqlite3.connect('../../textTable.sqlite')
cursor = conn.cursor()

words = []

for row in cursor.execute('''SELECT Unit FROM textTable WHERE Type='word' and Label=0'''):
    words.append((row[0]).encode('utf-8').strip())

conn.close()

# indices of each book

print(words[0:10])

angel = words[0:43900]


print(angel[0:100])

unique_counts = []


for i in range(0, len(angel), 64):
    unique = set(angel[i:i+64])
    unique_counts.append(len(unique))

print(unique_counts)
print(len(unique_counts))
N = 10
cumsum, moving_aves = [0.0], []

for i, x in enumerate(unique_counts, 1):
    cumsum.append(cumsum[i - 1] + x)
    if i >= N:
        moving_ave = (cumsum[i] - cumsum[i - N]) / N
        # can do stuff with moving_ave here
        moving_aves.append(moving_ave)

print(moving_aves)
print(len(moving_aves))

