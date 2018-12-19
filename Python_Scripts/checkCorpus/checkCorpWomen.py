import numpy as np
import regex as re
import os.path
import pandas as pd

f = open('dh-lawrence-women-in-love.txt', 'r')

data = f.read()

list = re.split('\s{4,}', data)

print(list[46])

### CLEAN CLEAN CLEAN

array_paragraphs = np.array(list)

print(np.shape(array_paragraphs))

df = pd.DataFrame(array_paragraphs)

print(df.head(15))

# write this over to a CSV and then we can use it in R.
# lit.

df.to_csv("WOMEN_paras.csv")

# for i in range(50, 60):
#   print(list[i])

# df = pd.DataFrame(np.array(my_list).reshape(3, 3), columns=list("abc"))

# print(len(list))
# df = pd.DataFrame(np.recarray(my_list).reshape())