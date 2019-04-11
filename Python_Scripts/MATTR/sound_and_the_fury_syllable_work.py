import numpy as np
import matplotlib.pylab as plt


def read_file(filename):

    words = []

    file = open(filename, "r")

    for line in file:
        for word in line.split():
            words.append(word)

    return words

def syllable_count(word):

    word = word.lower()
    count = 0
    vowels = "aeiouy"
    if word[0] in vowels:
        count += 1
    for index in range(1, len(word)):
        if word[index] in vowels and word[index - 1] not in vowels:
            count += 1
    if word.endswith("e"):
        count -= 1
    if count == 0:
        count += 1
    return count

def keep_track(words):

    syll_counts = []

    for word in words:
        if(syllable_count(word) >1 ):
            syll_counts.append(2)
        else:
            syll_counts.append(1)

    return syll_counts


def pltcolor(lst):
    cols = []
    for l in lst:

        if l == 1.0:
            
            cols.append('blue')
        else:
            cols.append('green')
    return cols


# Create the colors list using the function above


if __name__ == "__main__":
    sound = read_file("quent.txt")
    results = keep_track(sound)

    print results
    print(sound[525:545])
    print(len(results))
    #858 total words
    #1172 total syllables
    print(sum(results))
    polycount = 0

    for i in results:
        if i != 1:
            polycount += 1

    print 'poly ' + str(polycount)
    monocount =  1172 - polycount
    print 'mono ' + str(monocount)

    row = np.array(results)
    row = row.astype(float)
    x = np.arange(1, len(row) + 1, 1)

    plt.figure(figsize=(16, 3))

    # plt.xscale('log)
    cols = pltcolor(row)

    plt.scatter(x, row, s=3, c = cols)
    plt.title("Syllables Across Quentin's Last Monologue")
    plt.savefig("quent_plot.png", dpi=500)
    plt.gcf().clear()
    plt.show()