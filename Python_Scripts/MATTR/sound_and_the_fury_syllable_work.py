import numpy as np
import matplotlib.pylab as plt


def read_file(filename):

    words = []

    file = open(filename, "r")

    for line in file:
        for word in line.split():
            words.append(word)
            print(word)

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
        syll_counts.append(syllable_count(word))

    return syll_counts


if __name__ == "__main__":
    sound = read_file("quent.txt")
    results = keep_track(sound)

    print results
    print(len(results))
    #858 total words
    #1172 total syllables
    print(sum(results))
    polycount = 0

    for i in results:
        if i!=1:
            polycount+=1

    print 'poly ' + str(polycount)
    monocount =  1172 - polycount
    print 'mono ' + str(monocount)

    row = np.array(results)
    row = row.astype(float)
    x = np.arange(1, len(row) + 1, 1)
    print x
    plt.figure(figsize=(9, 9))

    plt.plot(x, row)
    plt.title("Syllables Across Quentin's Last Monologue")
    plt.savefig("quent_plot.png", dpi=500)
    plt.gcf().clear()
    plt.show()