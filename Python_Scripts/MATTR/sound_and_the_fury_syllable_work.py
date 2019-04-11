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

def dist_count(colors):

    print colors

    for i in range(0, len(colors)):

        print('---')

        if(colors[i] == 'red' and colors[i+1] == 'blue' and colors[i+2]=='blue' and colors[i+3]=='blue'):
            print i


        if(i == 737):
            break

    return 0


def pltcolor(lst):
    cols = []
    num_cols = []
    for l in lst:
        if l == 1.0:
            cols.append('blue')
            num_cols.append(1.0)
        else:
            cols.append('red')
            num_cols.append(2.0)
    return cols, num_cols


def indices(lst, element):
    result = []
    offset = -1
    while True:
        try:
            offset = lst.index(element, offset + 1)
        except ValueError:
            return result
        result.append(offset)


# Create the colors list using the function above


if __name__ == "__main__":
    sound = read_file("quent.txt")
    results = keep_track(sound)

    print(len(sound))

    print sound[213:231]

    row = np.array(results)
    row = row.astype(float)
    x = np.arange(1, len(row) + 1, 1)

    plt.figure(figsize=(12, 4))

    # plt.xscale('log)
    cols, num_cols = pltcolor(row)
    print num_cols

    reds = indices(num_cols, 2.0)

    print reds

    dists = []
    consec = 0
    for i in range(0, len(reds)-2):

        distance = reds[i+1] - reds[i]
        dists.append(distance)
        if distance == 1:
            consec+=1
        if distance == 18:
            print reds[i]
            print reds[i+1]
            print '---'

    print dists
    print sum(dists)/len(dists)
    print consec

    print(sorted(dists, reverse=True))

    #out = dist_count(cols)
    print('dfjkdjfkdajzkd')
    #print out
    print ('dlkjfkldfd')



    dummies = np.empty(739)


    dummies[0:100] = 9
    dummies[100:200] = 8
    dummies[200:300] = 7
    dummies[300:400] = 6
    dummies[400:500] = 5
    dummies[500:600] = 4
    dummies[600:700] = 3
    dummies[700:739] = 2

    #print(cols.index("red"))

    plt.scatter(x[0:100], dummies[0:100], s=10, c = cols, label='Monoysyllables')
    plt.scatter(x[0:100], dummies[100:200], s=10, c=cols[100:200])
    plt.scatter(x[0:100], dummies[200:300], s=10, c=cols[200:300])
    plt.scatter(x[0:100], dummies[300:400], s=10, c=cols[300:400])
    plt.scatter(x[0:100], dummies[400:500], s=10, c=cols[400:500])
    plt.scatter(x[0:100], dummies[500:600], s=10, c=cols[500:600])
    plt.scatter(x[0:100], dummies[600:700], s=10, c=cols[600:700])
    plt.scatter(x[0:39], dummies[700:739], s=10, c=cols[400:500])

    plt.scatter(x[0], dummies[500], s=10, color='red', label='Polysyllables')

    frame1 = plt.gca()
    # frame1.legend(('monosyllables', 'polysyllables'))

    frame1.axes.get_yaxis().set_visible(False)
    frame1.axes.get_xaxis().set_visible(False)

    plt.xlabel("Words")

    plt.legend(loc='best')
    plt.subplots_adjust(bottom=0.05)

    plt.title("Syllabic Variety Across Quentin's Closing Sequence")
    plt.savefig("quent_plot.png", dpi=500)
    plt.gcf().clear()
    plt.show()
