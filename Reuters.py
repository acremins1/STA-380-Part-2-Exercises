# directory name
dirName = "data/ReutersC50/C50train"

import os

# get list of subdirectories
subdirs = [x[0] for x in os.walk(dirName)]

print(subdirs)

# read in files for each subdirectory
data = {}
for subdir in subdirs:
    # get base name of subdirectory
    base = os.path.basename(subdir)
    data[base] = []
    authorData = data[base]

    files = os.listdir(subdir)
    for file in files:
        if(not file.endswith(".txt")):
            continue
        with open(os.path.join(subdir, file), 'r') as f:
            authorData.append(f.read())

# for each author, take our authors with zero documents
# and take the first document from each author with more than one document
toRemove = []
for author in data:
    if(len(data[author]) == 0):
        # remove key from dictionary
        toRemove.append(author)
# remove keys from dictionary
del data[toRemove[0]]


print("Hello")

# For each author iterate through the documents and remove the stop words
# and then tokenize the documents
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk import download

download('stopwords')
download('punkt')
stop_words = set(stopwords.words('english'))

# Add "said" to the stop words
stop_words.add("said")
# Add "would" to the stop words
stop_words.add("would")
for author in data:
    for i in range(len(data[author])):
        data[author][i] = word_tokenize(data[author][i])
        # Lower case all words, remove punctuation, and remove stop words
        data[author][i] = [w.lower() for w in data[author][i] if w.isalpha() and w.lower() not in stop_words]

# for every author get value counts of words
from collections import Counter
wordCounts = {}

for author in data:
    wordCounts[author] = Counter()
    for document in data[author]:
        wordCounts[author].update(document)

# get the top 100 words for each author
topWords = {}
for author in wordCounts:
    topWords[author] = [x[0] for x in wordCounts[author].most_common(50)]


import pandas as pd

bigAssList = []
# for each author, add a row for each word
for author in topWords:
    for word in topWords[author]:
        bigAssList.append((author, word))

# create a dataframe from the list
df = pd.DataFrame(bigAssList, columns=['user', 'artist'])

# write to csv
df.to_csv("data/ReutersC50/wordCounts(2).csv", index=False)




