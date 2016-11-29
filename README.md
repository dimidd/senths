# sentHs

A simple Naive Bayes classifier for sentences. The models are instances of Monoid.
This enables easy online learning, efficient cross-validation and parallelism.
For details see [1].

# Structure:

The main module is NB.hs, where the classifier is defined.
NBTrain.hs is a high-level interface for training NB models from files.
NBTest is meant for tests using a tsv dataset of news headlines from different catagories (tech, biz, health, etc):
The original is a dataset of references to news web pages collected from an online aggregator
in the period from March 10 to August 10 of 2014. The resources are grouped into
clusters that represent pages discussing the same news story. The dataset includes
also references to web pages that point (has a link to) one of the news page in the collection. [3]

# Implementation
We count the total occurencies of words and also per category statistics. This enables us to compute
the probabilities, and effciently combine models. Given an utterance we convert it to lowercase,
remove its non-alphabetic chars, and split it to words.
We remove the stop-words and update the counts of the remaining ones.
The stop words are taken from MySql [2].

# Results

We trained the classifier on 928 examples of two categories, and tested on 71 examples.
71 out of 72 were classified correctly (98%).

# Future plans

* Add an option for stemming
* Serialization
* Web-app

[1] https://izbicki.me/public/papers/icml2013-algebraic-classifiers.pdf

[2] http://xpo6.com/list-of-english-stop-words/

[3] http://archive.ics.uci.edu/ml/datasets/News+Aggregator
