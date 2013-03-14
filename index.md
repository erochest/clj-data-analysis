---
title: Clojure Data Analysis Cookbook
---

## Clojure Data Analysis Cookbook

<img class='bookframe' src='http://dgdsbygo8mp3h.cloudfront.net/sites/default/files/imagecache/productview_larger/2643OS_mockupcover_cb.jpg'/>

Looking to use Clojure for data analysis?

### Data

Throughout the book, I use a number of datasets. Some of these are standard
datasets, some are from the [UCI Machine Learning Repository][uci], some from
[census.ire.org][ire], some from other sources, and some I've put together
myself. I've uploaded them all here for archiving and easy access. Here they
all are, with a few notes about each:

2010 US Census Data

:   This data is downloaded from the [Investigative Reporters and Editors
    Census dataset][ire] site. You can also download raw census data from the
    [US Census Bureau][census].

    - [all_160.P3.csv](data/all_160.P3.csv): This is race data (P3) from the
    census. This is a place-level summary (160), and I've merged this data for
    all states.
    - [all_160_in_51.P3.csv](data/all_160_in_51.P3.csv): This is race data (P3)
    from the census. This is a place-level summary (160) for Virginia (51).
    - [all_160_in_51.P35.csv](data/all_160_in_51.P35.csv): This is family
    counts (P35) from the census. This is a place level summary (160) for
    Virginia (51).
    - [census-race.json](data/census-race.json): This is the data from
    `all_160.P3.csv`, mentioned above, translated into JSON.
    - [clusters.json](data/clusters.json): This is a graph of the clusters of
    the data from `all_160.P3.csv`, mentioned above. The clusters were
    generated K-means clusters from that dataset aggregated by state. The JSON
    data structure represents the nodes and links (edges) in the graph, along
    with the aggregated data.

[Abalone][abalone]

:   This is a 

    - abalone.data
    - abalone.json
    - abalone.names

Accident Fatalities

:   From DOJ

    - accident-fatalities.tsv

Chick Weights

:   UCI

    - chick-weight.json

Currencies and Exchange Rates

:   From and X-Rates

    - currencies.ttl
    - x-rates-usd.html

Doctor Who Companions

:   From Wikipedia, put together by me.

    - companions.clj
    - companions.txt

FASTA datasets

:   From ???

    - abc-transporter.fasta
    - dehydratase.fasta
    - elephas.fasta
    - maltophilia.fasta
    - mchu.fasta
    - ovax-chick.fasta
    - salmonella.fasta
    - seqeuence-1.fasta
    - sequences.fasta
    - transferase.fasta

IBM stock prices

:   From Google Finance

    - ibm.csv

Ionosphere data

:   Measurements in the ionosphere

    - ionosphere.arff

Iris dataset

:   A standard

    - iris.arff

Mushroom

:   UCI

    - agaricus-lepiota.data
    - agaricus-lepiota.names
    - mushroom.arff

Sample Datasets (TV-related)

:   Made-up

    - small-sample-header.csv
    - small-sample-header.xls
    - small-sample-list.html
    - small-sample-table.html
    - small-sample.csv
    - small-sample.json
    - small-sample.sqlite
    - small-sample.xml

Sherlock Holmes stories

:   From Project Gutenberg.

    - pg1661.txt

Text Corpus

:   For spell checker, from Norvig's site.

    - big.txt

World Bank dataset

:   A dataset filtered and pivoted by me.

    - world-bank-filtered.csv

<div class='bottom'></div>

[packt]: http://www.packtpub.com/
[packtclj]: http://www.packtpub.com/clojure-data-analysis-cookbook/book
[amazon]: http://www.amazon.com/gp/product/B00BECVV9C/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00BECVV9C&linkCode=as2&tag=httpwwwericro-20

[uci]: http://archive.ics.uci.edu/ml/datasets.html
[abalone]: http://archive.ics.uci.edu/ml/datasets/Abalone

[ire]: http://census.ire.org/
[census]: http://www.census.gov/

