---
title: Clojure Data Analysis Cookbook
---

## Clojure Data Analysis Cookbook

<img class='bookframe' src='http://dgdsbygo8mp3h.cloudfront.net/sites/default/files/imagecache/productview_larger/2643OS_mockupcover_cb.jpg'/>

Looking to use Clojure for data analysis?

This book covers [Incanter][incanter], [Weka][weka], and even goes into
creating data visualizations for the web with [D3][d3] and
[ClojureScript][cljs]. It provides over 100 recipes, some short and some more
extended.

**Due out March, 2013.**

### Data

Throughout the book, I use a number of datasets. Some of these are standard
datasets, some are from the [UCI Machine Learning Repository][uci], some from
[census.ire.org][ire], some from other sources, and some I've put together
myself. I've uploaded them all here for archiving and easy access. Here they
all are, with a few notes about each:

[2010 US Census Data][ire]

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

:   This dataset is from the [UCI Machine Learning Repository][uci]. It
    contains sex, age, and measurements of abalone. This can be used to
    predict the age from the fish's physical measurements.

    - [abalone.data](data/abalone.data): This is the data in CSV format.
    - [abalone.json](data/abalone.json): The data from `abalone.data` formatted
      as JSON.
    - [abalone.names](data/abalone.names): This is information about the data,
      including the fields and their ranges of values.

Accident Fatalities

:   This dataset was selected and downloaded from the US [National Highway
    Traffic Safety Administration][nhtsa]. This dataset includes the speed
    limit and other factors related to the accidents.

    - [accident-fatalities.tsv](data/accident-fatalities.tsv)

Chick Weights

:   This is from the [Incanter datasets package][datasets]. It's also found in
    the R datasets package.

    - [chick-weight.json](data/chick-weight.json): This is the Incanter dataset
      converted to JSON.

Currencies and Exchange Rates

:   This is a couple of datasets used to illustrate working with semantic web
    data and web scraping.

    - [currencies.ttl](data/currencies.ttl): This dataset is from
      [Telegraphis][telegraphis], and it contains linked data with information
      about various currencies, such as the name, ISO codes, symbols.

    - [x-rates-usd.html](data/x-rates-usd.html): This is a snapshot of a rates
      table from [X-Rates][xrates].

Doctor Who Companions

:   This is a dataset that I've pulled together from [Wikipedia][wikip] listing
    the actors and companions from the British television program [Doctor
    Who][drwho].

    - [companions.clj](data/companions.clj): This is a set of Clojure forms
      that define the in-memory data for this information.
    - [companions.txt](data/companions.txt): This is a list of the companions
      as CSV. It lists an identifier (usually the first name) for each and
      their first name.

FASTA datasets

:   FASTA files are used in bioinformatics to exchange nucleotide and peptide
    sequences. This is a small collection of them to use for testing a custom
    FASTA parser.

    - [abc-transporter.fasta](data/abc-transporter.fasta)
    - [dehydratase.fasta](data/dehydratase.fasta)
    - [elephas.fasta](data/elephas.fasta)
    - [maltophilia.fasta](data/maltophilia.fasta)
    - [mchu.fasta](data/mchu.fasta)
    - [ovax-chick.fasta](data/ovax-chick.fasta)
    - [salmonella.fasta](data/salmonella.fasta)
    - [seqeuence-1.fasta](data/seqeuence-1.fasta)
    - [sequences.fasta](data/sequences.fasta)
    - [transferase.fasta](data/transferase.fasta)

IBM stock prices

:   This dataset was downloaded from [Google Finance][gfinance]. It contains
    the prices of IBM stock for the decade between <time>Nov 26, 2001</time>
    and <time>Nov 23, 2012</time>.

    - [ibm.csv](data/ibm.csv)

[Ionosphere data][ionosphere]

:   This dataset is from an antenna array in Labrador. It contains a number of
    measurements of free electrons in the ionosphere. This dataset can be
    found in the [UCI Machine Learning Repository][uci], but this dataset is
    in [Attribute-Relation File Format (ARFF)][arff] format for use with
    [Weka][weka].

    - [ionosphere.arff](data/UCI/ionosphere.arff): This is pulled from the
      [Weka][weka] distribution for easier access.

[Iris][iris]

:   This is a standard dataset that's almost everywhere. We also use the copy
    that ships with Incanter several times in the book. For more information
    about this dataset, see [its page][iris] at the [UCI Machine Learning
    Repository][uci].

    - [iris.arff](data/UCI/iris.arff): This is pulled from the [Weka][weka]
      distribution for easier access.

[Mushroom][mushroom]

:   This is another standard dataset from the [UCI Machine Learning
    Repository][uci]. This contains categorical data on mushrooms, including
    whether they're edible or poisonous.

    - [agaricus-lepiota.data](data/agaricus-lepiota.data): The data file from
      the UCI web site.
    - [agaricus-lepiota.names](data/agaricus-lepiota.names): Information about
      the data, including field names.
    - [mushroom.arff](data/mushroom.arff): The same dataset packages as an ARFF
      file for [Weka][weka].

TV-Related Sample Datasets

:   These are a series of datasets I threw together to illustrate loading
    different data formats.

    - [small-sample-header.csv](data/small-sample-header.csv)
    - [small-sample-header.xls](data/small-sample-header.xls)
    - [small-sample-list.html](data/small-sample-list.html)
    - [small-sample-table.html](data/small-sample-table.html)
    - [small-sample.csv](data/small-sample.csv)
    - [small-sample.json](data/small-sample.json)
    - [small-sample.sqlite](data/small-sample.sqlite)
    - [small-sample.xml](data/small-sample.xml)

*The Adventures of Sherlock Holmes*

:   This text is from [Project Gutenberg][gutenberg]. It's a collection of
    Sherlock Holmes short stories written by Sir Arthur Conan Doyle.

    - [pg1661.txt](data/pg1661.txt)

Spelling Training Corpus

:   This is the training corpus used in [Peter Norvig's article, "How to Write
    a Spelling Corrector."][norvig]

    - [big.txt](data/big.txt)

World Bank dataset

:   I downloaded this dataset about income inequality from the [World
    Bank][worldbank]. It need to be filtered and pivoted, and here is the final
    result.

    - [world-bank-filtered.csv](data/world-bank-filtered.csv)

<div class='bottom'></div>

[packt]: http://www.packtpub.com/
[packtclj]: http://www.packtpub.com/clojure-data-analysis-cookbook/book
[amazon]: http://www.amazon.com/gp/product/B00BECVV9C/ref=as_li_ss_tl?ie=UTF8&camp=1789&creative=390957&creativeASIN=B00BECVV9C&linkCode=as2&tag=httpwwwericro-20

[incanter]: http://incanter.org/
[d3]: http://d3js.org/
[cljs]: https://github.com/clojure/clojurescript

[weka]: http://www.cs.waikato.ac.nz/ml/weka/
[arff]: http://www.cs.waikato.ac.nz/ml/weka/arff.html
[datasets]: http://liebke.github.com/incanter/datasets-api.html

[uci]: http://archive.ics.uci.edu/ml/datasets.html
[abalone]: http://archive.ics.uci.edu/ml/datasets/Abalone
[ionosphere]: http://archive.ics.uci.edu/ml/datasets/Ionosphere
[iris]: http://archive.ics.uci.edu/ml/datasets/Iris
[mushroom]: http://archive.ics.uci.edu/ml/datasets/Mushroom

[ire]: http://census.ire.org/
[census]: http://www.census.gov/

[nhtsa]: http://www-fars.nhtsa.dot.gov/QueryTool/QuerySection/selectyear.aspx

[telegraphis]: http://telegraphis.net/data/currencies/currencies.ttl
[xrates]: http://www.x-rates.com/

[wikip]: http://en.wikipedia.org/wiki/Doctor_Who_companions
[drwho]: http://www.bbc.co.uk/programmes/b006q2x0

[fasta]: http://blast.ncbi.nlm.nih.gov/blastcgihelp.shtml

[gfinance]: https://www.google.com/finance

[gutenberg]: http://www.gutenberg.org/
[holmes]: http://www.gutenberg.org/ebooks/1661

[norvig]: http://norvig.com/spell-correct.html

[worldbank]: http://data.worldbank.org/

