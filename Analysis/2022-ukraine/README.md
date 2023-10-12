# ukraine 2022 (pilot project)

This is data from a pilot project to test the possibilites and restrictions of semantic coding.

We analyzed posts from two major parties and their members in Germany about the war on Ukraine.
We arbitrarily chose 5 posts per party and per platform (Facebook or Twitter).

Social media posts were chosen for two reasings.
First, they are short, and manual semantic coding takes very long.
Second, social media content is maybe the most important part of the early 21th century (semi-)public communication.

One major finding of the pilot study was that seminatic coding works, 
and the data allows for reasonable interpretation. 
We provide the encoded articles (see the links for the original content) 
and the R code used for analyses.
There is also R code to export the encoded networks to a *.graphml file,
that you can open with yEd, then run its automated layout feature,
and watch the network structure.

Importantly, the semantic coding was done before we decided that
operators shall not carry meaning beyond the relation between two enties,
especially that shall not describe actions or behavioral schemes.
Therefore, we must advise that the articles would have been coded otherwise,
given a revised operator set.

The original codebook from the pilot study is available in the `codebook` directory.