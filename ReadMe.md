### Speech Learn: Model Relationship Between Congressional Speech and Ideology 

[![Code Issues](https://www.quantifiedcode.com/api/v1/project/f1ad09312b9148b5bb9469132bbdb3d7/badge.svg)](https://www.quantifiedcode.com/app/project/f1ad09312b9148b5bb9469132bbdb3d7)

This is an example for how to use text as data. 

#### Steps
1. Using the Sunlight Foundation Capitol Words API, [Download the Congressional Speech Data](scripts/capitol_speech.py) 
2. Optional (also part of model): [Clean text data using Python](https://github.com/soodoku/text-as-data/tree/master/preprocess_csv)
3. [Merge speech data with DW-Nominate data from Voteview](scripts/capitol_vote.R)
4. [Model](scripts/capitol_words.md)

#### Suggested Reading
1. See [What Drives Media Slant (pdf)](http://web.stanford.edu/~gentzkow/research/biasmeas.pdf)
2. [A Measure of Media Bias (pdf)](http://www.sscnet.ucla.edu/polisci/faculty/groseclose/pdfs/MediaBias.pdf)

#### License
Scripts are released under the [MIT License](License.md).
