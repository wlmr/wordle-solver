# wordle-solver

## Install

Download [stack](https://www.haskellstack.org/)
in root of project:
`$ stack build`

## Run

`$ stack exec wordle-solver-exe`

## use

After having guessed "arose" you get feedback from wordle. The format for inputting the feedback is as follows:

`Ga Yr o s e`

G(reen)a means the a is in the right spot, Y(ellow)r means that r is in the wrong spot, simply o, with no prefixed capital, means o is not part of the word, etc.
