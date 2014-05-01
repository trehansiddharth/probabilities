Probabilities
=============

Probabilities is a library for simulating probabilistic events. It provides a monadic datatype, Distribution, that accepts a random seed and produces random values according to a given probability distribution, as well as some common discrete and continuous probability distributions, like uniform, bernoulli, binomial, geometric, and normal. The real power of this library comes from being able to compose distributions easily: for example, you can model a geometric distribution where the parameter p is distributed normally about a mean by simply using the bind operator. The library also supports Bayesian updates, joint probabilities, and generating sample points given a Distribution.

Examples
--------

This repository contains several examples of the Probabilities library in use:

* Chess simulates a chess piece randomly moving across a chessboard. chess.hs contains distributions for various chess pieces, and chess.plots.hs contains code for creating plots of a chess piece's distribution (it uses Brent Yorgey's diagrams package).

They can be found within the "examples" folder.

Plots
-----

This repository also includes histograms for various distributions (with 10,000 sample points), found in the "plots" folder.