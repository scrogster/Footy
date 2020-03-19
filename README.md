Footy
===============

My attempt to forecast Australian Football League (AFL) results using Bayesian statistical models.

1. Historic results and future game fixtures are obtained using the API provided by [squiggle.com.au](http://api.squiggle.com.au)

2. Modified Bradley-Terry models are trained on a few recent years' results using [JAGS](http://mcmc-jags.sourceforge.net/) and used to predict the outcomes (win/lose) of forthcoming games.

3. Predictions will be submitted weekly to the official AFL online footy tipping contest [tipping.afl.com.au](http://tipping.afl.com.au).

