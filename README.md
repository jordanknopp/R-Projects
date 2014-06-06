R-Projects
==========

clustering.R - unsupervised learning (clustering) used to analyze two unlabeled datasets.  
These datasets were digits (i.e. 0-9)...this was essentially categorizing them.  Explores hierarchical clustering with
single-linkage, complete-linkage, and variance-minimizing linkage

clustering.doc - visualization of results.  Some (humorous) analysis of the most cluster outliers (odd-looking digits)

envelopes.R - implementation of two envelopes problem.  see: http://en.wikipedia.org/wiki/Two_envelopes_problem 

gtd.R - uses linear regression on gtd_dataset to build a model of average global temperatures to predict the average 
global temperature for 2012 and 2013 with a 95% confidence interval.

gtd_dataset.csv - global temperature deviations dataset: gives, by month, the global temp dev from 1881 - 2011. 

gtd_linearfit.png - image file of linear regression applied to gtd_dataset

gtd_residuals.png - image file of plot of residuals (diff between observed and estimated values) from linear fit

loans.doc - description of most important variables, the creation of predictive models, a howto for running models, and 
visual representations of some results.

loans.R - predictive model of the interest rate based on loan dataset.  uses both regression with factor variables and 
ordinary (covariate) linear regression in order to create multiple predictive models based on different things 
(e.g. factor variable, continuous numeric variable)

loans_dataset.csv - a dataset about loans.  loan#,amtreq,amtfunded,interestrate,loanlength, etc.

loans_exploratory.doc - some exploratory graphs of the loans dataset.  used in determining most useful variables in dataset.

MontyHall.R - implementation of Monty Hall problem

titanic_dataset.csv - list of passengers on the Titanic with name,sex,#ofsiblings,ticketclass,boardingstatus,didtheysurvive, etc.

titanic_f.R - tree-based predictive model for whether or not a passenger would have survived the sinking of the titanic.

titanic_f.png - image of tree-based predictive model from titanic_f.R

titanic_rf.R - random forests to create a predictive model for the titanic dataset for whether or not a passenger would have 
survived the sinking of the Titanic.

unsupervised algorithms.R - some more work with clustering

wordcloud.png - example wordcloud.  input = "robot".  some visual effects added for presentation flair.

wordcloud.R - text mining from Twitter that builds a wordcloud based on data collected from search term (input).
