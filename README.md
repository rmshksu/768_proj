room 10 wednesday 230
# 768_proj
Final project for STAT 768 at KSU, Applied Bayesian Modeling and Prediction

Data we need N_drugs -> # of deaths and also N_users -> # of users
We need an estimate of the number of users seeking drugs in Kansas (need sources for number of users seeking drugs in Kansas)

z_t is the true overdose count 
y_t is the observed overdose count
[z_t|y_t] = NB(y_t, phi_t) 
[y_t|lambda_t] = Pois(lambda_t)

Here lambda is associated with supply chain network with weights
[lambda_t] = [something with networks]

The network comes in at P, the network is a parameter model for P. 

We can estimate lambda from the data or assume it

Results of the model -> 

RESEARCH GOAL: Inform primary prevention practices for Fentanyl overdose

RESEARCH QUESTION: 

PRIMARY

1. What are the primary spatial sources for Fentanyl overdose (Within the study area of Kansas)?

2. What is the most effecient practice for reduction of Fentanyl overdose (Within the study area of Kansas)?

SECONDARY

1. How much of drug overdose can be linked to Fentanyl?

2. How do our results change as our study area expands?

DEVELOPMENT STAGES

- Stage 1: Gathering of resources

- Stage 2: Cleaning data and organizing citations

- Stage 3: Model development

- sStage 3a: Mathematical model

- sStage 3b: Probabilistic model

- sStage 3c: Programming the total model

- Stage 4: Model fitting and checking

- Stage 5: Writing and building our presentation

- sStage 5a: Write report

- sStage 5b: Write presentation

- sStage 5c: Write tutorial

