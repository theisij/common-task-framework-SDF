## Overview
Repository for the common task framework competition associated with the paper "The Power of the Common Task Framework' by Hellum, Jensen, Kelly, and Pedersen (2025), where the goal is to find the stochastic discount factor, or more precisely, the portfolio with the highest Sharpe ratio. This repo contains examples of script that can be submitted to the competition. The repo is still work-in-progress.

To request data for the competition, go to: [jkpfactors.com/request-ctf-data](https://jkpfactors.com/request-ctf-data)

To submit a model to the competition, go to: [jkpfactors.com/common-task-framework](https://jkpfactors.com/common-task-framework)

## Guidelines for adding a new model
1. Save script in separate folder under 'models_python' if it's a python model, or under 'models_R' if it's an R model, and the main function that we upload to the competition website should have have meaningfull name. For example, I saved the 1/N model as 'models_python/one_over_n/one_over_n.py'
2. Save the csv output in a separate folder under 'data/processed' with a meaningfull name. For example, I saved the 1/N model as 'data/processed/one_over_n/one_over_n.csv'
3. Save the documentation in a separate folder under 'documentation' with a meaningfull name. For example, I saved the 1/N model as 'documentation/one_over_n/one_over_n.pdf'