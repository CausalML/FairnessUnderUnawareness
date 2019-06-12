This repo contains the code for replicating the figures of the following paper:

[Chen, Jiahao, et al. "Fairness Under Unawareness: Assessing Disparity When Protected Class Is Unobserved." Proceedings of the Conference on Fairness, Accountability, and Transparency. ACM, 2019.](https://arxiv.org/pdf/1811.11154.pdf)


The data downloading instruction and data preprocessing instruction are included in the data_cleaning.Rmd. After downloading the data and obtaining the cleaned data "hmda\_census\_combined.csv", the figures in the paper can be replicated via the code in data\_analysis\_fat.Rmd. We used a stratified subsample (except the last figure) to speed up the plotting, so the figures may be slightly different from the figures in the paper that use the whole sample. 
