# Estimating countries’ peace index through the lens of the world news as monitored by GDELT

## Table of contents  
1. [Citing](#Citing)
2. [Abstract](#Abstract) 
3. [Data](#Data)
4. [Machine learning](#Machinelearning)
5. [Extension of the study](#studyextended)

## Citing
<a name="Citing"/>

If you use our approach or code in this repository, please cite our paper:

Voukelatou, V., Pappalardo, L., Miliou, I., and Gabrielli, L. & Giannotti, F., 
Estimating countries’ peace index through the lens of the world news as monitored by GDELT
 <br/>
https://ieeexplore.ieee.org/abstract/document/9260052

`@article{voukelatou2020estimating, `<br/>
  `title={Estimating countries’ peace index through the lens of the world news as monitored by GDELT},` <br/>
  `author={Voukelatou, Vasiliki and Pappalardo, Luca and Miliou, Ioanna and Gabrielli, Lorenzo and Giannotti, Fosca},` <br/>
  `booktitle={2020 IEEE 7th international conference on data science and advanced analytics (DSAA)},` <br/>
  `volume={11},`<br/>
  `pages={216--225},`<br/>
 ` year={2020}` <br>
 `organization={IEEE}` <br>
} `

<a name="Abstract"/>

## Abstract

Peacefulness is a principal dimension of well-being, and its measurement has lately drawn the attention of researchers and 
policy-makers. During the last years, novel digital data streams have drastically changed research in this field. In the current 
study, we exploit information extracted from Global Data on Events, Location, and Tone (GDELT) digital news database, to capture 
peacefulness through the Global Peace Index (GPI). Applying machine learning techniques, we demonstrate that news media attention, 
sentiment, and social stability from GDELT can be used as proxies for measuring GPI at a monthly level. Additionally, through the 
variable importance analysis, we show that each country's socio-economic, political, and military profile emerges. This could bring 
added value to researchers interested in "Data Science for Social Good", to policy-makers, and peacekeeping organizations since 
they could monitor peacefulness almost real-time, and therefore facilitate timely and more efficient policy-making.


<a name="Data"/>

## Data 

All data can be found in the folder named `data`.

### GDELT data
For GPI prediction, we derive several variables from the GDELT database. 

### GPI data
GPI ranks 163 independent states and territories according to their level of peace, and it was created by the Institute for Economics & Peace (IEP). 
GPI data are available at a yearly level. We download the yearly GPI data from the IEP site. 

<a name="Machinelearning"/>

## Machine learning

We used 3 different algorithms for the training of our models, i.e.,
1. Elasticnet (the R code and the packages used can be found in `Elasticnet_prediction.R`),
2. Decision Tree (the python code and the packages used can be found in `Decision_tree_prediction.R`),
3. Random Forest (the python code and the packages used can be found in `Random_forest_prediction.R`), and

<a name="studyextended"/>

## Extension of the study

You can find the extension of the study in the following link: https://doi.org/10.1140/epjds/s13688-022-00315-z
Also, the GitHub repository of the extended study can be found here: https://github.com/VickyVouk/GDELT_GPI_SHAP_project



