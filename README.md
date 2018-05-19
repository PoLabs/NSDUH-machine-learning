# NSDUH machine learning

Contents:
- data cleaning: extensive recoding to prep survey data for ML 
- elastic net regularization thins ~200 variables to 25
- neural net using R's 'neuralnet' package
- gradient boosted trees with 'xgboost' package

Old code that attempts to classify respondents as positive/negative for past year opioid use disorder. Opioid use disorder is a relatively rare occurance (<1%) and needs upsampling, so accuracy is always 99%.
