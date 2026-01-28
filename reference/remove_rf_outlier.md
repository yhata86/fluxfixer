# Remove outliers detected by a random forest model

\`remove_rf_outlier()\` detects and removes outliers by a random forest
model whose hyperparameters are calibrated using a grid search approach
and out-of-bag evaluation.

## Usage

``` r
remove_rf_outlier(
  df,
  colname_label,
  vctr_colname_feature = NULL,
  vctr_min_nodesize = c(5),
  vctr_m_try = NULL,
  vctr_subsample_outlier = c(0.1),
  frac_train = 0.75,
  n_tree = 500,
  ran_seed = 12345,
  coef_iqr = 1.5,
  label_err = -9999
)
```

## Arguments

- df:

  A data frame including label (explained variable) and feature
  (explanatory variables) time series for model input. It is acceptable
  to include missing values in each column.

- colname_label:

  A character representing the name of the column for the label time
  series.

- vctr_colname_feature:

  A vector of characters indicating the name of the feature time series
  columns used in constructing a random forest model. If \`NULL\`
  (default), all columns excluding the label column specified as
  \`colname_label\` in the input data frame are used as feature columns.

- vctr_min_nodesize:

  A vector of positive integers indicating candidates of a
  hyperparameter for the random forest model, defining the minimal node
  size (the minimum number of data points included in each leaf node).
  Default is \`c(5)\`.

- vctr_m_try:

  A vector of positive integers indicating candidates of a
  hyperparameter for the random forest model, defining the number of
  features to be used in splitting each node. If \`NULL\` (default),
  integers between two and the number of all feature variables are
  tested.

- vctr_subsample_outlier:

  A vector of numerical values between 0 and 1, indicating candidates of
  a hyperparameter for the random forest model, defining the fraction of
  input training data points to be sampled in constructing the random
  forest. Default is \`c(0.1)\`.

- frac_train:

  A numerical value between 0 and 1, defining the fraction of data
  points to be categorized as training data for the random forest model
  construction. The other data points are classified as test data.
  Default is 0.75.

- n_tree:

  An integer representing the number of trees in the random forest.
  Default is 500.

- ran_seed:

  An integer representing the random seed for the random forest model
  construction. Default is 12345.

- coef_iqr:

  A positive value defining a multiplier of the interquartile range
  (IQR). If the value to be checked is less than Q1 (first quartile) -
  \`coef_iqr\` \* IQR or more than Q3 (third quartile) + \`coef_iqr\` \*
  IQR, the value is detected as a random forest outlier. Default is 1.5.

- label_err:

  A numeric value representing a missing value in the input vector(s).
  Default is -9999.

## Value

A list with two elements. The first element \`mse\` is the mean squared
error between predicted and original values in the test data set. The
second element \`stats\` is a data frame with columns below:

\* The first column, \`cleaned\`, gives the cleaned time series after
replacing the detected outliers with the value specified by
\`label_err\`.

\* The second column, \`flag_out\`, gives a flag variable time series
indicating the status of the cleaned time series (0: the input data
point is not originally missing and not detected as an outlier; 1: the
input data point is not originally missing but detected as an outlier;
2: the input data point is originally missing).

\* The third column, \`med\`, gives the ensemble median time series
calculated from estimated values at each time point for each tree in the
constructed random forest.

\* The fourth column, \`q1\`, gives the ensemble Q1 (first quartile)
time series calculated from estimated values at each time point for each
tree in the constructed random forest.

\* The fifth column, \`q3\`, gives the ensemble Q3 (third quartile) time
series calculated from estimated values at each time point for each tree
in the constructed random forest.

## Details

A random forest model is constructed for the targeted time series to
remove outliers. The time series is assumed to be stationary, so
detrending is needed before inputting if it has a trend. Users can input
any feature from the dataset, and out-of-bag evaluation is used to
determine the hyperparameters. This evaluation is applied to a training
dataset separated from the entire input data. To reduce the
computational cost, the only hyperparameter used by default for grid
search is the number of candidate features. To reduce the risk of
learning noise, the training data sampling ratio is set to 0.1 by
default. After determining the optimal hyperparameters, they are used to
construct the optimal random forest model. Output values are obtained
from 500 (default) trees, and the first quartile (Q1), third quartile
(Q3), and interquartile range (IQR) of the output values at each time
point are calculated. If the targeted value is less than Q1 minus 1.5IQR
or more than Q3 plus 1.5IQR (default), the data point is identified as
an outlier and removed.

## Author

Yoshiaki Hata

## Examples

``` r
## Load data
data(dt_noisy)
df_raw <- dt_noisy[c(12097:14400), ]

## Remove outliers
result <-
  remove_rf_outlier(df = df_raw, colname_label = "dt",
                    vctr_colname_feature = c("sw_in", "vpd", "swc", "p"),
                    coef_iqr = 3.0)$stats
#> Random forest outlier detection started
#> --- Hyperparameter optimization using grid search started
#> --- MSE: Mean square error for out-of-bag data
#> --- Hyperparameter set: [m_try, min_nodesize, subsample]
#> --- MSE: 0.6155865213, Hyperparameter set: [2, 5, 0.1]
#> --- MSE: 0.6308466906, Hyperparameter set: [3, 5, 0.1]
#> --- MSE: 0.6589179693, Hyperparameter set: [4, 5, 0.1]
#> --- Optimal hyperparameter set: [2, 5, 0.1]
#> --- Hyperparameter optimization using grid search finished
#> --- Random forest construction started
#> --- Random forest construction finished
#> Random forest outlier detection finished
```
