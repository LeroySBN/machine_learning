MODIFIED NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY DATABASE (MNIST) digits

We will build a machine learning algorithm to apply logistic regression to determine whether a digit is 2 or 7 from predictors. A sample of 10,000 random rows from the training set and 1,000 random rows from the test set will be selected to enable a smooth run.

Load.R
Load required libraries and the data

Preprocessing.R
In this section we transform predictors before running the machine learning algorithm. We also remove predictors that are clearly not useful, in this case, features with zero variability, or almost zero variability.

ModelFitting.R
Implement k-nearest neighbors and random forest on the MNIST data.

analysis.R
We will extract two simple predictors from the 784. These will be the proportion of dark pixels that are in the upper left quadrant and the proportion of pixels that are black in the lower right quadrant. To have a more manageable data set, we will select a random sample of 1,000 digits from the training set that has 60,000 digits. 500 will be in the training set, and 500 will be in the test set.