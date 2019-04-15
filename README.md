# machine_learning_tool
Using this code, one can do all the machine learning steps at one place i.e., get a summary of all the variables such as minimum, maximum, percentiles, missing values, etc for numerical, missing and unique values for categorical and a correlation table; Plots to see the distribution; Data Cleaning to treat missing values and outliers; Variable Transformation tab to transform the variables by exponential, squares, square root, etc.; Training tab wherein you can choose the target variable and predictor variable and choose from a list of algorithms to train the dataset, Predict tab to predict the values, Segmentation tab to do clustering. 
This has been built on RShiny and caret package has been used for the modelling.

Given below is the brief description about the tabs created in short-
1. Input tab- You input a .csv file here
2. Explore tab- Here you can see different statistical values for the file input in the 'Input' tab. For numerical columns, you can see the maximum, minimum, skewness, different percentile values, etc and can detect outliers in a much more simple way. For categorical variables you can see the number of missing and unique values for each categorical column and a correlation table to see the correlation between different columns
3. Plot tab- You can choose between histogram, scatterplot, correlation plot and boxplot to visualise the data which has been input in the 'Input' tab.
4. Data Cleaning tab- wait for 5-10 seconds. Can clean a column one-by-one or everything at once. For treating missing values, the option for numerical variables include Mean, Median, Mode and zero value replacement and for categorical it is mode. It is stored in the backend as 'input_file.csv'.
5. Variable Transformation tab- In this tab, you can perform various numerical transformation tasks such as log values, squares of the numerical column, square root of the column chosen, z-score transformation, etc. You can choose multiple columns and multiple transformation types at a time.
6. Training tab- here you choose the file on which you want to train the model ie variable transformed/data cleaned/file from the input tab or a new file altogether. Choose the 'Y' variable. If the problem chosen is of classification type but the values are 1/0, then you need to choose 'Yes' for the box wherein the title is 'Is your variable of classification type?'. The next option is if you want to include all the variables or only select variables. If you want only a few variables in the model, you choose the variables. You then choose the machine learning algorithm you want to run. If you want to fine-tune the parameters of the model, then there is an option for that as well. Then you can choose between the type of model output you want to see- statistical values or the variable importance. The model is then run and the desired output from the option chosen is returned.
7. Predict tab- Upload a .csv file with all the required columns mentioned when creating the model. You then click predict and you will see a table displaying the output. The same is created in a .csv file named 'predicted.csv'. Note that there should be no missing values when uploading the dataset.
8. Segmentation tab- choose a file, then the type of clustering i.e. hierarchial and non-hierarchial, choose the columns and then you get an elbow plot/dendogram to assist you in choosing the number of clusters. Choose the columns, enter the number of clusters, fine-tune the parameters you wish to fine tune, type of output i.e., distribution or the data frame as it is, click on 'Run' button and you get the desired output. The file is stored in a .csv format named 'clusters_created.csv'
