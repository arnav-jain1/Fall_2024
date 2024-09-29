'''
Program: EECS 658 Assignment 3
Description: Various different classifiers tested on Iris dataset
to see which performs the best (with more)
Name: Arnav Jain
Date: 9/28/24
Inputs: iris.csv
Outputs: models and their performance
Collaborators: None
Sources: StackOverflow
'''
from sklearn.pipeline import make_pipeline
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import LabelEncoder, PolynomialFeatures
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis, QuadraticDiscriminantAnalysis
from sklearn import svm
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier
from sklearn.neural_network import MLPClassifier
import pandas as pd
import numpy as np
# Load the iris dataset
data = pd.read_csv("./iris.csv", header=None)

# x vals are all cols except the last one
x = data.iloc[:, :-1]

# y is the opposite
y = data.iloc[:, -1]

# Encode the target variable so it is 0, 1, 2 instead of a string
encoder = LabelEncoder()
y = encoder.fit_transform(y)

# Split the data
f1_train, f2_train, f1_test, f2_test = train_test_split(x,y,test_size=.5, shuffle=True)
print(f"fold 1 samples: {f1_train.shape[0]}")
print(f"fold 2 samples: {f2_train.shape[0]}")

# Boilerplate function that takes a name to print and the classifier
def run_classifier(name, classifier):
    print(f"--------------------{name}--------------------")
    print("----------------Training on fold 1 and testing on fold 2----------------")

    # Fit the model on fold 1
    model = classifier
    model.fit(f1_train, f1_test)

    # Predict it on fold 2, then round result (for regression) and make sure it goes from 0 to 2
    y_pred = model.predict(f2_train)
    y_pred = np.clip(np.round(y_pred), 0, 2).astype(int)


    print(f"Accuracy on fold 2: {accuracy_score(f2_test, y_pred):.4f}")

    # Do the same but for the full daataset
    y_pred = model.predict(x)
    y_pred = np.clip(np.round(y_pred), 0, 2).astype(int)

    # print confusion matrix
    matrix = confusion_matrix(y, y_pred)
    print("\nConfusion Matrix on full dataset:")
    print(matrix)

    # Print the report with the names being the values of the last column 
    report = classification_report(y, y_pred, target_names=data.iloc[:, -1].unique())
    print("\nClassification Report for full dataset:")
    print(report)

    # Repeat entire process but train on fold 2 instead
    print("----------------Training on fold 2 and testing on fold 1----------------")
    model = classifier
    model.fit(f2_train, f2_test)

    y_pred = model.predict(f1_train)
    y_pred = np.clip(np.round(y_pred), 0, 2).astype(int)
    print(f"Accuracy on fold 1: {accuracy_score(f1_test, y_pred):.4f}")

    y_pred = model.predict(x)
    y_pred = np.clip(np.round(y_pred), 0, 2).astype(int)
    matrix = confusion_matrix(y, y_pred)
    print("\nConfusion Matrix on full dataset:")
    print(matrix)

    report = classification_report(y, y_pred, target_names=data.iloc[:, -1].unique())
    print("\nClassification Report for full dataset:")
    print(report)

def main():
    # Run the classifiers
    run_classifier("Linear Regression", LinearRegression())
    # Make sure degrees is 2 or 3 respectively
    run_classifier("Polynomial Regression (degree 2)", make_pipeline(PolynomialFeatures(degree=2), LinearRegression()))
    run_classifier("Polynomial Regression (degree 3)", make_pipeline(PolynomialFeatures(degree=3), LinearRegression()))
    run_classifier("Naive Bayes", GaussianNB())
    run_classifier("K-Nearest Neighbors", KNeighborsClassifier(n_neighbors=3)) # Make sure neighbors is 3 because thats the num of classes
    run_classifier("Linear Discriminant Analysis", LinearDiscriminantAnalysis())
    run_classifier("Quadratic Discriminant Analysis", QuadraticDiscriminantAnalysis())



    # New classifiers
    run_classifier("Support Vector Machine", svm.LinearSVC(dual=False)) # Turn off dual for more control
    run_classifier("Decision Tree", DecisionTreeClassifier())
    run_classifier("Random Forest", RandomForestClassifier())
    run_classifier("Extra Trees", ExtraTreesClassifier())
    run_classifier("Neural Network", MLPClassifier(max_iter=1000)) # Set max to 1000 so it converges
if __name__ == '__main__':
    main()
