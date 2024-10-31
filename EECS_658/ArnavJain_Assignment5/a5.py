'''
Program: EECS 658 Assignment 5
Description: Testing different sampling methods
Name: Arnav Jain
Date: 10/23/24
Inputs: imbalanced_iris.csv
Outputs: models and their performance
Collaborators: None
Sources: StackOverflow, ChatGPT

'''
import pandas as pd
import numpy as np
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import confusion_matrix, accuracy_score, balanced_accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from imblearn.over_sampling import RandomOverSampler, SMOTE, ADASYN
from imblearn.under_sampling import RandomUnderSampler, ClusterCentroids, TomekLinks

# Load the iris dataset
data = pd.read_csv("imbalanced iris.csv")

# x vals are all cols except the last one
x = data.iloc[:, :-1]

# y is the opposite
y = data.iloc[:, -1]

# Encode the target variable so it is 0, 1, 2 instead of a string
encoder = LabelEncoder()
y = encoder.fit_transform(y)

# Set up the classifier w hidden layer size of 15 (only 1) and 2k iterations
nn = MLPClassifier(hidden_layer_sizes=(15,), max_iter=2000)

def run_classifier(name, x, y, balance=False):
    print(f"--------------------{name}--------------------")
    print("----------------Training on fold 1 and testing on fold 2----------------")
    # Split data into 2 folds
    f1_train, f2_train, f1_test, f2_test = train_test_split(x,y,test_size=.5, shuffle=True)

    # Fit the model on fold 1
    nn.fit(f1_train, f1_test)

    # Predict it on fold 2, then print accuracy
    y_pred = nn.predict(f2_train)

    print(f"Accuracy on fold 2: {accuracy_score(f2_test, y_pred):.4f}")

    # Do the same but for the full daataset
    y_pred = nn.predict(x)

    # print confusion matrix (full dataset)
    matrix = confusion_matrix(y, y_pred)
    print("\nConfusion Matrix on full dataset:")
    print(matrix)

    # Print the accuracy score for the fulle dataset
    acc = accuracy_score(y, y_pred)
    print("\nAccuracy Score for full dataset:")
    print(acc)


    # Only for the first one
    if balance:
        # 3 arrays to keep track of precision, recall, and specificity for each of the 3 datapoints
        precision = [0,0,0]
        recall = [0,0,0]
        specificity = [0,0,0]
        
        # Calculate true pos, false pos, false neg, true neg, and then use it to get precision recall for each label
        for i in range(3):
            tp = matrix[i, i]
            fp = np.sum(matrix[:, i]) - tp
            fn = np.sum(matrix[i, :]) - tp
            tn = np.sum(matrix) - tp - fp - fn
            
            precision[i] = tp / (tp + fp)
            recall[i] = tp / (tp + fn)
            specificity[i] = tn / (tn + fp)
        

        # Print the balanced acc which is the avg of the min of the precision/recall for each label
        class_bal_acc = np.mean(np.minimum(precision, recall))
        print("\nClass Balanced Accuracy for full dataset:", class_bal_acc)

        # Calc the balanced acc which is the mean of the mean of the recall/specificity 
        bal_acc = np.mean(np.mean([recall, specificity], axis=0))
        print("\nBalanced Accuracy for full dataset:", bal_acc)

        # Do the sklearn balanced acc
        sklearn_balanced_acc = balanced_accuracy_score(y, y_pred)
        print("\nSKLearn Balanced Accuracy for full dataset:", sklearn_balanced_acc)

    # Repeat entire process but train on fold 2 instead
    print("----------------Training on fold 2 and testing on fold 1----------------")
    nn.fit(f2_train, f2_test)

    y_pred = nn.predict(f1_train)
    print(f"Accuracy on fold 1: {accuracy_score(f1_test, y_pred):.4f}")

    y_pred = nn.predict(x)

    matrix = confusion_matrix(y, y_pred)
    print("\nConfusion Matrix on full dataset:")
    print(matrix)

    acc = accuracy_score(y, y_pred)
    print("\nAccuracy Score for full dataset:")
    print(acc)


    if balance:
        precision = [0,0,0]
        recall = [0,0,0]
        specificity = [0,0,0]
        
        for i in range(3):
            tp = matrix[i, i]
            fp = np.sum(matrix[:, i]) - tp
            fn = np.sum(matrix[i, :]) - tp
            tn = np.sum(matrix) - tp - fp - fn
            
            precision[i] = tp / (tp + fp)
            recall[i] = tp / (tp + fn)
            specificity[i] = tn / (tn + fp)
        

        class_bal_acc = np.mean(np.minimum(precision, recall))
        print("\nClass Balanced Accuracy for full dataset:", class_bal_acc)

        bal_acc = np.mean(np.mean([recall, specificity], axis=0))
        print("\nBalanced Accuracy for full dataset:", bal_acc)

        sklearn_balanced_acc = balanced_accuracy_score(y, y_pred)
        print("\nSKLearn Balanced Accuracy for full dataset:", sklearn_balanced_acc)


def main():
    # Run the classifier for the regular set
    print(f"--------------------------PART 1--------------------------")
    run_classifier("Part 1", x, y, True)

    # Run it for each oversampling method, first is random then SMOTE then ADASYN
    print(f"\n\n--------------------------PART 2--------------------------")
    ros = RandomOverSampler()
    x_ros, y_ros = ros.fit_resample(x,y)
    run_classifier("Part 2 Random Oversampling", x_ros, y_ros)

    ros = SMOTE()
    x_ros, y_ros = ros.fit_resample(x,y)
    run_classifier("Part 2 SMOTE Oversampling", x_ros, y_ros)

    ros = ADASYN(sampling_strategy="minority")
    x_ros, y_ros = ros.fit_resample(x,y)
    run_classifier("Part 2 ADASYN Oversampling", x_ros, y_ros)


    # Run it for each undersampling method, first is random then clusters then Tomek Links
    print(f"\n\n--------------------------PART 3--------------------------")
    rus = RandomUnderSampler()
    x_rus, y_rus = rus.fit_resample(x, y)
    run_classifier("Part 3 Random Undersampling", x_rus, y_rus)


    rus = ClusterCentroids()
    x_rus, y_rus = rus.fit_resample(x, y)
    run_classifier("Part 3 Cluster Undersampling", x_rus, y_rus)

    rus = TomekLinks()
    x_rus, y_rus = rus.fit_resample(x, y)
    run_classifier("Part 3 Tomek Link Undersampling", x_rus, y_rus)


if __name__ == "__main__":
    main()
