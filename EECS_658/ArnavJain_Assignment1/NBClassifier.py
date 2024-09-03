from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
import pandas as pd

# Load the iris dataset
data = pd.read_csv("./iris.csv", header=None)

# x vals are all cols except the last one
x = data.iloc[:, :-1]

# y is the opposite
y = data.iloc[:, -1]

# Split the data
f1_train, f2_train, f1_test, f2_test = train_test_split(x,y,test_size=.5, shuffle=True)
print(f"fold 1 samples: {f1_train.shape[0]}")
print(f"fold 2 samples: {f2_train.shape[0]}")


print("-------------------------Naive Bayes Classifier-------------------------")
print("----------------Training on fold 1 and testing on fold 2----------------")


# Create a GaussianNB classifier, train
Guassian = GaussianNB()
Guassian.fit(f1_train, f1_test)

# Predict it on all of X as mentioned in the assignment
predicted = Guassian.predict(x)

# Predict it on the test set
predicted_test = Guassian.predict(f2_train)
print(f"Accuracy on fold 2: {accuracy_score(f2_test, predicted_test):.4f}")


# Print overall accuracy
accuracy = accuracy_score(y, predicted)
print(f"Overall Accuracy: {accuracy:.4f}")

# Print confusion matrix
matrix = confusion_matrix(y, predicted)
print("\nConfusion Matrix:")
print(matrix)

# Print classification report (includes precision, recall, and F1-score for each class)
report = classification_report(y, predicted, target_names=data.iloc[:, -1].unique())
print("\nClassification Report:")
print(report)


# Repeat but with the folds reversed
print("----------------Training on fold 2 and testing on fold 1----------------")

# Create a GaussianNB classifier, train
Guassian = GaussianNB()
Guassian.fit(f2_train, f2_test)

# Predict it on all of X as mentioned in the assignment
predicted = Guassian.predict(x)

# Predict it on the train set
predicted_train = Guassian.predict(f1_train)
print(f"Accuracy on fold 1: {accuracy_score(f1_test, predicted_train):.4f}")


# Print overall accuracy
accuracy = accuracy_score(y, predicted)
print(f"Overall Accuracy: {accuracy}")

# Print confusion matrix
matrix = confusion_matrix(y, predicted)
print("\nConfusion Matrix:")
print(matrix)

# Print classification report (includes precision, recall, and F1-score for each class)
scores = classification_report(y, predicted, target_names=data.iloc[:, -1].unique())
print("\nClassification Report:")
print(scores)