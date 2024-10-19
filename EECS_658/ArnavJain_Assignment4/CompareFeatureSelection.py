'''
Program: EECS 658 Assignment 4
Description: Testing different dimentionality reduction to see which works best
Name: Arnav Jain
Date: 9/28/24
Inputs: iris.csv
Outputs: models and their performance
Collaborators: None
Sources: StackOverflow
'''
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
from sklearn.preprocessing import LabelEncoder, StandardScaler
from sklearn.tree import DecisionTreeClassifier
from sklearn.decomposition import PCA
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


# Boilerplate function that takes a name to print and the classifier
def run_classifier(classifier, x, y):
    print("----------------Training on fold 1 and testing on fold 2----------------")

    # Split the data
    f1_train, f2_train, f1_test, f2_test = train_test_split(x,y,test_size=.5, shuffle=True)
    print(f"fold 1 samples: {f1_train.shape[0]}")
    print(f"fold 2 samples: {f2_train.shape[0]}")

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


def simulated_annealing_feature_selection(x, y, pca, n_iterations=100, restart_value=10):
    x = np.hstack((x, pca[:, :4]))

    n_features = x.shape[1]
    current_subset = np.ones(n_features, dtype=bool)
    current_accuracy = evaluate_subset(x[:, current_subset], y)
    best_subset = current_subset.copy()
    best_accuracy = current_accuracy

    for iteration in range(n_iterations):
        new_subset = perturb_subset(current_subset)
        new_accuracy = evaluate_subset(x[:, new_subset], y)

        delta = new_accuracy - current_accuracy
        temperature = 1 - (iteration / n_iterations)
        acceptance_prob = np.exp(delta / temperature)

        random_uniform = np.random.random()

        if new_accuracy > current_accuracy:
            status = "Improved"
            current_subset = new_subset
            current_accuracy = new_accuracy
            if current_accuracy > best_accuracy:
                best_subset = current_subset.copy()
                best_accuracy = current_accuracy
        elif random_uniform < acceptance_prob:
            status = "Accepted"
            current_subset = new_subset
            current_accuracy = new_accuracy
        else:
            status = "Discarded"

        if iteration % restart_value == 0 and iteration > 0:
            status = "Restart"
            current_subset = np.ones(n_features, dtype=bool)
            current_accuracy = evaluate_subset(x[:, current_subset], y)

        print(f"Iteration {iteration + 1}:")
        print(f"  Subset: {get_feature_names(current_subset)}")
        print(f"  Accuracy: {current_accuracy:.4f}")
        print(f"  Pr[accept]: {acceptance_prob:.4f}")
        print(f"  Random Uniform: {random_uniform:.4f}")
        print(f"  Status: {status}")
        print()

    return best_subset, best_accuracy

def evaluate_subset(X, y):
    clf = DecisionTreeClassifier()
    return np.mean(cross_val_score(clf, X, y, cv=5))

def perturb_subset(subset):
    new_subset = subset.copy()
    n_changes = np.random.choice([1, 2])
    indices = np.random.choice(len(subset), n_changes, replace=False)
    new_subset[indices] = ~new_subset[indices]
    return new_subset

def get_feature_names(subset):
    original_features = ['sepal-length', 'sepal-width', 'petal-length', 'petal-width']
    transformed_features = ['z1', 'z2', 'z3', 'z4']
    all_features = original_features + transformed_features
    return [all_features[i] for i in range(len(subset)) if subset[i]]


def main():



    name = "Original Features"
    print(f"--------------------{name}--------------------")
    run_classifier(DecisionTreeClassifier(), x, y)



    name = "Part 2"
    print(f"--------------------{name}--------------------")
    scalar = StandardScaler()
    x_pca = scalar.fit_transform(x)

    pca = PCA()
    x_pca = pca.fit_transform(x_pca)

    print("Eigenvalues:")
    print(pca.explained_variance_)
    print("\nEigenvectors:")
    print(pca.components_)
    pov = np.cumsum(pca.explained_variance_ratio_)
    print("\nProportion of Variance (PoV):")
    print(pov)
    n_components = np.argmax(pov > 0.90) + 1
    print(f"\nNumber of components selected (PoV > 0.90): {n_components}")

    run_classifier(DecisionTreeClassifier(), x_pca, y)

    name = "Part 3"
    print(f"--------------------{name}--------------------")
    best_subset, best_accuracy = simulated_annealing_feature_selection(x, y, x_pca)

    print("Best subset of features:", get_feature_names(best_subset))
    print("Best accuracy:", best_accuracy)

# Select subset of transformed features with PoV > 0.90


if __name__ == '__main__':
    main()
