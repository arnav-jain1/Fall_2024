import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import LabelEncoder
from sklearn.cluster import KMeans
from sklearn.metrics import confusion_matrix, accuracy_score
from scipy.optimize import linear_sum_assignment

data = pd.read_csv("iris.csv")

# x vals are all cols except the last one
x = data.iloc[:, :-1]

# y is the opposite
y = data.iloc[:, -1]

# Encode the target variable so it is 0, 1, 2 instead of a string
encoder = LabelEncoder()
y = encoder.fit_transform(y)

def calculate_reconstruction_error(k, X):
    """Calculate reconstruction error (inertia) for k-means clustering."""
    kmeans = KMeans(n_clusters=k, random_state=615)
    kmeans.fit(X)
    return kmeans.inertia_

def plot_elbow_curve(errors, k_range):
    """Plot the elbow curve for k-means clustering."""
    plt.figure(figsize=(10, 6))
    plt.plot(k_range, errors, 'bo-')
    plt.xlabel('Number of Clusters (k)')
    plt.ylabel('Reconstruction Error')
    plt.title('Elbow Curve for K-means Clustering')
    plt.grid(True)
    plt.show()

def get_optimal_mapping(confusion_mat):
    """Get optimal mapping between true labels and cluster labels."""
    row_ind, col_ind = linear_sum_assignment(-confusion_mat)
    return col_ind

def evaluate_clustering(X, y, n_clusters):
    """Evaluate k-means clustering with specified number of clusters."""
    kmeans = KMeans(n_clusters=n_clusters, random_state=615)
    cluster_labels = kmeans.fit_predict(X)
    
    # Calculate confusion matrix
    conf_matrix = confusion_matrix(y, cluster_labels)
    
    print("\nConfusion Matrix:")
    print(conf_matrix)
    
    if n_clusters == 3:
        # Get optimal mapping between true labels and cluster labels
        optimal_mapping = get_optimal_mapping(conf_matrix)
        
        # Remap cluster labels
        remapped_labels = np.zeros_like(cluster_labels)
        for i, j in enumerate(optimal_mapping):
            remapped_labels[cluster_labels == j] = i
            
        # Calculate accuracy
        acc = accuracy_score(y, remapped_labels)
        print(f"\nAccuracy Score: {acc:.4f}")
    else:
        print("\nCannot calculate Accuracy Score because the number of classes is not the same as the number of clusters")

# Calculate reconstruction error for k=1 to k=20
k_range = range(1, 21)
errors = [calculate_reconstruction_error(k, data) for k in k_range]

print("Part 1: K-means Clustering Analysis")
print("\nPlotting Elbow Curve...")
plot_elbow_curve(errors, k_range)

# For this implementation, we'll set elbow_k to 4 based on visual inspection
# Note: The actual elbow point might vary based on interpretation
elbow_k = 4

print(f"\nAnalysis with elbow_k = {elbow_k}:")
evaluate_clustering(x, y, 4)

print("\nAnalysis with k = 3:")
evaluate_clustering(x, y, 3)


