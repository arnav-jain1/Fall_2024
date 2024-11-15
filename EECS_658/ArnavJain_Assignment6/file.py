import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.mixture import GaussianMixture
from sklearn.preprocessing import LabelEncoder
from sklearn.cluster import KMeans
from sklearn.metrics import confusion_matrix, accuracy_score
from scipy.optimize import linear_sum_assignment
from minisom import MiniSom
from sklearn.preprocessing import MinMaxScaler
# Get the data and split it
data = pd.read_csv("iris.csv")

x = data.iloc[:, :-1]

y = data.iloc[:, -1]

# Encode the target variable so it is 0, 1, 2 instead of a string
encoder = LabelEncoder()
y = encoder.fit_transform(y)

# Recombine the data
data = np.column_stack((x, y))

# Calculates the error by doing Kmeans and then returning the inertia
def calculate_reconstruction_error(k, x):
    kmeans = KMeans(n_clusters=k, random_state=615).fit(x)
    return kmeans.inertia_



# Evals the clustering
def evaluate_clustering(x, y, n_clusters):
    # Evaluate with inputed clusters
    kmeans = KMeans(n_clusters=n_clusters, random_state=615)
    cluster_labels = kmeans.fit_predict(x)
    
    # get the conf matrix
    conf_matrix = confusion_matrix(y, cluster_labels)
    
    print("\nConfusion Matrix:")
    print(conf_matrix)
    
    # gets the optimal mapping, remaps it, and then gets the accuracy
    col_ind = linear_sum_assignment(-conf_matrix)[1]
    remapped_labels = np.zeros_like(cluster_labels)
    for i, label in enumerate(col_ind):
        remapped_labels[cluster_labels == label] = i
        
    # Get acc
    acc = accuracy_score(y, remapped_labels)
    print(f"\nAccuracy Score: {acc:.4f}")

# Calculate the errors for range of [1,20]
errors = [calculate_reconstruction_error(k, data) for k in range(1, 21)]

print("Part 1: K-means Clustering")
print("\nPlotting Elbow Curve")
# Plot
plt.figure(figsize=(10, 6))
plt.plot(range(1,21), errors, 'bo-')
plt.xlabel('Number of Clusters (k)')
plt.ylabel('Reconstruction Error')
plt.title('Elbow Curve for K-means Clustering')
plt.grid(True)
plt.show()


# set elbow to 3 (based on results)
print("\n k = 3:")
evaluate_clustering(x, y, 3)


print("Question 1: Yes because k=3 is the elbow and there are 3 classes")



def calculate_metrics_vs_k(x):
    # Gets the aic and bic scores first make empty lists
    aic_scores = []
    bic_scores = []
    
    # For k between 1 and 20, do gmm and then append the aic and bic result
    for k in range(1,21):
        gmm = GaussianMixture(n_components=k, covariance_type='diag', random_state=615)
        gmm.fit(x)
        aic_scores.append(gmm.aic(x))
        bic_scores.append(gmm.bic(x))
    
    # Return the scores
    return aic_scores, bic_scores

def plot_metric_curve(scores, metric_name):
    # Plots the curve, takes metric name depending on which metric it is
    plt.figure(figsize=(10, 6))
    plt.plot(range(1,21), scores, 'bo-')
    plt.xlabel('Number of Components (k)')
    plt.ylabel(metric_name)
    plt.title(f'{metric_name} vs Number of Components')
    plt.grid(True)
    plt.show()

def evaluate_clusteringGMM(x, y, n_components):
    # Do the gmm clustering with the number of components
    gmm = GaussianMixture(n_components=n_components, covariance_type='diag', random_state=615)
    cluster_labels = gmm.fit_predict(x)
    
    # Calculate confusion matrix
    conf_matrix = confusion_matrix(y, cluster_labels)
    print("\nConfusion Matrix:")
    print(conf_matrix)
    
    # gets the optimal mapping, remaps it, and then gets the accuracy (same as before)
    col_ind = linear_sum_assignment(-conf_matrix)[1]
    
    remapped_labels = np.zeros_like(cluster_labels)
    for i, j in enumerate(col_ind):
        remapped_labels[cluster_labels == j] = i
        
    acc = accuracy_score(y, remapped_labels)
    print(f"\nAccuracy Score: {acc:.4f}")



aic_scores, bic_scores = calculate_metrics_vs_k(x)

# Plot AIC 
plot_metric_curve(aic_scores, 'AIC')

# Plot BIC 
plot_metric_curve(bic_scores, 'BIC')

# Based on graph
aic_elbow_k = 3  
bic_elbow_k = 3  

print("\nGMM Results using AIC elbow k =", aic_elbow_k)
evaluate_clusteringGMM(x, y, aic_elbow_k)

print("\nGMM Results using BIC elbow k =", bic_elbow_k)
evaluate_clusteringGMM(x, y, bic_elbow_k)

# Answer the questions
print("\nQuestion 2a: Yes because again k=3 is the elbow and thats how many classes there are")
print("\nQuestion 2b: Yes same reason")


# Normalize the features using Min-Max scaling
scaler = MinMaxScaler()
x = scaler.fit_transform(x)




def train_and_evaluate_som(x, y, grid_size, title_prefix=""):
    # Initialize the SOM with vars
    som = MiniSom(grid_size, grid_size, x.shape[1], sigma=1.0, learning_rate=0.5, random_seed=615)
    som.random_weights_init(x)

    # Train it for 50k iterations
    som.train_random(x, 50000)
    
    # calc quant error
    qe = som.quantization_error(x)
    
    # Create the figure (the subplots)
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 5))
    
    # Plot U-Matrix
    um = som.distance_map()
    ax1.imshow(um, cmap='bone_r')
    ax1.set_title(f'{title_prefix} U-Matrix ({grid_size}x{grid_size})')
    
    # Initialize the colors
    colors = ['r', 'g', 'b']
    

    # Get the winning position for each sample
    for i, sample in enumerate(x):
        winner = som.winner(sample)
        # Plot the winner in the subplot
        ax2.scatter(winner[0], winner[1], 
                   c=colors[np.where(np.unique(y) == y[i])[0][0]],
                   marker='o')
    
    # Show the full plot
    ax2.set_title(f'{title_prefix} SOM Response ({grid_size}x{grid_size})')
    plt.tight_layout()
    plt.show()
    
    return qe


# Train SOMs with the given grid sizes
grid_sizes = [3, 7, 15, 25]
# Create an empty list to save the errors
quantization_errors = []

# Go through each of the sizes and then train it
for size in grid_sizes:
    print(f"\nTraining {size}x{size}")
    qe = train_and_evaluate_som(x, y, size, f"{size}x{size}")
    # Then print and append the error
    print(f"Quantization Error for {size}x{size} grid: {qe:.4f}")
    quantization_errors.append(qe)

# Plot quantization error vs grid sizes
plt.figure(figsize=(10, 6))
plt.plot(grid_sizes, quantization_errors, 'bo-')
plt.xlabel('Grid Size')
plt.ylabel('Quantization Error')
plt.title('Quantization Error vs Grid Size')
plt.grid(True)
plt.show()

# Print analysis and answers to questions
print("\nQuestion 3a: The elbow is between 7x7 and 15x15 so I will say 7x7")
print("\nQuestion 3b: The better the grid size, the better the lower the error but it reaches a point of overfitting. It also takes longer to train")


print("\nQuestion 3c: 7x7 because the dataset only has 150 samples, 25x25 would be too much and wouldn't actually be a good classifier")
