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
data = pd.read_csv("iris.csv", header=None)

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


# Function for sim annealing
def simulated_annealing(x, y, pca, n_iterations=100):
    # combine the x vars and the PCA vars
    x = np.hstack((x, pca[:, :4]))

    # create a list of 1s of size 8 (number of features) that show which are being used and which aren't
    current_subset = np.ones(8, dtype=bool)

    # Get the score
    clf = DecisionTreeClassifier()
    current_accuracy = np.mean(cross_val_score(clf, x[:, current_subset], y, cv=5))

    # Create vars that keep track of the best
    best_subset = current_subset.copy()
    best_accuracy = current_accuracy

    for iteration in range(n_iterations):
        # Perturb the subset and then evaluate
        new_subset = perturb_subset(current_subset)
        new_accuracy = np.mean(cross_val_score(clf, x[:, new_subset], y, cv=5))


        # get the probability it gets accepted by doing e^(-100/1 * (old - new) / old) and a random var
        pr_accept = np.exp(-n_iterations * (current_accuracy-new_accuracy) / current_accuracy)
        random_uniform = np.random.random()

        # If it is better, keep it and have the status as improved
        if new_accuracy > current_accuracy:
            status = "Improved"
            current_subset = new_subset
            current_accuracy = new_accuracy
            # If it is better than the best then change the best vars
            if current_accuracy > best_accuracy:
                best_subset = current_subset.copy()
                best_accuracy = current_accuracy
        # Otherwise, if the random var is lower than pr_accept, keep it
        elif random_uniform < pr_accept:
            status = "Accepted"
            current_subset = new_subset
            current_accuracy = new_accuracy
        # Otherwise discard it
        else:
            status = "Discarded"

        # Reset every 10 iterations (set the current to the best)
        if iteration % 10 == 0 and iteration > 0:
            status = "Restart"
            current_subset = best_subset
            current_accuracy = best_accuracy

        # Print the results
        print(f"Iteration {iteration + 1}:")
        print(f"  Subset: {get_feature_names(current_subset)}")
        print(f"  Accuracy: {current_accuracy:.4f}")
        print(f"  Pr[accept]: {pr_accept:.4f}")
        print(f"  Random Uniform: {random_uniform:.4f}")
        print(f"  Status: {status}\n")

    x = x[:, best_subset]
    run_classifier(DecisionTreeClassifier(), x, y)
    return best_subset, best_accuracy


# Perturb by creating a copy, randomly deleting one or 2, and then setting them to0
def perturb_subset(subset):
    new_subset = subset.copy()
    n_changes = np.random.choice([1, 2])
    indices = np.random.choice(len(subset), n_changes, replace=False)
    new_subset[indices] = ~new_subset[indices]
    return new_subset

# Gets the name of the featurs
def get_feature_names(subset):
    features = ['sepal-length', 'sepal-width', 'petal-length', 'petal-width', 'z1', 'z2', 'z3', 'z4']
    # If the subset has a 1 for each index, it returns the name associated with that index
    return [features[i] for i in range(len(subset)) if subset[i]]

# Func for gen alg
def genetic_algorithm(x, y, x_pca, population_size=5, n_generations=50):
    # Combine featurs, same as before
    x = np.hstack((x, x_pca[:, :4]))

    # Init the classifier
    clf = DecisionTreeClassifier()
    
    # initial population, chosen at random
    population = [
        [1, 1, 1, 1, 1, 0, 0, 0],  # sepal-length, sepal-width, petal-length, petal-width, z1 
        [1, 0, 1, 1, 1, 1, 0, 0],  # sepal-length, petal-length, petal-width, z1, z2
        [0, 0, 0, 1, 1, 1, 1, 1],  # petal-width, z1, z2, z3, z4
        [0, 1, 1, 0, 0, 1, 1, 1],  # sepal-width, petal-length, z2, z3, z4 
        [1, 1, 0, 0, 0, 1, 1, 1],  # sepal-length, sepal-width, z2, z3, z4
    ]
    
    feature_names = ['sepal-length', 'sepal-width', 'petal-length', 'petal-width', 'z1', 'z2', 'z3', 'z4']
    
    for generation in range(n_generations):
        # Get how fit the population is 
        fitnesses = []
        for individual in population:
            # Create a mask and then get the fitness (same logic as before) and then save it
            mask = np.array(individual).astype(bool)
            accuracy = np.mean(cross_val_score(clf, x[:, mask], y, cv=5))

            fitnesses.append(accuracy)
        
        # sort the population by the fitness by combining first
        temp = list(zip(population, fitnesses))

        # Then sort by the score
        temp.sort(key=lambda x: x[1], reverse=True)

        # seperate the lists
        sorted_population = []
        sorted_fitness = []
        for elem in temp:
            sorted_population.append(elem[0])
            sorted_fitness.append(elem[1])

        
        
        # Print top 5 individuals
        print(f"\nGeneration {generation + 1}:")
        for i in range(5):
            selected_features = []
            # Go through each of the 8 features and add the name
            for j in range(8):
                if sorted_population[i][j] == 1:
                    selected_features.append(feature_names[j])
            print(f"Rank {i + 1}: Features: {selected_features}, Accuracy: {sorted_fitness[i]:.4f}")
        
        # reset the pop and get the 2 best individuals
        population = []
        population = sorted_population[:2]
        
        # For new individuals, do crossover and mutations (parents are top 3)
        while len(population) < population_size:
            parent1, parent2 = np.random.choice([0,1,2], 2)
            parent1 = sorted_population[parent1]
            parent2 = sorted_population[parent2]
            child = crossover(parent1, parent2)
            child = mutate(child)
            population.append(child)
        
    
    # Return the top performer and its features using same logic as above
    best = sorted_population[0]
    best_features = []
    for j in range(8):
        if best[j] == 1:
            best_features.append(feature_names[j])
    best_accuracy = sorted_fitness[0]


    # Get a list of the indexes of the best features, then change the x value to only those features and run the classifer
    best_indexes = [feature_names.index(feature) for feature in best_features]
    x = x[:, best_indexes]
    run_classifier(DecisionTreeClassifier(), x, y)
    
    return best_features, best_accuracy


# Get a random point and do crossover from that point
def crossover(parent1, parent2):
    crossover_point = np.random.randint(1, len(parent1))
    child = parent1[:crossover_point] + parent2[crossover_point:]
    return child

# Randomly changes the input based on a rate
def mutate(individual, mutation_rate=0.1):
    mutated_individual = []
    for gene in individual:
        if np.random.random() < mutation_rate:
            # This flips it, if it is 0 then it becomes 1 otherwise stays 0
            mutated_individual.append(1 - gene)
        else:
            mutated_individual.append(gene)
    return mutated_individual


def main():

    # From prev
    name = "Original Features"
    print(f"--------------------{name}--------------------")
    run_classifier(DecisionTreeClassifier(), x, y)



    # creates PCA then fits
    name = "Part 2"
    print(f"--------------------{name}--------------------")

    pca = PCA()
    x_pca = pca.fit_transform(x)

    # Prints the eigen vals
    print("Eigenvalues:")
    print(pca.explained_variance_)
    
    # Prints the eigenvecs
    print("\nEigenvectors:")
    print(pca.components_)
    # Gets the cumulative sum and prints
    pov = np.cumsum(pca.explained_variance_ratio_)
    print("\nProportion of Variance (PoV):")
    print(pov)
    # Gets the index of the component where it is first >.9 then adds 1 and prints
    n_components = np.argmax(pov > 0.90) + 1
    print(f"\nNumber of components selected (PoV > 0.90): {n_components}")
    run_classifier(DecisionTreeClassifier(), x_pca, y)

    # Runs the sim anneal function and then prints with x, y, and the x_pca values
    name = "Part 3"
    print(f"--------------------{name}--------------------")
    best_subset, best_accuracy = simulated_annealing(x, y, x_pca)

    print("Best subset of features:", get_feature_names(best_subset))
    print("Best accuracy:", best_accuracy)

    

    name = "Part 4"
    print(f"--------------------{name}--------------------")

    best_features, acc = genetic_algorithm(x,y,x_pca)


    print(f"Best features: {best_features}")
    print(f"Accuracy: {acc}")


if __name__ == '__main__':
    main()
