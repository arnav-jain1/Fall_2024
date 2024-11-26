'''
Program: EECS 658 Assignment 7
Description: Different RL to Gridworld
Name: Arnav Jain
Date: 11/25/24
Inputs: None
Outputs: Value/policy itereration algorithm results
Collaborators: None
Sources: StackOverflow, Google, ChatGPT, Anthropic Claude
'''
import numpy as np
import matplotlib.pyplot as plt
import time

class Gridworld:
    def __init__(self):
        # params for training given by assignment
        self.size = 5
        self.gamma = 0.25  
        
        # Create empty grid to represent the "world"
        self.grid = np.zeros((5, 5))
        
        # Goal states
        self.end = [(0, 0), (4, 4)]
        
        # Possible actions like up, down, right, left
        self.moves = [(-1, 0), (0, 1), (1, 0), (0, -1)]
    
    def get_next_state(self, state, action):
        # Get the new pos
        new_x = state[0] + action[0]
        new_y = state[1] + action[1]
        
        # Make sure the new position is valid, if so return. Otherwise return the old
        if 0 <= new_x <= 4 and 0 <= new_y <= 4:
            return (new_x, new_y)
        return state
    
    def get_reward(self, state):
        # Reward function

        # if the sttae is in the end array then it reached the goal and return 0
        if state in self.end:  
            return 0

        # All other moves return -1
        return -1


class PolicyIteration(Gridworld):
    def __init__(self):
        super().__init__()
        
        # The v values are the same dim of the grid
        self.values = self.grid.copy()

        # errors for plotting
        self.errors = []

    def policy_evaluation(self):

        # Arbitrary threshold and error to keep track of
        threshold = 1e-3
        error = float('inf')
        self.errors = []

        iteration = 0
        start = time.time()


        # Keep evaluating until the error is less than the threshold (converge)
        while error > threshold:
            # save the old values to lookup for calc new values
            old_values = self.values.copy()

            # Go through each state
            for x in range(5):
                for y in range(5):

                    # If the state is in the end skip this iteration
                    if (x, y) in self.end:
                        self.values[x, y] = 0
                        continue
                    
                    # Save the state
                    state = (x, y)
                    # Set the new val to -1
                    new_value = -1

                    # Store the old value
                    old_value = old_values[x, y]
                    
                    # For each move, add 0.25 * (reward + gamma * each move val) to the current value
                    for move in self.moves:
                        # Get next state and if it is not the current state (ie not prohibited by wall)
                        next_state = self.get_next_state(state, move)
                        if next_state == state:
                            val = old_values[x, y]
                        else:
                            val = old_values[next_state[0], next_state[1]]

                        
                        # Multiply by 0.25 (given) and add it to the new value
                        new_value += .25 * val
                    # Update the value for this state
                    self.values[x, y] = new_value

                            
                    # The new error is the min between the running error and old val - new val
                    error = min(error, abs(new_value - old_value))
            # Store the error
            self.errors.append(error)

            iteration += 1
        
        print(f"Finished after {iteration} iterations and {time.time() - start} seconds")



                    

    def train(self):
        print("Starting")
        
        # Start 
        self.policy_evaluation() 
        
        # Basic plotting
        plt.figure(figsize=(8, 5))
        plt.plot(self.errors)
        plt.xlabel('Iteration')
        plt.ylabel('Error')
        plt.title('Policy iteration graph error')
        plt.grid(True)
        plt.show()


    def show_policy(self):

        # Create an array for the policy (0s for now)
        policy = np.zeros((5, 5))


        # Arrows to correspond move with printing
        arrows = ['^', '>', 'V', '<']
        
        print("\nBest Actions:")

        # Go through each square
        for x in range(5):
            for y in range(5):

                # If the square is the terminal square then print .
                if (x, y) in self.end:
                    print('.', end=' ')
                    continue
                
                best_val = float("-inf")
                best_move = 0

                # Get the next move and ensure its valid (same as before)
                for i, move in enumerate(self.moves):
                    new_x = x + move[0]
                    new_y = y + move[1]
                    
                    if 0 <= new_x <= 4 and 0 <= new_y <= 4:
                        value = self.values[new_x, new_y]
                        if value > best_val:
                            best_val = value
                            best_move = i

                policy[x, y] = best_move
                print(arrows[best_move], end=' ')
            print()


                    

        # Print the final score board
        print("Score board")
        print(self.values)


# Class for value iteration model
class ValueIteration(Gridworld):
    def __init__(self):
        super().__init__()

        # Initialize value function array to zeros
        self.values = np.zeros((5, 5))

        # Store the errors to graph them
        self.errors = []

        # Store value functions at different iterations
        self.value_history = []
        

    def calculate_state_value(self, state):
        # Takes in  a state and then returns the value of the state

        # If the state is the end state then we are golden, return 0
        if state in self.end:
            return 0
            
        # Keep track of all the values
        potential_values = []

        # Go through each move and the the next state for each move, then calc the reward 
        for action in self.moves:
            next_state = self.get_next_state(state, action)
            reward = self.get_reward(next_state)

            # Add the reward to the gamma * the value at the next state
            value = reward + self.values[next_state[0], next_state[1]]
            potential_values.append(value)
            
        # Return the max of the values
        return max(potential_values)
    
    def iterate(self):
        iteration = 0
        start = time.time()
        
        # Store initial values in the history
        self.value_history.append(self.values.copy())
        
        # Loop until we reach the threshold
        while True:
            # Store the maximum difference (to be compared with to stop) and make a copy of the current values
            max_diff = 0
            new_values = np.zeros_like(self.values)
            
            # Update each value in the grid
            for x in range(5):
                for y in range(5):
                    # If it is the end, the val is 0
                    if (x, y) in self.end:
                        new_values[x, y] = 0
                        continue
                        
                    # Calculate teh score for each space then store it
                    new_value = self.calculate_state_value((x, y))
                    new_values[x, y] = new_value

                    # Find the difference between the new value and the current value, then compare it with the current max diff and save it if it is more, otherwise ignore
                    max_diff = max(max_diff, abs(new_value - self.values[x, y]))
            
            # Storing the first 3 iterations as per assignment
            if iteration <= 2:
                self.value_history.append(new_values.copy())
            
            # Update values and track error so that it can be graphed
            self.values = new_values
            self.errors.append(max_diff)
            
            iteration += 1

            # Check for convergence (no diff)
            if max_diff == 0:
                # Store the final one
                self.value_history.append(new_values.copy())
                break
                
        print(f"Finished after {iteration} iterations and {time.time() - start} seconds")
    
    def get_policy(self):

        # Create an empty policy arr
        policy = np.zeros((5, 5), dtype=int)
        
        # Go through each location and find the best move
        for x in range(5):
            for y in range(5):
                if (x, y) in self.end:
                    continue
                
                # Get the best move from each possible one
                potential_moves = []
                for move in self.moves:
                    # Get the next move and calculate the reward and the value
                    next_state = self.get_next_state((x, y), move)
                    reward = self.get_reward(next_state)

                    value = reward + self.values[next_state[0], next_state[1]]
                    potential_moves.append(value)
                
                # Update the policy based on the best move, it will be the index of the highest number
                policy[x, y] = np.argmax(potential_moves)
        
        return policy
    
    def train_and_visualize(self):
        # Trains the model

        print("Starting Value Iteration")
        self.iterate()
        print("Done")
        
        # Print value history
        for i, values in enumerate(self.value_history):
            print(f"Iteration {i} values:")
            print(np.round(values, 2))
            print()

        # Print the final values
        print(f"Final values:")
        print(np.round(self.value_history[-1], 2))
        print()

        
        # Plot convergence
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors, label='Error')
        plt.xlabel('Iteration')
        plt.ylabel('Error')
        plt.title('Value iteration convergence')
        plt.grid(True)
        plt.legend()
        plt.show()
        
        # Show final policy same as before
        policy = self.get_policy()
        arrows = ['^', '>', 'V', '<']
        
        print("\nOptimal Policy:")
        for x in range(5):
            for y in range(5):
                if (x, y) in self.end:
                    print('.', end=' ')
                else:
                    print(arrows[policy[x, y]], end=' ')
            print()

if __name__ == "__main__":
    solver = PolicyIteration()
    solver.train()
    solver.show_policy()
    # Q1
    print("The method I chose for measuring convergance was to remember the old value and then see the difference between the current and the old, then compare this with the threshold. If it was lower than the threshold then we have converged. The convergence method is to see whether the values had stopped changing a significant amount. I picked it because it made sense in my head and it was easy to implement. You keep track of how much it changed from the previous one and thats it. It was also in the slides")

    solver = ValueIteration()
    solver.train_and_visualize()

    # Q2
    print("The convergence method was essentially the same. Go through each state and get the max change between each possible move. The change comes into play when checking against a threshold. Instead of checking against an arbitrary small number, we just see if the change was 0. If the change is 0, then no changes were made and we have converged. Again, I picked it because it made the most sense in my head, we go until no changes were made, and it was easy to implmenet.")

    # Q3
    print("When running, the value iteration was unsurprisingly much faster taking about .0006 seconds to converge vs .01 (though this difference could be due to implementation details) as well as significatly less iterations. Policy iteration can be better because it was also a bit easier for me to implement compared to value iteration. It can also potentially be better for more complex tasks where convergence isnt as simple as no changes from the previous. According to google, it is also better when trying to getting intermediate policies and is generally more stable")
