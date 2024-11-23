import numpy as np
import matplotlib.pyplot as plt

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

        # The q vals are going to be similar but one spot for each move
        self.q_values = np.zeros((5, 5, 4))
        # errors for plotting
        self.errors = []

    def policy_evaluation(self, threshold=0.001):
        # Keep evaluating until we break (converge)

        while True:
            # Reset the change and save the old values
            change = 0
            old_values = self.values.copy()
            
            # Go through each state
            for x in range(self.size):
                for y in range(self.size):
                    # If the state is in the end skip this iteration
                    if (x, y) in self.end:
                        continue
                    
                    # Save the state, then try each move
                    state = (x, y)
                    old_value = self.values[x, y]
                    move_vals = []
                    
                    for i, action in enumerate(self.moves):
                        # Get next state and reward depending on the state
                        next_state = self.get_next_state(state, action)
                        reward = self.get_reward(next_state)
                        
                        # Calculate v, reward + gamma * oldv
                        value = reward + self.gamma * old_values[next_state[0], next_state[1]]
                        # Save the v in q, i is so that it is for the right move
                        self.q_values[x, y, i] = value
                        # Append it to move val
                        move_vals.append(value)
                    
                    # Pick the best move and save it to the state
                    self.values[x, y] = max(move_vals)
                    # Get the bigger change between either the older move or current 
                    change = max(change, abs(old_value - self.values[x, y]))
            
            # Track convergence
            self.errors.append(change)
            
            # Check if we've converged, if we have then quit
            if change < threshold:
                break

    def get_best_actions(self):
        # Find best actions (policy) for each state
        policy = np.zeros((self.size, self.size), dtype=int)
        
        # Go through each x and y
        for x in range(self.size):
            for y in range(self.size):
                # if it is an ending square, skip
                if (x, y) in self.end:
                    continue
                # Otherwise, set the policy to be the max of the 4 moves at each x,y
                policy[x, y] = np.argmax(self.q_values[x, y])
        
        return policy

    def train(self):
        print("Starting")
        
        # Start it with .0001 as the threshold
        self.policy_evaluation(.0001) 
        
        print("\nDone!")
        
        # Basic plotting
        plt.figure(figsize=(8, 5))
        plt.plot(self.errors)
        plt.xlabel('Iteration')
        plt.ylabel('Error')
        plt.title('yuh')
        plt.grid(True)
        plt.show()


    def show_policy(self):
        # Gets the best actions for each square
        policy = self.get_best_actions()

        # Arrows to correspond move with printing
        arrows = ['^', '>', 'V', '<']
        
        print("\nBest Actions:")

        # Go through each square
        for x in range(self.size):
            for y in range(self.size):

                # If the square is the terminal square then print .
                if (x, y) in self.end:
                    print('.', end=' ')
                else:
                    # Othewrwise print the corresp arrow
                    print(arrows[policy[x, y]], end=' ')
            print()

        print("Score board (higher the more likely to get)")
        print(self.policy_score(policy))
    def policy_score(self, policy):
        # Create an empty grid to keep track of the score
        score_grid = np.zeros((5, 5))
        
        # Go through each state
        for x in range(5):
            for y in range(5):
                if (x, y) in self.end:
                    score_grid[x, y] = 0
                    continue
                    
                # Keep track of input arrowsj
                incoming_arrows = 0

                # Check above, below, right, and left
                for x_neightbor, y_neighbor in [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]:

                    # Skip if neighbor is outside grid
                    if not (0 <= x_neightbor <= 4 and 0 <= y_neighbor <= 4):
                        continue
                        
                    # Check if neighbor points to current state
                    # 0 = up, 1 = right, 2 = down, 3 = left
                    if (x_neightbor < x and policy[x_neightbor, y_neighbor] == 2) or (x_neightbor > x and policy[x_neightbor, y_neighbor] == 0) or (y_neighbor < y and policy[x_neightbor, y_neighbor] == 1) or (y_neighbor > y and policy[x_neightbor, y_neighbor] == 3):

                        incoming_arrows += 1
                
                # Divide incoming arrows by 4 (max possible) and subtract 1 to keep it from [-1, 0]
                score_grid[x, y] = -1 + (incoming_arrows / 4)
        
        return score_grid

if __name__ == "__main__":
    # Run the algorithm
    solver = PolicyIteration()
    solver.train()
    solver.show_policy()
    """The method I chose for doing this was to go through each state and find the one that will have the best reward. The convergence message is to see whether the optimal paths changed was less than .001 because that means that there were no changes being made. I picked it because it made sense in my head and it was easy to implement. You just get the max change for each state and check if it is less than a threshold which is really easy to do"""


