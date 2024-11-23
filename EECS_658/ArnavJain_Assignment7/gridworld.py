import numpy as np
import matplotlib.pyplot as plt

# Base gridworld class that will be inherited by each of the policy classes
class Gridworld:
    def __init__(self):
        # Given params
        self.size = 5
        self.y = 1
        self.p = .25
        
        # init the grid with 0s (5x5 cause given)
        self.grid = np.zeros((5, 5))
        
        # Set the end states the top left and bottom right
        self.end = [(0, 0), (4, 4)]
        
        # The valid moves are going up, right, down, left
        self.moves= [(-1, 0), (0, 1), (1, 0), (0, -1)]
        
    
    def get_next_state(self, state, action):
        # Gets the next state
        next_state = (state[0] + action[0], state[1] + action[1])

        # makes sure the state is more than 0 and less than or equal to 4 on both x and y
        if 0 <= next_state[0] and next_state[0] <= 4:
            if 0 <= next_state[1] and next_state[1] <= 4:
                # if it is, return the new state otherwise return the old one
                return next_state
        return state
    
    def get_reward(self, state, next_state):
        """Get reward for transition"""
        if state == next_state:  # Hit a wall
            return -1
        if next_state in self.end:
            return 0
        return -1
    
    def reset(self):
        """Reset the grid to initial state"""
        self.grid = np.zeros((self.size, self.size))


class PolicyIteration(Gridworld):
    def __init__(self):
        """
        Policy Iteration implementation for Gridworld
        
        Inherits from base Gridworld class and adds policy iteration specific functionality
        """
        super().__init__()
        
        # Initialize value function
        self.V = np.zeros((5, 5))
        
        # Initialize Q-values
        self.Q = np.zeros((5, 5, 4))
        
        # Keep track of errors for convergence plotting
        self.errors = []

    def policy_evaluation(self, theta=0.001):
        """Evaluate current policy"""
        while True:
            delta = 0
            V_old = self.V.copy()
            
            for i in range(self.size):
                for j in range(self.size):
                    if (i, j) in self.end:
                        continue
                        
                    state = (i, j)
                    v = self.V[i, j]
                    
                    # Calculate Q-values for all actions
                    for a_idx, action in enumerate(self.moves):
                        q_value = 0
                        next_state = self.get_next_state(state, action)
                        reward = self.get_reward(state, next_state)
                        q_value += self.p * (reward + self.y * V_old[next_state[0], next_state[1]])
                        self.Q[i, j, a_idx] = q_value
                    
                    # Update value function
                    self.V[i, j] = np.max(self.Q[i, j])
                    delta = max(delta, abs(v - self.V[i, j]))
            
            # Store error for convergence plotting
            self.errors.append(delta)
            
            if delta < theta:
                break

    def get_policy(self):
        """Get optimal policy based on current Q-values"""
        policy = np.zeros((self.size, self.size), dtype=int)
        for i in range(self.size):
            for j in range(self.size):
                if (i, j) not in self.end:
                    policy[i, j] = np.argmax(self.Q[i, j])
        return policy

    def train(self, max_iterations=1000, theta=0.001):
        """Run policy iteration algorithm"""
        print("Initial Values (Iteration 0):")
        print(self.V)
        
        for iteration in range(1, max_iterations + 1):
            # Policy evaluation
            self.policy_evaluation(theta)
            
            # Print specific iterations
            if iteration in [1, 10] or len(self.errors) < 2 or self.errors[-1] < theta:
                print(f"\nIteration {iteration}:")
                print(self.V)
            
            # Check for convergence
            if len(self.errors) >= 2 and self.errors[-1] < theta:
                print("\nConverged!")
                break

        # Plot convergence
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors)
        plt.xlabel('Iteration')
        plt.ylabel('Error (ε)')
        plt.yscale('log')
        plt.title('Convergence of Policy Iteration')
        plt.grid(True)
        plt.show()

    def visualize_policy(self):
        """Visualize the optimal policy using arrows"""
        policy = self.get_policy()
        arrows = ['^', '>', 'V', '<']
        
        for i in range(self.size):
            for j in range(self.size):
                if (i, j) in self.end:
                    print('.', end=' ')
                else:
                    print(arrows[policy[i, j]], end=' ')
            print()


# Example usage
if __name__ == "__main__":
    # Create and train the policy iteration agent
    pi = PolicyIteration()
    pi.train()
    
    print("\nOptimal Policy:")
    pi.visualize_policy()
