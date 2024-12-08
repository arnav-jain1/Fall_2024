"""
This file has all the Qlearning code
Please know that 99% of the code is reused with only some small changes
"""
from gw import Gridworld
import numpy as np
import matplotlib.pyplot as plt
import math

# Take the gridworld as a super class 
class QLearning(Gridworld):
    def __init__(self):
        super().__init__()
        self.gamma = .9

        # Initialize Q mat with 0s
        self.Q = np.zeros((5, 5, len(self.moves)))

        # Create R matrix, every sq is -1 except for the final which are 100 each
        self.R = np.full((self.size, self.size), -1)
        for end in self.end:
            self.R[end] = 100
        
        # Track the errors for polotting
        self.errors = []


        # Threshold for measuring if converged or not
        self.epsillon = 1e-6

        print("Rewards Matrix (R):")
        print(self.R)


        # To track rewards for later graphing
        self.episode_rewards = []

        
    # similar
    def train(self):
        epoch = 0
        converged = False

        # Loop through while not converged
        while not converged:
            episode_reward = 0
            
            # Save the old q value and get a random state
            old_Q = np.copy(self.Q)
            state = self.rand_non_final_state()
            
            # Keep looping while the current state is not at the end
            while state not in self.end:

                # Get all valid moves from current state
                valid_moves = self.get_valid_moves(state)
                
                # Randomly select one of those moves (Q learning)
                move_idx = valid_moves[np.random.randint(len(valid_moves))]
                move = self.moves[move_idx]
                
                # Get next state based off that move
                next_state = self.get_next_state(state, move)
                
                # Get valid moves from next state
                next_valid_moves = self.get_valid_moves(next_state)
                
                # Get max Q-value for next state (only considering valid moves)
                if next_state in self.end:
                    next_max_q = 0
                else:
                    # Get all of the Q values for the next state
                    q = [self.Q[next_state[0], next_state[1], idx] for idx in next_valid_moves]
                    # Get the max of those q vals
                    next_max_q = max(q)
                
                # Update the q val with the eq from the slides
                self.Q[state[0], state[1], move_idx] = self.R[state[0], state[1]] + self.gamma * next_max_q

                # Add the reward of the state to the running reward
                episode_reward += self.R[state[0], state[1]]
                
                # Save the next state as the current state
                state = next_state
            
            # Add the reward to the list
            self.episode_rewards.append(episode_reward)

            # Calculate error for convergence checking
            error = np.sum(np.abs(self.Q - old_Q))
            self.errors.append(error)

            if epoch in [0, 1, 10]:
                print(f"\nQ-matrix at epoch {epoch}:")
                print(self.Q)
            
            # Check for convergence, done via rolling sum of errors. If the rolling sum of errors is lower than threshold, then we have converged
            # This is done because we know we have conv if the change is next to 0, start checking after the 20th epoch to avoid converging to quickly
            if  epoch > 20 and (sum(self.errors[-10:]) < self.epsillon) :
                print(f"\nConverged at epoch {epoch}")
                print("\nFinal Q-matrix:")
                print(self.Q)
                converged = True
            
            epoch += 1
    
    # Plotting, same as always
    def plot_convergence(self):
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors, label='Error')

        # THis is the line for the threshold 
        plt.axhline(y=self.epsillon, color='r', linestyle='--', label='ε threshold')

        plt.xlabel('Epoch')
        plt.ylabel('Errors (e)')
        plt.title('Convergence (Base Q-Learning)')
        plt.legend()
        plt.yscale('log')
        plt.grid(True)
        plt.show()

        self.print_policy()
    
    # Printing the optimal policy, same as prev lab p much
    def print_policy(self):
        # Create an 0s policy arr
        policy = np.empty((self.size, self.size), dtype=str)
        arrows = ['^', '>', 'V', '<']
        
        # Go through each sqare
        for x in range(self.size):
            for y in range(self.size):
                # If it is the end space then its just a .
                if (x, y) in self.end:
                    policy[x, y] = '.'  # Goal state
                else:
                    # Find all the valid moves and the q values associated with each valid move
                    valid_moves = self.get_valid_moves((x, y))
                    q_values = [self.Q[x, y, idx] for idx in valid_moves]

                    # Find the best move by finding the index of the highest q val
                    best_move = valid_moves[np.argmax(q_values)]
                    # Then set the policy to the arrow that corresponds with it
                    policy[x, y] = arrows[best_move]
        print(policy)

class SARSA(Gridworld):
    def __init__(self):
        super().__init__()
        self.gamma = .9

        # Initialize Q mat with 0s
        self.Q = np.zeros((5, 5, len(self.moves)))

        # Create R matrix, every sq is -1 except for the final which are 100 each
        self.R = np.full((self.size, self.size), -1)
        for end in self.end:
            self.R[end] = 100
        
        # Track the errors for polotting
        self.errors = []


        # Threshold for measuring if converged or not
        self.epsillon = 1e-6

        print("Rewards Matrix (R):")
        print(self.R)

        # To track rewards for later graphing
        self.episode_rewards = []

        
    # similar
    def train(self):
        epoch = 0
        converged = False

        # Loop through while not converged
        while not converged:
            # 0 out the running reward
            episode_reward = 0    

            # Save the old q value and get a random state
            old_Q = np.copy(self.Q)
            state = self.rand_non_final_state()
            
            # Keep looping while the current state is not at the end
            while state not in self.end:
                # NOTE CHANGE: Get the best move not a random move

                # Get all valid moves from current state
                valid_moves = self.get_valid_moves(state)
                
                
                # Get q vals for all valid moves from current state
                q_values = [self.Q[state[0], state[1], idx] for idx in valid_moves]

                # Find indices of moves that have the max q val (for tie-breaking)
                max_q_indices = np.where(q_values == np.max(q_values))[0]

                # Randomly select one of the moves with the highest Q-value
                move_idx = valid_moves[np.random.choice(max_q_indices)]
                move = self.moves[move_idx] 


                # Get next state based off that move
                next_state = self.get_next_state(state, move)
                
                # Get valid moves from next state
                next_valid_moves = self.get_valid_moves(next_state)
                
                # Get max Q-value for next state (only considering valid moves)
                if next_state in self.end:
                    next_max_q = 0
                else:
                    # Get all of the Q values for the next state
                    q = [self.Q[next_state[0], next_state[1], idx] for idx in next_valid_moves]
                    # Get the max of those q vals
                    next_max_q = max(q)
                
                # Update the q val with the eq from the slides
                self.Q[state[0], state[1], move_idx] = self.R[state[0], state[1]] + self.gamma * next_max_q

                # Add the reward of the state to the running reward
                episode_reward += self.R[state[0], state[1]]
                
                # Save the next state as the current state
                state = next_state

            # Add the reward to the list
            self.episode_rewards.append(episode_reward)
            
            # Calculate error for convergence checking
            error = np.sum(np.abs(self.Q - old_Q))
            self.errors.append(error)

            if epoch in [0, 1, 10]:
                print(f"\nQ-matrix at epoch {epoch}:")
                print(self.Q)
            
            # Check for convergence, done via rolling sum of errors. If the rolling sum of errors is lower than threshold, then we have converged
            # This is done because we know we have conv if the change is next to 0, start checking after the 20th epoch to avoid converging to quickly
            if  epoch > 20 and (sum(self.errors[-10:]) < self.epsillon) :
                print(f"\nConverged at epoch {epoch}")
                print("\nFinal Q-matrix:")
                print(self.Q)
                converged = True
            
            epoch += 1
    
    # Plotting, same as always
    def plot_convergence(self):
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors, label='Error')

        # THis is the line for the threshold 
        plt.axhline(y=self.epsillon, color='r', linestyle='--', label='ε threshold')

        plt.xlabel('Epoch')
        plt.ylabel('Errors (e)')
        plt.title('Convergence (SARSA)')
        plt.legend()
        plt.yscale('log')
        plt.grid(True)
        plt.show()

        self.print_policy()
    
    # Printing the optimal policy, same as prev lab p much
    def print_policy(self):
        # Create an 0s policy arr
        policy = np.empty((self.size, self.size), dtype=str)
        arrows = ['^', '>', 'V', '<']
        
        # Go through each sqare
        for x in range(self.size):
            for y in range(self.size):
                # If it is the end space then its just a .
                if (x, y) in self.end:
                    policy[x, y] = '.'  # Goal state
                else:
                    # Find all the valid moves and the q values associated with each valid move
                    valid_moves = self.get_valid_moves((x, y))
                    q_values = [self.Q[x, y, idx] for idx in valid_moves]

                    # Find the best move by finding the index of the highest q val
                    best_move = valid_moves[np.argmax(q_values)]
                    # Then set the policy to the arrow that corresponds with it
                    policy[x, y] = arrows[best_move]
        print(policy)

class EpsilonGreedy(Gridworld):
    def __init__(self):
        super().__init__()
        self.gamma = .9

        # Initialize Q mat with 0s
        self.Q = np.zeros((5, 5, len(self.moves)))

        # Create R matrix, every sq is -1 except for the final which are 100 each
        self.R = np.full((self.size, self.size), -1)
        for end in self.end:
            self.R[end] = 100
        
        # Track the errors for polotting
        self.errors = []


        # Threshold for measuring if converged or not
        self.epsillon = 1e-6

        print("Rewards Matrix (R):")
        print(self.R)


        # Arb decay cosnt
        self.decay_const = 10


        # To track rewards for later graphing
        self.episode_rewards = []
        
    # similar
    def train(self):
        epoch = 0
        converged = False

        # Loop through while not converged
        while not converged:

            # 0 out the running rewards func
            episode_reward = 0    

            # Save the old q value and get a random state
            old_Q = np.copy(self.Q)
            state = self.rand_non_final_state()
            
            # Keep looping while the current state is not at the end
            while state not in self.end:
                # NOTE CHANGE: Combination of the two
                epsillon = np.exp(-epoch/self.decay_const)


                # Get all valid moves from current state
                valid_moves = self.get_valid_moves(state)

                # if its less than the epsillon then random move
                if np.random.random() < epsillon:
                    move_idx = valid_moves[np.random.randint(len(valid_moves))]
                    move = self.moves[move_idx]
                else:
                    # Otherwise do SARSA

                    # Get q vals for all valid moves from current state
                    q_values = [self.Q[state[0], state[1], idx] for idx in valid_moves]

                    # Find indices of moves that have the max q val (for tie-breaking)
                    max_q_indices = np.where(q_values == np.max(q_values))[0]

                    # Randomly select one of the moves with the highest Q-value (this is for breaking ties only)
                    move_idx = valid_moves[np.random.choice(max_q_indices)]
                    move = self.moves[move_idx] 


                # Get next state based off that move
                next_state = self.get_next_state(state, move)
                
                # Get valid moves from next state
                next_valid_moves = self.get_valid_moves(next_state)
                
                # Get max Q-value for next state (only considering valid moves)
                if next_state in self.end:
                    next_max_q = 0
                else:
                    # Get all of the Q values for the next state
                    q = [self.Q[next_state[0], next_state[1], idx] for idx in next_valid_moves]
                    # Get the max of those q vals
                    next_max_q = max(q)
                
                # Update the q val with the eq from the slides
                self.Q[state[0], state[1], move_idx] = self.R[state[0], state[1]] + self.gamma * next_max_q

                # Add the reward of the state to the running reward
                episode_reward += self.R[state[0], state[1]]
                
                # Save the next state as the current state
                state = next_state
            
            # Add the reward to the list
            self.episode_rewards.append(episode_reward)

            # Calculate error for convergence checking
            error = np.sum(np.abs(self.Q - old_Q))
            self.errors.append(error)

            if epoch in [0, 1, 10]:
                print(f"\nQ-matrix at epoch {epoch}:")
                print(self.Q)
            
            # Check for convergence, done via rolling sum of errors. If the rolling sum of errors is lower than threshold, then we have converged
            # This is done because we know we have conv if the change is next to 0, start checking after the 20th epoch to avoid converging to quickly
            if  epoch > 20 and (sum(self.errors[-10:]) < self.epsillon) :
                print(f"\nConverged at epoch {epoch}")
                print("\nFinal Q-matrix:")
                print(self.Q)
                converged = True
            
            epoch += 1
    
    # Plotting, same as always
    def plot_convergence(self):
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors, label='Error')

        # THis is the line for the threshold 
        plt.axhline(y=self.epsillon, color='r', linestyle='--', label='ε threshold')

        plt.xlabel('Epoch')
        plt.ylabel('Errors (e)')
        plt.title('Convergence (Greedy-Epsilon)')
        plt.legend()
        plt.yscale('log')
        plt.grid(True)
        plt.show()

        self.print_policy()
    
    # Printing the optimal policy, same as prev lab p much
    def print_policy(self):
        # Create an 0s policy arr
        policy = np.empty((self.size, self.size), dtype=str)
        arrows = ['^', '>', 'V', '<']
        
        # Go through each sqare
        for x in range(self.size):
            for y in range(self.size):
                # If it is the end space then its just a .
                if (x, y) in self.end:
                    policy[x, y] = '.'  # Goal state
                else:
                    # Find all the valid moves and the q values associated with each valid move
                    valid_moves = self.get_valid_moves((x, y))
                    q_values = [self.Q[x, y, idx] for idx in valid_moves]

                    # Find the best move by finding the index of the highest q val
                    best_move = valid_moves[np.argmax(q_values)]
                    # Then set the policy to the arrow that corresponds with it
                    policy[x, y] = arrows[best_move]
        print(policy)
