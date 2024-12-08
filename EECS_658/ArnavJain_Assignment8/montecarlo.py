"""
Code for all Montecarlo algorithms
Please know that 99% of the code is reused with only some small changes
"""
from gw import Gridworld

import numpy as np
import matplotlib.pyplot as plt
from collections import defaultdict

class MonteCarloFirstVisit:
    def __init__(self, ):
        # Make a gridworld
        self.gridworld = Gridworld()
        self.gridworld.gamma = .9
        self.size = 5
        # N(s) array (number of visits)
        self.N = np.zeros((5, 5))  
        # S(s) array (total return)
        self.S = np.zeros((5, 5))  
        # V(s) value matrix
        self.V = np.zeros((5, 5))


        # The convergence threshold
        self.epsilon = .001

        # Errors to be graphed
        self.errors = []
        
    def generate_episode(self):
        # Start from random state
        state = self.gridworld.rand_non_final_state()
                
        episode = []
        while True:
            # Get the next state and the corresp move
            next_state, move = self.gridworld.next_move(state)
            
            # Get reward based on the move
            reward = self.gridworld.get_reward(next_state)
            
            # Add to episode
            episode.append((state, reward))
            
            # The next state is now the current state
            state = next_state
            
            # Check if the old next state is in the end, if it is then break (done)
            if state in self.gridworld.end:
                break
                
        # Return the episode
        return episode
    
    # Function to calculate the returns S(s)
    def calculate_returns(self, episode):
        # Create a dict that defaults the keys to 0.0 to keep track of G(s)
        G = defaultdict(float)
        returns = []
        G_s = 0
        
        # Calculate returns for each state in the episode by looping through them backwards
        for index in range(len(episode)-1, -1, -1):
            # Get the state and the corresp reward for each episode
            state, reward = episode[index]
            # Update the running G(t) var by multiplying gamma by the previous G(s) (equivalent to exponenting gamma for the previous ones) and then adding the current reward
            G_s = self.gridworld.gamma * G_s + reward
            # Update teh dict
            G[state] = G_s
            # Add it to the returns to be printed, gamma is always the same so just ignore that for now
            returns.append((index+1, state, reward, G_s))
            
        # Reverse the returns list and return it so it is in numerical order
        return list(reversed(returns))
    
    def train(self):
        # Keep track of epochs and make a converged variable to loop
        epoch = 0
        converged = False
        
        # Keep looping until converged
        while not converged:
            # Do an episode
            episode = self.generate_episode()
            
            # Calculate returns
            returns = self.calculate_returns(episode)
            
            # Track first visits, done by tracking all the visited states in a set so that lookup is shorter and we dont repeat
            visited_states = set()
            
            # Store old V(s) so that we can check for convergence later
            old_V = self.V.copy()
            
            # Update v(s) for first visits ONLY
            # Done by going through each of the returns and checking to se
            for t, state, reward, G_t in returns:
                # Check to see if the state was in visited states, if it isnt then 
                if state not in visited_states:
                    # Add it, update the S(s), N(s), and V(s) accordingly
                    visited_states.add(state)
                    self.N[state[0]][state[1]] += 1
                    self.S[state[0]][state[1]] += G_t
                    # V(s) is updated acc the eq on the slides
                    self.V[state[0]][state[1]] = self.S[state[0]][state[1]] / self.N[state[0]][state[1]]
            
            # Calculate error by taking current V - old V for convergence check
            error = np.max(np.abs(self.V - old_V))
            self.errors.append(error)
            
            # Check if the error was lower than the epsilon we set, if it is we are done
            if error < self.epsilon:
                converged = True
                
            # Print required epochs (and the last one)
            if epoch in [0, 1, 10] or converged:
                # Just print the returns
                print(f"\nEpoch {epoch}")
                print("N(s):")
                print(self.N)
                print("S(s):")
                print(self.S)
                print("V(s):")
                print(self.V)
                
                print("\nReturns for this episode:")
                print("k\t s\t r\t Y\t G(s)")
                for k, s, r, g in returns:
                    print(f"{k}\t {s}\t {r}\t {self.gridworld.gamma}\t {g:.3f}")
            
            # Increment the epoch
            epoch += 1
            
        return epoch
    
    def plot_convergence(self):
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors, label='Error')

        # THis is the line for the threshold 
        plt.axhline(y=self.epsilon, color='r', linestyle='--', label='ε threshold')

        plt.xlabel('Epoch')
        plt.ylabel('Errors (e)')
        plt.title('Convergence (First Value MC)')
        plt.legend()
        plt.yscale('log')
        plt.grid(True)
        plt.show()

# This is literally the same thing exvept for one change. That change is denoted with NOTE
class MonteCarloEveryVisit:
    def __init__(self, ):
        # Make a gridworld
        self.gridworld = Gridworld()
        self.gridworld.gamma = .9
        self.size = 5
        # N(s) array (number of visits)
        self.N = np.zeros((5, 5))  
        # S(s) array (total return)
        self.S = np.zeros((5, 5))  
        # V(s) value matrix
        self.V = np.zeros((5, 5))


        # The convergence threshold
        self.epsilon = .001

        # Errors to be graphed
        self.errors = []
        
    def generate_episode(self):
        # Start from random state
        state = self.gridworld.rand_non_final_state()
                
        episode = []
        while True:
            # Get the next state and the corresp move
            next_state, move = self.gridworld.next_move(state)
            
            # Get reward based on the move
            reward = self.gridworld.get_reward(next_state)
            
            # Add to episode
            episode.append((state, reward))
            
            # The next state is now the current state
            state = next_state
            
            # Check if the old next state is in the end, if it is then break (done)
            if state in self.gridworld.end:
                break
                
        # Return the episode
        return episode
    
    # Function to calculate the returns S(s)
    def calculate_returns(self, episode):
        # Create a dict that defaults the keys to 0.0 to keep track of G(s)
        G = defaultdict(float)
        returns = []
        G_s = 0
        
        # Calculate returns for each state in the episode by looping through them backwards
        for index in range(len(episode)-1, -1, -1):
            # Get the state and the corresp reward for each episode
            state, reward = episode[index]
            # Update the running G(t) var by multiplying gamma by the previous G(s) (equivalent to exponenting gamma for the previous ones) and then adding the current reward
            G_s = self.gridworld.gamma * G_s + reward
            # Update teh dict
            G[state] = G_s
            # Add it to the returns to be printed, gamma is always the same so just ignore that for now
            returns.append((index+1, state, reward, G_s))
            
        # Reverse the returns list and return it so it is in numerical order
        return list(reversed(returns))
    
    def train(self):
        # Keep track of epochs and make a converged variable to loop
        epoch = 0
        converged = False
        
        # Keep looping until converged
        while not converged:
            # Do an episode
            episode = self.generate_episode()
            
            # Calculate returns
            returns = self.calculate_returns(episode)
            
            
            # Store old V(s) so that we can check for convergence later
            old_V = self.V.copy()
            
            # NOTE THIS IS THE ONLY CHANGE. The visited states is not kept track of because we are not only doing first visits
            # Update v(s) for ALL visits
            for t, state, reward, G_t in returns:
                # update the S(s), N(s), and V(s) accordingly
                self.N[state[0]][state[1]] += 1
                self.S[state[0]][state[1]] += G_t
                # V(s) is updated acc the eq on the slides
                self.V[state[0]][state[1]] = self.S[state[0]][state[1]] / self.N[state[0]][state[1]]
            
            # Calculate error by taking current V - old V for convergence check
            error = np.max(np.abs(self.V - old_V))
            self.errors.append(error)
            
            # Check if the error was lower than the epsilon we set, if it is we are done
            if error < self.epsilon:
                converged = True
                
            # Print required epochs (and the last one)
            if epoch in [0, 1, 10] or converged:
                # Just print the returns
                print(f"\nEpoch {epoch}")
                print("N(s):")
                print(self.N)
                print("S(s):")
                print(self.S)
                print("V(s):")
                print(self.V)
                
                print("\nReturns for this episode:")
                print("k\t s\t r\t Y\t G(s)")
                for k, s, r, g in returns:
                    print(f"{k}\t {s}\t {r}\t {self.gridworld.gamma}\t {g:.3f}")
            
            # Increment the epoch
            epoch += 1
            
        return epoch
    
    def plot_convergence(self):
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors, label='Error')

        # THis is the line for the threshold 
        plt.axhline(y=self.epsilon, color='r', linestyle='--', label='ε threshold')

        plt.xlabel('Epoch')
        plt.ylabel('Errors (e)')
        plt.title('Convergence (Every Value MC)')
        plt.legend()
        plt.yscale('log')
        plt.grid(True)
        plt.show()

# On policy, very similar, comments will be same
class MonteCarloEveryVisitOnPolicy:
    def __init__(self):
        # Make a gridworld
        self.gridworld = Gridworld()
        self.gridworld.gamma = .9
        self.size = 5
        # N(s) array (number of visits)
        self.N = np.zeros((5, 5))  
        # S(s) array (total return)
        self.S = np.zeros((5, 5))  
        # V(s) value matrix
        self.V = np.zeros((5, 5))

        # Policy matrix stores best action (index that corresp to the actions) for each state
        self.policy = np.zeros((5, 5), dtype=int)
        
        # The convergence threshold
        self.epsilon = .001
        
        # Errors to be graphed
        self.errors = []


        # How often a random move is picked
        self.exploration_rate = .25
        
    def get_best_action(self, state):
        # Function that takes in a state and gets the best move that corresponds with it

        # Epislon greedy, if a random val is lower than the rate, then pick a random move
        if np.random.random() < self.exploration_rate:
            next_state, move = self.gridworld.next_move(state)
            return move

        # Start with -inf to keep track
        best_value = float('-inf')
        best_action_idx = 0
        
        # Fo through each move
        for idx, move in enumerate(self.gridworld.moves):
            # Get the next state and the value from V(s) that corresponds
            next_state = self.gridworld.get_next_state(state, move)
            value = self.V[next_state[0]][next_state[1]]
            # If it is better than the current, replace it
            if value > best_value:
                best_value = value
                best_action_idx = idx
                
        # If all moves are bad (have less than current val), pick a random move
        if best_value <= self.V[state[0]][state[1]]:
            next_state, move = self.gridworld.next_move(state)
            return move

        # Return the move that corresponds with the indx
        return self.gridworld.moves[best_action_idx]
    
    
    def generate_episode(self):
        # Start from random non-final state
        state = self.gridworld.rand_non_final_state()
        
        # Keep track of the full episode
        episode = []
        while True:
            # Check the value for the state, if it is nonzero, then we follow the policy otherwise we random walk until we get to a state with a policy
            if self.V[state[0]][state[1]] != 0:

                # Get the best move and the next state that corresponds with that action
                move = self.get_best_action(state)
                next_state = self.gridworld.get_next_state(state, move)
            else:
                # Get a random move
                next_state, move = self.gridworld.next_move(state)
            
            # Same as before, get the reward associated with it and then append it to the episode tracker
            reward = self.gridworld.get_reward(next_state)
            episode.append((state, reward))

            # Set the current state to the next, then check if it is the last one, if so break
            state = next_state
            
            if state in self.gridworld.end:
                break
                
        return episode
    
    # Function to calculate the returns S(s)
    def calculate_returns(self, episode):
        # Create a dict that defaults the keys to 0.0 to keep track of G(s)
        G = defaultdict(float)
        returns = []
        G_s = 0
        
        # Calculate returns for each state in the episode by looping through them backwards
        for index in range(len(episode)-1, -1, -1):
            # Get the state and the corresp reward for each episode
            state, reward = episode[index]
            # Update the running G(t) var by multiplying gamma by the previous G(s) (equivalent to exponenting gamma for the previous ones) and then adding the current reward
            G_s = self.gridworld.gamma * G_s + reward
            # Update teh dict
            G[state] = G_s
            # Add it to the returns to be printed, gamma is always the same so just ignore that for now
            returns.append((index+1, state, reward, G_s))
            
        # Reverse the returns list and return it so it is in numerical order
        return list(reversed(returns))

    # Func that updates the policy
    def update_policy(self):
        # Go through each element

        for x in range(self.size):
            for y in range(self.size):
                # If it is not the end then
                if (x, y) not in self.gridworld.end:
                    # Get the best move from the state
                    best_move = self.get_best_action((x, y))
                    # Update the policy with the best move
                    self.policy[x][y] = self.gridworld.moves.index(best_move)
    
    def train(self):
        # Keep track of epochs and make a converged variable to loop
        epoch = 0
        converged = False
        
        # Keep looping until converged
        while not converged:
            # Do an episode
            episode = self.generate_episode()
            
            # Calculate returns
            returns = self.calculate_returns(episode)
            
            
            # Store old V(s) so that we can check for convergence later
            old_V = self.V.copy()
            
            # Update v(s) for ALL visits
            for t, state, reward, G_t in returns:
                # update the S(s), N(s), and V(s) accordingly
                self.N[state[0]][state[1]] += 1
                self.S[state[0]][state[1]] += G_t
                # V(s) is updated acc the eq on the slides
                self.V[state[0]][state[1]] = self.S[state[0]][state[1]] / self.N[state[0]][state[1]]

            # NOTE, changed: Update policy based on new values
            self.update_policy()
            
            # Calculate error by taking current V - old V for convergence check
            error = np.max(np.abs(self.V - old_V))
            self.errors.append(error)
            
            # Check if the error was lower than the epsilon we set, if it is we are done
            if error < self.epsilon:
                converged = True
                
            # Print required epochs (and the last one)
            if epoch in [0, 1, 10] or converged:
                # Just print the returns
                print(f"\nEpoch {epoch}")
                print("N(s):")
                print(self.N)
                print("S(s):")
                print(self.S)
                print("V(s):")
                print(self.V)
                
                print("\nReturns for this episode:")
                print("k\t s\t r\t Y\t G(s)")
                for k, s, r, g in returns:
                    print(f"{k}\t {s}\t {r}\t {self.gridworld.gamma}\t {g:.3f}")
            
            # Increment the epoch
            epoch += 1
            
        return epoch

    def plot_convergence(self):
        plt.figure(figsize=(10, 6))
        plt.plot(self.errors, label='Error')

        # THis is the line for the threshold 
        plt.axhline(y=self.epsilon, color='r', linestyle='--', label='ε threshold')

        plt.xlabel('Epoch')
        plt.ylabel('Errors (e)')
        plt.title('Convergence (Every Value On Policy MC)')
        plt.legend()
        plt.yscale('log')
        plt.grid(True)
        plt.show()
