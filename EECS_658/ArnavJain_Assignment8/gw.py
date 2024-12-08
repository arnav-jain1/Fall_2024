"""
Class for gridworld taken directly from the prev assignment
"""
import numpy as np
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
    # Gets a random state that is not the final state
    def rand_non_final_state(self):
        while True:
            x = np.random.randint(0, 5)
            y = np.random.randint(0, 5)
            state = (x, y)
            if state not in self.end:
                return state

    def next_move(self, state):
        while True:
            # Get a random move
            move = self.moves[np.random.randint(len(self.moves))]
            
            # Calculate the new location
            new_x = state[0] + move[0]
            new_y = state[1] + move[1]
            
            # Check if the new state is valid
            if 0 <= new_x < self.size and 0 <= new_y < self.size:
                # If it is, return the new state and the move
                return (new_x, new_y), move

    # Take in a move and the state and check if within dim
    def is_valid_move(self, state, move):
        new_x = state[0] + move[0]
        new_y = state[1] + move[1]
        return 0 <= new_x < self.size and 0 <= new_y < self.size

    # Get all valid moves when in a certain spot
    def get_valid_moves(self, state):
        valid_moves = []
        # Go through each move, check if valid, if so add it to the valid moves list. then return the list
        for i, move in enumerate(self.moves):
            if self.is_valid_move(state, move):
                valid_moves.append(i)
        return valid_moves

