'''
Program: EECS 658 Assignment 8
Description: Different RL to Gridworld
Name: Arnav Jain
Date: 12/07/24
Inputs: None
Outputs: Montecarlo/RL itereration algorithm results
Collaborators: None
Sources: StackOverflow, Google, ChatGPT, Anthropic Claude
'''
from montecarlo import *
from qlearning import *
import matplotlib.pyplot as plt
import numpy as np

# Run the algorithm
print("--------------------------- Part 1 Montecarlo First Visit ---------------------------")
mcfv = MonteCarloFirstVisit()
mcfv.train()
mcfv.plot_convergence()
print()

print("--------------------------- Part 2 Montecarlo Every Visit ---------------------------")
mcev = MonteCarloEveryVisit()
mcev.train()
mcev.plot_convergence()
print()

print("--------------------------- Part 3 Montecarlo On Policy ---------------------------")
mcev = MonteCarloEveryVisitOnPolicy()
mcev.train()
mcev.plot_convergence()
print()

print("--------------------------- Part 4 Standard Q Learning---------------------------")
q_learning = QLearning()
q_learning.train()
q_learning.plot_convergence()
print()

print("--------------------------- Part 5 SARSA ---------------------------")
sarsa = SARSA()
sarsa.train()
sarsa.plot_convergence()
print()

print("--------------------------- Part 6 Epsilon Greedy ---------------------------")
greedy = EpsilonGreedy()
greedy.train()
greedy.plot_convergence()




def compare():
    plt.figure(figsize=(12, 6))
    
    # Get the cumulative av for each of the rl
    q_avg = np.cumsum(q_learning.episode_rewards) / np.arange(1, len(q_learning.episode_rewards) + 1)
    sarsa_avg = np.cumsum(sarsa.episode_rewards) / np.arange(1, len(sarsa.episode_rewards) + 1)
    epsilon_avg = np.cumsum(greedy.episode_rewards) / np.arange(1, len(greedy.episode_rewards) + 1)
    
    # Plot all three 
    plt.plot(q_avg, label='Q-Learning (Random)', alpha=0.8)
    plt.plot(sarsa_avg, label='SARSA (Greedy)', alpha=0.8)
    plt.plot(epsilon_avg, label='Epsilon-Greedy', alpha=0.8)
    
    plt.xlabel('Epoch')
    plt.ylabel('Cumulative Average Reward')
    plt.title('Comparison')
    plt.legend()
    plt.grid(True)
    plt.show()

print()

print("--------------------------- Part 7 Comparing Rewards ---------------------------")

compare()
