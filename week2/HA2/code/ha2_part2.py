# -*- coding: utf-8 -*-
"""HA2_part2.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1_L7hWfwakviOwu7XptiiNHZ1qUP_usvz
"""

import numpy as np
import matplotlib.pyplot as plt


# Parameters for the Bernoulli distribution
p = 0.1  # Probability of success (e.g., 0.5 for a fair coin flip)
n = 20   # Number of Bernoulli random variables to sample

# Define the thresholds you want to check
#thresholds = [0.5 + i * 1/20 for i in range(11)]
thresholds = np.arange(0.1, 1.0, 1/20)


# Number of simulation runs
num_simulations = 10**3

# Perform simulations
results = [0]*len(thresholds)


# Run simulations
for _ in range(num_simulations):
        # Generate 20 random Bernoulli (ie one binomial) and divide by n to get mean
        mean_of_samples = np.random.binomial(n, p, 1) / n

        # Check if the mean is above the threshold
        for i in range(len(thresholds)):
          if mean_of_samples >= thresholds[i]:
            results[i] += 1

# Divide all elements by the divider using a list comprehension
result_prob_vector = [element / num_simulations for element in results]

# Print the results
print(thresholds)
print(result_prob_vector)
print(results)

# Create a new figure and axis
fig, ax = plt.subplots()

ax.plot(thresholds, result_prob_vector, color='black', marker='o', linestyle='-', label = "sim")
# Set y-limits for the plot
ax.set_ylim(0, 1)  # Adjust the limits as needed

# Add labels for the x and y axes
ax.set_xlabel('alpha')
ax.set_ylabel('Probability')
ax.set_title('p = 0.1')

# Add a legend to differentiate the lines and markers
ax.legend()

markov_bounds = 0.1 / thresholds
# Reference the existing plot by retrieving the current axes
# Create a new figure and axis
fig, ax = plt.subplots()

ax.plot(thresholds, result_prob_vector, color='black', marker='o', linestyle='-', label = "sim")
ax.plot(thresholds, markov_bounds, color='red', marker='o', linestyle='-', label = "Markov Bound")

# Set y-limits for the plot
ax.set_ylim(0, 1.2)  # Adjust the limits as needed

# Add labels for the x and y axes
ax.set_xlabel('alpha')
ax.set_ylabel('Probability')
# Add a legend to differentiate the lines and markers
ax.legend()

thresholds_cheb = thresholds - p

bound_cheb = np.minimum((p*(1-p))/(2*n*thresholds_cheb**2), 1)

# Create a new figure and axis
fig, ax = plt.subplots()

ax.plot(thresholds, result_prob_vector, color='black', marker='o', linestyle='-', label = "sim")
ax.plot(thresholds, markov_bounds, color='red', marker='o', linestyle='-', label = "Markov Bound")
ax.plot(thresholds, bound_cheb, color='orange', marker='o', linestyle='-', label = "Chebechev Bound")

# Set y-limits for the plot
ax.set_ylim(0, 1.2)  # Adjust the limits as needed

# Add labels for the x and y axes
ax.set_xlabel('alpha')
ax.set_ylabel('Probability')
# Add a legend to differentiate the lines and markers
ax.legend()

thresholds_hoef = thresholds - p



bound_hoef = np.minimum(np.exp(-2*n*thresholds_cheb**2), 1)

# Create a new figure and axis
fig, ax = plt.subplots()

ax.plot(thresholds, result_prob_vector, color='black', marker='o', linestyle='-', label = "sim")
ax.plot(thresholds, markov_bounds, color='red', marker='o', linestyle='-', label = "Markov Bound")
ax.plot(thresholds, bound_cheb, color='orange', marker='o', linestyle='-', label = "Chebechev Bound")
ax.plot(thresholds, bound_hoef, color='green', marker='o', linestyle='-', label = "Hoef Bound")



# Set y-limits for the plot
ax.set_ylim(0, 1.2)  # Adjust the limits as needed

# Add labels for the x and y axes
ax.set_xlabel('alpha')
ax.set_ylabel('Probability')
# Add a legend to differentiate the lines and markers
ax.legend()

import scipy.stats as stats

#using cdf for bionomial dist.
tail_probability_1   = 1 - stats.binom.cdf(n-1, n, p)
tail_probability_095 = 1 - stats.binom.cdf(n-2, n, p)

print(tail_probability_1, tail_probability_095)

#by hand
res1 = p**20
res2 = p**19*(1-p)*20 + res1

print(res1, res2)