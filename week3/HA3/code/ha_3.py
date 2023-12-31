# -*- coding: utf-8 -*-
"""HA_3.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1GA7AlN05Eejnlsq2IlnEwtrg5rmFVYpH

4.1
"""

import numpy as np

mr_good = np.array([47, 35])
mr_bad = np.array([22, 40])
mr_unknown = np.array([21, 36])

# Calculate the Euclidean distance
def distance_scale(c, v1, v2):

  tmp_v1 = v1
  tmp_v2 = v2


  tmp_v1[1] = c*v1[1] #scale money /2nd coordinate
  tmp_v2[1] = c*v2[1] #scale money /2nd coordinate

  distance = np.linalg.norm(tmp_v1 - tmp_v2)

  return(distance)

# Print the result
print(
    distance_scale(1, mr_bad,  mr_unknown),
    distance_scale(1, mr_good, mr_unknown)
    )
print(
    distance_scale(10**3, mr_bad,  mr_unknown),
    distance_scale(10**3, mr_good, mr_unknown)
    )

"""correlation"""

import numpy as np
from matplotlib import pyplot as plt

plt.rcParams["figure.figsize"] = [7.50, 3.50]
plt.rcParams["figure.autolayout"] = True

def f(eps):
   return 2 + 2*np.sqrt(1-eps)

x = np.linspace(0, 1, 100)

plt.plot(x, f(x), color='red')

plt.show()

import numpy as np
M = 20
delta = 0.05


np.log(M / delta) / (2 * 0.05**2)