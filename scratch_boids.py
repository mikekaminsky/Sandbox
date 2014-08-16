
import matplotlib
matplotlib.use('TKAgg')

from random import randint, random
import time

import numpy as np
from scipy.spatial.distance import pdist, squareform

import matplotlib.pyplot as plt

import scipy.integrate as integrate
import matplotlib.animation as animation

from operator import itemgetter

np.random.seed(0)
init_state = -0.5 + np.random.random((8, 4))
init_state[:, :2] *= 3.9
state = init_state

D = squareform(pdist(state[:, :2]))

def nearest(items,n):
  obj = []
  mins = items.tolist()[:n]
  mins.sort()
  for i in items[n:]:
      if i < mins[-1]: 
          mins.append(i)
          mins.sort()
          mins= mins[:n]
  for i in mins:
    loc = items.tolist().index(i) 
    obj.append(loc)
  return obj

def newvecs(state, nears):
  vecs = np.asarray([state[:, :2][i] for i in nears])
  xvec = reduce(lambda x, y: x + y, vecs[:,0]) / len(vecs[:,0])
  yvec = reduce(lambda x, y: x + y, vecs[:,1]) / len(vecs[:,1])
  return [xvec, yvec]



test = nearest(D[1],4)
testnew = newvecs(state, test)
