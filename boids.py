"""
Trying to animate a 'boids-like' movement of objects through
2D space
Author: Michael Kaminsky
"""

import matplotlib
matplotlib.use('TKAgg')


from operator import itemgetter


import numpy as np
from scipy.spatial.distance import pdist, squareform

import matplotlib.pyplot as plt

import scipy.integrate as integrate
import matplotlib.animation as animation
import math


def nearest(dists,n):
  """Find the nearest n neighbors """
  obj = []
  mins = dists.tolist()[:n]
  mins.sort()
  for i in dists[n:]:
      if i < mins[-1]: 
          mins.append(i)
          mins.sort()
          mins= mins[:n]
  for i in mins:
    loc = dists.tolist().index(i) 
    obj.append(loc)
  return obj

def newvecs(state, nears):
  """Find average vector among nearest neighbors"""
  vecs = np.asarray([state[:, 2:][i] for i in nears])
  xvec = reduce(lambda x, y: x + y, vecs[:,0]) / len(vecs[:,0])
  yvec = reduce(lambda x, y: x + y, vecs[:,1]) / len(vecs[:,1])
  mag = math.hypot(xvec,yvec)
  xvec = xvec / mag
  yvec = yvec / mag

  return [xvec, yvec]


class BoidBox:
    """ State contains two pieces of information in 
    2D space. The first two columns contain the location
    of the particle, and the second two objects contain the 
    velocity vectors of the particles. Thus a single row
    describes a partilces point in space as well as the 
    speed and direction it is moving.
    """
    def __init__(self,
                 init_state = [[1, 0, 0, -1],
                               [-0.5, 0.5, 0.5, 0.5],
                               [-0.5, -0.5, -0.5, 0.5]],
                 bounds = [-1, 1, -1, 1],
                 size = 0.04,
                 M = 0.05,
                 G = 9.8):
        self.init_state = np.asarray(init_state, dtype=float)
        self.M = M * np.ones(self.init_state.shape[0])
        self.size = size
        self.state = self.init_state.copy()
        self.time_elapsed = 0
        self.bounds = bounds
        self.G = G

    def step(self, dt):
        """step once by dt seconds"""
        self.time_elapsed += dt
        
        # update positions
        # (this only updates the first two columns.
        # The second two columns never change
        self.state[:, :2] += dt * self.state[:, 2:]

        # find distance between points
        D = squareform(pdist(self.state[:, :2]))

        for idx, dist in enumerate(D):
            nears = nearest(dist,numboids)
            newv = newvecs(self.state, nears)
            newx = newv[0] 
            newy = newv[1] 

            # assign new velocities
            self.state[idx, 2] = newx
            self.state[idx, 3] = newy

        # check for crossing boundary
        crossed_x1 = (self.state[:, 0] < self.bounds[0] + self.size)
        crossed_x2 = (self.state[:, 0] > self.bounds[1] - self.size)
        crossed_y1 = (self.state[:, 1] < self.bounds[2] + self.size)
        crossed_y2 = (self.state[:, 1] > self.bounds[3] - self.size)

        self.state[crossed_x1, 0] = self.bounds[1]
        self.state[crossed_x2, 0]  = self.bounds[0]
        self.state[crossed_y1, 1] = self.bounds[3]
        self.state[crossed_y2, 1]  = self.bounds[2]

#------------------------------------------------------------
# set up initial state
np.random.seed(0)
init_state = -0.5 + np.random.random((300, 4))


init_state[:,2:] = init_state[:,:2] * -1
box = BoidBox(init_state, size=0.01)
dt = 1. / 30 # 30fps
numboids = 7

#dt = 1. / 200 

fig = plt.figure()


fig.subplots_adjust(left=0, right=1, bottom=0, top=1)
ax = fig.add_subplot(111, aspect='equal', autoscale_on=False,
                     xlim=(-3.2, 3.2), ylim=(-2.4, 2.4))


# particles holds the locations of the particles
particles, = ax.plot([], [], 'bo', ms=6)

# rect is the box edge
rect = plt.Rectangle(box.bounds[::2],
                     box.bounds[1] - box.bounds[0],
                     box.bounds[3] - box.bounds[2],
                     ec='none', lw=2, fc='none')
ax.add_patch(rect)


def init():
    """initialize animation"""
    global box, rect
    particles.set_data([], [])
    rect.set_edgecolor('none')
    return particles, rect

def animate(i):
    """perform animation step"""
    global box, rect, dt, ax, fig
    box.step(dt)

    ms = int(fig.dpi * 2 * box.size * fig.get_figwidth()
             / np.diff(ax.get_xbound())[0])
    
    # update pieces of the animation
    rect.set_edgecolor('k')
    particles.set_data(box.state[:, 0], box.state[:, 1])
    particles.set_markersize(ms)
    return particles, rect

ani = animation.FuncAnimation(fig, animate, frames=600,
                              interval=10, blit=True, init_func=init)


# save the animation as an mp4.  This requires ffmpeg or mencoder to be
# installed.  The extra_args ensure that the x264 codec is used, so that
# the video can be embedded in html5.  You may need to adjust this for
# your system: for more information, see
# http://matplotlib.sourceforge.net/api/animation_api.html
#ani.save('particle_box.mp4', fps=30, extra_args=['-vcodec', 'libx264'])

plt.show()

