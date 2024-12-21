import matplotlib.pyplot as plt
import numpy as np

distData = np.loadtxt('dist.dat') #column 1 x values, column 2 probabilities

def S(x):
    ps = 0.75*np.exp(-np.abs(x-0.25))
    return ps/np.sum(ps)

#print(distData)

plt.figure()
lineP = plt.plot(distData[:,0], distData[:,1], linestyle='-', color='indigo', label='$P_{part}(x)$')
lineS = plt.plot(distData[:,0], S(distData[:,0]), linestyle='--', color='pink', label="S(x)")  #red line: x vs. s(x)
plt.title('Stationary PMF of particle position')
plt.xlabel('x')
plt.ylabel('Probability')
plt.grid(True, linestyle='-')
plt.savefig('dataplot.png')
plt.legend(loc='upper right')
plt.show()
