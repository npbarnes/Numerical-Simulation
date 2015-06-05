#!/usr/bin/python3

import numpy as np
import matplotlib.pyplot as plt

def getData(filename, column):
    with open(filename) as f:
        data = f.read()
    data = data.split('\n')

    # For some reason data gets a blank line at the end.
    # This just ignores the last entry.
    return [row.split()[column] for row in data[:-1]]

x = getData("./velocities",1)
y = getData("./velocities",2)

plt.plot(x,y)

plt.show()
