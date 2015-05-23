# T' = -R[T-T_A]
# x^n+1 = x^n + dt*F^n

import matplotlib.pyplot as plt

interval = 15  # Minutes
divisions = 100
dt = interval/divisions
initialTemp = 83  # Degrees C

TempAir = 30
R = .5

Data = [
        (0,83),
        (1,77.7),
        (2,75.1),
        (3,73),
        (4,71.1),
        (5,69.4),
        (6,67.8),
        (7,66.4),
        (8,64.7),
        (9,63.4),
        (10,62.1),
        (11,61),
        (12,59.9),
        (13,58.7),
        (14,57.8),
        (15,56.6)
    ]

Temp = [(0,initialTemp)]

for i in range(0,divisions-1):
    Temp.append( ( i*dt, Temp[i][1] + dt * (-R*(Temp[i][1]-TempAir)) ) )

# This splits Temp into a list of times and a list of temperatures.
simX, simY = zip(*Temp)
plt.plot(simX,simY)

dataX, dataY = zip(*Data)
plt.plot(dataX,dataY,"ro")

plt.show()
