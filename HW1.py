# T' = -R[T-T_A]
# x^n+1 = x^n + dt*F^n

import matplotlib.pyplot as plt

interval = 15  # Minutes
divisions = 100
dt = interval/divisions
initialTemp = 83  # Degrees C

TempAir = 30
R = .5


Temp = [(0,initialTemp)]

for i in range(0,divisions-1):
    Temp.append( ( i*dt, Temp[i][1] + dt * (-R*(Temp[i][1]-TempAir)) ) )

# This splits Temp into a list of times and a list of temperatures.
simX, simY = zip(*Temp)
plt.plot(simX,simY)
plt.show()
