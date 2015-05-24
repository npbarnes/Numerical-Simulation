# T' = -R[T-T_A]
# x^n+1 = x^n + dt*F^n

import matplotlib.pyplot as plt
plt.ion()

# Newton Cooling simulation using Euler method
def NewtonCooling(initTemp, R, air, endTime, divisions):
    ret = [(0,initTemp)]
    dt = endTime/divisions
    for i in range(0,divisions-1):
        ret.append( ( i*dt, ret[i][1] + dt * (-R*(ret[i][1]-air)) ) )
    return ret

interval = 15  # Minutes
divisions = 100

initialTemp = 83  # Degrees C

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


# Data to be plotted is split into x and y components
dataX, dataY = zip(*Data)

while(True):
    TempAir =   float(input("Air Temperature     : "))
    R =         float(input("Thermal conductivity: "))

    Temp = NewtonCooling(initialTemp, R, TempAir, interval, divisions)
    simX, simY = zip(*Temp)

    plt.cla()
    plt.plot(simX,simY)
    plt.plot(dataX,dataY,"ro")

    plt.show()
