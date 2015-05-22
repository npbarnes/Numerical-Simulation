# T' = -R[T-T_A]
# x^n+1 = x^n + dt*F^n

import matplotlib


interval = 15  # Minutes
divisions = 100
dt = interval/divisions
initialTemp = 83  # Degrees C

TempAir = 30
R = .5


Temp = [(0,initialTemp)]

for i in range(0,divisions-1):
    Temp[i+1] = Temp[i] + dt * (-R*(Temp[i]-TempAir))