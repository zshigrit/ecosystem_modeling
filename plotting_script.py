#import matplotlib.pyplot as plt
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
mpl.use('tkagg')

df = pd.read_csv('net_photosyn.csv')
df.plot.scatter(x='co2',y='an')
plt.savefig("test.png")
plt.show()
