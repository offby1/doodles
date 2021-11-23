import pathlib

import matplotlib.pyplot as plt # python3 -m pip install matplotlib
import pandas as pd             # python3 -m pip install pandas

__here__ = pathlib.Path(__file__).parent.absolute()

df = pd.read_csv(__here__ / '2021-data.csv')
df[['cloudCover', 'windBearing']].plot(y='cloudCover', x='windBearing', kind='scatter')

plt.show()
