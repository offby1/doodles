import pathlib

import matplotlib.pyplot as plt
import pandas as pd

__here__ = pathlib.Path(__file__).parent.absolute()

df = pd.read_csv(__here__ / '2020-data.csv')
df[['cloudCover', 'windBearing']].plot(y='cloudCover', x='windBearing', kind='scatter')

plt.show()
