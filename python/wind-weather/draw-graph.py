import pathlib

import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv(pathlib.Path('~/git-repositories/me/doodles/python/wind-weather/2020-data.csv').expanduser())
df[['cloudCover', 'windBearing']].plot(y='cloudCover', x='windBearing', kind='scatter')

plt.show()
