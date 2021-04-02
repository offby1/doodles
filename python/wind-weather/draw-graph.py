import matplotlib.pyplot as plt
import pandas as pd

df = pd.read_csv('/Users/erichanchrow/git-repositories/me/doodles/python/wind-weather/2020-data.csv')
df[['cloudCover', 'windBearing']].plot(y='cloudCover', x='windBearing', kind='scatter')

plt.show()
