import pandas as pd
import numpy as np
import seaborn as sns

titanic = sns.load_dataset("titanic")
print(titanic.describe())
print(titanic.info())