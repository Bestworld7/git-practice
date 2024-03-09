import pandas as pd 
import numpy as np
import seaborn as sns
from matplotlib import pyplot as plt 
titanic = sns.load_dataset("titanic")
print(titanic.isnull().sum())