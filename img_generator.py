# -*- coding: utf-8 -*-
"""
Created on Sat Aug  6 16:34:08 2016

@author: mbesancon
"""

import numpy as np
import pandas as pd
from PIL import Image

matrix = pd.read_csv('mandelBlack.csv').values*255

img = Image.fromarray(matrix,'RGB')
img.save('mandelBlack.png')
img.show()