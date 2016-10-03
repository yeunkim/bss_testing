import sys
sys.path.append('/Users/yeunkim/bss_syncRNG')

from pandas import read_csv
import numpy as np
from pandas.util.testing import assert_frame_equal

from bss.modelspec import ModelSpec
from bss.stats_data import StatsData

model = ModelSpec('/Users/yeunkim/Data/Simulated_Data/modelspec_sim.ini')
sdata = StatsData(model.demographics, model)

csv = read_csv('/Users/yeunkim/Data/Simulated_Data/simulated_data_with_NA.csv')
csv =csv.dropna(subset=[['Sex','Score']])
csv= csv.reset_index(drop=True)

assert_frame_equal(sdata.demographic_data, csv)
