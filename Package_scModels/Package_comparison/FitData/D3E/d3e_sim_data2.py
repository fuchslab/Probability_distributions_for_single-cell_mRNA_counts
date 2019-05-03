import sys, pandas, time
import numpy as np
sys.path.insert(0, "D3E/")
from D3EUtil import readData, getParamsBayesian, getParamsMoments, cramerVonMises, logStatus, goodnessOfFit, distributionTest
from D3EUtil import Params, BioParams, Status, likelihoodRatio

data = pandas.read_csv("simData.out", sep = "\t")
data2 = data.set_index("GeneID")

d_moments = list()
d_bayesian = list()

seed_int = [52538352, 96846493, 5649396, 5847393]

for i in range(1,5):
  np.random.seed(seed_int[i-1])
  t_s = time.clock()
  params_t = getParamsMoments(data2.loc["SimGene"+`i`, : ])
  t_e = time.clock()
  d_moments.append([params_t.alpha, params_t.beta, params_t.gamma, t_e-t_s])
  
  t_s = time.clock()
  params_b, bio_paramsb = getParamsBayesian(data2.loc["SimGene" + `i`, : ])
  t_e = time.clock()
  d_bayesian.append([params_b.alpha.mean(), params_b.beta.mean(), round(params_b.gamma.mean())+1, t_e-t_s])

d3e_2_moments = pandas.DataFrame(data = d_moments, columns = ['alpha', 'beta', 'c', 'time'])
d3e_2_bayesian = pandas.DataFrame(data = d_bayesian, columns = ['alpha', 'beta', 'c', 'time'])
