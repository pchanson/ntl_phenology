#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 19 09:59:10 2022
Based on / using code from GitLab repo: https://gitlab.com/di.ma/Connectivity_from_event_timing_patterns

@author: cal
"""

###########################################################################
###################### Loading Dependencies ###############################
###########################################################################
from __future__ import division
import os
import numpy as np
from scipy.spatial import distance
import scipy as sc
from matplotlib import pyplot as plt
import pandas as pd

# set working directory before importing functions from script
os.chdir("/home/cal/Documents/PostDoc/Projects/ntl_phenology")
from src.Functions.otsu_method import otsu
from src.Functions.calculate_AUCs import calculate_AUCs
from src.Functions.solve_neuron import solve_per_neuron

# define files to read in
data_path = "./Data/derived/"
all_file = "netcon_data_all.csv"
s_file = "netcon_data_S.csv"
n_file = "netcon_data_N.csv"
delay_path = "delay.dat"

# foucs just on all lakes for now

# import spike data 
allspikes = np.genfromtxt(os.path.join(data_path, all_file), delimiter=",", skip_header=1) 
all_ids, all_inv = np.unique(allspikes[:,0],return_inverse=True) #unique neuron identifiers for excitatory neurons
N_all = all_ids.size #number of nodes
Sspikes = np.genfromtxt(os.path.join(data_path, s_file), delimiter=",", skip_header=1) 
S_ids, S_inv = np.unique(Sspikes[:,0],return_inverse=True) #unique neuron identifiers for excitatory neurons
N_S = S_ids.size #number of nodes
Nspikes = np.genfromtxt(os.path.join(data_path, n_file), delimiter=",", skip_header=1) 
N_ids, N_inv = np.unique(Nspikes[:,0],return_inverse=True) #unique neuron identifiers for excitatory neurons
N_N = N_ids.size #number of nodes

all_spiketimes =[] #create list of lists 
for i in range(N_all):
    all_spiketimes.append([])
for i,(_,j) in enumerate(allspikes):
    all_spiketimes[all_inv[i]].append(j)

S_spiketimes =[] #create list of lists 
for i in range(N_S):
    S_spiketimes.append([])
for i,(_,j) in enumerate(Sspikes):
    S_spiketimes[S_inv[i]].append(j)
    
N_spiketimes =[] #create list of lists 
for i in range(N_N):
    N_spiketimes.append([])
for i,(_,j) in enumerate(Nspikes):
    N_spiketimes[N_inv[i]].append(j)
    
#convert spiketrains to interspike intervals
ISIs_all = list(map(np.diff, all_spiketimes))
ISIs_S = list(map(np.diff, S_spiketimes))
ISIs_N = list(map(np.diff, N_spiketimes))
#number of recorded intervals per neuron
all_num_interv_per_neuron = list(map(len,ISIs_all))
S_num_interv_per_neuron = list(map(len,ISIs_S))
N_num_interv_per_neuron = list(map(len,ISIs_N))

# empty matrix for estimated gradients
# first dimension pre-synaptic neuron
# second dimension post-synaptic
Jhat_all = np.zeros((N_all,N_all))
Jhat_S = np.zeros((N_S,N_S))
Jhat_N = np.zeros((N_N,N_N))

# interaction delays same structure as connectivity matrix
# (interaction delay may be identified by optimisation procedure)
# delay = np.genfromtxt(delay_file)
delay = np.array([0]) # NOTE: come back and change this; DONE: no "delay" in neuron sense, set to 0
De_all = np.ones((N_all,N_all))*delay[0] #for uniform delay
De_S = np.ones((N_S,N_S))*delay[0] #for uniform delay
De_N = np.ones((N_N,N_N))*delay[0] #for uniform delay

# number of events that will be employed in the solution
num_events = 500 

###########################################################################
############### Reconstruct for every neuron independently ################
########################################################################### 
for i in range(N_all):
    print('{:#<3} {:^50} {:#>3}'.format('','%d/%d'%(i,N_all),''))
    Jhat_all[:,i] = solve_per_neuron(i,N_all, all_spiketimes, De_all[:,i], ISIs_all[i], all_num_interv_per_neuron[i], num_events)
print('{:#<58}'.format(''))    
print('{:<58}'.format(''))  

for i in range(N_S):
    print('{:#<3} {:^50} {:#>3}'.format('','%d/%d'%(i,N_S),''))
    Jhat_S[:,i] = solve_per_neuron(i,N_S, S_spiketimes, De_S[:,i], ISIs_S[i], S_num_interv_per_neuron[i], num_events)
print('{:#<58}'.format(''))    
print('{:<58}'.format(''))  

for i in range(N_N):
    print('{:#<3} {:^50} {:#>3}'.format('','%d/%d'%(i,N_N),''))
    Jhat_N[:,i] = solve_per_neuron(i,N_N, N_spiketimes, De_N[:,i], ISIs_N[i], N_num_interv_per_neuron[i], num_events)
print('{:#<58}'.format(''))    
print('{:<58}'.format(''))  

###########################################################################
####################### Calc threshold and plot ###########################
########################################################################### 
thresholds_all = np.zeros(2)
thresholds_S = np.zeros(2)
thresholds_N = np.zeros(2)

thresholds_all[0] = otsu(Jhat_all[Jhat_all>=0])
thresholds_all[1] = otsu(Jhat_all[Jhat_all<=0])
thresholds_S[0] = otsu(Jhat_S[Jhat_S>=0])
thresholds_S[1] = otsu(Jhat_S[Jhat_S<=0])
thresholds_N[0] = otsu(Jhat_N[Jhat_N>=0])
thresholds_N[1] = otsu(Jhat_N[Jhat_N<=0])

plt.hist(Jhat_all)
plt.axvline(thresholds_all[0], color="red", ls='--')
plt.axvline(thresholds_all[1], color="red", ls='--')
plt.title("All Lakes")

plt.hist(Jhat_S)
plt.axvline(thresholds_S[0], color="red", ls='--')
plt.axvline(thresholds_S[1], color="red", ls='--')
plt.title("S Lakes")

plt.hist(Jhat_N)
plt.axvline(thresholds_N[0], color="red", ls='--')
plt.axvline(thresholds_N[1], color="red", ls='--')
plt.title("N Lakes")

###########################################################################
####################### convert to data frame and write ###################
########################################################################### 
Jhat_all_df = pd.DataFrame(Jhat_all)
Jhat_all_df['row'] = Jhat_all_df.index
Jhat_all_df = Jhat_all_df.melt(id_vars='row', var_name='column')
Jhat_all_df['inhib_thresh'] = thresholds_all[0]
Jhat_all_df['excit_thresh'] = thresholds_all[1]
Jhat_all_df.to_csv("Data/analysis_ready/network_conn_alllakes.csv", index=False)

Jhat_S_df = pd.DataFrame(Jhat_S)
Jhat_S_df['row'] = Jhat_S_df.index
Jhat_S_df = Jhat_S_df.melt(id_vars='row', var_name='column')
Jhat_S_df['inhib_thresh'] = thresholds_S[0]
Jhat_S_df['excit_thresh'] = thresholds_S[1]
Jhat_S_df.to_csv("Data/analysis_ready/network_conn_Slakes.csv", index=False)

Jhat_N_df = pd.DataFrame(Jhat_N)
Jhat_N_df['row'] = Jhat_N_df.index
Jhat_N_df = Jhat_N_df.melt(id_vars='row', var_name='column', index=False)
Jhat_N_df['inhib_thresh'] = thresholds_N[0]
Jhat_N_df['excit_thresh'] = thresholds_N[1]
Jhat_N_df.to_csv("Data/analysis_ready/network_conn_Nlakes.csv", index=False)

