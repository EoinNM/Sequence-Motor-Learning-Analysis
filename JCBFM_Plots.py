import os
from os import *
import seaborn as sns
import pandas as pd
%matplotlib inline
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.patches as mp
import pylab

data_file = os.path.join('Path', 'Assessment.csv')
spft = pd.read_csv(data_file, index_col = 0)

#Left Panel - Learning Curves by Group

#get subsets
SSRI = spft.loc[(spft['Group'] == 'SSRI')]#subset to just SSRI group
PLAC = spft.loc[(spft['Group'] == 'Placebo')]#subset to just Placebo group

fig = plt.figure()
fig, (ax1) = plt.subplots(nrows = 1,ncols= 1) 

sns.set_style("white")

color1=["red"]
color2=["blue"]

lm1 = sns.pointplot(x='Block', y='Lag', data = PLAC, x_estimator = np.mean, fit_reg = False, ci=68, color="red", ax=ax1,
                    hue="Day", palette=color2) # CI = 68 to show SEM
title1 = plt.gca()
lm1.figure.set_size_inches(20,7)


lm3 = sns.pointplot(x='Block', y='Lag', data = SSRI, x_estimator = np.mean, fit_reg = False, ci=68, color = "blue", ax=ax1,
                   hue="Day", palette=color1) # CI = 68 to show SEM
title3 = plt.gca()
lm3.figure.set_size_inches(20,7)


ax1.set_title("Sequence Learning - Lag", fontsize = 25)
ax1.set_ylabel('Lag (ms)', fontsize = 25)
ax1.set_xlabel('', fontsize = 16, labelpad=25)
ax1.legend(["Escitalopram", "Placebo"], fontsize = 20)
ax1.tick_params(axis='both', labelsize="large")

leg = ax1.get_legend()
leg.legendHandles[0].set_color('red')
leg.legendHandles[1].set_color('blue')

lm1.figure.text(0.15, 0.05, "Baseline", ha ='left', fontsize = 20, weight='bold')
lm1.figure.text(0.3, 0.05, "Single D.", ha ='left', fontsize = 20, weight='bold')
lm1.figure.text(0.465, 0.05, "Sess. 3", ha ='left', fontsize = 20)
lm1.figure.text(0.615, 0.05, "Sess. 4", ha ='left', fontsize = 20)
lm1.figure.text(0.78, 0.05, "Steady St.", ha ='left', fontsize = 20, weight='bold')

for ind, label in enumerate(lm1.get_xticklabels()):
        label.set_visible(False)
        
ax1.set_ylim(0, 250)

sns.despine()

ax1.tick_params(labelsize=18)


#Left Panel plot - Boxplot
sns.set_style("white")
my_pal = {"Placebo": "blue", "Escitalopram": "red"}
my_pal3 = {"Placebo": "grey", "Escitalopram": "grey"}

fig, ax = plt.subplots(figsize=(10,8))

boxplot=sns.boxplot(x='Day', y='Lag', hue='Group', data=means, hue_order=['Placebo', 'Escitalopram'], 
                     palette=my_pal, fliersize=0, linewidth=0.5, whis=2, dodge=True, width=0.8, 
                     showmeans=True, 
                     meanprops={"marker":"s", "markerfacecolor":"white", "markeredgecolor":"black", "zorder":13},
                     boxprops={"zorder":13}, whiskerprops={'linewidth':1, "zorder":13}, 
                     ax=ax, zorder=13)

line =sns.pointplot(x='Day', y='Lag', hue='Group', data=means, legend=False, dodge= -0.4, 
                       linestyles='-', palette=my_pal3, ci=None, ax=ax, zorder=14, 
                        lineprops={'lines.linewidth':1})

plt.setp(line.lines, zorder=14)
plt.setp(line.collections, zorder=14, label="")

swarm=sns.swarmplot(x='Day', y='Lag', hue='Group', data=means, dodge=True, color=".25", 
                        hue_order=['Placebo', 'Escitalopram'], palette=None, size=6, ax=ax, zorder=15)

ax = plt.gca()
ax.set_ylabel("Lag (ms)", fontsize = 25)
ax.set_xlabel('', fontsize = 0, labelpad=0)
ax.set_xticklabels(["Baseline", "Steady State"], fontsize=25)
ax.tick_params(axis='y', labelsize="xx-large")
ax.tick_params(axis='x', pad= 25)
ax.set_ylim(-15, 350)
plt.yticks([0, 50, 100, 150, 200, 250, 300, 350])
ax.get_legend().remove()
sns.despine()