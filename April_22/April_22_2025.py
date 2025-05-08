# This file runs analysis and viz on the April 22, 2025 tidy tuesday challenge

# import libraries
import pandas as pd # for everything
import numpy as np    # so pandas doesn't fail
import seaborn as sns   # viz                    
import matplotlib.pyplot as plt # more viz
from cmdstanpy import CmdStanModel # Stan! my man!
import statsmodels.api as sm # for OLS comps and gut checks
import patsy # for making OLS easier
import random # for setting the seed


# set seed
random.seed(4232025)

# Load our data from Posit
df = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents_420.csv')

# take a quick peak
df.head()

# Convert the date column to datetime object
df['date'] = pd.to_datetime(df['date'])

# Make a count day of the week column
df['day_int'] = df['date'].dt.dayofweek

#  Make day of the week as a name
df['day_name'] = df['date'].dt.day_name()

# create a year Column
df['year'] = df['date'].dt.year

# And a month column for good measure
df['month'] = df['date'].dt.month

# let's look at outliers
sns.histplot(x=df['fatalities_count'])

# and let's look at our relationships across the columns
sns.pairplot(df,  hue="day_name")
plt.show()

# Looks like there might be something going on with the day of the week
df.groupby(['day_name'])['fatalities_count'].mean().sort_values(ascending=False)
# So there are definitely higher deaths on average on Saturday

# what about 420 and day?
df[df['e420']==True].groupby(['day_name'])['fatalities_count'].mean().sort_values(ascending=False)

# Let's see what that looks like

# make a facet grid
g = sns.FacetGrid(df, col="day_name", hue="e420", sharex=True, sharey=True, col_wrap=3)

# make a lil density plot by group
g.map_dataframe(sns.kdeplot, x="fatalities_count", fill=True)

# Add legends
g.add_legend()

plt.show()

# MODEL TIME~~~~~~~~------

# So since we have count data, we want to use a poisson regression
# there's a ton of good stuff online about why, but it basically comes down 
# to the fact that with OLS we could estimate *negative* fatalities, which makes no sense
# Poisson also handles zero counts better--we could also process our data with OLS
# and simply take the log, but log(0) takes you to the nether realm and Poisson avoids this

# also, I'm not going to mess with the 420 thing because I am not sure there's much there

# Because we're using Stan, we have to make some index variables
df['day_idx'] = df['day_name'].astype('category').cat.codes + 1
df['month_idx'] = df['month']
df['year_idx'] = df['year'] - df['year'].min() + 1

# Create our stan data object
stan_data = {
    'N': df.shape[0],
    'fatalities': df['fatalities_count'].values,
    'day': df['day_idx'].values,
    'month': df['month_idx'].values,
    'year': df['year_idx'].values,
    'N_day': df['day_idx'].nunique(),
    'N_month': df['month_idx'].nunique(),
    'N_year': df['year_idx'].nunique()
}

# fit out model!
stan_model = CmdStanModel(stan_file='April_22/April_22_2025.stan')
stan_fit = stan_model.sample(data=stan_data, chains=4, iter_sampling=1000, iter_warmup=1000, parallel_chains=4)

# now, we want to see if day of the week matters
# Pull out day effects
day_effects = stan_fit.draws_pd().filter(like='day_effect')
day_effects_mean = day_effects.mean()

# Cool, let's compare to OLS 
ols_formula = 'fatalities_count ~ C(day_name) + C(month) + C(year)'
ols_y, ols_X = patsy.dmatrices(ols_formula, data=df, return_type='dataframe')
ols_model = sm.OLS(ols_y, ols_X).fit()

# prep visualization
days = df['day_name'].astype('category').cat.categories

# poisson predicted means (already log(alpha) + day_effect)
day_effect_columns = [col for col in day_effects.columns if 'day_effect[' in col]
poisson_day_means = day_effects[day_effect_columns].mean().values  # Only 7 day effects
alpha_mean = stan_fit.draws_pd().filter(like='alpha').mean().values[0]
poisson_preds = np.exp(alpha_mean + poisson_day_means)


# OLS predicted means (set month and year to mean)
mean_month = df['month'].mean()
mean_year = df['year'].mean()

new_data = pd.DataFrame({
    'day_name': days,
    'month': [int(mean_month)] * len(days),
    'year': [int(mean_year)] * len(days)
})

# Generate the design matrix for the new data using the original design info
ols_X_pred = patsy.build_design_matrices([ols_X.design_info], new_data)[0]

# Predict using the OLS model
ols_preds = ols_model.predict(ols_X_pred)

# before we plot we need to order the days 
day_order = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday']

poisson_preds_ordered = [poisson_preds[list(days).index(d)] for d in day_order]
ols_preds_ordered = [ols_preds[list(days).index(d)] for d in day_order]

# actuals for comparison
observed_means = df.groupby('day_name')['fatalities_count'].mean().reindex(day_order)


# Set font to Gill Sans 
plt.rcParams["font.family"] = "Gill Sans"
plt.rcParams.update({'font.size': 16})
plt.figure(figsize=(10, 8))

# Plot Poisson
plt.plot(day_order, poisson_preds_ordered, color='#6495ED',  linewidth = 2)
plt.annotate('Poisson (for count data)', 
             xy=(len(day_order)-1, poisson_preds_ordered[-1]),
             xytext=(5, 0),  # Offset x=5 pixels to the right
             textcoords='offset points',
             va='center', ha='left',
             fontsize=14, fontweight='bold')

# Plot OLS
plt.plot(day_order, ols_preds_ordered, linestyle='--', color='#6F8FAF', linewidth = 2)
plt.annotate('OLS', 
             xy=(len(day_order)-1, ols_preds_ordered[-1]),
             xytext=(5, 0),
             textcoords='offset points',
             va='center', ha='left',
             fontsize=14, fontweight='bold')
# Labels
plt.ylabel('Predicted Fatalities')

plt.scatter(day_order, observed_means, color='black', zorder=5, label='Observed Means')

plt.suptitle('Predicted Fatalities by Day of the Week 1992-2016 (Poisson vs. OLS)', fontsize=16, ha = 'center')
plt.title('Models adjust for year and month, but Poisson is meant for count data and is avoids overestimation', ha = 'center', 
          fontsize=13, fontweight='normal', style='italic')
plt.legend().set_visible(False)
# I don't like graph borders
plt.gca().spines['top'].set_visible(False)
plt.gca().spines['right'].set_visible(False)
plt.gca().spines['bottom'].set_visible(False)
plt.gca().spines['left'].set_visible(False)
plt.grid(True)
plt.tight_layout()
plt.show()

# Save figure
plt.savefig("poisson_vs_ols.png", dpi=300)
