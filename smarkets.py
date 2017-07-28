'''
Solutions for the interview questions of SMarkets by Cenk Arioz
The assignment is to analyse data provided on https://smarkets.herokuapp.com/
'''

import requests
import pandas as pd
import numpy as np
from timeit import default_timer as timer

# Function to connect to the API and parse data
def getWebContent(page):
    url = "https://smarkets.herokuapp.com/" + page

    try:
        response = requests.get(url, timeout=(3.1, 27), verify=True)
        response.raise_for_status()
        data = response.json()
    except requests.ConnectionError as e:
        print ("Connection error: " + e)
    except requests.ConnectTimeout as e:
        print ("Connection timed out: " + e)
    except requests.HTTPError as e:
        print ("HTTP Error occured: " + e)

    response.close()
    return data

start = timer()

# Question 1. Find the affiliate with the maximum number of users.
user_data = pd.DataFrame(getWebContent('users'))

most_users = user_data.groupby(by='affiliate_id')['affiliate_id'].count()
most_users = np.array([most_users.index.values, most_users]).T

most_users = most_users[most_users[:,1].argsort()][::-1]
print('Affiliate {0} has the highest number of users with {1} \n'.format(most_users[0][0],most_users[0][1]))

# Question 2. Find the amount won by users coming through the top 3 affiliates - by user_count.

affiliate_data = pd.DataFrame(getWebContent('affiliates'))
affiliate_data.rename(columns={'id':'affiliate_id'},inplace=True)

user_performance = pd.DataFrame(data={'id':user_data['id'],
                                      'amount_won':np.zeros(len(user_data)),
                                      'lucky':np.zeros(len(user_data))})

# Walk through each user's bet records to reach amount gained and lucky bets (odds<25%)
for each in user_data['id']:
    winning_bets = pd.DataFrame(getWebContent("users/" + str(each) + "/bets"))
    winning_bets = winning_bets.loc[winning_bets['result']==True]

    user_performance.set_value(user_performance['id'] == each, 'amount_won', sum(winning_bets['amount']))

# Lucky bets (for Q3) are recorded here to avoid walking through the accounts twice.
    lucky_bets = sum(winning_bets['percentage_odds'].transform(int)<25)
    if lucky_bets > 1:
        user_performance.set_value(user_performance['id'] == each, 'lucky', 1)

user_data = user_data.merge(user_performance, how='left',on='id')

aff_amount_won = user_data.loc[user_data['affiliate_id']\
    .isin(most_users.T[0][:3])].groupby(by='affiliate_id')['amount_won'].sum()

print("Total amount won by users in top 3 affiliates are:")
for row in aff_amount_won.iteritems():
    print("Affiliate {0:d}: $ {1:.2f}".format(row[0],row[1]))

# Question 3. What is the percentage of users who have won 2 or more bets with low odds - say 25%.

lucky_user_pct = (sum(user_performance['lucky']) / len(user_data)) * 100
print("\n{0:.2f} % of all users have won 2 or more bets with lower than 25 % odds".format(lucky_user_pct))

end = timer()

print("\nAnalyses took {0:.2f} seconds".format(end - start))