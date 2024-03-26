# With this code, the parameters can be specified and the simulation can be ran. All the necessary functions are in 'functions.py'.
from functions import*
import itertools
import os.path
import pandas as pd

def dataframe(list_scores):
    df = pd.DataFrame(list_scores, columns = ['cc_lv', 'cc_av', 'pav_lv', 'pav_av', 'av_lv', 'av_av', 'nc', 'nv', 'g', 'p', 'phi', 'l', 'k'])
    return df

def config(list_scores):
    # Here the parameters for the simulation can be specified.
    n_cand_values = [24]        # Number of candidates (m in paper)
    n_voters_values = [1500]    # Number of voters (n in paper)
    g_values = [2,6,10]         # Number of parties
    p_values = [0.5]            # Probability for disjoint model
    k_values = [8,12,16] # 1/3, 1/2, or 2/3 of n_cand   # Committee size
    n_epochs = 1000                # Number of elections simulated per parameter value combination
    voter_partition = False     # If true, voters are distributed in a partition over parties in a partition
                                # taken uniformly at random from all partitions. If false, voters choose
                                # a party uniformly at random, so parties will have similar sizes.
    # # Main experiment:
    phi_values = [0, 0.25, 0.5, 0.75, 1]

    # Different values of l for phi=0 (l specified below, dependent on k)
    # phi_values = [0]

    # # Smaller values of phi: 
    # phi_values = [0.05, 0.1, 0.15, 0.2]


    #Make the following variable true if you want print feedback of intermediate steps:
    visible_comments = False

    df = pd.DataFrame(columns = ['cc_lv', 'cc_av', 'pav_lv', 'pav_av', 'av_lv', 'av_av', 'nc', 'nv', 'g', 'p', 'phi', 'l', 'k'])

    # Go through all different combinations of values of the parameters:
    for n_cand, n_voters, g, p, phi, k in itertools.product(n_cand_values, n_voters_values, g_values, p_values, phi_values, k_values):
        print(f'g: {g}, phi: {phi}, k: {k}')
        # Different values of l, dependent on k, uncomment the line you want:
        for l in [1, k/2, k]:
        # for l in [1, ka/4, ka/2, 3*ka/4, ka]: 
            if visible_comments:
                print(f'parameters: n_cand {n_cand}, n_voters {n_voters}, g {g}, p {p}, phi {phi}, l {l}, k {k}')
            ls = [] # Dataframe in which we will store the results
            for i in range(n_epochs):
                # In the 'setup' function, the actual simulation is done.
                ls.append(setup(n_cand, n_voters, g, p, phi, l ,k, visible_comments, voter_partition))
                if visible_comments:
                    print("---------------------------------------------------------")
            if df.empty:
                df = dataframe(ls)
            else:
                df = pd.concat([df, dataframe(ls)], ignore_index = True)
    if n_epochs > 0: 
        # Specify the path where you want to save the data:
        path = './Data/'
        # path = './Data/Voter_random_partition/'
        identifier = '_ncand' + str(n_cand_values) + '_nvoters'+ str(n_voters_values) + '_g'+ str(g_values) + '_p'+ str(p) + '_phi'+ str(phi_values) + '_k'+ str(k_values) + '_'
        number = 1
        name = 'results'  + identifier + str(number) + '.csv'
        filename = path+name
        while os.path.exists(filename): # If a file with this parameters already exists, add a higher number to create a new file
            number +=1
            name = 'results'  + identifier + str(number) + '.csv'
            filename = path+name
        df.to_csv(filename, index = False, sep=';') 

    return list_scores

def setup(n_cand, n_voters, g, p, phi, l ,k, visible_comments, voter_partition):
    # In this function, all steps of creating and running an election are done.
    # All unknown functions that are called can be found in 'functions.py'. 

    # Divide the candidates in a random partition over parties:
    sizes = create_parties_new(n_cand, g)
    if visible_comments:
        print(f"Parties: {sizes}")
    
    # Create a random approval profile according to the disjoint model (eventually modified in how to distribute voters over parties):
    approval_profile = []
    approval_profile = disjoint_model(n_voters, n_cand, sizes, g, phi, p, voter_partition= voter_partition)
    # if visible_comments:
    #     print(f"Approval profile: ")
    #     print(*approval_profile, sep='\n')
    
    # Determine original broadcasting order and modification:
    candidate_order = list(range(n_cand)) # Name all the candidates by giving them a number
    random.shuffle(candidate_order) # randomly shuffle them.
    if visible_comments:
        print(f'Original candidate order: {candidate_order}')
    candidate_order = mallows_sample(candidate_order, phi)
    if visible_comments:
        print(f'Noisy candidate order: {candidate_order}')

    # Determine the limited vote profile, based on the approval profile and the candidate order:
    limited_votes = limited_vote(approval_profile, candidate_order, l)
    # if visible_comments:
    #     print(f"Limited vote profile: ")
    #     print(*limited_votes, sep='\n')

# If you want to use randomization for the winners:
    # Determine winners:
    winner_lv = compute_random_winner(k, n_cand, limited_votes)
    if visible_comments: 
        print(f"winning_committee  LV = {winner_lv}")
    winner_av = compute_random_winner(k, n_cand, approval_profile)
    if visible_comments:
        print(f"winning_committee  AV = {winner_av}")

    # Calculate scores:
    cc_lv = cc_score(approval_profile, winner_lv) 
    if visible_comments: 
        print(f"cc-score LV: {cc_lv}")
    cc_av = cc_score(approval_profile, winner_av)
    if visible_comments: 
        print(f"cc-score AV: {cc_av}")
    pav_lv = pav_score(approval_profile, winner_lv) 
    if visible_comments:
        print(f"PAV-score LV: {pav_lv}")
    pav_av = pav_score(approval_profile, winner_av)
    if visible_comments: 
        print(f"PAV-score AV: {pav_av}")
    av_lv = av_score(approval_profile, winner_lv) 
    if visible_comments:
        print(f"AV-score LV: {av_lv}")
    av_av = av_score(approval_profile, winner_av)
    if visible_comments: 
        print(f"AV-score AV: {av_av}")   


# If you want to use the minimal scores for LV and maximal scores for AV: (but it takes too long to run)
    # winner_lv_min_cc = compute_minimal_winner(k, n_cand, limited_votes, approval_profile, cc_score)
    # if visible_comments: 
    #     print(f"winning_committee LV with minimal CC-score = {winner_lv_min_cc}")
    # cc_lv = cc_score(approval_profile, winner_lv_min_cc) 
    # if visible_comments: 
    #     print(f"minimal cc-score LV: {cc_lv}")

    # winner_lv_min_pav = compute_minimal_winner(k, n_cand, limited_votes, approval_profile, pav_score)
    # if visible_comments: 
    #     print(f"winning_committee LV with minimal PAV-score = {winner_lv_min_pav}")
    # pav_lv = pav_score(approval_profile, winner_lv_min_pav) 
    # if visible_comments:
    #     print(f"minimal PAV-score LV: {pav_lv}")

    # winner_av_max_cc = compute_maximal_winner(k, n_cand, approval_profile, approval_profile, cc_score)
    # if visible_comments:
    #     print(f"winning_committee AV with maximal CC-score = {winner_av_max_cc}")
    # cc_av = cc_score(approval_profile, winner_av_max_cc)
    # if visible_comments: 
    #     print(f"maximal cc-score AV: {cc_av}")
    
    # winner_av_max_pav = compute_maximal_winner(k, n_cand, approval_profile, approval_profile, pav_score)
    # if visible_comments:
    #     print(f"winning_committee AV with maximal PAV-score = {winner_av_max_pav}")
    # pav_av = pav_score(approval_profile, winner_av_max_pav)
    # if visible_comments: 
    #     print(f"maximal PAV-score AV: {pav_av}")

    return cc_lv, cc_av, pav_lv, pav_av, av_lv, av_av, n_cand, n_voters, g, p, phi, l ,k 


if __name__ == '__main__':
    list_scores = []
    config(list_scores)