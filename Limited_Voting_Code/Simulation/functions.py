import numpy as np 
import copy
import random
rng = np.random.default_rng()
# from config import*
import itertools
from bisect import insort

def pick(n, maxValue):
# Helper function for the random partition, from https://stackoverflow.com/questions/3722430/most-efficient-way-of-randomly-choosing-a-set-of-distinct-integers
    chosen = []
    for i in range(n):
        r = random.randint(0, maxValue - i)
        for e in chosen:
            if e <= r:
                r += 1
            else:
                break;
        insort(chosen, r)
    return chosen

def rand_part(n_cand, g):
# Helper function for 'create_parties_new'
# The  function generates a random partition of n_cand into g. 
# The idea comes from https://stackoverflow.com/questions/2161406/how-do-i-generate-a-uniform-random-integer-partition
# or from the book https://www2.math.upenn.edu/~wilf/website/CombinatorialAlgorithms.pdf , page 52
    a = pick(g-1, n_cand+g-1)
    r =[a[0]-1] #r_1 = a_1 - 1 
    for j in range(2,g): 
        # print(f'j {j}')
        # print(a[j-2])
        r.append(a[j-1]-a[j-2]-1)
    r.append(n_cand+g-1-a[g-2])
    return r

def create_parties_new(n_cand, g):
    # This function creates parties by taking a random partition, and returns a vector of sizes of the parties.
    sizes = rand_part(n_cand, g)
    while any(x <= 0 for x in sizes):# Since rand_part can return parties of size 0 or smaller, which we don't want, we redo it until no party has a <=0 number of members.
        sizes = rand_part(n_cand, g)
    return sizes


def disjoint_model(n_voters, n_cand, candidate_party_sizes, g, phi, p, voter_partition = True):
    # This function uses the disjoint model to generate an approval profile.
    # If voter_partition = True, the model is changed in how to distribute voters over parties.
    # If voter_partition = False, we use the original disjoint model.
    approval_profile = []
    if voter_partition: # We divide the voters in a random partition over the parties
        voter_party_sizes = create_parties_new(n_voters, g)
        party = 0 #To keep track of the party this voter is partitioned to
    for v in range(n_voters):
        if voter_partition:
            if v>= sum(voter_party_sizes[:party+1]): #To keep track of the party this voter is partitioned to
                party +=1        
        else: # We choose a party for each voter uniformly at random.
            # select a random party (numbered from 0 to g-1)
            party = random.randint(0, g-1)
        central_vote = [0]*n_cand
        party_start_position = sum(candidate_party_sizes[0:party])
        party_end_position = party_start_position+candidate_party_sizes[party]
        #set the selected party to approved
        for i in range(party_start_position, party_end_position):
            central_vote[i] = 1
        #loop through all candidates in the central vote 
        for candidate in range(len(central_vote)):
            # with probability phi, resample
            if random.random() < phi:
                #if we resample, we approve with probability p
                if random.random() < p:
                    central_vote[candidate] = 1 
                else:
                    central_vote[candidate] = 0
        approval_profile.append(central_vote)
    return approval_profile

def limited_vote(approval_profile, candidate_order, l):
    # This function gererates the limited vote profile.
    limited_votes =[]
    for approval_ballot in approval_profile:
        limited_vote = [0]*len(approval_ballot) # list of zeros of lenght of the approval ballot
        i = 0
        # Set the votes on the first l candidates of the order that are approved to 1:
        for candidate in candidate_order: #candidate_order is a list of names of candidates, the first is to be voted first.
            if i<l:
                if approval_ballot[candidate] == 1:
                    limited_vote[candidate] =1
                    i += 1
        if i<l: # all candidates are evaluated but still i<l: need to vote for not approved candidates:
            for candidate in candidate_order:
                if i<l:
                    if limited_vote[candidate] == 0:
                        limited_vote[candidate] =1
                        i+= 1
        limited_votes.append(limited_vote)
    return limited_votes

def compute_random_winner(k, n_cand, profile):
    # this function computes a winner given a profile, with random tiebreaking. 
    winning_committee = []
    # rank the candidates based on the number of votes for each candidate
    scores = [0]*n_cand #index indicates the candidate, content the score
    for ballot in profile:
        for candidate in range(len(ballot)):
            scores[candidate] += ballot[candidate]
    numpyscores = np.array(scores)
    # Fill the committee according to the highest scores:
    while len(winning_committee) < k: #there are still less than k candidates in the winning committee
        maximum = numpyscores.max()
        indices_maximum = np.where(scores == maximum)[0]
        if len(indices_maximum) > k - len(winning_committee): # only part of the candidates still fit in the winning committee
            indices_maximum = random.sample(population= list(indices_maximum),k=k-len(winning_committee))
        winning_committee.extend(indices_maximum)
        numpyscores = numpyscores[numpyscores != maximum]
    return winning_committee   

def cc_score(profile, committee):
    # This function computes the CC-score of a committee given an approval profile.
    cc = 0
    # Check for all voters whether they are represented:
    for ballot in profile:
        represented = 0
        # Check for all winning candidates whether they can represent that voter
        for winning_candidate in committee:
            if ballot[winning_candidate] == 1:
                represented = 1
                break
        cc += represented
    return cc 

def pav_score(profile, committee): 
    # This function computes the PAV-score of a committee given an approval profile.
    pav = 0.0
    # Check for all voters how much they are represented:
    for ballot in profile:
        W_intersect_Ai = 0
        for winning_candidate in committee:
            if ballot[winning_candidate] == 1:
                W_intersect_Ai += 1
        # apply formula for PAV score
        for i in range(W_intersect_Ai):
            pav += float(1.0/ (float(i)+1.0)) 
    return pav

def av_score(profile, committee): 
    # compute the AV-score
    av = 0
    # Check for all voters how much they are represented:
    for ballot in profile:
        W_intersect_Ai = 0
        for winning_candidate in committee:
            if ballot[winning_candidate] == 1:
                W_intersect_Ai += 1
        # sum scores for this agent:
        av += W_intersect_Ai
    return av


def mallows_sample(reference_ranking, phi):
    # Returns a ranking based on the reference_ranking 'candidate_list', with dispersion parameter phi
    # according to the Mallows model, using the Repeated Insertion Model as described in Lu, Boutilier (2014) (https://jmlr.org/papers/volume15/lu14a/lu14a.pdf)
    
    #Start with empty ranking r:
    r = []
    # For i= 1...m: insert the i'th element of the reference ranking at rank position j<=i with probability phi^(i-j)/(1+phi+...+phi^(i-1)).
    for i_index in range(len(reference_ranking)):
        i = i_index+1 #to account for the fact that the index starts at 0 but i starts at 1
        # p_i will be a list of probabilities of the positions where the i'th element could be placed:
        p_i = [0]*i
        for j_index in range (i):
            j = j_index+1
            # Calculate the probability of the i'th element being placed at position j:
            p_i[j_index]=  (phi**(i-j))/(sum(phi**z for z in range(i))) # Range(i) gives 0, 1, ..., i-1.
        # Pick the position of the i'th element at random according to the probability distribution specified in p_i:
        position_i = random.choices(population=range(i), weights=p_i)[0] # The [0] at the end is because 'choices' returns a list.
        # Insert the i'th element at the chosen position:
        r.insert(position_i, reference_ranking[i_index])
    return r

##################################################################################################################################
# The following two functions are not used, but could be used if one wanted to calculate the CC/PAV-improvement as described in 
# the paper, with the minimal score of LV compared to the maximal score of AV. For now, we used the above functions with random 
# tiebreaking, since it is a simulation study. 

def compute_minimal_winner(k, n_cand, profile, approval_profile, scoring_function):
    # This function computes the winning committee with the minimal score for 'scoring_function'.
    winning_committee = []
    # rank the candidates based on the number of votes for each candidate
    scores = [0]*n_cand #index indicates the candidate, content the score
    for ballot in profile:
        for candidate in range(len(ballot)):
            scores[candidate] += ballot[candidate]
    numpyscores = np.array(scores)
    # Fill the committee according to the highest scores:
    while len(winning_committee) < k: #there are still less than k candidates in the winning committee
        maximum = numpyscores.max()
        to_be_added = np.where(scores == maximum)[0] #was first called 'indices_maximum'
        if len(to_be_added) > k - len(winning_committee): # only part of the candidates still fit in the winning committee
            possible_complements = itertools.combinations(to_be_added, k-len(winning_committee)) # look at all possible ways to fill the current committee with the tied candidates
            winning_committee_reference = copy.deepcopy(winning_committee)
            minimal_score = 1500*n_cand # score can never be higher than this
            minimizing_complements = [] # not strictly neccessary to initialize here but neater
            for complement in possible_complements: # and choose the one which minimises the CC-score:
                winning_committee = copy.deepcopy(winning_committee_reference)
                winning_committee.extend(complement)
                score_this = scoring_function(approval_profile, winning_committee)
                if  score_this < minimal_score:
                    minimizing_complements = [complement]
                    minimal_score = score_this
                elif score_this == minimal_score:
                    minimizing_complements.append(complement)
            to_be_added = random.choice(minimizing_complements) # Choose a random one from the complements that minimize the score. Not necessary since we look only at the score, but neater.
        winning_committee.extend(to_be_added)
        numpyscores = numpyscores[numpyscores != maximum]
    return winning_committee 

def compute_maximal_winner(k, n_cand, profile, approval_profile, scoring_function):
    # This function computes the winning committee with the maximal score for 'scoring_function'.
    winning_committee = []
    # rank the candidates based on the number of votes for each candidate
    scores = [0]*n_cand #index indicates the candidate, content the score
    for ballot in profile:
        for candidate in range(len(ballot)):
            scores[candidate] += ballot[candidate]
    numpyscores = np.array(scores)
    # Fill the committee according to the highest scores:
    while len(winning_committee) < k: #there are still less than k candidates in the winning committee
        maximum = numpyscores.max()
        to_be_added = np.where(scores == maximum)[0] #was first called 'indices_maximum'
        if len(to_be_added) > k - len(winning_committee): # only part of the candidates still fit in the winning committee
            possible_complements = itertools.combinations(to_be_added, k-len(winning_committee)) # look at all possible ways to fill the current committee with the tied candidates
            winning_committee_reference = copy.deepcopy(winning_committee)
            maximal_score = 0 # score can never be < 0
            maximizing_complements = [] # not strictly neccessary to initialize here but neater
            for complement in possible_complements: # and choose the one which minimises the CC-score:
                winning_committee = copy.deepcopy(winning_committee_reference)
                winning_committee.extend(complement)
                score_this = scoring_function(approval_profile, winning_committee)
                if  score_this > maximal_score:
                    maximizing_complements = [complement]
                    maximal_score = score_this
                elif score_this == maximal_score:
                    maximizing_complements.append(complement)
            to_be_added = random.choice(maximizing_complements) # Choose a random one from the complements that maximize the score. Not necessary since we look only at the score, but neater.
        winning_committee.extend(to_be_added)
        numpyscores = numpyscores[numpyscores != maximum]
    return winning_committee 