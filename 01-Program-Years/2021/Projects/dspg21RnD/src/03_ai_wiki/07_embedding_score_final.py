#!/usr/bin/env python
# coding: utf-8

# # Implement Bert

# Author: Crystal

# Currently, it takes 9.663 seconds to run 100 abstracts. We are working with 690,815 abstract. 
# Model paraphrase-MiniLM-L6-v takes about 18.6 hours to run. 
# Model paraphrase-mpnet-base-v2 takes about hours to run, but has the best quality.

# In[1]:

conda install -c conda-forge sentence-transformers
conda install -c pytorch pytorch
conda install pandas
conda install -c conda-forge regex
conda install -c anaconda nltk
conda install -c conda-forge statistics
pip install python-dateutil

#bert
from sentence_transformers import SentenceTransformer, util

import torch

import pandas as pd

import re

import nltk
nltk.download("punkt") 
from nltk import tokenize


import statistics #calculate mean and others
import datetime

# In[2]:


embedder = SentenceTransformer('paraphrase-MiniLM-L6-v2') # this model provides quick model with high quality
#embedder = SentenceTransformer('paraphrase-mpnet-base-v2') # this model provides the best quality

abstracts = pd.read_pickle("/project/class/bii_sdad_dspg/uva_2021/dspg21RnD/FR-cleaned-2021FEB24.pkl")


# # Functions

# In[5]:


def get_corpus_embeddings(dir):
    with open(dir) as f:
        ai_text = f.read()
    ai_corpus = tokenize.sent_tokenize(ai_text) #sentence tokenization
    ai_embeddings = embedder.encode(ai_corpus, show_progress_bar=True) # embeddings
    return ai_embeddings


# k: number of similar sentences from AI corpus
# abstract: abstract from FEDERAL RePORTER
# print_result: if TRUE, print out the similar sentenses from AI corpus to each sentence in the abstract
def get_score(k, abstract, print_result = False):
    queries = tokenize.sent_tokenize(abstract) 

    # init a result list for scores
    result = []
    
    # Find the closest k sentences of the AI corpus for each query sentence (ML) based on cosine similarity
    top_k = min(k, len(ai_embeddings))
    
    for query in queries: #compare each sentence in the abstract to the ai corpus
        query_embedding = embedder.encode(query, show_progress_bar=False) 
        
        # We use cosine-similarity and torch.topk to find the highest k scores
        cos_scores = util.pytorch_cos_sim(query_embedding, ai_embeddings)[0]
        
        top_results = torch.topk(cos_scores, k=top_k)   #get the top k scores
        result.append(top_results.values.tolist()) #unlist the top result list
        if print_result:
            print("\n\n======================\n\n")
            print("Query:", query)
            print("Results:", top_results)
            print("\nTop", k, "most similar sentences in corpus:")
            for score, idx in zip(top_results[0], top_results[1]):
                print(ai_corpus[idx], "(Score: {:.4f})".format(score))
    return result


# In[6]:

start_time = datetime.datetime.now()
print("Start time for calculating embeddings:", start_time)

ai_embeddings = get_corpus_embeddings("/project/class/bii_sdad_dspg/uva_2021/dspg21RnD/ai_wiki_text.txt")
abstracts = abstracts.assign(score= abstracts["ABSTRACT"].apply(lambda x: get_score(10,x,False)))

end_time = datetime.datetime.now()
print("Finished calculating ", len(abstracts_try), "of", "embedding score at", end_time)
print("It took", end_time-start_time, "to run.")


# In[7]:
start_time = datetime.datetime.now()
print("Start time for calculating average score:", start_time)

sentence_score= []
for abstract in abstracts["score"]:
    sentence_score.append([statistics.mean(i) for i in abstract])

abstracts["sentence_score"]=sentence_score

end_time = datetime.datetime.now()
print("Finished calculating ", len(abstracts_try), "of", "average score at", end_time)
print("It took", end_time-start_time, "to run.")


# In[8]:
abstracts.to_csv(r'/project/class/bii_sdad_dspg/uva_2021/dspg21RnD/abstracts_embedding_score.csv', index = False)   





