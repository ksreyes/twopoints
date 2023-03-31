# LITERARY TEXTS DATA
# 22 March 2023


os.chdir('datasets/literary/')


# PACKAGES

# Standard packages
import numpy as np
import pandas as pd
import itertools
import sqlite3
import re
from scipy.spatial.distance import pdist
import json

# collections in Python >3.9 no longer has MutableSet module so we have to get it from collections.abc for the cache download to work
import collections
from collections.abc import MutableSet
collections.MutableSet = collections.abc.MutableSet

# Text analysis
from gutenbergpy.gutenbergcache import GutenbergCache, GutenbergCacheSettings
import gutenbergpy.textget
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity
from textblob import TextBlob
import umap.umap_ as umap


# ASSEMBLE GUTENBERG IDS

#GutenbergCache.create()

connection = sqlite3.connect(GutenbergCacheSettings.CACHE_FILENAME)

def query_author(name):
  inputs = name.split()
  query = (
    'SELECT DISTINCT books.gutenbergbookid, authors.name, titles.name, books.numdownloads '
    'FROM books, authors, book_authors, titles '
    'WHERE authors.id = book_authors.authorid ' 
		  'AND books.id = book_authors.bookid '
		  'AND books.id = titles.bookid '
		  'AND books.languageid = 1 '
		  'AND books.typeid = -1 '
      'AND %s '
    'ORDER BY books.numdownloads DESC '
    'LIMIT 50'
    % ' AND '.join(["authors.name LIKE ('%" + "%s" %n + "%')" for n in inputs])        
  )
  return query

def remove_duplicates(df):

  # Remove duplicates by Gutenberg book ID
  duplicated_ids = df['gutid'].duplicated(keep='first')
  df = df.loc[~duplicated_ids].reset_index(drop=True)

  # Extract similar titles
  vectorized = CountVectorizer(stop_words='english').fit_transform(list(df['title']))
  cosine_sim = cosine_similarity(vectorized)
  cosine_sim[np.triu_indices_from(cosine_sim)] = np.nan

  indices = []
  for i, row in enumerate(cosine_sim):
    if any(not np.isnan(x) and x >= 0.5 for x in row):
      indices.append(i)
  
  # But keep parts of a series
  indices_keep = np.where(df['title'].str.contains(pat='Vol|vol|Part|part'))[0]
  indices = [i for i in indices if i not in indices_keep]
  
  df = df.drop(indices).reset_index(drop=True)

  return df

authors = {
  'English': [
    'Walter Scott',
    'Jane Austen',
    'Mary Shelley',
    'William Thackeray',
    'Charles Dickens',
    'Charlotte Bronte',
    'Emily Bronte',
    'George Eliot',
    'Anthony Trollope',
    'Thomas Hardy'
  ], 
  'American': [
    'Louisa May Alcott',
    'Nathaniel Hawthorne',
    'Herman Melville',
    'Mark Twain',
    'Henry James'
  ], 
  'French': [
    'Stendhal',
    'Alexander Dumas',
    'Victor Hugo',
    'Gustave Flaubert',
    'Honore de Balzac',
    'Emile Zola'
  ], 
  'Russian': [
    'Nikolai Gogol',
    'Ivan Turgenev',
    'Fyodor Dostoevsky',
    'Leo Tolstoy'
  ]
}

library = pd.DataFrame(columns=['gutid', 'author', 'title', 'downloads', 'nationality'])

for nationality, names in authors.items():
  for name in names:
    results = pd.read_sql_query(query_author(name), connection)
    results.columns = ['gutid', 'author', 'title', 'downloads']
    results = remove_duplicates(results)
    results['query'] = name
    results['nationality'] = nationality
    library = pd.concat([library, results], ignore_index=True)

exclude_authors = [
  'Herr, Charlotte Bronte',
  'Stark, James Henry', 
  'Schmitz, James Henry', 
  'Maine, Henry James Sumner, Sir'
]

exclude_titles = [
  'Gutenberg', 
  
  # Poems
  'Poems', 
  'Ballads', 
  'Lyrics', 
  'Verses', 
  'Marmion: A Tale Of Flodden Field',
  'The Lady of the Lake',
  'The Mahogany Tree',
  'Richard Coeur de Lion and Blondel',
  'The Loving Ballad of Lord Bateman',
  
  # Biographical
  'Letters', 
  'Speeches',
  'literary and scientific men', 
  'Biographical Notes', 
  'My Memoirs', 
  'The Memoirs of Victor Hugo', 
  'An Autobiography of Anthony Trollope', 
  'The trial of Emile Zola',
  'The George Sand-Gustave Flaubert Letters', 
  
  # Duplicates
  'Les Misérables, v. 1/5: Fantine', 
  'The Grand Inquisitor', 
  'Fathers and Children',
  'The Charterhouse of Parma, Volume', 
  'Madame Bovary: A Tale of Provincial Life',
  'Home Life in Russia'
]

library = library.loc[~library['author'].str.contains('|'.join(exclude_authors))].reset_index(drop=True)
library = library.loc[~library['title'].str.contains('|'.join(exclude_titles))].reset_index(drop=True)


# DOWNLOAD TEXTS

def get_and_clean_text(id):
  
  # Get text
  raw_text = gutenbergpy.textget.get_text_by_id(id)
  text = gutenbergpy.textget.strip_headers(raw_text).decode()
  
  # Remove page-based line breaks and excessive paragraph breaks
  text = re.sub('(?<=[^\n])\n{1}(?=[^\n])', ' ', text)
  text = re.sub('[\n]{3,}', '\n\n', text)

  # Go paragraph by paragraph
  df = pd.DataFrame({'paragraph': text.split('\n\n')})
  patterns = [
    '^$',                   # Empty
    '^[A-Z\s\W\d]+$',       # Contains only uppercase letters (likely a heading)
    '^Chapter',             # Starts with "Chapter"
    '^CHAPTER',             # Starts with "Chapter"
    '^\s+Chapter',          # Starts with "Chapter"
    '^[A-Za-z]+$',          # Contains just one word, no punctuation
    'Gutenberg', 'Illustration', 'Online Distributed Proofreading Team', '.jpg'
  ]
  df = df.loc[~df['paragraph'].str.contains(pat='|'.join(patterns), regex=True)]
  clean_text = '\n\n'.join(list(df['paragraph']))
  
  return clean_text

texts = []
for row in range(len(library)):
  text = get_and_clean_text(library['gutid'][row])
  texts.append(text)

library['text'] = texts

connection.close()


# ASSEMBLE library_stats

library_stats = library[['nationality', 'query', 'title']].copy()

# Word count, sentiment, and subjectivity
library_stats['words'] = library['text'].apply(lambda n: len(n.split()))
library_stats['sentiment'] = library['text'].apply(lambda n: TextBlob(n).polarity)

# Average paragraph length
def parlength(text):
  df = pd.DataFrame({'paragraph': text.split('\n\n')})
  df['words'] = df['paragraph'].apply(lambda n: len(re.split(r' |—', n)))
  return df['words'].mean()
library_stats['parlength'] = library['text'].apply(lambda n: parlength(n))

library_vec = CountVectorizer(min_df=5, stop_words='english').fit_transform(library['text'])
embeddings = umap.UMAP(random_state=42).fit_transform(library_vec)
library_stats["x"] = embeddings[:,0]
library_stats["y"] = embeddings[:,1]


# ASSEMBLE library_network

pairs = list(set(itertools.combinations(library_stats.index.tolist(), 2)))
pairs.sort()
distances = pdist(library_stats[['x', 'y']].values)
values = max(distances) - distances

links = pd.DataFrame(pairs, columns=['source', 'target'])
links['value'] = values
links = links[~(links['value'] < 8.5)]
links['value'] = (links['value']-links['value'].min())*(5/(links['value'].max()-links['value'].min()))

nodes = pd.DataFrame({
  'id': library_stats.index,
  'title': library_stats['title'],
  'query': library_stats['query'],
  'nationality': library_stats['nationality']
})

# Convert dataframes to dictionaries, then to json
nodes_dict = nodes.to_dict(orient='records')
links_dict = links.to_dict(orient='records')
network_dict = {'nodes': nodes_dict, 'links': links_dict}
network = json.dumps(network_dict)


# EXPORT

library.to_csv('library.csv', index=False)
library_stats.to_csv('library_stats.csv', index=False)

with open('library_network.json', 'w') as f:
  f.write(network)
