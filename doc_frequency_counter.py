#-*- coding:utf-8 -*-

import jieba
import sys
import os
import csv

from sklearn import feature_extraction
from sklearn.feature_extraction.text import CountVectorizer

def main():
	
	result = []

	lexicon = []
	lexicon_collaborate = []
	lexicon_compete = []
	lexicon_control = []
	lexicon_create = []


	# jieba.load_userdict('2016_ma/culture_fit_lexicon/collaborate.txt')
	jieba.load_userdict('lexicon/total_lexicon.txt')

	with open("lexicon/collaborate.txt", 'r', encoding='utf-8', errors='ignore') as f:
		for line in f:
			lexicon_collaborate.append(line.rstrip())
	with open("lexicon/compete.txt", 'r', encoding='utf-8', errors='ignore') as f:
		for line in f:
			lexicon_compete.append(line.rstrip())
	with open("lexicon/control.txt", 'r', encoding='utf-8', errors='ignore') as f:
		for line in f:
			lexicon_control.append(line.rstrip())
	with open("lexicon/create.txt", 'r', encoding='utf-8', errors='ignore') as f:
		for line in f:
			lexicon_create.append(line.rstrip())


	for filename in os.listdir("2016_ma/company/2015_txt/"):
		# print(filename)
		# filename = "2347_2015"
		corpus = []
		doc = ''

		collaborate_score = 0
		compete_score = 0 
		control_score = 0
		create_score = 0
		
		with open("2016_ma/company/2015_txt/"+filename, 'r', encoding='big5', errors='ignore') as f:
		    for line in f:
		        doc = doc + line

		

		# print ((doc.rstrip()))

		corpus.append(" ".join(jieba.cut(doc, cut_all=True)))

		vectorizer = CountVectorizer()

		X = vectorizer.fit_transform(corpus)

		word = vectorizer.get_feature_names()

		# print (len(word))
		# print (lexicon)


		for i in range(len(word)):
			if (word[i] in lexicon_collaborate):
				collaborate_score = collaborate_score + X[0,i]
				# print("lexicon_collaborate: ", word[i], X[0,i])

			elif(word[i] in lexicon_compete):
				compete_score = compete_score + X[0,i]
				# print("lexicon_compete: ", word[i], X[0,i])

			elif(word[i] in lexicon_control):
				control_score = control_score + X[0,i]
				# print("lexicon_control: ", word[i], X[0,i])

			elif(word[i] in lexicon_create):
				create_score = create_score + X[0,i]
				# print("lexicon_create: ", word[i], X[0,i])

		print(collaborate_score, compete_score, control_score, create_score)
		result.append([filename.replace(".txt",""), collaborate_score, compete_score, control_score, create_score])
	print(result)

	f = open("culture_fit.csv","w")
	w = csv.writer(f)
	w.writerows(result)
	f.close()

if __name__ == "__main__":
	main()