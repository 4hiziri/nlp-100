import re

nlp = ''
with open('resource/nlp.txt', 'r') as f:
    nlp = f.read()


# 50
def split_sentence(string):
    sentence_list = []
    pile = 0
    for pos in re.finditer(r'(\.|;|:|\?|!) [A-Z]', string):
        pos = pos.span()
        start = pos[0]
        end = pos[1]
        tmp = string[pile:start]
        sentence_list.append(tmp)
        pile = end

    return sentence_list


def parse_sentence(string):
    return list(map(lambda x: x.replace('\n', ' '), split_sentence(string)))


sentences = parse_sentence(nlp)


# 51
def split_word(sentence):
    return sentence.split()


def parse_words(sentences):
    return list(map(split_word, sentences))


words = parse_words(sentences)

# 52
from stemming.porter2 import stem

def parse_word_list(word_list):
    return list(map(lambda x: (x, stem(x)), word_list))

def parse_words(words):
    return list(map(parse_word_list, words))
