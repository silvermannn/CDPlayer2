import xml.etree.ElementTree as ET

tree = ET.parse('dict.opcorpora.xml')

root = tree.getroot()

lemmata = root.find('lemmata')

lemmas = {}

i = 0

for lemma in lemmata:
    id = lemma.attrib['id']
    l = lemma.find('l')
    pos = l.find('g').attrib['v']
    fs = lemma.findall('f')
    #print(lemma.attrib['id'], pos)
    forms = {}
    first = None
    for f in fs:
        word = f.attrib['t']
        if first is None:
            first = word
        tags = [tag.attrib['v'] for tag in f.findall('g')]
        forms[word] = tags
        #print('\t', word, tags)

    lemmas[id] = {'pos' : pos, 'forms':forms, 'links':{}, 'first':first}
    i += 1
    #if i > 10:
    #    break

"""
<link_types>
<type id="1">ADJF-ADJS</type>
<type id="2">ADJF-COMP</type>
<type id="3">INFN-VERB</type>
<type id="4">INFN-PRTF</type>
<type id="5">INFN-GRND</type>
<type id="6">PRTF-PRTS</type>
<type id="7">NAME-PATR</type>
<type id="8">PATR_MASC-PATR_FEMN</type>
<type id="9">SURN_MASC-SURN_FEMN</type>
<type id="10">SURN_MASC-SURN_PLUR</type>
<type id="11">PERF-IMPF</type>
<type id="12">ADJF-SUPR_ejsh</type>
<type id="13">PATR_MASC_FORM-PATR_MASC_INFR</type>
<type id="14">PATR_FEMN_FORM-PATR_FEMN_INFR</type>
<type id="15">ADJF_eish-SUPR_nai_eish</type>
<type id="16">ADJF-SUPR_ajsh</type>
<type id="17">ADJF_aish-SUPR_nai_aish</type>
<type id="18">ADJF-SUPR_suppl</type>
<type id="19">ADJF-SUPR_nai</type>
<type id="20">ADJF-SUPR_slng</type>
<type id="21">FULL-CONTRACTED</type>
<type id="22">NORM-ORPHOVAR</type>
<type id="23">CARDINAL-ORDINAL</type>
<type id="24">SBST_MASC-SBST_FEMN</type>
<type id="25">SBST_MASC-SBST_PLUR</type>
<type id="26">ADVB-COMP</type>
<type id="27">ADJF_TEXT-ADJF_NUMBER</type>
</link_types>
"""

links = root.find('links')

for link in links:
    lfrom = link.attrib['from']
    lto = link.attrib['to']
    ltype = link.attrib['type']
    if lto in lemmas:
        lemmas[lto]['links'][ltype] = lfrom


uPOSTags = [
    "<>",   # service tag
    "ADJ",  # adjective
    "ADP",  # adposition
    "ADV",  # adverb
    "AUX",  # auxiliary
    "CCONJ",# coordinating conjunction
    "DET",  # determiner
    "INTJ", # interjection
    "NOUN", # noun
    "NUM",  # numeral
    "PART", # particle
    "PRON", # pronoun
    "PROPN",# proper noun
    "PUNCT",# punctuation
    "SCONJ",# subordinating conjunction
    "SYM",  # symbol
    "VERB", # verb
    "X"     # other
    ]

featureNames = [
    "PronType","Gender","VerbForm","NumType","Animacy","Mood","Poss","Tense","Reflex",
    "Number","Aspect","Other","Case","Voice","Abbr","Definite","Evident","Typo","Deixis","Polarity",
    "Foreign","DeixisRef","Person","ExtPos","Degree","Polite","Clusivity","NameType","Subcat","Style",
    "Variant",   # added for short verbs/adjs
    "Decl",      # added for noun w/o declension
    "Lang"       # added for multilingual support
    ]

featureValues = [
    "Art", "Dem", "Emp", "Exc", "Ind", "Int", "Neg", "Prs", "Rcp", "Rel",
    "Tot", "Com", "Fem", "Masc", "Neut", "Conv", "Fin", "Gdv", "Ger", "Inf",
    "Part", "Sup", "Vnoun", "Card", "Dist", "Frac", "Mult", "Ord", "Range",
    "Sets", "Anim", "Hum", "Inan", "Nhum", "Adm", "Cnd", "Des", "Imp", "Ind",
    "Int", "Irr", "Jus", "Nec", "Opt", "Pot", "Prp", "Qot", "Sub", "Yes",
    "Fut", "Imp", "Past", "Pqp", "Pres", "Yes", "Coll", "Count", "Dual",
    "Grpa", "Grpl", "Inv", "Pauc", "Plur", "Ptan", "Sing", "Stan", "Tri",
    "Hab", "Imp", "Iter", "Perf", "Prog", "Prosp", "Abs", "Acc", "Erg", "Nom",
    "Abe", "Ben", "Cau", "Cmp", "Cns", "Com", "Dat", "Dis", "Equ", "Gen",
    "Ins", "Par", "Tem", "Tra", "Voc", "Abl", "Add", "Ade", "All", "Del",
    "Ela", "Ess", "Ill", "Ine", "Lat", "Loc", "Per", "Sbe", "Sbl", "Spl",
    "Sub", "Sup", "Ter", "Act", "Antip", "Bfoc", "Cau", "Dir", "Inv", "Lfoc",
    "Mid", "Pass", "Rcp", "Yes", "Com", "Cons", "Def", "Ind", "Spec", "Fh",
    "Nfh", "Yes", "Abv", "Bel", "Even", "Med", "Nvis", "Prox", "Remt", "Neg",
    "Pos", "Yes", "1", "2", "0", "1", "2", "3", "4", "ADJ", "ADP", "ADV",
    "AUX", "CCONJ", "DET", "INTJ", "PRON", "PROPN", "SCONJ", "Abs", "Aug",
    "Cmp", "Dim", "Equ", "Pos", "Sup", "Elev", "Form", "Humb", "Infm", "Ex",
    "In", "Com", "Geo", "Giv", "Nat", "Oth", "Pat", "Pro", "Prs", "Sur",
    "Zoon", "Ditr", "Indir", "Intr", "Tran", "Arch", "Coll", "Expr", "Form",
    "Rare", "Slng", "Vrnc", "Vulg", "Short", "Zero"
    ]

posTAGMap = {
        "ADJS": "ADJ",
        "ADJF": "ADJ",
        "ADVB": "ADV",
        "COMP": "ADJ",
        "CONJ": "SCONJ",
        "GRND": "VERB",
        "INFN": "ADJ",
        "INTJ": "ADJ",
        "NOUN": "NOUN",
        "NPRO": "PRON",
        "NUMR": "NUM",
        "PRCL": "PART",
        "PRED": "ADV",
        "PREP": "ADP",
        "PRTF": "VERB",
        "PRTS": "VERB",
        "PNCT": "PUNCT",
        "SYM":  "SYM",
        "VERB": "VERB"
    }

posTAGAdditionalTags = {
        "ADJS": {"Variant": "Short"},
        "GRND": {"VerbForm":"Ger"},
        "INFN": {"VerbForm":"Inf"},
        "PRTF": {"VerbForm":"Part"},
        "PRTS": {"VerbForm":"Part", "Variant": "Short"}
    }

tagReplaceMap = {
        "Ques": {"PronType": "Int"},
        "Dmns": {"PronType": "Dem"},

        "anim": {"Animacy": "Anim"},
        "inan": {"Animacy": "Inan"},

        "sing": {"Number": "Sing"},
        "plur": {"Number": "Plur"},
        "Sgtm": {"Number": "Sing"},
        "Pltm": {"Number": "Plur"},

        "Anum": {"NumType": "Ord"},
        "Coll": {"NumType": "Sets"},

        "masc": {"Gender": "Masc"},
        "femn": {"Gender": "Fem"},
        "neut": {"Gender": "Neut"},

        "impf": {"Aspect": "Imp"},
        "perf": {"Aspect": "Perf"},

        "pres": {"Tense": "Pres"},
        "past": {"Tense": "Past"},
        "futr": {"Tense": "Fut"},

        "actv": {"Voice": "Act"},
        "pssv": {"Voice": "Pass"},

        "impr": {"Mood": "Imp"},
        "indc": {"Mood": "Ind"},

        "Impe": {"Subcat": "Indir"},
        "Impx": {"Subcat": "Indir"},
        "intr": {"Subcat": "Intr"},
        "tran": {"Subcat": "Tran"},

        "excl": {"Clusivity": "Ex"},
        "incl": {"Clusivity": "In"},

        "nomn": {"Case": "Nom"},
        "gent": {"Case": "Gen"},
        "gen1": {"Case": "Gen"},
        "gen2": {"Case": "Gen"},
        "datv": {"Case": "Dat"},
        "accs": {"Case": "Acc"},
        "acc2": {"Case": "Acc"},
        "ablt": {"Case": "Abl"},
        "loct": {"Case": "Loc"},
        "loc1": {"Case": "Loc"},
        "loc2": {"Case": "Loc"},
        "voct": {"Case": "Voc"},

        "Fixd": {"Decl": "Zero"},

        "1per": {"Person": "1"},
        "2per": {"Person": "2"},
        "3per": {"Person": "3"},

        "Abbr": {"Abbr": "Yes"},
        "Init": {"Abbr": "Yes"},
        "Name": {"NameType": "Giv"},
        "Surn": {"NameType": "Sur"},
        "Patr": {"NameType": "Pat"},
        "Orgn": {"NameType": "Com"},
        "Trad": {"NameType": "Com"},
        "Geox": {"NameType": "Geo"},

        "Supr": {"Degree": "Sup"},
        "Cmp":  {"Degree": "Cmp"},
        "Cmp2": {"Degree": "Cmp"},
        "Poss": {"Poss": "Yes"},

        "Erro": {"Typo": "Yes"},
        "Dist": {"Style": "Vrnc"},
        "Slng": {"Style": "Slng"},
        "Arch": {"Style": "Arch"},
        "Infr": {"Style": "Vrnc"},
        "Litr": {"Style": "Form"}
     }

class Dictionary:
    def __init__(self):
        self.posTAGs = {t:i for (t,i) in zip(uPOSTags, range(0, len(uPOSTags)))} 
        self.fNames = {t:i for (t,i) in zip(featureNames, range(0, len(featureNames)))}
        self.fValues = {t:i for (t,i) in zip(featureValues, range(0, len(featureValues)))}
        self.words = {}
        self.forms = []

    def getWord(self, word):
        if not word in self.words:
            self.words[word] = len(self.words)
        return self.words[word]

    def replaceTags(self, tags):
        result = {}
        for tag in tags:
            if tag in tagReplaceMap:
                result.update(tagReplaceMap[tag])
        return result

    def addForms(self, lemma, initial, pos):
        initialWordId = self.getWord(initial)
        for word, tags in lemma['forms'].items():
            wordId = self.getWord(word)
            if pos in posTAGAdditionalTags:
                additionalTags = posTAGAdditionalTags[pos]
            else:
                additionalTags = {}
            self.forms.append((wordId, initialWordId, self.posTAGs[posTAGMap[pos]], 
                               {(self.fNames[k], self.fValues[v]) for (k,v) in (additionalTags | self.replaceTags(tags)).items()}))
        
    def print(self):
        print([self.posTAGs, self.fNames, self.fValues, self.words, self.forms])

dictionary = Dictionary()

for id, lemma in lemmas.items():
    match lemma['pos']:
        case 'VERB':
            if '3' in lemma['links']:
                dictionary.addForms(lemma, lemmas[lemma['links']['3']]['first'], lemma['pos'])
            else:
                dictionary.addForms(lemma, lemma['first'], lemma['pos'])
        case 'PRTF':
            if '4' in lemma['links']:
                dictionary.addForms(lemma, lemmas[lemma['links']['4']]['first'], lemma['pos'])
            else:
                dictionary.addForms(lemma, lemma['first'], lemma['pos'])
        case 'PRTS':
            if '6' in lemma['links']:
                full = lemmas[lemma['links']['6']]
                dictionary.addForms(lemma, lemmas[full['links']['4']]['first'], lemma['pos'])
            else:
                dictionary.addForms(lemma, lemma['first'], lemma['pos'])
        case 'GRND':
            if '5' in lemma['links']:
                dictionary.addForms(lemma, lemmas[lemma['links']['5']]['first'], lemma['pos'])
            else:
                dictionary.addForms(lemma, lemma['first'], lemma['pos'])
        case 'COMP':
            if '2' in lemma['links']:
                dictionary.addForms(lemma, lemmas[lemma['links']['2']]['first'], lemma['pos'])
            else:
                dictionary.addForms(lemma, lemma['first'], lemma['pos'])
        case 'ADJS':
            if '1' in lemma['links']:
                dictionary.addForms(lemma, lemmas[lemma['links']['1']]['first'], lemma['pos'])
            else:
                dictionary.addForms(lemma, lemma['first'], lemma['pos'])
        case _: 
            dictionary.addForms(lemma, lemma['first'], lemma['pos'])
       
    #print(lemma)

dictionary.print()
