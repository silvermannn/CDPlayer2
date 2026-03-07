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
    if i > 220:
        break

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

def printForms(lemma, initial, pos):
    for word, tags in lemma['forms'].items():
        print(word, initial, pos, tags)

for id, lemma in lemmas.items():
    match lemma['pos']:
        case 'VERB':
            printForms(lemma, lemmas[lemma['links']['3']]['first'], lemma['pos'])
        case 'PRTF':
            printForms(lemma, lemmas[lemma['links']['4']]['first'], lemma['pos'])
        case 'PRTS':
            full = lemmas[lemma['links']['6']]
            printForms(lemma, lemmas[full['links']['4']]['first'], lemma['pos'])
        case 'GRND':
            printForms(lemma, lemmas[lemma['links']['5']]['first'], lemma['pos'])
        case 'COMP':
            printForms(lemma, lemmas[lemma['links']['2']]['first'], lemma['pos'])
        case 'ADJS':
            printForms(lemma, lemmas[lemma['links']['1']]['first'], lemma['pos'])
        case _: 
            printForms(lemma, lemma['first'], lemma['pos'])
       
    print(lemma)

