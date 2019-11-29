import os

available_sequences = os.listdir("public/sequences")
f = open("src/Data.fs",'w')

text = """
module Data

let sequences = [{}]

""".format(";".join(['"{}"'.format(sq) for sq in available_sequences]))
print(text)
f.write(text)
f.close()