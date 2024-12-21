
import numpy as np
import matplotlib.pyplot as plt


def codonHistogram(geneStr):
    codon_dictionary = dict()
    for i in range(len(geneStr)):
        codon = geneStr[i:i+3]
        if codon not in codon_dictionary:
            codon_dictionary[codon] = 1
        else:
            codon_dictionary[codon] += 1
			
    codon3_dictionary = dict()
    for codon, freq  in codon_dictionary.items():
       if len(codon) >= 3:
          codon3_dictionary[codon] = freq
    
    nCodons = len(geneStr) - 2
    for codon in codon3_dictionary:
       codon3_dictionary[codon] /= nCodons

    return codon3_dictionary

def plotCodonHist(data, title='A Graph', saveAs='A file'):
    codonKey = list(data.keys())
    codonVal = list(data.values())

    plt.figure(figsize=[7,5])
    plt.bar(range(len(data)),list(data.values()), color='darkorchid')
    plt.xticks(range(len(data)), codonKey, fontsize=6, rotation=90)
    plt.yticks(fontsize=8)
    plt.xlabel('Codon')
    plt.ylabel('Frequency')
    plt.grid(True, linestyle='-')
    plt.title(title)
    plt.savefig(saveAs)
    plt.show()


def baseDensity(geneStr, nWind=200):
    num_A = np.zeros(len(geneStr)-(nWind+1))
    num_C = np.zeros(len(geneStr)-(nWind+1))
    num_G = np.zeros(len(geneStr)-(nWind+1))
    num_T = np.zeros(len(geneStr)-(nWind+1))

    for i in range((len(geneStr)-(nWind+1))):
       window  = geneStr[i:i+nWind] # creates substring
       num_A[i-1] = window.count('A')/nWind

    for i in range((len(geneStr)-(nWind+1))):
       window  = geneStr[i:i+nWind]
       num_T[i-1] = window.count('T')/nWind

    for i in range((len(geneStr)-(nWind+1))):
       window  = geneStr[i:i+nWind]
       num_C[i-1] = window.count('C')/nWind

    for i in range((len(geneStr)-(nWind+1))):
       window  = geneStr[i:i+nWind]
       num_G[i-1] = window.count('G')/nWind

    return tuple(num_A),tuple(num_T), tuple(num_C), tuple(num_G)

def plotDensity(A,T,C,G, title='A Title', saveAs='A file'):
    A_Xval = range(len(A))
    A_Yval = A

    T_Xval = range(len(T))
    T_Yval = T

    C_Xval = range(len(C))
    C_Yval = C

    G_Xval = range(len(G))
    G_Yval = G

    plt.figure(figsize=[10,5])
    lineA = plt.plot(A_Xval, A_Yval, linestyle='-', color='royalblue', label="A")
    lineT = plt.plot(T_Xval, T_Yval, linestyle='-', color='darkorange', label="T")
    lineC = plt.plot(C_Xval, C_Yval, linestyle='-', color='forestgreen', label="C")
    lineG = plt.plot(G_Xval, G_Yval, linestyle='-', color='firebrick', label="G")
    plt.title(title)
    plt.xlabel('Sequence Position')
    plt.ylabel('Fraction per Window')
    plt.grid(True, linestyle='-')
    plt.savefig(saveAs)
    plt.legend(loc='upper right')
    plt.show()


def klDivergence(HistP, HistQ):
    divergence  = 0
    for key in HistP:
       if key in  HistQ:
          divergence += HistP[key] * np.log(HistP[key]/HistQ[key])
       else:
          divergence += HistP[key]

    return divergence


if __name__=="__main__":
    # Open genome 
    with open('seqSC2.txt','r') as geneFile:
        scov2 = geneFile.readline()

    #Generate histogram for SARS-Cov-2
    histSC2 = codonHistogram(scov2)
    
    #print(sorted(histSC2.items()))
    #print(len(histSC2))
    #print(histSC2)
    
	# Plot histogram with nice formatting
    plotCodonHist(histSC2,'Codon frequency in SARS-CoV-2','histoSC2.png')
   
   # Find base-pair density
    dA,dT,dC,dG = baseDensity(scov2)
    #print(dA)#, dT, dC, dG)
    # Or supply a different window width
    # dA,dT,dC,dG = baseDensity(geneStr, nWind=500)

    plotDensity(dA,dT, dC, dG, 'Density of base pairs through gene sequence','density.png')

    with open('seqA.txt','r') as geneFile:
       seqA = geneFile.readline()

    histSeqA = codonHistogram(seqA)
    plotCodonHist(histSeqA, 'Codon Frequency in seqA', 'histoSeqA.png')

    with open('seqB.txt', 'r') as geneFile:
       seqB = geneFile.readline()

    histSeqB = codonHistogram(seqB)
    plotCodonHist(histSeqB, 'Codon Frequency in seqB', 'histoSeqB.png')

    print(f"kl Divergence of histSC2 and seqA is: {klDivergence(histSC2, histSeqA)}")
    print(f"kl Divergence of histSC2 and seqB is: {klDivergence(histSC2, histSeqB)}")


