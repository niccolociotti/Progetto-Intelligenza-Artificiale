import sys
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import os

# Funzione che stampa la matrice di confusione
def print_confusion_matrix(values, algoritmo):
    VO, VSO, VN, VST, OSO, ON, OST, SOO, SN, SOST, NO, NSO, NST, STO, STSO, STN = values
    
    # Crea la matrice di confusione
    cm = np.array([[VO, OSO, ON, OST],
                   [SOO, VSO, SN, SOST],
                   [NO, NSO, VN, NST],
                   [STO, STSO, STN, VST]])

    # Stampa la matrice di confusione
    print("Matrice di Confusione:")
    print(cm)
    
    # Visualizza la matrice di confusione con una heatmap
    plt.figure(figsize=(8, 6))
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues', xticklabels=['Obeso', 'Sovrappeso', 'Normopeso', 'Sottopeso'], yticklabels=['Obeso', 'Sovrappeso', 'Normopeso', 'Sottopeso'])
    plt.xlabel('Predizioni')
    plt.ylabel('Etichette vere')
    plt.title('Matrice di confusione con algoritmo: C4.5' if algoritmo == "C45" else 'Matrice di confusione con algoritmo: Gini')
    
    # Salva la figura come PNG
    filename = os.path.join(os.getcwd(), 'confusion_matrix_'+ str(algoritmo)+'.png')
    plt.savefig(filename, format='png')
    print(f'Matrice di confusione salvata in: {filename}')
    
    # Mostra la figura
    plt.show()

# Prendi i valori passati come argomenti
# Ignora il primo elemento (il nome dello script)
values = [int(arg) for arg in sys.argv[1:17]]  
algoritmo = sys.argv[17]

# Stampa e salva la matrice di confusione
print_confusion_matrix(values, algoritmo)

