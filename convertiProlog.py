import pandas as pd

# Specifica il nome del file Excel e il file di output Prolog
input_excel = "/Users/niccolociotti/Desktop/IA/Obesity-prediction-modificato.xlsx"
output_prolog = "/Users/niccolociotti/Desktop/IA/Obesity-prediction-elaborato.pl"

# Leggi il file Excel
df = pd.read_excel(input_excel)

# Apri il file di output Prolog
with open(output_prolog, "w") as f:
    # Per ogni riga del DataFrame, crea un fatto Prolog
    for index, row in df.iterrows():
        # Genera un fatto Prolog con i valori delle colonne
        fatto = "aa(" + ", ".join([repr(val) for val in row]) + ").\n"
        f.write(fatto)

print(f"Fatti Prolog salvati in {output_prolog}")
