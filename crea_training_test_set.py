import random

# Percorsi dei file
input_file = "/Users/niccolociotti/Desktop/IA/dataset_senza_attributi.pl"  # Sostituisci con il percorso del file .pl di input
output_training = "/Users/niccolociotti/Desktop/IA/Obesity_prediction-training-set.pl"  # File di training
output_test = "/Users/niccolociotti/Desktop/IA/Obesity_prediction-test-set.pl"  # File di test

# Funzione per leggere i dati dal file Prolog
def read_prolog_file(file_path):
    with open(file_path, "r") as file:
        lines = file.readlines()
    return [line.strip() for line in lines if line.strip()]

# Funzione per scrivere i dati in un file Prolog
def write_prolog_file(file_path, data, predicate):
    with open(file_path, "w") as file:
        for fact in data:
            # Sostituisci il predicato con il nuovo
            updated_fact = fact.replace("e(", f"{predicate}(")
            file.write(updated_fact + "\n")

# Leggi i fatti dal file di input
facts = read_prolog_file(input_file)

# Mescola i fatti e dividi in training (70%) e test (30%)
random.seed(42)
random.shuffle(facts)
split_index = int(0.7 * len(facts))
training_data = facts[:split_index]
test_data = facts[split_index:]

# Scrivi i fatti nei rispettivi file
write_prolog_file(output_training, training_data, "e")
write_prolog_file(output_test, test_data, "s")

print("File di training e test generati con successo!")
