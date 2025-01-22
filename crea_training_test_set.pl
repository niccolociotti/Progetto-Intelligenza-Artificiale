:- ensure_loaded('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/Obesity-prediction-elaborato.pl').

% Predicato che divide i dati in training (70%) e test (30%)
create_datasets :-
    % Recuperiamo tutti i dati
    findall(
        aa(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, Class),
        aa(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, Class),
        AllData
    ),
    % Calcoliamo la dimensione del set di training (70%)
    length(AllData, Total),
    TrainingSize is round(0.7 * Total),
    % Mescoliamo i dati
    random_permutation(AllData, ShuffledData),
    % Dividiamo in Training e Test
    length(TrainingData, TrainingSize),
    append(TrainingData, TestData, ShuffledData),
    % Scriviamo i dati nel file training
    tell('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/obesity_training.pl'),
    write_data(TrainingData, e),
    told,
    % Scriviamo i dati nel file test
    tell('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/obesity_test.pl'),
    write_data(TestData, s),
    told.

% Predicato per scrivere i dati nel formato richiesto nel file
write_data([], _).
write_data([aa(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, Class)|Rest], Type) :-
    write_entry(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, Class, Type),
    write_data(Rest, Type).

% Scriviamo ogni riga per i dati nel formato richiesto
write_entry(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, 'Obesity', Type) :-
    call(Type, 'obeso', Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans).

write_entry(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, 'Overweight', Type) :-
    call(Type, 'sovrappeso', Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans).

write_entry(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, 'Normal_Weight', Type) :-
    call(Type, 'normopeso', Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans).

write_entry(Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans, 'Insufficient_Weight', Type) :-
    call(Type, 'sottopeso', Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans).

% Predicati per scrivere in formato per training (e/17) e test (s/17)
e(Label, Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans) :-
    write('e('), write(Label), write(',['),
    write('gender = '), writeq(Gender), write(', '),
    write('age = '), writeq(Age), write(', '),
    write('height = '), writeq(Height), write(', '),
    write('weight = '), writeq(Weight), write(', '),
    write('family = '), writeq(Family), write(', '),
    write('favc = '), writeq(FAVC), write(', '),
    write('ncp = '), writeq(NCP), write(', '),
    write('smoke = '), writeq(Smoke), write(', '),
    write('water = '), writeq(Water), write(', '),
    write('activity = '), writeq(Activity), write(', '),
    write('alcohol = '), writeq(Alcohol), write(', '),
    write('mtrans = '), writeq(Mtrans), writeln(']).').

s(Label, Gender, Age, Height, Weight, Family, FAVC, NCP, Smoke, Water, Activity, Alcohol, Mtrans) :-
    write('s('), write(Label), write(',['),
    write('gender = '), writeq(Gender), write(', '),
    write('age = '), writeq(Age), write(', '),
    write('height = '), writeq(Height), write(', '),
    write('weight = '), writeq(Weight), write(', '),
    write('family = '), writeq(Family), write(', '),
    write('favc = '), writeq(FAVC), write(', '),
    write('ncp = '), writeq(NCP), write(', '),
    write('somke = '), writeq(Smoke), write(', '),
    write('water = '), writeq(Water), write(', '),
    write('activity = '), writeq(Activity), write(', '),
    write('alcohol = '), writeq(Alcohol), write(', '),
    write('mtrans = '), writeq(Mtrans), writeln(']).').
