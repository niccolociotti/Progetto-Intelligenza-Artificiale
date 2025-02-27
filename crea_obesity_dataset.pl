:- ensure_loaded('/Users/niccolociotti/Desktop/IA/Obesity-prediction-elaborato.pl').

startt :-
    tell('obesity_dataset.pl'),
    setof(Gender,A^B^C^D^E^F^G^H^I^L^M^N^aa(Gender,A,B,C,D,E,F,G,H,I,L,M,N),AttributiGender),
    write('a(gender,'), writeq(AttributiGender), writeln(').'),
    setof(Age,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,Age,C,D,E,F,G,H,I,L,M,N),AttributiAge),
    write('a(age,'), writeq(AttributiAge), writeln(').'),
    setof(Height,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,Height,D,E,F,G,H,I,L,M,N),AttributiHeight),
    write('a(height,'), writeq(AttributiHeight), writeln(').'),
    setof(Weight,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,Weight,E,F,G,H,I,L,M,N),AttributiWeight),
    write('a(weight,'), writeq(AttributiWeight), writeln(').'),
    setof(Family,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,Family,F,G,H,I,L,M,N),AttributiFamily),
    write('a(family,'), writeq(AttributiFamily), writeln(').'),
    setof(FAVC,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,F,FAVC,G,H,I,L,M,N),AttributiFAVC),
    write('a(favc,'), writeq(AttributiFAVC), writeln(').'),
    setof(NCP,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,F,G,NCP,H,I,L,M,N),AttributiNCP),
    write('a(ncp,'), writeq(AttributiNCP), writeln(').'),
    setof(Smoke,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,F,G,H,Smoke,I,L,M,N),AttributiSmoke),
    write('a(smoke,'), writeq(AttributiSmoke), writeln(').'),
    setof(Water,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,F,G,H,I,Water,L,M,N),AttributiWater),
    write('a(water,'), writeq(AttributiWater), writeln(').'),
    setof(Activity,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,F,G,H,I,L,Activity,M,N),AttributiActivity),
    write('a(activity,'), writeq(AttributiActivity), writeln(').'),
    setof(Alcohol,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,F,G,H,I,L,M,Alcohol,N),AttributiAlcohol),
    write('a(alcohol,'), writeq(AttributiAlcohol), writeln(').'),
    setof(MTrans,A^B^C^D^E^F^G^H^I^L^M^N^aa(A,B,C,D,E,F,G,H,I,L,M,N,MTrans),AttributiMTrans),
    write('a(mtrans,'), writeq(AttributiMTrans), writeln(').'),
    starttt.

starttt :-
    aa(Gender,Age,Height,Weight,Family,FAVC,NCP,Smoke,Water,Activity,Alcohol,Mtrans,'Obesity'),
    write('e(obeso,['),
    write('sesso = '),writeq(Gender), write(', '),
    write('età = '),writeq(Age), write(', '),
    write('altezza = '),writeq(Height), write(', '),
    write('peso = '),writeq(Weight), write(', '),
    write('storico_familiare = '),writeq(Family), write(', '),
    write('frequenza_cibi_calorici = '),writeq(FAVC), write(', '),
    write('numero_pasti = '),writeq(NCP), write(', '),
    write('fumo = '),writeq(Smoke), write(', '),
    write('acqua = '),writeq(Water), write(', '),
    write('attività_fisica = '),writeq(Activity), write(', '),
    write('alcool = '),writeq(Alcohol), write(', '),
    write('mezzi_trasporto = '),writeq(Mtrans), writeln(']).'),
    fail.

starttt :-
    aa(Gender,Age,Height,Weight,Family,FAVC,NCP,Smoke,Water,Activity,Alcohol,Mtrans,'Overweight'),
    write('e(sovrappeso,['),
    write('sesso = '),writeq(Gender), write(', '),
    write('età = '),writeq(Age), write(', '),
    write('altezza = '),writeq(Height), write(', '),
    write('peso = '),writeq(Weight), write(', '),
    write('storico_familiare = '),writeq(Family), write(', '),
    write('frequenza_cibi_calorici = '),writeq(FAVC), write(', '),
    write('numero_pasti = '),writeq(NCP), write(', '),
    write('fumo = '),writeq(Smoke), write(', '),
    write('acqua = '),writeq(Water), write(', '),
    write('attività_fisica = '),writeq(Activity), write(', '),
    write('alcool = '),writeq(Alcohol), write(', '),
    write('mezzi_trasporto = '),writeq(Mtrans), writeln(']).'),
    fail.

starttt :-
    aa(Gender,Age,Height,Weight,Family,FAVC,NCP,Smoke,Water,Activity,Alcohol,Mtrans,'Normal_Weight'),
    write('e(normopeso,['),
    write('sesso = '),writeq(Gender), write(', '),
    write('età = '),writeq(Age), write(', '),
    write('altezza = '),writeq(Height), write(', '),
    write('peso = '),writeq(Weight), write(', '),
    write('storico_familiare = '),writeq(Family), write(', '),
    write('frequenza_cibi_calorici = '),writeq(FAVC), write(', '),
    write('numero_pasti = '),writeq(NCP), write(', '),
    write('fumo = '),writeq(Smoke), write(', '),
    write('acqua = '),writeq(Water), write(', '),
    write('attività_fisica = '),writeq(Activity), write(', '),
    write('alcool = '),writeq(Alcohol), write(', '),
    write('mezzi_trasporto = '),writeq(Mtrans), writeln(']).'),
    fail.

starttt :-
    aa(Gender,Age,Height,Weight,Family,FAVC,NCP,Smoke,Water,Activity,Alcohol,Mtrans,'Insufficient_Weight'),
    write('e(sottopeso,['),
    write('sesso = '),writeq(Gender), write(', '),
    write('età = '),writeq(Age), write(', '),
    write('altezza = '),writeq(Height), write(', '),
    write('peso = '),writeq(Weight), write(', '),
    write('storico_familiare = '),writeq(Family), write(', '),
    write('frequenza_cibi_calorici = '),writeq(FAVC), write(', '),
    write('numero_pasti = '),writeq(NCP), write(', '),
    write('fumo = '),writeq(Smoke), write(', '),
    write('acqua = '),writeq(Water), write(', '),
    write('attività_fisica = '),writeq(Activity), write(', '),
    write('alcool = '),writeq(Alcohol), write(', '),
    write('mezzi_trasporto = '),writeq(Mtrans), writeln(']).'),
    fail.


starttt :- told.
