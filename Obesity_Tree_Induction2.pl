% programma per apprendere inducendo Alberi di Decisione testandone
% l' efficacia

%:- working_directory(_, '/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale').


:- ensure_loaded('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/obesity_attributi.pl').
:- ensure_loaded('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/obesity_training.pl').
:- ensure_loaded('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/obesity_test.pl').

:- dynamic alb/1.

induce_albero(Albero, Algoritmo) :-
	findall( e(Classe,Oggetto), e(Classe,Oggetto), Esempi),
        findall( Att,a(Att,_), Attributi),
        induce_albero( Attributi, Esempi, Algoritmo, Albero),
	mostra( Albero ),
	scrivi_albero_su_file('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/albero.txt', Albero),
	assert(alb(Albero)).

% Predicate to write the tree to a file
scrivi_albero_su_file(FileName, Albero) :-
    open(FileName, write, Stream),
    write(Stream, Albero),
	write(Stream, '.'),
    close(Stream).

carica_albero_da_file(FileName, Albero) :-
	open(FileName, read, Stream),  	
	read(Stream, Albero),    		
	close(Stream).            		
	

% induce_albero( +Attributi, +Esempi, -Albero):
% l'Albero indotto dipende da questi tre casi:
% (1) Albero = null: l'insieme degli esempi è vuoto
% (2) Albero = l(Classe): tutti gli esempi sono della stessa classe
% (3) Albero = t(Attributo, [Val1:SubAlb1, Val2:SubAlb2, ...]):
%     gli esempi appartengono a più di una classe
%     Attributo è la radice dell'albero
%     Val1, Val2, ... sono i possibili valori di Attributo
%     SubAlb1, SubAlb2,... sono i corrispondenti sottoalberi di
%     decisione.
% (4) Albero = l(Classi): non abbiamo Attributi utili per
%     discriminare ulteriormente
induce_albero( _, [],_, null ) :- !.			         % (1)
induce_albero( _, [e(Classe,_)|Esempi],_, l(Classe)) :-	         % (2)
	\+ ( member(e(ClassX,_),Esempi), ClassX \== Classe ),!.  % no esempi di altre classi (OK!!)
induce_albero( Attributi, Esempi, Algoritmo, t(Attributo,SAlberi) ) :-	 % (3)
	scegli_attributo( Attributi, Esempi, Algoritmo, Attributo), !,     % implementa la politica di scelta
	del( Attributo, Attributi, Rimanenti ),			 % elimina Attributo scelto
	a( Attributo, Valori ),					 % ne preleva i valori
	induce_alberi( Attributo, Valori, Rimanenti, Algoritmo, Esempi, SAlberi).
induce_albero( _, Esempi, Algoritmo, l(Classi)) :-                          % finiti gli attributi utili (KO!!)
	findall( Classe, member(e(Classe,_),Esempi), Classi).


scegli_attributo(Attributi, Esempi, Algoritmo, Attributo) :-
	(   Algoritmo == 'Gini'
	->  scegli_attributo_gini(Attributi, Esempi, Attributo)
	;   Algoritmo == 'C45'
	->  scegli_attributo_C4_5(Attributi, Esempi, Attributo)
	;   format('Algoritmo non supportato: ', [Algoritmo]),
		fail
	).


% Caso finale: nessun attributo rimanente, si restituisce la lista delle classi
induce_albero(_, Esempi,_, l(Classi)) :-
    findall(Classe, member(e(Classe,_), Esempi), Classi).

% scegli_attributo_gini( +Attributi, +Esempi, -MigliorAttributo):
% seleziona l'Attributo che meglio discrimina le classi; si basa sul
% concetto della "Gini-disuguaglianza"; utilizza il setof per ordinare
% gli attributi in base al valore crescente della loro disuguaglianza
% usare il setof per far questo è dispendioso e si può fare di meglio ..
scegli_attributo_gini( Attributi, Esempi, MigliorAttributo )  :-
	setof( Disuguaglianza/A,
	      (member(A,Attributi) , disuguaglianza(Esempi,A,Disuguaglianza)),
	      [MinorDisuguaglianza/MigliorAttributo|_] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% scegli_attributo_C4_5( +Attributi, +Esempi, -MigliorAttributo):
% Sceglie l'attributo che massimizza il guadagno informativo
scegli_attributo_C4_5(Attributi, Esempi, MigliorAttributo) :-
    setof(Gain/A, (member(A, Attributi), guadagno_informativo(Esempi, A, Gain)), [MaxGain/MigliorAttributo|_]),
    % L'attributo con il guadagno informativo massimo è scelto
    writeln(MigliorAttributo).

% guadagno_informativo(+Esempi, +Attributo, -Gain):
% Calcola il guadagno informativo di un attributo
guadagno_informativo(Esempi, Attributo, Gain) :-
    entropia(Esempi, EntropiaE),
    a(Attributo, Valori), % prendi i valori dell'attributo
    somma_entropie(Esempi, Attributo, Valori, Somma),
    Gain is EntropiaE - Somma.

% entropia(+Esempi, -Entropia):
% Calcola l'entropia di un insieme di esempi
entropia(Esempi, Entropia) :-
    findall(Classe, member(e(Classe,_), Esempi), Classi),
    distribuzione_classi(Classi, Distribuzione),
    calcola_entropia(Distribuzione, Entropia).

% distribuzione_classi(+Classi, -Distribuzione):
% Calcola la distribuzione delle classi in Esempi
distribuzione_classi(Classi, Distribuzione) :-
    length(Classi, N),
    findall(X, (member(X, Classi)), Unici),
    length(Unici, M),
    findall(P, (member(U, Unici), findall(1, (member(U, Classi)), S), length(S, L), P is L / N), Distribuzione).

% calcola_entropia(+Distribuzione, -Entropia):
% Calcola l'entropia data una distribuzione delle probabilità
calcola_entropia(Distribuzione, Entropia) :-
    findall(P * log(P), (member(P, Distribuzione), P > 0), LogTerms),
    sum_list(LogTerms, S),
    Entropia is -S.

% somma_entropie(+Esempi, +Attributo, +Valori, -Somma):
% Calcola la somma delle entropie per ciascun valore dell'attributo
somma_entropie(Esempi, Attributo, [Val|Valori], Somma) :-
    attval_subset(Attributo=Val, Esempi, Subset),
    entropia(Subset, EntropiaVal),
    length(Subset, NSubset),
    length(Esempi, N),
    Peso is NSubset / N,
    SommaParziale is Peso * EntropiaVal,
    somma_entropie(Esempi, Attributo, Valori, Somma1),
    Somma is SommaParziale + Somma1.
somma_entropie(_, _, [], 0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% disuguaglianza(+Esempi, +Attributo, -Dis):
% Dis è la disuguaglianza combinata dei sottoinsiemi degli esempi
% partizionati dai valori dell'Attributo
disuguaglianza( Esempi, Attributo, Dis) :-
	a( Attributo, AttVals),
	somma_pesata( Esempi, Attributo, AttVals, 0, Dis).

% somma_pesata( +Esempi, +Attributo, +AttVals, +SommaParziale, -Somma)
% restituisce la Somma pesata delle disuguaglianze
% Gini = sum from{v} P(v) * sum from{i <> j} P(i|v)*P(j|v)
somma_pesata( _, _, [], Somma, Somma).
somma_pesata( Esempi, Att, [Val|Valori], SommaParziale, Somma) :-
	length(Esempi,N),                                            % quanti sono gli esempi
	findall( C,						     % EsempiSoddisfatti: lista delle classi ..
		 (member(e(C,Desc),Esempi) , soddisfa(Desc,[Att=Val])), % .. degli esempi (con ripetizioni)..
		 EsempiSoddisfatti ),				     % .. per cui Att=Val
	length(EsempiSoddisfatti, NVal),			     % quanti sono questi esempi
	NVal > 0, !,                                                 % almeno uno!
	findall(P,			           % trova tutte le P robabilità
                (bagof(1,		           %
                       member(_,EsempiSoddisfatti),
                       L),
                 length(L,NVC),
                 P is NVC/NVal),
                ClDst),
        gini(ClDst,Gini),
	NuovaSommaParziale is SommaParziale + Gini*NVal/N,
	somma_pesata(Esempi,Att,Valori,NuovaSommaParziale,Somma)
	;
	somma_pesata(Esempi,Att,Valori,SommaParziale,Somma). % nessun esempio soddisfa Att = Val

% gini(ListaProbabilità, IndiceGini)
%    IndiceGini = SOMMATORIA Pi*Pj per tutti i,j tali per cui i\=j
%    E' equivalente a 1 - SOMMATORIA Pi*Pi su tutti gli i
gini(ListaProbabilità,Gini) :-
	somma_quadrati(ListaProbabilità,0,SommaQuadrati),
	Gini is 1-SommaQuadrati.
somma_quadrati([],S,S).
somma_quadrati([P|Ps],PartS,S)  :-
	NewPartS is PartS + P*P,
	somma_quadrati(Ps,NewPartS,S).

%(Attributo, Valori, Rimanenti, Esempi, SAlberi).
% induce decisioni SAlberi per sottoinsiemi di Esempi secondo i Valori
% degli Attributi
%induce_alberi(_,[],[],_,[]).     % nessun valore, nessun sottoalbero
%induce_alberi(Att,[Val1|Valori],AttRimasti,Esempi,[Val1:Alb1|Alberi])  :-
%	attval_subset(Att=Val1,Esempi,SottoinsiemeEsempi),
%	induce_albero(AttRimasti,SottoinsiemeEsempi,Alb1),
%	induce_alberi(Att,Valori,AttRimasti,Esempi,Alberi).

% Caso base: nessun valore, nessun sottoalbero
induce_alberi(_, [], _ , _ , _ , []) :- !.

% Caso ricorsivo: per ogni valore di un attributo, genera un sottoalbero
induce_alberi(Att, [Val1|Valori], AttRimasti, Algoritmo, Esempi, [Val1:Alb1|Alberi]) :-
    % Prendi il sottoinsieme degli esempi per cui l'attributo ha il valore Val1
    attval_subset(Att=Val1, Esempi, SottoinsiemeEsempi),

    % Induzione dell'albero per i sottoinsiemi di esempi relativi al valore Val1
    induce_albero(AttRimasti, SottoinsiemeEsempi, Algoritmo, Alb1),

	% Chiamata ricorsiva per il prossimo valore dell'attributo
	induce_alberi(Att, Valori, AttRimasti,Algoritmo, Esempi, Alberi).
    
    

% attval_subset( Attributo = Valore, Esempi, Subset):
%   Subset è il sottoinsieme di Examples che soddisfa la condizione
%   Attributo = Valore
attval_subset(AttributoValore,Esempi,Sottoinsieme) :-
	findall(e(C,O),(member(e(C,O),Esempi),soddisfa(O,[AttributoValore])),Sottoinsieme).

% soddisfa(Oggetto, Descrizione):
soddisfa(Oggetto,Congiunzione)  :-
	\+ (member(Att=Val,Congiunzione),
	    member(Att=ValX,Oggetto),
	    ValX \== Val).

del(T,[T|C],C) :- !.
del(A,[T|C],[T|C1]) :-
	del(A,C,C1).

mostra(T) :-
	mostra(T,0).
mostra(null,_) :- writeln(' ==> ???').
mostra(l(X),_) :- write(' ==> '),writeln(X).
mostra(t(A,L),I) :-
	nl,tab(I),write(A),nl,I1 is I+2,
	mostratutto(L,I1).
mostratutto([],_).
mostratutto([V:T|C],I) :-
	tab(I),write(V), I1 is I+2,
	mostra(T,I1),
	mostratutto(C,I).


% ================================================================================
% classifica( +Oggetto, -Classe, t(+Att,+Valori))
%  Oggetto: [Attributo1=Valore1, .. , AttributoN=ValoreN]
%  Classe: classe a cui potrebbe appartenere un oggetto caratterizzato da quelle coppie
%  Attributo=Valore
%  t(-Att,-Valori): Albero di Decisione
% presuppone sia stata effettuata l'induzione dell'Albero di Decisione

classifica(Oggetto, nc ,t(Att,Valori)) :- % dato t(+Att,+Valori), Oggetto è della Classe
	writeln(Oggetto),  % Aggiungi questa riga per vedere l'oggetto
	member(Att=Val,Oggetto),  % se Att=Val è elemento della lista Oggetto
        member(Val:null,Valori). % e Val:null è in Valori


classifica(Oggetto,Classe,t(Att,Valori)) :- % dato t(+Att,+Valori), Oggetto è della Classe
	writeln(Oggetto), % Aggiungi questa riga per vedere l'oggetto
	member(Att=Val,Oggetto),  % se Att=Val è elemento della lista Oggetto
    	member(Val:l(Classe),Valori). % e Val:l(Classe) è in Valori
	

classifica(Oggetto,Classe,t(Att,Valori)) :-
	member(Att=Val,Oggetto),  % se Att=Val è elemento della lista Oggetto
	delete(Oggetto,Att=Val,Resto),
	member(Val:t(AttFiglio,ValoriFiglio),Valori),
	classifica(Resto,Classe,t(AttFiglio,ValoriFiglio)).


stampa_matrice_di_confusione :-
	alb(Albero),
	carica_albero_da_file('/users/tiasb/Desktop/Progetto-Intelligenza-Artificiale/albero.txt', Albero),	
	findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
	%write(TestSet),
	length(TestSet,N),
	valuta(Albero,TestSet,VO,0,VSO,0,VN,0,VST,0,OSO,0,ON,0,OST,0,SOO,0,SN,0,SOST,0,NO,0,NSO,0,NST,0,STO,0,STSO,0,STN,0,NC,0),
	A is (VO + VSO + VN + VST) / (N - NC), % Accuratezza
	E is 1 - A,		   % Errore
	write('Test effettuati :'),  writeln(N),
	
	write('Veri obesi: '), writeln(VO),
	write('Veri sovrappeso: '), writeln(VSO),
	write('Veri normopeso: '), writeln(VN),
	write('Veri sottopeso: '), writeln(VST),
	write('Obesi classificati come sovrappeso: '), writeln(OSO),
	write('Obesi classificati come normopeso: '), writeln(ON),
	write('Obesi classificati come sottopeso: '), writeln(OST),
	write('Sovrappeso classificati come obesi: '), writeln(SOO),
	write('Sovrappeso classificati come normopeso: '), writeln(SN),
	write('Sovrappeso classificati come sottopeso: '), writeln(SOST),
	write('Normopeso classificati come obesi: '), writeln(NO),
	write('Normopeso classificati come sovrappeso: '), writeln(NSO),
	write('Normopeso classificati come sottopeso: '), writeln(NST),
	write('Sottopeso classificati come obesi: '), writeln(STO),
	write('Sottopeso classificati come sovrappeso: '), writeln(STSO),
	write('Sottopeso classificati come normopeso: '), writeln(STN),
	
	write('Test non classificati :'),  writeln(NC),

	write('Accuratezza: '), writeln(A),
	write('Errore: '), writeln(E).

valuta(_,[],VO,VO,VSO,VSO,VN,VN,VST,VST,OSO,OSO,ON,ON,OST,OST,SOO,SOO,SN,SN,SOST,SOST,NO,NO,NSO,NSO,NST,NST,STO,STO,STSO,STSO,STN,STN,NC,NC).
% testset vuoto -> valutazioni finali

% Le corrispettive versioni per l'accuratezza hanno  un -A finale

% Vero obeso      			  VO
% Vero sovrappeso 			  VSO
% Vero normopeso  			  VN
% Vero sottopeso  			  VST

% Obeso come sovrappeso 	  OSO
% Obeso come normopeso  	  ON
% Obeso come sottopeso  	  OST

% Sovrappeso come obeso       SOO
% Sovrappeso come normopeso	  SN
% Sovrappeso come sottopeso   SOST

% Normopeso come obeso		  NO
% Normopeso come sovrappeso	  NSO
% Normopeso come sottopeso	  NST

% Sottopeso come obeso		  STO
% Sottopeso come sovrappeso   STSO
% Sottopeso come normopeso    STN

% Non classificato 			  NC

% Corretta classificazione "obeso"
valuta(Albero, [obeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, obeso, Albero), !,
    VOA1 is VOA + 1,
	writeln('VOA1: '), writeln(VOA1),
    valuta(Albero, Coda,VO,VOA1,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).


% Corretta classificazione "sovrappeso"
valuta(Albero, [sovrappeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sovrappeso, Albero), !,
	VSOA1 is VSOA + 1,
	writeln('VSOA1: '), writeln(VSOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA1,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Corretta classificazione "normopeso"
valuta(Albero, [normopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, normopeso, Albero), !,
	VNOA1 is VNOA + 1,
	writeln('VNOA1: '), writeln(VNOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA1,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Corretta classificazione "sottopeso"
valuta(Albero, [sottopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sottopeso, Albero), !,
	VSTA1 is VSTA + 1,
	writeln('VSTA1: '), writeln(VSTA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA1,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Obeso classificato come sovrappeso
valuta(Albero, [obeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sovrappeso, Albero), !,
	OSOA1 is OSOA + 1,
	writeln('OSOA1: '), writeln(OSOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA1,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Obeso classificato come normopeso
valuta(Albero, [obeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, normopeso, Albero), !,
	ONA1 is ONA + 1,
	writeln('ONA1: '), writeln(ONA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA1,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Obeso classificato come sottopeso
valuta(Albero, [obeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sottopeso, Albero), !,
	OSTA1 is OSTA + 1,
	writeln('OSTA1: '), writeln(OSTA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA1,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Sovrappeso classificato come obeso
valuta(Albero, [sovrappeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, obeso, Albero), !,
	SOOA1 is SOOA + 1,
	writeln('SOOA1: '), writeln(SOOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA1,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Sovrappeso classificato come normopeso
valuta(Albero, [sovrappeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, normopeso, Albero), !,
	SNA1 is SNA + 1,
	writeln('SNA1: '), writeln(SNA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA1,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Sovrappeso classificato come sottopeso
valuta(Albero, [sovrappeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sottopeso, Albero), !,
	SOSTA1 is SOSTA + 1,
	writeln('SOSTA1: '), writeln(SOSTA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA1,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Normopeso classificato come obeso
valuta(Albero, [normopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, obeso, Albero), !,
	NOA1 is NOA + 1,
	writeln('NOA1: '), writeln(NOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA1,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Normopeso classificato come sovrappeso
valuta(Albero, [normopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sovrappeso, Albero), !,
	NSOA1 is NSOA + 1,
	writeln('NSOA1: '), writeln(NSOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA1,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Normopeso classificato come sottopeso
valuta(Albero, [normopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sottopeso, Albero), !,
	NSTA1 is NSTA + 1,
	writeln('NSTA1: '), writeln(NSTA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA1,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA).

% Sottopeso classificato come obeso
valuta(Albero, [sottopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, obeso , Albero), !,
	STOA1 is STOA + 1,
	writeln('STOA1: '), writeln(STOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA1,STSO,STSOA,STN,STNA,NC,NCA).

% Sottopeso classificato come sovrappeso
valuta(Albero, [sottopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, sovrappeso, Albero), !,
	STSOA1 is STSOA + 1,
	writeln('STSOA1: '), writeln(STSOA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA1,STN,STNA,NC,NCA).

% Sottopeso classificato come normopeso
valuta(Albero, [sottopeso/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, normopeso, Albero), !,
	STNA1 is STNA + 1,
	writeln('STNA1: '), writeln(STNA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA1,NC,NCA).

% Oggetto non classificato
valuta(Albero, [_/Oggetto | Coda], VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA) :-
	classifica(Oggetto, _ , Albero), !,
	NCA1 is NCA + 1,
	writeln('NCA1: '), writeln(NCA1),
	valuta(Albero, Coda,VO,VOA,VSO,VSOA,VN,VNOA,VST,VSTA,OSO,OSOA,ON,ONA,OST,OSTA,SOO,SOOA,SN,SNA,SOST,SOSTA,NO,NOA,NSO,NSOA,NST,NSTA,STO,STOA,STSO,STSOA,STN,STNA,NC,NCA1).

% ================================================================================