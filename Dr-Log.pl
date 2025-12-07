:- dynamic yes/1, no/1.

clear_memory :-
    retractall(yes(_)),
    retractall(no(_)).

%% Disease definitions
disease(flu,  [fever, cough, fatigue]).
disease(cold, [runny_nose, sore_throat, cough]).
disease(covid,[fever, cough, loss_of_smell, fatigue]).


%% Ask a symptom only once
ask(Symptom) :-
    ( yes(Symptom) -> true
    ; no(Symptom)  -> fail
    ; write('Do you have '), 
      write(Symptom), 
      write('? (yes/no): '),
      read(Reply),
      ( Reply == yes -> 
        asserta(yes(Symptom))
      ; asserta(no(Symptom)), 
        fail
      )
    ).


%% Check if a disease is still possible
possible(D) :-
    disease(D, Symptoms),
    \+ ( yes(S), \+ member(S, Symptoms) ).


%% Choose next symptom to ask
next_symptom_to_ask(S) :-
    % Gather all possible diseases
    findall(D, possible(D), Ds),
    
    % Collect all symptoms of remaining diseases
    findall(Sym,
            (member(D, Ds),
             disease(D, Syms),
             member(Sym, Syms)),
            AllSyms),
    sort(AllSyms, Unique),
    
    % Pick a symptom that hasn't been asked
    member(S, Unique),
    \+ yes(S),
    \+ no(S).


%% Diagnosis loop
diagnose :-
    clear_memory,
    diagnose_loop.

diagnose_loop :-
    findall(D, possible(D), Ds),
    length(Ds, Count),

    ( Count =:= 0 ->
         write('No disease matches your symptoms.'), nl, !
    ; Count =:= 1 ->
         Ds = [Final],
         write('You most likely have: '), write(Final), nl, !
    ; next_symptom_to_ask(S),
      ask(S),
      diagnose_loop
    ).
