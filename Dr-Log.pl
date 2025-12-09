:- dynamic yes/1, no/1.

%% Clear all recorded answers
clear_memory :-
    retractall(yes(_)),
    retractall(no(_)).

%% Disease definitions
disease(flu,  [fever, cough, fatigue, sneezing]).
disease(cold, [runny_nose, sore_throat, cough]).
disease(covid,[fever, cough, loss_of_smell, fatigue]).
disease(strep_throat, [sore_throat, fever, swollen_lymph_nodes, difficulty_swallowing]).
disease(bronchitis, [persistent_cough, chest_discomfort, fatigue, shortness_of_breath]).
disease(sinusitis, [facial_pain, nasal_congestion, headache, post_nasal_drip]).
disease(pneumonia, [fever, cough, chest_pain, shortness_of_breath]).
disease(allergies, [sneezing, itchy_eyes, runny_nose, congestion]).
disease(food_poisoning, [nausea, vomiting, diarrhea, stomach_cramps]).
disease(migraine, [headache, nausea, light_sensitivity, sound_sensitivity]).
disease(asthma, [shortness_of_breath, wheezing, chest_tightness, cough]).
disease(mononucleosis, [fatigue, fever, sore_throat, swollen_lymph_nodes]).
disease(measles, [fever, cough, runny_nose, skin_rash]).
disease(meningitis,[fever, seizures, rash, stiff_neck]).
disease(acne,[papules, cysts, pustules]).
disease(laryngitis,[sore_throat, fever, hoarseness, cough]).
disease(malaria,[fever, aches, chills, vomiting]).

%% Ask a symptom only once (always succeeds)
ask(Symptom) :-
    ( yes(Symptom) -> true
    ; no(Symptom)  -> true
    ; write('Do you have '), write(Symptom), write('? (yes/no): '),
      read(Reply),
      ( Reply == yes -> asserta(yes(Symptom))
      ; Reply == no  -> asserta(no(Symptom))
      ; write('Please answer yes or no.'), nl,
        ask(Symptom)
      )
    ).

%% Check if a disease is still possible
possible(D) :-
    disease(D, Symptoms),
    % All 'yes' symptoms must be present
    \+ ( yes(S), \+ member(S, Symptoms) ),
    % None of the 'no' symptoms can be present
    \+ ( no(S), member(S, Symptoms) ).

%% Count how many diseases a symptom appears in
symptom_frequency(Sym, Freq) :-
    findall(D, (disease(D, Symptoms), member(Sym, Symptoms)), Ds),
    length(Ds, Freq).

%% Choose the next symptom to ask (most common first)
next_symptom_to_ask(S) :-
    findall(D, possible(D), Ds),
    
    % Collect all unasked symptoms of remaining diseases
    findall(Sym,
            (member(D, Ds),
             disease(D, Syms),
             member(Sym, Syms),
             \+ yes(Sym),
             \+ no(Sym)),
            AllSyms),
    sort(AllSyms, Unique),

    % Sort by descending frequency
    map_list_to_pairs(symptom_frequency, Unique, Pairs),
    keysort(Pairs, SortedAsc),
    reverse(SortedAsc, SortedDesc),

    % Pick the most frequent symptom
    SortedDesc = [_Freq-S | _].

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
