% A dynamic predicate that holds the patient's data
:- dynamic patient/4.
:- dynamic development/3.

% A predicate that adds a new patient or updates an existing one
add_patient(Name, Age, Sex, Conditions) :-
    (   patient(Name, _, _, _) % check if the patient already exists
    ->  retractall(patient(Name, _, _, _)), % remove the old data
        assertz(patient(Name, Age, Sex, Conditions)) % add the new data
    ;   assertz(patient(Name, Age, Sex, Conditions)) % add the new patient
    ).
% A predicate that adds a new patient or updates an existing one
add_development(Name, Date, Symptoms) :-
    (   development(Name, _, _) % check if the patient already exists
    ->  retractall(patient(Name, _, _)), % remove the old data
        assertz(development(Name, Date, Symptoms)) % add the new data
    ;   assertz(development(Name, Date, Symptoms)) % add the new patient
    ).

% A predicate that prints the patient's data
print_patient(Name) :-
    patient(Name, Age, Sex, Conditions),
    format("Name: ~w~nAge: ~w~nSex: ~w~nConditions: ~w~n", [Name, Age, Sex, Conditions]).

% A predicate that prints the patient's symptom development data
print_dev(Name) :-
    development(Name, Date, Symptoms), % get the patient's data
    format("Name: ~w~nDate: ~w~nSymptoms: ~w~n", [Name, Date, Symptoms]).

% Define some facts about symptoms
% Common symptoms
symptom(fever, common).
symptom(dry_cough, common).
symptom(tiredness,common).
% Uncommon symptoms
symptom(smell_loss, uncommon).
symptom(taste_loss, uncommon).
symptom(running_nose, uncommon).
symptom(pain, uncommon).
symptom(sore_throat, uncommon).
symptom(diarrhea, uncommon).
symptom(headache, uncommon).
symptom(conjunctivities, uncommon).
% Severe symptoms
symptom(short_breath, severe).
symptom(diff_breath, severe).
symptom(chest_pain, severe).
symptom(chest_pressure, severe).
symptom(speech_loss, severe).
symptom(movement_loss, severe).

% Pre-existent health conditions that puts one to a higher risk
precondition([hypertension,diabetes,cardiovascular,chronic_respiratory,cancer]).

subset([], _).
subset([H|T], List) :-
    member(H, List),
    subset(T, List).

% Define a predicate that takes a patient name and returns the risk level as an integer 
risk(Name, Factor) :- 
    patient(Name, Age, Sex, Conditions), 
% get the patient information 
    risk(Age, Sex, Conditions, Factor). % calculate the risk level based on the information

% Define a helper predicate that takes the age, sex, and conditions of a 
% patient and returns the risk level 
risk(Age, Sex, Conditions, Factor) :- 
    Age > 70, % if the patient is older than 70 
    precondition(L1), % get the list of preconditions 
    subset(Conditions, L1), % check if the patient has any preconditions 
    Factor = 3, !. % the risk level is 3 (high)
risk(Age, Sex, Conditions, Factor) :- 
    Sex = male, % if the patient is male 
    Factor = 2, !. % the risk level is 2 (medium) 
risk(Age, Sex, Conditions, Factor) :- 
    Age < 70,
    Sex= female,
    Factor = 1. % the risk level is 1 (low)

% % Define some facts about the risk level of symptoms
% % If common, medium = 2 risk of infectious
% % if uncommon, low = 1 risk of infectious
% % if severe, high risk = 3, needs medical attention, highly infectious
risk_level(uncommon, 1).
risk_level(common, 2).
risk_level(severe, 3).
max(Risk1, Risk2, Risk) :- Risk is max(Risk1, Risk2).

% Define a rule that calculates the risk of infection based on the symptoms and the patient's risk factor
infection_risk(Name, Risk) :-
    development(Name, Date, Symptoms),
    risk(Name, Factor),
    infection_risk(Symptoms, Factor, Risk).

% Define a helper rule that calculates the risk of infection based on a list of symptoms and a risk factor % if risk is true, then weigh on the decision 
infection_risk([], _, 0). % base case: empty list of symptoms 
infection_risk([H|T], Factor, Risk) :- % recursive case: non-empty list of symptoms 
    symptom(H, Level), % get the risk level of the head of the list 
    risk_level(Level, Risk1), % get the corresponding risk value 
    infection_risk(T, Factor, Risk2), % get the risk of infection for the rest of the list 
    max(Risk1, Risk2, Risk3), % use the max/3 predicate to get the highest risk value 
    (Factor == high -> Risk is Risk3 + 1; Risk is Risk3). % if the patient has a high risk factor, increase the risk by 1

has_severe_symptom([]) :- fail.
has_severe_symptom([H|T]) :- % recursive case: non-empty list
    symptom(H, severe) % check if the head of the list is a severe symptom
    ; % or
    has_severe_symptom(T). % check the rest of the list

% Define a rule that determines if a patient needs medical attention based on the symptoms
medical_attention(Name) :-
    development(Name, Date, Symptoms),
    has_severe_symptom(Symptoms).

% Define a rule that checks if a patient has either dry cough or running nose symptoms
has_dry_cough_or_running_nose(Name) :-
    development(Name, Date, Symptoms),
    (member(dry_cough, Symptoms) -> !; member(running_nose, Symptoms) -> !).

% Define a rule that determines if a patient is highly infectious based on the symptoms
highly_infectious(Name) :-
    has_dry_cough_or_running_nose(Name), % if the patient has either dry cough or running nose symptoms
    (infection_risk(Name, 2) -> !; % AND if the patient has a medium
    infection_risk(Name, 3) -> !). % OR has a high risk of infection

% Define a predicate that calculates the end date of the infectious period given the start date 
% The start date is a date structure of the form 
% date(Year, Month, Day, Hour, Minute, Second, Offset, TimeZone, DaylightSaving) 
% The end date is a date structure of the same form 
infectious_end_date(StartDate, EndDate) :- 
    date_time_stamp(StartDate, StartStamp), 
    % convert the start date to a timestamp 
    EndStamp is StartStamp + 15 * 86400, % add 15 days to the timestamp 
    stamp_date_time(EndStamp, EndDate, local). % convert the timestamp to a date structure

date_string_to_date(DateString, StartDate) :- 
    parse_time(DateString, iso_8601, Stamp),
    stamp_date_time(Stamp, StartDate, 'UTC').

date_to_date_string(EndDate, EndString) :- format_time(string(EndString), '%F', EndDate).
% use the %F format specifier to get the date in ISO 8601 format

% Define a rule that determines the infectious period of a patient based on the date of symptom onset
% infectious for at least 14-16 days ---- uncertainty
infectious_period(Name, Date, EndString) :-
    development(Name, Date, Symptoms),
    highly_infectious(Name),
    date_string_to_date(Date,Start), % the start of the infectious period is the same as the date of symptom onset
    infectious_end_date(Start,EndDate), % the end of the infectious period is 15 days after the date of symptom onset
    date_to_date_string(EndDate,EndString).


% Define a predicate that represents the user interface of the expert system
start :-
    write('Welcome to the medical diagnosis expert system.'), nl,
    write('Please enter the name of the patient: '),
    read(Name),
    write('Please enter the age of the patient: '),
    read(Age),
    write('Please enter the sex of the patient: '),
    read(Sex),
    write('Please enter the date of symptom onset (YYYY-MM-DD): '),
    read(Date),
    write('Please enter the list of symptoms separated by commas: '),
    read(Symptoms),
    write('Please enter the list of pre-existing conditions separated by commas: '),
    read(Conditions),
    add_patient(Name, Age, Sex, Conditions),
    add_development(Name, Date, Symptoms),
    print_patient(Name),
    print_dev(Name),
    write('Thank you for your input. Here is the diagnosis: '), nl,
    diagnose(Name).


% Define a predicate that gives the diagnosis for a patient based on the symptoms, the risk factor, the medical attention, and the infectious period
diagnose(Name) :-
    infection_risk(Name, Risk),
    write('The risk of infection is '), write(Risk), write('.'), nl,
    (medical_attention(Name) -> write('The patient needs medical attention. Call the nearest health facility immediately!!'), nl; write('The patient should manage symptoms at home.'), nl),
    (highly_infectious(Name) -> write('The patient is highly infectious and should isolate from others.'), nl; write('The patient is not highly infectious, keep distance from others when possible.')),
    infectious_period(Name, Date, EndString),
    write('The infectious period is from '), write(Date), write(' to '), write(EndString), write('.'), nl.

% Please note that this is not a complete or accurate diagnosis service,
% and you should always consult a medical professional for any health-related issues.
% Stay safe and healthy! 😊