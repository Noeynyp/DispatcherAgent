:- dynamic incident/6.
:- dynamic unit/5.
:- dynamic assigned/2.
:- dynamic travel_time/3.
:- dynamic handling_time/2.

% incident(Id, Type, Severity, LocationNode, ReportedTimeEpoch, Urgency)
% unit(Id, Service, LocationNode, Status, Notes)
% assigned(UnitId, IncidentId)
% travel_time(UnitId, IncidentId, Seconds)
% handling_time(IncidentId, Seconds)

% ----------------------------------------
% SERVICES REQUIRED BY INCIDENT TYPE
% ----------------------------------------

required_service(fire, fire).
required_service(medical, medical).
required_service(accident, police).
required_service(accident, medical).

% ----------------------------------------
% URGENCY INITIALIZATION
% ----------------------------------------

initial_urgency(Severity, low)      :- Severity =< 2.
initial_urgency(Severity, medium)   :- Severity =< 3, Severity > 2.
initial_urgency(Severity, high)     :- Severity =< 4, Severity > 3.
initial_urgency(Severity, critical) :- Severity > 4.

add_incident(Id, Type, Severity, Loc, Time) :-
    initial_urgency(Severity, U),
    assertz(incident(Id, Type, Severity, Loc, Time, U)).

add_unit(Id, Service, Loc, Status, Notes) :-
    assertz(unit(Id, Service, Loc, Status, Notes)).

% ----------------------------------------
% EFFECTIVE RESPONSE TIME
% ----------------------------------------

remaining_task_time(Unit, Time) :-
    assigned(Unit, OldIncident),
    handling_time(OldIncident, Time), !.
remaining_task_time(_, 0).

effective_response_time(Unit, Incident, TotalTime) :-
    travel_time(Unit, Incident, Travel),
    remaining_task_time(Unit, Remaining),
    TotalTime is Travel + Remaining.

% ----------------------------------------
% WEIGHTED SCORE LOGIC
% ----------------------------------------

% Adjustable weights
weight_urgency(15).
weight_time(1).

weighted_score(IncidentId, Unit, Score) :-
    incident(IncidentId, _, _, _, _, Urg),
    urgency_rank(Urg, Rank),
    effective_response_time(Unit, IncidentId, Time),
    weight_urgency(Wu),
    weight_time(Wt),
    Score is Wu * Rank - Wt * Time.
    

% ----------------------------------------
% GLOBAL WEIGHTED SELECTION
% ----------------------------------------

select_best_global_assignment(IncidentId, Service, Unit, Score) :-
    incident(IncidentId, _, _, _, _, _),
    required_service_for_incident(IncidentId, Service),
    candidate_unit(IncidentId, Service, Unit),
    weighted_score(IncidentId, Unit, Score).

best_global_assignment(IncidentId, Service, Unit, Score) :-
    findall(Score-I-S-U,
        select_best_global_assignment(I, S, U, Score),
        All),
    All \= [],
    sort(All, Sorted),
    reverse(Sorted, Desc),
    Desc = [Score-IncidentId-Service-Unit | _].

% ----------------------------------------
% CONTROLLED REASSIGNMENT LOGIC
% ----------------------------------------

better_reassignment(Unit, OldIncident, NewIncident) :-

    % Unit must already be assigned
    assigned(Unit, OldIncident),

    % Both incidents must exist
    incident(OldIncident, _, _, _, _, OldUrg),
    incident(NewIncident, _, _, _, _, NewUrg),

    % New urgency must be strictly higher
    urgency_rank(NewUrg, Rnew),
    urgency_rank(OldUrg, Rold),
    Rnew > Rold,

    % Compare weighted scores
    weighted_score(NewIncident, Unit, NewScore),
    weighted_score(OldIncident, Unit, OldScore),

    NewScore > OldScore.

% ----------------------------------------
% CANDIDATE UNIT SELECTION
% ----------------------------------------

candidate_unit(IncidentId, Service, Unit) :-
    incident(IncidentId, Type, _, _, _, _),
    required_service(Type, Service),
    unit(Unit, Service, _, Status, _),
    ( Status = available ; Status = busy ).

% ----------------------------------------
% SELECT BEST UNIT (MAXIMIZE SCORE)
% ----------------------------------------

select_best_unit_for_service(IncidentId, Service, BestUnit) :-
    findall(Score-Unit,
        (
            candidate_unit(IncidentId, Service, Unit),
            weighted_score(IncidentId, Unit, Score)
        ),
        Pairs),
    Pairs \= [],
    sort(Pairs, Sorted),        % ascending
    reverse(Sorted, Descending),% descending (max first)
    Descending = [_-BestUnit | _].

% ----------------------------------------
% MULTI-SERVICE DISPATCH
% ----------------------------------------

dispatch(IncidentId, Assignments, Explanation) :-
    findall(Service,
        required_service_for_incident(IncidentId, Service),
        Services),

    findall(Service-Unit,
        (
            member(Service, Services),
            select_best_unit_for_service(IncidentId, Service, Unit)
        ),
        Assignments),

    Assignments \= [],
    build_explanation(IncidentId, Assignments, Explanation).

required_service_for_incident(IncidentId, Service) :-
    incident(IncidentId, Type, _, _, _, _),
    required_service(Type, Service).

% ----------------------------------------
% EXPLANATION
% ----------------------------------------

build_explanation(IncidentId, Assignments, Explanation) :-
    incident(IncidentId, Type, _, _, _, Urg),
    format_assignments(IncidentId, Assignments, AssignmentText),
    format(atom(Explanation),
        'Incident ~w (type=~w, urgency=~w). Dispatched: ~w.',
        [IncidentId, Type, Urg, AssignmentText]).

format_assignments(_, [], '').

format_assignments(IncidentId, [Service-Unit], Text) :-
    weighted_score(IncidentId, Unit, Score),
    format(atom(Text),
        '~w unit ~w (score=~2f)',
        [Service, Unit, Score]).

format_assignments(IncidentId, [Service-Unit | Rest], Text) :-
    weighted_score(IncidentId, Unit, Score),
    format_assignments(IncidentId, Rest, RestText),
    format(atom(Text),
        '~w unit ~w (score=~2f), ~w',
        [Service, Unit, Score, RestText]).

% ----------------------------------------
% URGENCY ESCALATION
% ----------------------------------------

urgency_rank(low, 1).
urgency_rank(medium, 2).
urgency_rank(high, 3).
urgency_rank(critical, 4).

escalation_rule(low, 300, medium).
escalation_rule(medium, 600, high).
escalation_rule(high, 1200, critical).

escalate_urgency(IncidentId, CurrentTime) :-
    incident(IncidentId, Type, Sev, Loc, ReportedTime, Urg),
    Elapsed is CurrentTime - ReportedTime,
    escalation_rule(Urg, Threshold, NewUrg),
    Elapsed >= Threshold,
    urgency_rank(NewUrg, R1),
    urgency_rank(Urg, R0),
    R1 > R0,
    retract(incident(IncidentId, Type, Sev, Loc, ReportedTime, Urg)),
    assertz(incident(IncidentId, Type, Sev, Loc, ReportedTime, NewUrg)).