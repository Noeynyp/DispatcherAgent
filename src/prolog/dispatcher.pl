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
% UNIT SELECTION
% ----------------------------------------

candidate_unit(IncidentId, Unit, Time) :-
    incident(IncidentId, Type, _, _, _, _),
    required_service(Type, Service),
    unit(Unit, Service, _, Status, _),
    ( Status = available ; assigned(Unit, _) ),
    effective_response_time(Unit, IncidentId, Time).

select_best_unit(IncidentId, BestUnit) :-
    findall(Time-Unit,
        candidate_unit(IncidentId, Unit, Time),
        Pairs),
    Pairs \= [],
    sort(Pairs, Sorted),
    Sorted = [_-BestUnit | _].

% ----------------------------------------
% ASSIGNMENT
% ----------------------------------------

assign_unit(Unit, Incident) :-
    retractall(assigned(Unit, _)),
    retract(unit(Unit, S, L, _, N)),
    assertz(unit(Unit, S, L, busy, N)),
    assertz(assigned(Unit, Incident)).

dispatch(IncidentId, Unit, Explanation) :-
    select_best_unit(IncidentId, Unit),
    assign_unit(Unit, IncidentId),
    format(atom(Explanation),
        'Unit ~w dispatched to incident ~w using effective response time selection.',
        [Unit, IncidentId]).

% ----------------------------------------
% REASSIGNMENT LOGIC
% ----------------------------------------

should_reassign(Unit, OldIncident, NewIncident) :-
    incident(OldIncident, _, _, _, _, OldUrg),
    incident(NewIncident, _, _, _, _, NewUrg),
    urgency_rank(NewUrg, R1),
    urgency_rank(OldUrg, R0),
    R1 > R0,
    effective_response_time(Unit, NewIncident, NewET),
    effective_response_time(Unit, OldIncident, OldET),
    NewET < OldET.

reassign(Unit, OldIncident, NewIncident) :-
    should_reassign(Unit, OldIncident, NewIncident),
    retract(assigned(Unit, OldIncident)),
    assertz(assigned(Unit, NewIncident)).

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

% ----------------------------------------
% EXPLANATION
% ----------------------------------------

explain_assignment(IncidentId, Unit, Explanation) :-
    incident(IncidentId, Type, _, _, _, Urg),
    effective_response_time(Unit, IncidentId, T),
    format(atom(Explanation),
        'Unit ~w selected for incident type ~w (urgency=~w) with total response time ~2f seconds.',
        [Unit, Type, Urg, T]).