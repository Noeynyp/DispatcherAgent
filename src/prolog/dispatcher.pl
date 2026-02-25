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
% CANDIDATE UNIT SELECTION (NO BLIND REASSIGNMENT)
% ----------------------------------------

% Only AVAILABLE units are candidates.
% Assigned units are NOT automatically considered.
% Reassignment will be implemented later explicitly.

candidate_unit(IncidentId, Service, Unit, Time) :-
    incident(IncidentId, Type, _, _, _, _),
    required_service(Type, Service),
    unit(Unit, Service, _, available, _),
    effective_response_time(Unit, IncidentId, Time).

% ----------------------------------------
% SELECT BEST UNIT FOR A GIVEN SERVICE
% ----------------------------------------

select_best_unit_for_service(IncidentId, Service, BestUnit) :-
    findall(Time-Unit,
        candidate_unit(IncidentId, Service, Unit, Time),
        Pairs),
    Pairs \= [],
    sort(Pairs, Sorted),
    Sorted = [_-BestUnit | _].

% ----------------------------------------
% MULTI-SERVICE DISPATCH
% ----------------------------------------

% For each required service,
% select the best available unit.
% Returns a list like:
%   [medical-m1, police-p2]

dispatch(IncidentId, Assignments, Explanation) :-
    findall(Service-Unit,
        (
            required_service_for_incident(IncidentId, Service),
            select_best_unit_for_service(IncidentId, Service, Unit)
        ),
        Assignments),
    Assignments \= [],
    build_explanation(IncidentId, Assignments, Explanation).

% Helper: get required services for an incident
required_service_for_incident(IncidentId, Service) :-
    incident(IncidentId, Type, _, _, _, _),
    required_service(Type, Service).

% ----------------------------------------
% EXPLANATION GENERATION
% ----------------------------------------

build_explanation(IncidentId, Assignments, Explanation) :-
    incident(IncidentId, Type, _, _, _, Urg),
    format_assignments(Assignments, AssignmentText),
    format(atom(Explanation),
        'Incident ~w (type=~w, urgency=~w). Dispatched: ~w.',
        [IncidentId, Type, Urg, AssignmentText]).

format_assignments([], '').
format_assignments([Service-Unit], Text) :-
    format(atom(Text), '~w unit ~w', [Service, Unit]).
format_assignments([Service-Unit | Rest], Text) :-
    format_assignments(Rest, RestText),
    format(atom(Text), '~w unit ~w, ~w',
        [Service, Unit, RestText]).

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