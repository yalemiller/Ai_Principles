% Probabilistic facts
0.05::faulty_motion_sensor(living_room).
0.05::faulty_motion_sensor(hallway).
0.02::faulty_light_sensor.
0.03::faulty_temp_sensor.
0.01::faulty_thermostat.
0.01::power_failure.

% Component behavior rules
motion_detected(Room) :-
    \+ faulty_motion_sensor(Room),
    movement(Room).

light_on :-
    \+ faulty_light_sensor,
    light_switch_on.

heating_on :-
    \+ faulty_thermostat,
    \+ power_failure.


% Rules defining observations
motion_suspect(Room) :-
    faulty_motion_sensor(Room),
    \+ movement(Room).

unexpected_light_on :-
    \+ faulty_light_sensor,
    \+ light_switch_on.

heating_problem :-
    faulty_thermostat;
    power_failure.

temp_reading_unreliable :-
    faulty_temp_sensor,
    heating_on.

% Example movements and light switch status
movement(living_room).
movement(hallway).
light_switch_on.

% Observations (evidence)
evidence(motion_detected(living_room), false).
evidence(light_on, false).

% Queries
query(faulty_motion_sensor(living_room)).
query(faulty_motion_sensor(hallway)).
query(power_failure).
query(faulty_light_sensor).
query(faulty_temp_sensor).
query(faulty_thermostat).
query(heating_problem).
query(temp_reading_unreliable).
