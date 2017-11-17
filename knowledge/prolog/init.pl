:- register_ros_package(knowrob_common).
:- register_ros_package(knowrob_srdl).

:- use_module(poke_position_calculator,[calculate_poke_position/6, get_object_height/1]).

:- owl_parser:owl_parse('../owl/suturo_object.owl').

