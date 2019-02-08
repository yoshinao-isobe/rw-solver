/*
  Copyright (C) 2018
  AIST Program-IP No. H30PRO-2219
  National Institute of Advanced Industrial Science and Technology (AIST),
  East Japan Railway Company (JR-East)
*/

%{
open Model
%}

%token EOF
%token MAX_TIME
%token REPEAT_INTERVAL
%token MODULE
%token TRAIN
%token STATION
%token NAME
%token CAPACITY
%token INTERVAL
%token FIFOS
%token LINKS
%token SINGLETRACKS
%token START_TIME
%token END_TIME
%token ROUTE
%token REQ_TIME
%token REQ_TIMES
%token REQ_PLATFORMS
%token START_TIME_BETWEEN
%token MAX_TOTAL_TIME
%token MAX_TOTAL_TIME_UNTIL
%token INFINITY
%token <string> STRING
%token <int> NUM
%token EQ
%token LPAR
%token RPAR
%token LCURBRACE
%token RCURBRACE
%token LSQBRACKET
%token RSQBRACKET
%token COMMA
%token SEMICOLON
%token WILDCARD
%token MODEL_PARAMETER_MARK
%token <string> IDENTIFIER

%type <Model.model_element> model
%start model

%%

model:
  /* epsilon */
	{ default_model_element }
| model MAX_TIME EQ NUM
	{
      if $1.max_time_opt = None then
        { $1 with max_time_opt = Some $4; }
      else
        raise (RWError "dup max_time")
    }
| model REPEAT_INTERVAL EQ NUM
	{
      if $1.repeat_interval_opt = None then
        { $1 with repeat_interval_opt = Some $4; }
      else
        raise (RWError "dup repeat_interval")
    }
| model model_parameter_def
    { let (name, value) = $2 in
      { $1 with parameter_map = StringMap.add name value $1.parameter_map }}
| model rwmodule
	{ { $1 with module_list = $1.module_list @ [$2] } }
| model train
	{ { $1 with train_list = $1.train_list @ [$2] } }
;

rwmodule:
  MODULE LCURBRACE module_properties RCURBRACE
	{ $3 }
;

train:
  TRAIN LCURBRACE train_properties RCURBRACE
	{ $3 }
;

module_properties:
  /* empty */
    { { id = -1; capacity = CapacityNone; interval = -1; links = []; fifos = []; singletracks = []; station = false;
        req_time = None;
        name = None; } }
| module_properties STATION
    { { $1 with station = true; } }
| module_properties CAPACITY EQ capacity_spec
    { { $1 with capacity = $4; } }
| module_properties INTERVAL EQ NUM
    { { $1 with interval = $4; } }
| module_properties LINKS EQ LSQBRACKET num_list RSQBRACKET
    { { $1 with links = $5; } }
| module_properties FIFOS EQ LSQBRACKET num_pair_list RSQBRACKET
    { { $1 with fifos = $5; } }
| module_properties SINGLETRACKS EQ LSQBRACKET num_pair_list RSQBRACKET
    { { $1 with singletracks = $5; } }
| module_properties NAME EQ STRING
    { { $1 with name = Some $4; } }
| module_properties REQ_TIME EQ req_time
    { { $1 with req_time = $4; } }
;

train_properties:
  /* empty */
     { default_train }
| train_properties START_TIME EQ NUM
    { { $1 with start_time = Some $4; } }
| train_properties END_TIME EQ NUM
    { { $1 with end_time = Some $4; } }
| train_properties ROUTE EQ LSQBRACKET num_list RSQBRACKET
    { { $1 with route = $5; } }
| train_properties REQ_TIMES EQ LSQBRACKET req_time_list RSQBRACKET
    { { $1 with req_times = Some $5; } }
| train_properties REQ_PLATFORMS EQ LSQBRACKET req_platforms RSQBRACKET
    { { $1 with req_platforms = $5; } }
| train_properties START_TIME_BETWEEN EQ num_pair
    { { $1 with start_time_between = Some $4; } }
| train_properties MAX_TOTAL_TIME EQ NUM
    { { $1 with max_total_time = Some $4; } }
| train_properties MAX_TOTAL_TIME_UNTIL EQ num_pair
    { { $1 with max_total_time_untils = $4::$1.max_total_time_untils; } }
;

capacity_spec:
  NUM
	{ Capacity $1 }
| INFINITY
	{ CapacityInfinity }
;

num_list:
  /* empty */
    { [] }
| NUM
    { [$1] }
| num_list COMMA NUM
    { List.append $1 [$3] }
;

num_pair_list:
  /* empty */
    { [] }
| LPAR NUM COMMA NUM RPAR
    { [($2, $4)] }
| num_pair_list COMMA LPAR NUM COMMA NUM RPAR
    { List.append $1 [($4, $6)] }
;

num_pair:
  LPAR NUM COMMA NUM RPAR
    { ($2, $4) }
;

req_time:
  WILDCARD
    { None }
| LPAR NUM COMMA WILDCARD RPAR
    { Some ($2, None) }
| LPAR NUM COMMA NUM RPAR
    { Some ($2, Some $4) }
;

req_time_list:
  /* empty */
    { [] }
| req_time
    { [$1] }
| req_time_list COMMA req_time
    { List.append $1 [$3] }
;

req_platforms:
  /* empty */
    { [] }
| platforms
    { [$1] }
| req_platforms COMMA platforms
    { List.append $1 [$3] }
;

platforms:
  WILDCARD
    { [] }
| LCURBRACE int_list RCURBRACE
    { $2 }
;

int_list:
  /* empty */
    { [] }
| NUM
    { [$1] }
| int_list COMMA NUM
    { List.append $1 [$3] }
;

model_parameter_def:
  MODEL_PARAMETER_MARK IDENTIFIER EQ model_parameter_value
    { ($2, $4) }
;

model_parameter_value:
  NUM
    { ModelParameterInt $1 }
| STRING
    { ModelParameterString $1 }
;
