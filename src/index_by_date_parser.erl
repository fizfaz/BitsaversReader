-module(index_by_date_parser).
%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
%-endif.

-export([parse_line/1]).

% 2014-03-09 09:31:00 univac/1100/exec/UP-8723r2_Text_Editor_Level_16R1C_Reference_Apr84.pdf

parse_line(Line) ->
    DatePart = string:substr(Line,1,19), 
    [Date_string, Time_string ] = string:tokens(DatePart, " "), 
    Path = string:substr(Line, 21, length(Line)),
    PathArray = string:tokens(Path, "/"),
    Date = to_integer_tokens(Date_string, "-"),
    Time = to_integer_tokens(Time_string, ":"),
    { { Date, Time } ,  PathArray }.

string_to_integer(S) -> {I,_} = string:to_integer(S), I. 

to_integer_tokens(S,Delimiter) ->
    Tokens = string:tokens(S,Delimiter),
    L = [ string_to_integer(X) || X <- Tokens],
    list_to_tuple(L).

%-ifdef(TEST).

parse_line_test() ->   
    Line = "2014-03-09 09:31:00 univac/1100/exec/UP-8723r2_Text_Editor_Level_16R1C_Reference_Apr84.pdf",
    { { Date, Time } , Path }  = parse_line(Line),
    ?assert( Date =:= {2014,3,9}),
    ?assert( Time =:= {9,31,0}),
    ?assert( Path =:= ["univac", "1100", "exec", "UP-8723r2_Text_Editor_Level_16R1C_Reference_Apr84.pdf"]).

parse_line_with_spaces_test() ->   
    Line = "2014-03-09 09:31:00 apple/mac/prototypes/1985_Big_Mac/pictures/bigmac proto.jpg",
    { { Date, Time } , Path }  = parse_line(Line),
    ?assert( Date =:= {2014,3,9}),
    ?assert( Time =:= {9,31,0}),
    ?assert( Path =:= ["apple", "mac", "prototypes", "1985_Big_Mac", "pictures", "bigmac proto.jpg"]).

    


%-endif.
