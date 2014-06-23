-module(bitsavers_db).
-export([init/0,  insert/2,  get/1, children/1, is_defined/1, insert_index_by_date/1]).

-record(item, {path, data}).

init() ->
    mnesia:create_table(item, [{type, ordered_set}, {disc_copies, [node()]}]).


insert(Path, Value) ->
    make_sure_path_exists(Path), 
    write(Path, Value).

% mmhm, doesn't work, string = list? 
make_sure_path_exists([]) -> ok;

make_sure_path_exists(A) -> 
    L = lists:sublist(A, length(A)-1),
    write_stub(L), 
    make_sure_path_exists(L).

write_stub([]) -> ok; 
write_stub(L) ->
    case is_defined(L) of
        false ->write(L, {});
        true -> ok 
    end.

    

write(Path, Value) ->
    Row = #item{path=Path, data=Value},
    F = fun() ->
            mnesia:write(Row)
        end,
    mnesia:transaction(F). 

% (bitsavers@felix.local)38> mnesia:dirty_select(item, [{#item{path=["foo",'_'], _='_'}, [], ['$_']}]).
%[#item{path = ["foo","bar"],data = "foobarvalue"},
% #item{path = ["foo","baz"],data = "foobazvalue"},
% #item{path = ["foo","chulu"],data = "chulu"}]

is_defined(Key) ->
    F = fun() ->
        mnesia:read(item, Key)
    end, 
    {atomic, R} = mnesia:transaction(F),
    case R of
        [] -> false;
        _ -> true
    end. 


get(Key) ->
    F = fun() ->
            mnesia:read(item, Key)
        end, 
    {atomic, [Item] } = mnesia:transaction(F),
    Item#item.data. 

children([]) ->
    F = fun() ->
        mnesia:select(item, [{#item{path=['_'], _='_'}, [], ['$_']}])
    end,
    {atomic, Items} = mnesia:transaction(F),
    Paths= [X#item.path || X <- Items],
    Paths;

% mmmhm, this is kind of wrong. we select all and filter later, 
children(Key) ->
    Pattern = Key ++ '_',
    F = fun() ->
            mnesia:select(item, [{#item{path=Pattern, _='_'},[], ['$_']}] )
        end,
    {atomic, Items} = mnesia:transaction(F),
    Paths= [X#item.path || X <- Items],
    lists:filter(fun(X) -> length(X) =:= length(Key)+1 end, Paths).



insert_index_by_date(Filename) ->
    {ok, S} = file:open(Filename, read),
    process_file(S).

process_file(S) ->
    Line = io:get_line(S, ''),
    case Line of 
        eof -> ok;
        Line -> process_line(Line), process_file(S)
    end.
    

process_line(eof) -> ok;

process_line(Line) ->
    Data = string:substr(Line, 1, length(Line)-1), % strip \n
    { AddedDate, Path} = index_by_date_parser:parse_line(Data), 
    insert(Path, {added_date, AddedDate}).
    
    
