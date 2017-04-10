%%
%% Interactions with underlying storage.
%% This module is a bit hacky.
%% Functions assumes (and asserts) that disk is setup as it should,
%% permissions and all.
%%
-module (daphnia_storage).

-define (STATE_FILENAME, "state").
-define (ID_FILENAME, "id").
-define (ERROR_FILENAME, "error").

% id file functions
-export ([read_id/1, write_id/3, delete_id/1, list_ids/0]).
% state file functions
-export ([read_state/1, write_state/4]).
% cleanup
-export ([delete_task/1]).


%% Write state to disk. State file is written in a side-file and moved in
-spec write_state(term(), term(), queue:queue(), map()) -> ok | {error, any()}.
write_state(Id, State, Mailbox, Opts) when is_map(Opts) ->
    Term = #{id=>Id,state=>State,mailbox=>Mailbox,options=>Opts},
    {ok, StateFile} = get_task_state_file(Id),
    % grab some random to use in data side-file name
    BinRand    = crypto:strong_rand_bytes(8),
    StrRand    = daphnia_hex:bin_to_hex_string(BinRand),
    StrUnixSec = integer_to_list(erlang:system_time(seconds)),
    StrPostfix = StrUnixSec++"."++StrRand,
    TmpFile   = StateFile ++ ".data."++StrPostfix,
    % write data to tmp file. Will ensure dir exists:
    ok = write_term(TmpFile, Term),
    % move into place:
    ok = file:rename(TmpFile, StateFile).


%% Read state from disk
%% return {ok, {State,Mailbox,Options}} tuple
-spec read_state(term()) ->
    {ok, {State :: term(),Mailbox :: queue:queue()}} |
    {error, not_found} |
    {error, {invalid_state_term, term()}} |
    {error, Reason :: any()}.
read_state(Id) ->
    {ok, StateFile} = get_task_state_file(Id),
    case read_term(StateFile) of
        {ok, #{id:=Id,state:=State,mailbox:=Mailbox,options:=Opts}} ->
            {ok, {State,Mailbox, Opts}};
        {ok, Term} ->
            {error, {invalid_state_term, Term}};
        {error, _} = E ->
            E
    end.


%% Write id file to disk
%% does not bother with symlink switching etc.
-spec write_id(term(), module(), term()) -> ok | {error, any()}.
write_id(Id, Module, Args) ->
    Term = #{id=>Id,module=>Module,args=>Args},
    {ok, IdFile} = get_task_id_file(Id),
    write_term(IdFile, Term).

%% read back id file and return {ok, {Id, Module, Args}} tuple
-spec read_id(term()) ->
    {ok, {Id :: term(), Module :: module(), Args :: term()}} |
    {error, not_found} |
    {error, {invalid_id_term, term()}} |
    {error, Reason :: any()}.
read_id(Id) ->
    {ok, IdFile} = get_task_id_file(Id),
    case read_term(IdFile) of
        {ok, #{id:=Id,module:=Module,args:=Args}} ->
            {ok, {Id, Module, Args}};
        {ok, Term} ->
            {error, {invalid_id_term, Term}};
        {error, _} = E ->
            E
    end.

delete_id(Id) ->
    {ok, IdFile} = get_task_id_file(Id),
    case file:delete(IdFile) of
        {error, enoent} -> {error, not_found};
        ok              -> ok;
        {error, _} = E  -> E
    end.


-spec delete_task(term()) -> ok | {error, not_found} | {error, any()}.
delete_task(Id) ->
    {ok, TaskDir} = get_task_dir(Id),
    case file:list_dir(TaskDir) of
        {error, enoent} ->
            {error, not_found};
        {ok, Filenames} ->
            % we delete the id file first so other processes can't find it
            ok = case file:delete(filename:join([TaskDir, ?ID_FILENAME])) of
                {error, enoent} -> ok;
                ok              -> ok
            end,
            % then delete the rest
            ok = lists:foldl(fun
                (?ID_FILENAME, ok) ->
                    ok;
                (F, ok) ->
                    ok = file:delete(filename:join([TaskDir, F]))
            end, ok, Filenames),
            Res = file:del_dir(TaskDir),

            % clean-up the 2-char prefix dir if it is empty
            PrefixDir = filename:dirname(TaskDir),
            case file:del_dir(PrefixDir) of
                {error, eexist} -> ok;
                ok              -> ok
            end,
            Res
    end.



%% return a list of ids
list_ids() ->
    {ok, BaseDir} = get_base_dir(),
    WildcardExpr = BaseDir ++ "/*/*/" ++ ?ID_FILENAME,
    Files = filelib:wildcard(WildcardExpr),
    Ids = lists:filtermap(fun(File) ->
        case read_term(File) of
            {error, not_found} ->
                false;
            {ok, #{id:=Id}} ->
                {true, Id};
            {ok, _} ->
                false
        end
    end, Files),
    {ok, Ids}.





%% Path construction helpers for disk storage:
%% =======

get_base_dir() ->
    application:get_env(daphnia, tasks_directory).

get_task_dir(Id) ->
    {ok, BaseDir} = get_base_dir(),
    {ok, make_task_dir(filename:absname(BaseDir), Id)}.

get_task_id_file(Id) ->
    {ok, TaskDir} = get_task_dir(Id),
    {ok, filename:join([TaskDir, ?ID_FILENAME])}.

get_task_state_file(Id) ->
    {ok, TaskDir} = get_task_dir(Id),
    {ok, filename:join([TaskDir, ?STATE_FILENAME])}.

make_task_dir(BaseDir, Id) ->
    BinHash = crypto:hash(sha256, term_to_binary(Id)),
    StrHash = daphnia_hex:bin_to_hex_string(BinHash),
    Prefix  = lists:sublist(StrHash, 2),
    filename:join([BaseDir, Prefix, StrHash]).
    
%% File r/w helpers
%% =======

-spec read_term(string()) -> {ok, term()} | {error, not_found} | {error, Reason :: any()}.
read_term(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            {ok, binary_to_term(Bin)};
        {error, R} when R == enotdir; R == enoent ->
            {error, not_found};
        {error, _} = E ->
            E
    end.

-spec write_term(string(), term()) -> ok | {error, Reason :: any()}.
write_term(Filename, Term) ->
   case filelib:ensure_dir(Filename) of
        {error, _} = E ->
            E;
        ok ->
            Bin = term_to_binary(Term),
            file:write_file(Filename, Bin)
    end.
