-module(daphnia_SUITE).
-compile(export_all).

-define (LIFETIME_TASK, daphnia_SUITE_lifetime).
-define (CALLABLE_TASK, daphnia_SUITE_callee).
-define (REC_TIMEOUT, 1000).

suite() ->
    [{timetrap, {seconds, 10}}].

init_per_suite(Config) ->
    application:ensure_all_started(daphnia),
    Config.

end_per_suite(_Config) ->
    ok.

groups() ->
    [
        {lifetime, [], [
            test_notify_nonexistent,
            test_task_normal_lifetime,
            test_task_cancel_lifetime,
            test_task_crash_lifetime,
            test_not_task_module,
            test_not_module,
            test_send_to_pid,
            test_send_to_pid_mix_notify
        ]},
        {callable, [parallel], [
            test_task_callable,
            test_task_not_callable,
            test_task_callable_timeout
        ]}
    ].

all() ->
    [{group, lifetime}, {group, callable}].


%% -- TEST CASES ----------------------------------------------

test_notify_nonexistent(_Config) ->
    {error, not_found} = daphnia:notify_task(my_nonexistent_task_id, foobar).

test_task_normal_lifetime(_Config) ->
    ok = daphnia:start_task(my_normal_lifetime, ?LIFETIME_TASK, []),
    ok = daphnia:notify_task(my_normal_lifetime, {ping, self()}),
    % receive a pong
    Pid = recpong(my_normal_lifetime),
    % monitor task process
    MRef = monitor(process, Pid),
    % ask process to stop normal
    ok = daphnia:notify_task(my_normal_lifetime, complete),
    % try to notify it again, expect it to tell is that it is gone
    % (note that if the task process get's to clean up before this code runs
    %  it will report it as not_found instead)
    {error, {gone, complete}} = daphnia:notify_task(my_normal_lifetime, foobar),
    normal = recdown(MRef).

test_task_cancel_lifetime(_Config) ->
    ok = daphnia:start_task(my_cancel_lifetime, ?LIFETIME_TASK, []),
    ok = daphnia:notify_task(my_cancel_lifetime, {ping, self()}),
    % receive a pong
    Pid = recpong(my_cancel_lifetime),
    % monitor task process
    MRef = monitor(process, Pid),
    % ask process to stop normal
    ok = daphnia:notify_task(my_cancel_lifetime, cancel),
    % try to notify it again, expect it to tell is that it is gone
    % (note that if the task process get's to clean up before this code runs
    %  it will report it as not_found instead)
    {error, {gone, cancel}} = daphnia:notify_task(my_cancel_lifetime, foobar),
    {shutdown, cancel} = recdown(MRef).

test_task_crash_lifetime(_Config) ->
    ok = daphnia:start_task(my_crash_lifetime, ?LIFETIME_TASK, []),
    % set a value in task
    ok = daphnia:notify_task(my_crash_lifetime, {set, 1337}),
    ok = daphnia:notify_task(my_crash_lifetime, {ping, self()}),
    % receive a pong
    Pid = recpong(my_crash_lifetime),
    % monitor task process
    MRef = monitor(process, Pid),
    % forcefully kill
    exit(Pid, kill),
    % receive the down message. the task is now dead (due to crash)
    killed = recdown(MRef),
    % poke it, should come back to life as new pid
    ok = daphnia:notify_task(my_crash_lifetime, {ping, self()}),
    NewPid = recpong(my_crash_lifetime),
    NewPid =:= Pid andalso error(same_pids),

    % ask to get the value back from the crashes process. manager should restore it
    ok = daphnia:notify_task(my_crash_lifetime, {get, self()}),
    1337 = recval(),
    ok = daphnia:notify_task(my_crash_lifetime, complete),
    {error, {gone, complete}} = daphnia:notify_task(my_crash_lifetime, foobar).


test_task_callable(_Config) ->
    ok = daphnia:start_task(my_callable_task, ?CALLABLE_TASK, []),
    % set the value a few times
    ok = daphnia:notify_task(my_callable_task, {set, 1}),
    ok = daphnia:notify_task(my_callable_task, {set, 2}),
    ok = daphnia:notify_task(my_callable_task, {set, 3}),
    % get the value
    {ok, 3} = daphnia:call_task(my_callable_task, get),
    % expect a gone,complete
    {error, {gone, complete}} = daphnia:call_task(my_callable_task, get),
    % wait a bit
    timer:sleep(1),
    % now the process should have cleaned up, and be unknown, so not_found:
    {error, not_found} = daphnia:call_task(my_callable_task, get),
    ok.


test_task_callable_timeout(_Config) ->
    ok = daphnia:start_task(my_timeout_task, ?CALLABLE_TASK, []),
    ok = daphnia:notify_task(my_timeout_task, {sleep, 50}),
    try daphnia:call_task(my_timeout_task, get, 10) of
        _ -> error(did_not_get_timeout)
    catch
        exit:timeout -> ok
    end.


test_task_not_callable(_Config) ->
    ok = daphnia:start_task(my_uncallable_task, ?LIFETIME_TASK, []),
    ok = daphnia:notify_task(my_uncallable_task, {set, 1}),
    ok = daphnia:notify_task(my_uncallable_task, {set, 2}),
    % try to call task (it should not be callable)
    {error, not_callable} = daphnia:call_task(my_uncallable_task, get),
    ok.


test_not_task_module(_Config) ->
    {error, {invalid_task_module, ?MODULE}}
        = daphnia:start_task(my_not_a_task_module, ?MODULE, []).

test_not_module(_Config) ->
    {error, {not_a_module, my_notmodule}}
        = daphnia:start_task(my_not_a_module, my_notmodule, []).

test_send_to_pid(_Config) ->
    ok = daphnia:start_task(my_send_to_pid_lifetime, ?LIFETIME_TASK, []),
    ok = daphnia:notify_task(my_send_to_pid_lifetime, {ping, self()}),
    % receive a pong
    Pid = recpong(my_send_to_pid_lifetime),
    % now send a ping by hand
    Pid ! {ping, self()},
    Pid = recpong(my_send_to_pid_lifetime),
    ok.

test_send_to_pid_mix_notify(_Config) ->
    Value = <<>>,
    ok = daphnia:start_task(my_send_to_pid_lifetime2, ?LIFETIME_TASK, []),
    ok = daphnia:notify_task(my_send_to_pid_lifetime2, {set, Value}),
    ok = daphnia:notify_task(my_send_to_pid_lifetime2, {ping, self()}),
    % receive a pong
    Pid = recpong(my_send_to_pid_lifetime2),
    ok = daphnia:notify_task(my_send_to_pid_lifetime2, {schedule_noops, 10}),
    % now send a ping by hand
    Pid ! {ping, self()},
    Pid = recpong(my_send_to_pid_lifetime2),
    ok.

%% helpers:
recpong(Id) ->
    receive {pong, Id, P} -> P after ?REC_TIMEOUT -> error(timeout) end.
recval(Pid) ->
    receive {value, Pid, V} -> V after ?REC_TIMEOUT -> error(timeout) end.
recval() ->
    receive {value, _, V} -> V after ?REC_TIMEOUT -> error(timeout) end.
recdown(MRef) ->
    receive {'DOWN', MRef, process, _, Reason} -> Reason after ?REC_TIMEOUT -> error(timeout) end.

