

# Module daphnia #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

daphnia public API.

<a name="types"></a>

## Data Types ##




### <a name="type-id">id()</a> ###


<pre><code>
id() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call_task-2">call_task/2</a></td><td>Same as call_task/3 with infinity given as timeout.</td></tr><tr><td valign="top"><a href="#call_task-3">call_task/3</a></td><td>Send a message to a daphnia task identified by Id and wait for the response.</td></tr><tr><td valign="top"><a href="#notify_task-2">notify_task/2</a></td><td>Send a message to a daphnia task identified by Id.</td></tr><tr><td valign="top"><a href="#start_task-3">start_task/3</a></td><td>Create a new task identified by Id,
implemented in the Mod task module,
initialized with Args as task argument.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call_task-2"></a>

### call_task/2 ###

<pre><code>
call_task(TaskId::<a href="#type-id">id()</a>, Data::term()) -&gt; {ok, term()} | {error, not_callable} | {error, not_found} | {error, Reason::any()}
</code></pre>
<br />

Same as call_task/3 with infinity given as timeout.

__See also:__ [call_task/3](#call_task-3).

<a name="call_task-3"></a>

### call_task/3 ###

<pre><code>
call_task(TaskId::<a href="#type-id">id()</a>, Data::term(), Timeout::timeout()) -&gt; {ok, term()} | {error, not_callable} | {error, not_found} | {error, Reason::any()}
</code></pre>
<br />

Send a message to a daphnia task identified by Id and wait for the response.

<a name="notify_task-2"></a>

### notify_task/2 ###

<pre><code>
notify_task(TaskId::<a href="#type-id">id()</a>, Info::term()) -&gt; ok | {error, not_found} | {error, Reason::any()}
</code></pre>
<br />

Send a message to a daphnia task identified by Id.

<a name="start_task-3"></a>

### start_task/3 ###

<pre><code>
start_task(Id::term(), Mod::module(), Args::term()) -&gt; ok | {error, already_started} | {error, {invalid_task_module, atom()}} | {error, any()}
</code></pre>
<br />

Create a new task identified by Id,
implemented in the Mod task module,
initialized with Args as task argument.

