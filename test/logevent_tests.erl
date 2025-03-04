-module(logevent_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([]).

%% API

normalize_sanity_test() ->
    ?assertMatch(
        #{data := #{msg := <<"Foo">>}},
        test_msg({string, <<"Foo">>})
    ),
    ?assertMatch(
        #{data := #{format := <<"~s">>, args := ["Foo"]}},
        test_msg({"~s", ["Foo"]})
    ),
    ?assertMatch(
        #{data := #{format := <<"~x">>, args := ["Foo"]}},
        test_msg({"~x", ["Foo"]})
    ),
    ?assertMatch(
        #{data := #{msg := <<"Foo">>}},
        test_msg({report, [{msg, <<"Foo">>}]})
    ),
    ?assertMatch(
        #{data := #{msg := "Foo"}},
        test_msg({report, #{msg => "Foo"}})
    ),
    ?assertMatch(
        #{data := #{format := <<"[[[~s ~p]]]">>, args := ["bebebe", 123]}},
        test_msg({report, #{args => ["bebebe", 123], format => "[[[~s ~p]]]", label => {foo, bar, baz}}})
    ),
    ?assertMatch(
        #{data := #{format := <<"~s">>, args := ["Foo"], user := olala, caption := caption}},
        test_msg({"~s", ["Foo"]}, #{ctx => #{user => olala}, caption => caption})
    ),
    ?assertMatch(
        #{data := #{format := <<"~s">>, args := ["Foo"], user := olala, caption := caption, stacktrace := []}},
        test_msg({"~s", ["Foo"]}, #{ctx => #{user => olala}, caption => caption, stacktrace => []})
    ),
    ok.

progress_report_test() ->
    ?assertEqual(
        #{level => info, time => 123, data => #{label => {app, start}, app => app, node => node}},
        test_report([{application, app}, {started_at, node}])
    ),
    ?assertEqual(
        #{level => notice, time => 123, data => #{label => {app, exit}, app => app, reason => stopped, type => temporary}},
        test_report([{application, app}, {exited, stopped}, {type, temporary}])
    ),
    ?assertEqual(
        #{level => error, time => 123, data => #{label => {app, exit}, app => remind3, reason => failed_to_start_child, stacktrace => [<<"remind3_app:start/2">>], type => permanent}},
        test_report([{application, remind3}, {exited, {{shutdown,{failed_to_start_child,remind3_prc,ok}},{remind3_app,start,[normal,[]]}}},{type,permanent}])
    ),
    ?assertEqual(
        #{level => info, time => 123, data => #{label => {proc, start}, id => disk_log_error, pid => self(), stacktrace => [<<"logger_olp:start_link/4">>], sup => logger_sup}},
        test_report([
            {supervisor,{local,logger_sup}},
            {started, [
                {pid,self()},
                {id,disk_log_error},
                {mfargs, {logger_olp,start_link, [logger_disk_log_h_disk_log_error,logger_h_common,#{},#{}]}}
            ]}
        ])
    ),
    ?assertEqual(
        #{level => info, time => 123, data => #{label => {proc, start}, id => unknown, pid => self(), stacktrace => [<<"inet_gethost_native:init/1">>], sup => inet_gethost_native_sup}},
        test_report([
            {supervisor,{local,inet_gethost_native_sup}},
            {started,[{pid,self()},
            {mfa,{inet_gethost_native,init,[[]]}}]}
        ])
    ),
    ?assertEqual(
        #{level => info, time => 123, data => #{label => {proc, start}, id => {acceptor,list_to_pid("<0.231.0>"),10}, pid => self(), stacktrace => [<<"ranch_acceptor:start_link/4">>], sup => ranch_acceptors_sup}},
        test_report([
            {supervisor,{list_to_pid("<0.231.0>"),ranch_acceptors_sup}},
            {started, [
                {pid,self()},
                {id,{acceptor,list_to_pid("<0.231.0>"),10}},
                {mfargs, {ranch_acceptor,start_link, ["#Port<0.19>",ranch_tcp,error_logger,"<0.230.0>"]}},
                {restart_type,permanent},
                {shutdown,brutal_kill},
                {child_type,worker}
            ]}
        ])
    ),
    ?assertEqual(
        #{level => error, time => 123, data => #{label => {proc, exit}, id => ranch_acceptors_sup, pid => undefined, error => start_error, reason => {listen_error,remind3_web,eaddrinuse}, stacktrace => [<<"ranch_acceptors_sup:start_link/2">>], sup => ranch_listener_sup}},
        test_report([
            {supervisor, {list_to_pid("<0.231.0>"), ranch_listener_sup}},
            {errorContext, start_error},
            {reason, {listen_error, remind3_web, eaddrinuse}},
            {offender, [
                {pid, undefined},
                {id, ranch_acceptors_sup},
                {mfargs, {ranch_acceptors_sup,start_link,[remind3_web,ranch_tcp]}},
                {restart_type, permanent},
                {shutdown, infinity},
                {child_type, supervisor}
            ]}
        ])
    ),
    ?assertEqual(
        #{level => error, time => 123, data => #{
            label => {proc, exit},
            cause => {listen_error,remind3_web,eaddrinuse},
            error => start_error,
            id => ranch_acceptors_sup,
            pid => self(),
            reason => failed_to_start_child,
            stacktrace => [<<"ranch_listener_sup:start_link/5">>],
            sup => ranch_sup}},
        test_report([
            {supervisor, {local,ranch_sup}},
            {errorContext, start_error},
            {reason, {shutdown, {failed_to_start_child, ranch_acceptors_sup, {listen_error, remind3_web, eaddrinuse}}}},
            {offender, [
                {pid, self()},
                {id, {ranch_listener_sup, remind3_web}},
                {mfargs, {ranch_listener_sup,start_link,[remind3_web,ranch_tcp,#{backlog => 8192,connection_type => supervisor,max_connections => infinity,socket_opts => [{port,8080}]},cowboy_clear,#{connection_type => supervisor,env => #{dispatch => {persistent_term,remind3_web_dispatch}},max_keepalive => infinity,middlewares => [cowboy_router,cowboy_handler]}]}},
                {restart_type, permanent},
                {shutdown, infinity},
                {child_type, supervisor}
            ]}
        ])
    ),
    ok.

crash_report_test() ->
    ?assertEqual(
        #{level => error, time => 123, data => #{
            label => {proc, exit},
            error => error,
            pid => self(),
            reason => badarith,
            stacktrace => [<<"erlang://2">>, <<"proc_lib:init_p/3 at proc_lib.erl:211">>, <<"erl_eval:-expr/5-fun-3-/0">>]
        }},
        test_report([
            [
                {initial_call,{erl_eval,'-expr/5-fun-3-',[]}},
                {pid,self()},
                {registered_name,[]},
                {error_info, {error,
                    badarith,
                    [{erlang,'/',[1,0],[]}, {proc_lib,init_p,3, [{file,"proc_lib.erl"},{line,211}]}]}},
                {ancestors,[list_to_pid("<0.107.0>")]}, {message_queue_len,0}, {messages,[]}, {links,[]}, {dictionary,[]}, {trap_exit,false}, {status,running}, {heap_size,233}, {stack_size,28}, {reductions,183}
            ], []
        ])
    ),
    ?assertEqual(
        #{level => error, time => 123, data => #{
            label => {proc, exit},
            error => error,
            pid => self(),
            reason => {assert,"VarResult =/= undefined",true,false},
            stacktrace => [<<"web_protocol:-analytics_query_node_result_to_json/1-fun-0-/1 at src/web_protocol.erl:792">>,
                <<"web_protocol:analytics_query_node_result_to_json/1 at src/web_protocol.erl:792">>,
                <<"igor_json:-pack_value/2-lc$^0/1-1-/2 at c:/PMY/Toadman/Palladium/deps/igor/src/igor_json.erl:133">>,
                <<"web_protocol:analytics_query_result_to_json/1 at src/web_protocol.erl:811">>,
                <<"web_rest_analytics_query:handle_put/1 at src/rest/generated/web_rest_analytics_query.erl:35">>,
                <<"web_rest_analytics_query:init/2 at src/rest/generated/web_rest_analytics_query.erl:15">>,
                <<"cowboy_handler:execute/2 at c:/PMY/Toadman/Palladium/deps/cowboy/src/cowboy_handler.erl:41">>,
                <<"cowboy_stream_h:execute/3 at c:/PMY/Toadman/Palladium/deps/cowboy/src/cowboy_stream_h.erl:296">>,
                <<"cowboy_stream_h:request_process/3">>]
        }},
        test_report([
            [
                {initial_call, {cowboy_stream_h,request_process, ['Argument__1','Argument__2','Argument__3']}},
                {pid,self()},
                {registered_name,[]},
                {error_info, {error,
                    {assert, [{module,web_protocol}, {line,792}, {expression,"VarResult =/= undefined"}, {expected,true}, {value,false}]},
                    [{web_protocol, '-analytics_query_node_result_to_json/1-fun-0-',1, [{file,"src/web_protocol.erl"},{line,792}]}, {web_protocol,analytics_query_node_result_to_json, 1, [{file,"src/web_protocol.erl"},{line,792}]}, {igor_json,'-pack_value/2-lc$^0/1-1-',2, [{file, "c:/PMY/Toadman/Palladium/deps/igor/src/igor_json.erl"}, {line,133}]}, {web_protocol,analytics_query_result_to_json,1, [{file,"src/web_protocol.erl"},{line,811}]}, {web_rest_analytics_query,handle_put,1, [{file, "src/rest/generated/web_rest_analytics_query.erl"}, {line,35}]}, {web_rest_analytics_query,init,2, [{file, "src/rest/generated/web_rest_analytics_query.erl"}, {line,15}]}, {cowboy_handler,execute,2, [{file, "c:/PMY/Toadman/Palladium/deps/cowboy/src/cowboy_handler.erl"}, {line,41}]}, {cowboy_stream_h,execute,3, [{file, "c:/PMY/Toadman/Palladium/deps/cowboy/src/cowboy_stream_h.erl"}, {line,296}]}]}},
                {ancestors, [list_to_pid("<0.632.0>"),list_to_pid("<0.240.0>"),list_to_pid("<0.239.0>"),ranch_sup,list_to_pid("<0.128.0>")]},
                {message_queue_len,0},
                {messages,[]},
                {links,[list_to_pid("<0.632.0>")]},
                {dictionary,[]},
                {trap_exit,false},
                {status,running},
                {heap_size,6772},
                {stack_size,27},
                {reductions,65052}
            ], []
        ])
    ),
    ?assertEqual(
        #{level => error, time => 123, data => #{
            label => {proc, exit},
            error => exit,
            pid => self(),
            reason => killed,
            stacktrace => [<<"ranch_conns_sup:terminate/3 at src/ranch_conns_sup.erl:263">>, <<"proc_lib:init_p_do_apply/3 at proc_lib.erl:226">>, <<"ranch_conns_sup:init/4">>]
        }},
        test_report([
            [
                {initial_call,{ranch_conns_sup,init,['Argument__1','Argument__2','Argument__3','Argument__4']}},
                {pid, self()},
                {registered_name,[]},
                {error_info, {exit,
                    killed,
                    [{ranch_conns_sup,terminate,3,[{file,"src/ranch_conns_sup.erl"},{line,263}]},{proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,226}]}]}},
                {ancestors,[list_to_pid("<0.107.0>"),ranch_sup,list_to_pid("<0.108.0>")]},{message_queue_len,0},{messages,[]},{links,[]},{dictionary,[]},{trap_exit,true},{status,running},{heap_size,376},{stack_size,28},{reductions,248}
            ], []
        ])
    ),
    ?assertEqual(
        #{level => error, time => 123, data => #{
            label => {proc, exit},
            error => throw,
            pid => self(),
            reason => ohoho,
            stacktrace => [<<"shell:apply_fun/3 at shell.erl:907">>, <<"proc_lib:init_p/3 at proc_lib.erl:211">>, <<"erl_eval:-expr/5-fun-3-/0">>]
        }},
        test_report([
            [
                {initial_call,{erl_eval,'-expr/5-fun-3-',[]}},
                {pid,self()},
                {registered_name,[]},
                {error_info, {throw,
                    ohoho,
                    [{shell,apply_fun,3,[{file,"shell.erl"},{line,907}]},{proc_lib,init_p,3,[{file,"proc_lib.erl"},{line,211}]}]}},
                {ancestors,[list_to_pid("<0.107.0>")]},{message_queue_len,0},{messages,[]},{links,[]},{dictionary,[]},{trap_exit,false},{status,running},{heap_size,233},{stack_size,28},{reductions,191}
            ], []
        ])
    ),
    ?assertEqual(
        #{level => error, time => 123, data => #{
            id => httpc_profile_sup,
            label => {gen_server, terminate},
            last_message => {'EXIT', self(), killed},
            reason => killed
        }},
        test_msg(
          {report, #{client_info => undefined, label => {gen_server, terminate}, last_message => {'EXIT', self(), killed}, log => [], name => httpc_profile_sup, reason => killed, state => {state, {local, httpc_profile_sup}, one_for_one, {[httpc_manager], #{httpc_manager => {child, self(), httpc_manager, {httpc_manager, start_link, [default, only_session_cookies, inets]}, permanent, 4000, worker, [httpc_manager]}}}, undefined, 10, 3600, [], 0, httpc_profile_sup, [[{httpc, {default, only_session_cookies}}]]}}}
        )
    ),
    ok.

user_error_test() ->
    ?assertEqual(
        #{level => error, time => 1614679835500809, data => #{
            caption => web_images,
            format => <<"Failed to add an image <~ts> (reason: ~p:~p)">>,
            args => [<<"169-0-4.jpg">>, error, {badmatch, {error, unknown_error}}],
            pid => self(),
            stacktrace => [
                <<"web_images:add/4 at src/web_images.erl:60">>,
                <<"intranet_sync:assign_photo/2 at src/intranet_sync.erl:227">>,
                <<"intranet_sync:-sync/0-fun-4-/7 at src/intranet_sync.erl:129">>,
                <<"lists:foldl/3 at lists.erl:1267">>,
                <<"intranet_sync:sync/0 at src/intranet_sync.erl:115">>,
                <<"proc_lib:init_p/3 at proc_lib.erl:211">>
            ]
        }},
        test_event(
            #{level => error,
              meta =>
                  #{caption => web_images,gl => "<0.221.0>",pid => self(),
                    stacktrace =>
                        [{web_images,add,4,[{file,"src/web_images.erl"},{line,60}]},
                         {intranet_sync,assign_photo,2,
                                        [{file,"src/intranet_sync.erl"},{line,227}]},
                         {intranet_sync,'-sync/0-fun-4-',7,
                                        [{file,"src/intranet_sync.erl"},{line,129}]},
                         {lists,foldl,3,[{file,"lists.erl"},{line,1267}]},
                         {intranet_sync,sync,0,
                                        [{file,"src/intranet_sync.erl"},{line,115}]},
                         {proc_lib,init_p,3,[{file,"proc_lib.erl"},{line,211}]}],
                    time => 1614679835500809},
              msg =>
                  {"Failed to add an image <~ts> (reason: ~p:~p)",
                   [<<"169-0-4.jpg">>,error,{badmatch,{error,unknown_error}}]}}
        )
    ),
    ?assertEqual(
        #{level => debug, time => 1614859987310474, data => #{
            caption => intranet_sync,
            format => <<"Failed to process image from ~ts, reason: ~s">>,
            args => [
                <<"https://images4.bamboohr.com/292284/photos/169-0-4.jpg">>,
                {badmatch, undefined}
            ],
            pid => self(),
            stacktrace => [
                <<"intranet_sync:assign_photo/2 at src/intranet_sync.erl:223">>,
                <<"intranet_sync:-sync/0-fun-4-/7 at src/intranet_sync.erl:129">>,
                <<"lists:foldl/3 at lists.erl:1267">>,
                <<"intranet_sync:sync/0 at src/intranet_sync.erl:115">>,
                <<"proc_lib:init_p/3 at proc_lib.erl:211">>
            ]
        }},
        test_event(#{
            level => debug,
            meta => #{
                caption => intranet_sync, gl => list_to_pid("<0.224.0>"),
                pid => self(),
                stacktrace => [
                    {intranet_sync,assign_photo,2,[{file,"src/intranet_sync.erl"},{line,223}]},
                    {intranet_sync,'-sync/0-fun-4-',7,[{file,"src/intranet_sync.erl"},{line,129}]},
                    {lists,foldl,3,[{file,"lists.erl"},{line,1267}]},
                    {intranet_sync,sync,0,[{file,"src/intranet_sync.erl"},{line,115}]},
                    {proc_lib,init_p,3,[{file,"proc_lib.erl"},{line,211}]}
                ],
                time => 1614859987310474
            },
            msg => {"Failed to process image from ~ts, reason: ~s", [
                <<"https://images4.bamboohr.com/292284/photos/169-0-4.jpg">>,
                {badmatch, undefined}
            ]}
        })
    ),
    ok.

otp_error_test() ->
    ?assertEqual(
        #{level => error, time => 1614685751723000, data => #{
            client => #{
                pid => list_to_pid("<0.144.0>"),
                stacktrace => [
                    <<"gen:do_call/4 at gen.erl:169">>
                ]
            },
            id => list_to_pid("<0.244.0>"),
            label => {gen_server,terminate},
            last_message => {equery, <<"SELECT">>, [5,<<>>]},
            pid => self(),
            reason => function_clause,
            stacktrace => [<<"lists:zip/2 at lists.erl:387">>]
        }},
        test_event(
            #{level => error,
              meta =>
                  #{domain => [otp],
                    error_logger => #{tag => error},
                    file => "gen_server.erl",gl => "<0.128.0>",line => 888,
                    mfa => {gen_server,error_info,7},
                    pid => self(),report_cb => fun gen_server:format_log/1,
                    time => 1614685751723000},
              msg =>
                  {report,
                      #{client_info =>
                            {list_to_pid("<0.144.0>"),
                             {list_to_pid("<0.144.0>"),
                              [{gen,do_call,4,[{file,"gen.erl"},{line,169}]}]}},
                        label => {gen_server,terminate},
                        last_message =>
                            {equery,
                                <<"SELECT">>,
                                [5,<<>>]},
                        name => list_to_pid("<0.244.0>"),
                        reason =>
                            {function_clause,
                                [{lists,zip,[[],[<<>>]],[{file,"lists.erl"},{line,387}]}]},
                        state => {state,list_to_pid("<0.145.0>")}}}}
        )
    ),
    ?assertEqual(
        #{level => error, time => 1614685751723000, data => #{
            client => #{
                pid => list_to_pid("<0.144.0>"),
                stacktrace => [
                    <<"gen:do_call/4 at gen.erl:169">>,
                    <<"gen_server:call/3 at gen_server.erl:219">>,
                    <<"poolboy:transaction/3 at src/poolboy.erl:84">>,
                    <<"db_query:equery/3 at src/helpers/db_query.erl:84">>,
                    <<"db_query:select/3 at src/helpers/db_query.erl:101">>,
                    <<"db_if_file_attachments:get/6 at src/logic/db_if_file_attachments.erl:82">>,
                    <<"erl_eval:do_apply/6 at erl_eval.erl:681">>,
                    <<"shell:exprs/7 at shell.erl:686">>
                ]
            },
            id => list_to_pid("<0.244.0>"),
            label => {gen_server,terminate},
            last_message => {equery, <<"SELECT fa.id AS id, fa.file_size AS file_size, fa.file_sha AS file_sha, fa.filename AS filename, fa.thumb_filename AS thumb_filename, fa.original_filename AS original_filename, fa.content_type AS content_type, fa.personnel_id AS personnel_id, per.username AS personnel_name, fa.created_at AS created_at FROM file_attachments AS fa LEFT OUTER JOIN personnel AS per ON (per.id = fa.personnel_id) WHERE fa.id IN (SELECT * FROM get_project_file_attachment_ids($1))  ORDER BY id ASC NULLS FIRST OFFSET 0 LIMIT 0">>, [5,<<>>]},
            pid => self(),
            reason => function_clause,
            stacktrace => [<<"lists:zip/2 at lists.erl:387">>, <<"lists:zip/2 at lists.erl:387">>, <<"epgsql:equery/3 at src/epgsql.erl:237">>, <<"postgres_worker:handle_call/3 at src/workers/postgres_worker.erl:45">>, <<"gen_server:try_handle_call/4 at gen_server.erl:661">>, <<"gen_server:handle_msg/6 at gen_server.erl:690">>, <<"proc_lib:init_p_do_apply/3 at proc_lib.erl:249">>]
        }},
        test_event(
            #{level => error,
              meta =>
                  #{domain => [otp],
                    error_logger => #{tag => error},
                    file => "gen_server.erl",gl => "<0.128.0>",line => 888,
                    mfa => {gen_server,error_info,7},
                    pid => self(),report_cb => fun gen_server:format_log/1,
                    time => 1614685751723000},
              msg =>
                  {report,
                      #{client_info =>
                            {list_to_pid("<0.144.0>"),
                             {list_to_pid("<0.144.0>"),
                              [{gen,do_call,4,[{file,"gen.erl"},{line,169}]},
                               {gen_server,call,3,[{file,"gen_server.erl"},{line,219}]},
                               {poolboy,transaction,3,
                                   [{file,"src/poolboy.erl"},{line,84}]},
                               {db_query,equery,3,
                                   [{file,"src/helpers/db_query.erl"},{line,84}]},
                               {db_query,select,3,
                                   [{file,"src/helpers/db_query.erl"},{line,101}]},
                               {db_if_file_attachments,get,6,
                                   [{file,"src/logic/db_if_file_attachments.erl"},
                                    {line,82}]},
                               {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,681}]},
                               {shell,exprs,7,[{file,"shell.erl"},{line,686}]}]}},
                        label => {gen_server,terminate},
                        last_message =>
                            {equery,
                                <<"SELECT fa.id AS id, fa.file_size AS file_size, fa.file_sha AS file_sha, fa.filename AS filename, fa.thumb_filename AS thumb_filename, fa.original_filename AS original_filename, fa.content_type AS content_type, fa.personnel_id AS personnel_id, per.username AS personnel_name, fa.created_at AS created_at FROM file_attachments AS fa LEFT OUTER JOIN personnel AS per ON (per.id = fa.personnel_id) WHERE fa.id IN (SELECT * FROM get_project_file_attachment_ids($1))  ORDER BY id ASC NULLS FIRST OFFSET 0 LIMIT 0">>,
                                [5,<<>>]},
                        name => list_to_pid("<0.244.0>"),
                        reason =>
                            {function_clause,
                                [{lists,zip,[[],[<<>>]],[{file,"lists.erl"},{line,387}]},
                                 {lists,zip,2,[{file,"lists.erl"},{line,387}]},
                                 {epgsql,equery,3,[{file,"src/epgsql.erl"},{line,237}]},
                                 {postgres_worker,handle_call,3,
                                     [{file,"src/workers/postgres_worker.erl"},{line,45}]},
                                 {gen_server,try_handle_call,4,
                                     [{file,"gen_server.erl"},{line,661}]},
                                 {gen_server,handle_msg,6,
                                     [{file,"gen_server.erl"},{line,690}]},
                                 {proc_lib,init_p_do_apply,3,
                                     [{file,"proc_lib.erl"},{line,249}]}]},
                        state => {state,list_to_pid("<0.145.0>")}}}}
        )
    ),
    ok.

%% Local functions

test_event(Event) ->
    logevent:normalize(Event).

test_msg(Msg) ->
    test_event(#{msg => Msg, meta => #{time => 123}, level => info}).
test_msg(Msg, Ctx) when is_map(Ctx) ->
    test_event(#{msg => Msg, meta => Ctx#{time => 123}, level => info}).

test_report(Msg) ->
    test_msg({report, #{report => Msg}}).

