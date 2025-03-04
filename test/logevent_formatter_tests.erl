-module(logevent_formatter_tests).

%% Include files

-include_lib("eunit/include/eunit.hrl").

%% Exported functions

-export([]).

%% API

raw_formatter_test() ->
    Config = #{style => raw},
    ?assertEqual(
        <<"#{data => #{app => app,label => {app,start},node => 'bebebe@127.0.0.1'},level => info,time => 0}">>,
        logevent_formatter:format(test_application_started_event(), Config)
    ),
    ok.

map_formatter_test() ->
    Config = #{style => map},
    ?assertEqual(
        <<"#{data => #{app => app,label => {app,start},node => 'bebebe@127.0.0.1'},level => info,time => <<\"1970-01-01T03:00:00.000000+03:00\">>}\n">>,
        logevent_formatter:format(test_application_started_event(), Config)
    ),
    ?assertEqual(
        <<"#{data => #{app => app,label => {app,exit},reason => stopped,type => temporary},level => notice,time => <<\"1970-01-01T03:00:00.000000+03:00\">>}\n">>,
        logevent_formatter:format(test_application_exited_event(), Config)
    ),
    ?assertEqual(
        <<"#{data => #{id => {disk_log_error,foo},label => {proc,start},pid => <0.166.0>,stacktrace => [<<\"logger_olp:start_link/4\">>],sup => logger_sup},level => info,time => <<\"1970-01-01T03:00:00.000000+03:00\">>}\n">>,
        logevent_formatter:format(test_process_started_event(), Config)
    ),
    ?assertEqual(
        <<"#{data => #{cause => {listen_error,remind3_web,eaddrinuse},error => start_error,id => ranch_acceptors_sup,label => {proc,exit},pid => <0.166.0>,reason => failed_to_start_child,stacktrace => [<<\"ranch_listener_sup:start_link/5\">>],sup => ranch_sup},level => error,time => <<\"1970-01-01T03:00:00.000000+03:00\">>}\n">>,
        logevent_formatter:format(test_process_exited_event(), Config)
    ),
    ok.

line_formatter_test() ->
    Config = #{style => line, template => [datetime, "\t", level, "\t", data, "\n"]},
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00\tinfo\t#{app => app,label => {app,start},node => 'bebebe@127.0.0.1'}\n">>,
        logevent_formatter:format(test_application_started_event(), Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00\tnotice\t#{app => app,label => {app,exit},reason => stopped,type => temporary}\n">>,
        logevent_formatter:format(test_application_exited_event(), Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00\tinfo\t#{id => {disk_log_error,foo},label => {proc,start},pid => <0.166.0>,stacktrace => [<<\"logger_olp:start_link/4\">>],sup => logger_sup}\n">>,
        logevent_formatter:format(test_process_started_event(), Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00\terror\t#{cause => {listen_error,remind3_web,eaddrinuse},error => start_error,id => ranch_acceptors_sup,label => {proc,exit},pid => <0.166.0>,reason => failed_to_start_child,stacktrace => [<<\"ranch_listener_sup:start_link/5\">>],sup => ranch_sup}\n">>,
        logevent_formatter:format(test_process_exited_event(), Config)
    ),
    ok.

pretty_formatter_test() ->
    Config = #{style => pretty, template => [datetime, " ", level, " ", msg, data, stacktrace, "\n"]},
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00 info Application 'app' started at 'bebebe@127.0.0.1'\n">>,
        logevent_formatter:format(test_application_started_event(), Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00 notice Application 'app' exited with reason 'stopped'\n">>,
        logevent_formatter:format(test_application_exited_event(), Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00 info Supervisor 'logger_sup' started '{disk_log_error,foo}' as <0.166.0> via logger_olp:start_link/4\n">>,
        logevent_formatter:format(test_process_started_event(), Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00 error Process <0.166.0> exited with error 'start_error', reason 'failed_to_start_child'
          ~~~~~~~~~~
          #{cause => {listen_error,remind3_web,eaddrinuse},id => ranch_acceptors_sup,sup => ranch_sup}
          ^^^^^^^^^^
          ~~~~~~~~~~
          ranch_listener_sup:start_link/5
          ^^^^^^^^^^\n">>,
        logevent_formatter:format(test_process_exited_event(), Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00 error Process <0.166.0> exited with error 'start_error', reason 'failed_to_start_child'
          ~~~~~~~~~~
          #{cause => {listen_error,remind3_\n">>,
        logevent_formatter:format(test_process_exited_event(), Config#{max_size => 186})
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00 info [[[bebebe 123]]]\n">>,
        logevent_formatter:format(#{
            level => info,
            meta => #{time => 0},
            msg => {report, #{args => ["bebebe", 123], format => "[[[~s ~p]]]", label => label}}
        }, Config)
    ),
    ?assertEqual(
        <<"1970-01-01T03:00:00.000000+03:00 info ** Could not io_lib:format\n          ~~~~~~~~~~\n          #{attention => true,original => #{args => [\"Foo\"],format => <<\"~x\">>}}\n          ^^^^^^^^^^\n">>,
        logevent_formatter:format(#{
            level => info,
            meta => #{time => 0},
            msg => {report, #{args => ["Foo"], format => "~x", label => label}}
        }, Config)
    ),
    ok.

pretty_formatter_caption_test() ->
    EventWithCaption = #{
        level => info,
        meta => #{caption => {bebebe, bububu}, time => 0},
        msg => {report, #{args => ["abc"], format => "abc: ~s", label => label}}
    },
    ?assertEqual(
        <<"03:00:00.000000 [info] {bebebe,bububu}: abc: abc\n">>,
        logevent_formatter:format(EventWithCaption, #{
            style => pretty,
            template => [time, " [", level, "] ", {caption, [caption, ": "], ""}, msg, data, stacktrace, "\n"]
        })
    ),
    EventWithoutCaption = #{
        level => info,
        meta => #{time => 0},
        msg => {report, #{args => ["abc"], format => "abc: ~s", label => label}}
    },
    ?assertEqual(
        <<"03:00:00.000000 [info] abc: abc\n">>,
        logevent_formatter:format(EventWithoutCaption, #{
            style => pretty,
            template => [time, " [", level, "] ", {caption, caption, ""}, {caption, ": ", ""}, msg, data, stacktrace, "\n"]
        })
    ),
    ok.

%% Local functions

test_event(Msg) ->
    #{level => info, meta => #{time => 0}, msg => {report, #{report => Msg}}}.

test_application_started_event() ->
    test_event([
        {application, app},
        {started_at, 'bebebe@127.0.0.1'}
    ]).

test_application_exited_event() ->
    test_event([
        {application, app},
        {exited, stopped},
        {type, temporary}
    ]).

test_process_started_event() ->
    test_event([
        {supervisor, {local, logger_sup}},
        {started, [
            {pid, list_to_pid("<0.166.0>")},
            {id, {disk_log_error, foo}},
            {mfargs, {logger_olp, start_link, [logger_disk_log_h_disk_log_error, logger_h_common, #{}, #{}]}}
        ]}
    ]).

test_process_exited_event() ->
    test_event([
        {supervisor, {local, ranch_sup}},
        {errorContext, start_error},
        {reason, {shutdown, {failed_to_start_child, ranch_acceptors_sup, {listen_error, remind3_web, eaddrinuse}}}},
        {offender, [
            {pid, list_to_pid("<0.166.0>")},
            {id, {ranch_listener_sup, remind3_web}},
            {mfargs, {ranch_listener_sup, start_link, [
                remind3_web,
                ranch_tcp,
                #{
                    backlog => 8192,
                    connection_type => supervisor,
                    max_connections => infinity,
                    socket_opts => [{port, 8080}]
                },
                cowboy_clear,
                #{
                    connection_type => supervisor,
                    env => #{dispatch => {persistent_term, remind3_web_dispatch}},
                    max_keepalive => infinity,
                    middlewares => [cowboy_router, cowboy_handler]
                }
            ]}},
            {restart_type, permanent},
            {shutdown, infinity},
            {child_type, supervisor}
        ]}
    ]).
