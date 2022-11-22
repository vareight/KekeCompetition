:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).		% new
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_header)).

:- consult('graphplan.pl').
%:- consult('examples/baba_v2.1.pl').
%:- consult('examples/baba_v3.6.pl').
%:- once(lvl_01(P)).
%:- write('porova').

:- http_handler(root(baba), reply, []).		        % (1)

server(Port) :-						% (2)
        http_server(http_dispatch, [port(Port)]).

reply(Request) :-					% (3)
        http_parameters(Request,
                    [ level(Level, [])
                    ]),
        
        %format('Content-type: text/plain'),
        consult('baba_agent.pl'),
        %call(Level,P),
        
        catch(
                call_with_time_limit(60, solve_level(Level,P)),
                time_limit_exceeded,
                true).
