:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).		% new
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_header)).

:- consult('graphplan.pl').
:- consult('examples/baba_v2.1.pl').
:- consult('examples/baba_v3.6.pl').
%:- once(lvl_01(P)).
%:- write('porova').

:- http_handler(root(low), reply, []).		        % (1)

server(Port) :-						% (2)
        http_server(http_dispatch, [port(Port)]).

reply(Request) :-					% (3)
        http_parameters(Request,
                    [ goal(Goal, [])
                    ]),
        
        format('Content-type: text/plain~n~n'),
        %write(Goal),
        %call(test,P,Goal).
        %test(P,Goal).
        %write(Goal).
        %once(solve_level(Level,P)).

        format('|~w|~n',Goal),
        %call(test,P,Goal),
        call(test,P,won(baba,flag)).
        %reply_html_page(title('Hello ~w'-[Goal]),[h1('Hello World')]).
        %http_reply('Hello ~w'-[Name]).
        %[ h1('Hello World'),
        %        p(['This example demonstrates generating HTML ',
        %                'messages from Prolog'
        %       ])
        %]).