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
        consult('examples/baba_v2.2.pl'),
        %call(Level,P),
        solve(Level,P),

        %format('~w~n~n',[Level]).   % [[won(baba,flag)]]
        elaborate(P,Level).           % [won(baba,flag)]

solveLvl([G],Level) :- 
        %normalize_space(atom(X), G),  
        %format('"~w"~n',G),       % won(baba,flag)
        consult('examples/baba_v3.8.pl'),
        solve(Level,P,G).             % not working! 
        %test(P,won(baba,flag)).

elaborate([],Level).
elaborate([H|T],Level) :-
        %consult('examples/baba_v3.6.pl'),
        %format('H=~w~n',H),
        solveLvl(H,Level),           % won(baba,flag)
        elaborate(T,Level).           % []
        %format('~w~n',[H]),
        %format('~w~n',[T]).