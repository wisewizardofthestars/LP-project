:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).

:- json_object message(content: string).

webhook("https://discord.com/api/webhooks/934932970249281577/mWgT2mAvsLVd1onw0vylX2htel6MD6-SkGzXNtYDnwa7HOQvCQ1Te7pCYk-1CF69a5vh").

send_message(Message) :-
    webhook(Url),
    prolog_to_json(message(Message), JsonMessage),
    atom_json_term(Data, JsonMessage, []),
    http_post(Url, atom(application/json, Data), Reply, []),
    json_write(current_output, Reply).

edit_message(MessageId, Message) :-
    webhook(BaseUrl),
    string_concat(BaseUrl, "/messages/", MessagesUrl),
    string_concat(MessagesUrl, MessageId, Url),
    prolog_to_json(message(Message), JsonMessage),
    atom_json_term(Data, JsonMessage, []),
    http_patch(Url, atom(application/json, Data), Reply, []),
    json_write(current_output, Reply).

delete_message(MessageId) :-
    webhook(BaseUrl),
    string_concat(BaseUrl, "/messages/", MessagesUrl),
    string_concat(MessagesUrl, MessageId, Url),
    http_delete(Url, Data, []),
    json_write(current_output, Data).