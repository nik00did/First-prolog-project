:-use_module(library(pce)).
:-consult(data).

add(El, List,[El|List]).

%Метод отката после неудачи
print_list(L):-belong(X,L),write(X),nl,fail.
print_list(_).


%Рекурсия, деление списка на голову и хвост
print_list_str([]).
print_list_str([H|T]):-write(H), write(" "), print_list_str(T).


%Повторение и рекурсия
len([],0).
len([_|T], N):-len(T,X), N is X+1.


%Метод анализа состояний
belong(X,[X|_]).
belong(X,[_|T]):-belong(X, T).


%Метод организации восходящих рекурсивных вычислений
revers([],[]).
revers([H|T],R):-revers(T,RevT), append(RevT,[H],R).


pairs_value([],[]).
pairs_value([_-V|T0],[V|T]):-pairs_value(T0,T).


%Метод отсечения
min([Item], Item).
min([Item|List],Item):-min(List, List_Answer),Item=<List_Answer,!.
min([_Item|List], Answer):-min(List,Answer).


%Использование цикла if
max([Max],Max).
max([H|List],Max):-max(List, MaxList),(H>MaxList->Max=H;Max=MaxList).


%Сортировка вставкой
insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),
						i_sort(T,NAcc,Sorted).
insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).


%Метод глобальной переменной
set_count(N, StartVal):-retractall(count(N,_)),
                        assert(count(N,StartVal)).
increase_count(N, Delta):-retract(count(N, OldVal)),
                          NewVal = OldVal+Delta,
                          assert(count(N, NewVal)),!.
delete_count(N, FinalVal):-retract(count(N,FinalVal)).

%Реализация member
member1(X, [Y|T]):-X=Y; member1(X,T).

%Реализация findall
myfindall(Template,Goal,_) :-
        call(Goal),
        asserta(findallsol(Template)),
        fail.
myfindall(_,_,Solutions) :-
        collect(Solutions).
collect(Solutions) :-
        retract(findallsol(Template)),
        !,
        Solutions = [Template|RestSols],
        collect(RestSols).
collect([]).

%вывод списка
все_императоры(L):-findall(X,император(X,_,_,_,_,_),L).
все_супруги_императоров(L):-findall(X,супруга_императора(X,_,_,_),L).
все_наследники_императоров(L):-findall(V,наследник_императора(V,_,_,_),L).


срок(Имя,Срок):-император(Имя,_,Срок,_,_,_).

информация_императоров(Имя_императора,Госпринадлежность):-
    император(Имя_императора,Госпринадлежность,A,B,C,D),
    write("Этап правления:  "),write(A),nl,
    write("Год рождения:  "),write(B),nl,
    write("Год смерти:   "),write(C),nl,
    write("Продолжительность правления (в днях):    "),write(D),nl.


информ_супруга_императора:-
    супруга_императора(B,A,_,_),
    write("император:  "),write(A),nl,
    write("супруга_императора:  "),write(B),nl.


%Продолжительность правления больше и меньше тридцати лет
больше_одного(Имя):-император(Имя,_,_,_,_,Дни), Дни>11000.
меньше_одног(Имя):-император(Имя,_,_,_,_,Дни),Дни=<11000.



/*отсечение и откат*/
романовы:-
    император(Имя,Государство,_,_,_,_),
    Государство=российская_империя,
    write(Имя),nl,
    fail.
    main:-new(D,dialog("Информация о императорах")),
    send(D,append,button(обновить_данные_из_базы,message(@prolog,cons)),below),
	send(D, append, button(сохранить_базу, message(@prolog, save)),below),
	send(D, append, new(Изменения, dialog("Изменения", size(250,250)))),
    send(Изменения, append, label("","Редактирование:")),
    send(Изменения, append,button(добавить_императора, message(@prolog, add_el_man)),below),
    send(Изменения, append,button(удалить_императора, message(@prolog, remove_el_man)),below),
	send(Изменения, append,button(добавить_супругу_императора, message(@prolog, add_el_wom)),below),
	send(Изменения, append,button(удалить_супругу_императора, message(@prolog, remove_el_wom)),below),
	send(Изменения, append,button(добавить_наследника_императора, message(@prolog, add_el_nasl)),below),
	send(Изменения, append,button(удалить_наследника_императора, message(@prolog, remove_el_nasl)),below),

	send(D, append, label("","Список:")),
    все_императоры(L),

    send(D,append,new(LB,list_browser),below),
    send(LB,width,35),
    send(LB,members(L)),
    send(LB,open_message,message(@prolog,император,@arg1?key)),
     nb_setval(listB,LB),

    
    send(D, append,button(виход, message(D,destroy)),below),
    send(D, open).
	
cons:-consult('C:/Users/User/PROJECTS/Пролог/lab1/data.pl').

save:-tell('C:/Users/User/PROJECTS/Пролог/lab1/save.pl'),
    listing(император),
    listing(супруга_императора),
    listing(наследник_императора),
    told.

add_el_man:-new(Add, frame('Добавить императора')),
    send(Add,append(new(D, dialog))),
    send(D, append, new(TMan, text_item(император,''))),
    send(D, append, button(ok, message(@prolog,добавить_императора,TMan?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Add,open).

добавить_императора(Император):-
    term_string(N, Император),
    assert(император(N,_,_,_,_,_)),
      все_императоры(List),
    nb_getval(listB, LB),
    send(LB, members(List)).

remove_el_man:-new(Delete, frame('Удалить императора')),
    send(Delete, append(new(D, dialog))),
    send(D, append, new(TMan, text_item(император,''))),
    send(D, append, button(ok, message(@prolog,delete_element_man,TMan?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Delete,open).

delete_element_man(Man):-term_string(N,Man),
    retract(император(N,_,_,_,_,_)),
    все_императоры(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
add_el_wom:-new(Add, frame('Добавить супругу императора')),
    send(Add,append(new(D, dialog))),
    send(D, append, new(TWoman, text_item(супруга_императора,''))),
    send(D, append, button(ok, message(@prolog,добавить_супругу_императора,
                                                   TWoman?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Add,open).

добавить_супругу_императора(Супруга_императора):-
    term_string(Woman, Супруга_императора),
    assert(супруга_императора(Woman,_,_,_)),
      все_супруги_императоров(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
remove_el_wom:-new(Delete, frame('Удалить супругу императора')),
    send(Delete, append(new(D, dialog))),
    send(D, append, new(TWoman, text_item(супруга_императора,''))),
    send(D, append, button(ok, message(@prolog,удалить_супругу_императора,TWoman?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Delete,open).

удалить_супругу_императора(Супруга_императора):-
	term_string(Woman, Супруга_императора),
    retract(супруга_императора(Woman,_,_,_)),
		все_супруги_императоров(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
add_el_nasl:-new(Add, frame('Добавить наследника императора')),
    send(Add,append(new(D, dialog))),
    send(D, append, new(TNasl, text_item(наследник_императора,''))),
    send(D, append, button(ok, message(@prolog,добавить_наследника_императора,
                                                   TNasl?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Add,open).

добавить_наследника_императора(Наследник_императора):-
    term_string(Nasl, Наследник_императора),
    assert(наследник_императора(Nasl,_,_,_)),
      все_наследники_императоров(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
remove_el_nasl:-new(Delete, frame('Удалить наследника императора')),
    send(Delete, append(new(D, dialog))),
    send(D, append, new(TNasl, text_item(наследник_императора,''))),
    send(D, append, button(ok, message(@prolog,удалить_наследника_императора,TNasl?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Delete,open).

удалить_наследника_императора(Наследник_императора):-
	term_string(Nasl, Наследник_императора),
    retract(наследник_императора(Nasl,_,_,_)),
		все_наследники_императоров(List),
    nb_getval(listB, LB),
    send(LB, members(List)).