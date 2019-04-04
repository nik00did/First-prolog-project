:-use_module(library(pce)).
:-consult(data).

add(El, List,[El|List]).

%����� ������ ����� �������
print_list(L):-belong(X,L),write(X),nl,fail.
print_list(_).


%��������, ������� ������ �� ������ � �����
print_list_str([]).
print_list_str([H|T]):-write(H), write(" "), print_list_str(T).


%���������� � ��������
len([],0).
len([_|T], N):-len(T,X), N is X+1.


%����� ������� ���������
belong(X,[X|_]).
belong(X,[_|T]):-belong(X, T).


%����� ����������� ���������� ����������� ����������
revers([],[]).
revers([H|T],R):-revers(T,RevT), append(RevT,[H],R).


pairs_value([],[]).
pairs_value([_-V|T0],[V|T]):-pairs_value(T0,T).


%����� ���������
min([Item], Item).
min([Item|List],Item):-min(List, List_Answer),Item=<List_Answer,!.
min([_Item|List], Answer):-min(List,Answer).


%������������� ����� if
max([Max],Max).
max([H|List],Max):-max(List, MaxList),(H>MaxList->Max=H;Max=MaxList).


%���������� ��������
insert_sort(List,Sorted):-i_sort(List,[],Sorted).
i_sort([],Acc,Acc).
i_sort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),
						i_sort(T,NAcc,Sorted).
insert(X,[Y|T],[Y|NT]):-X>Y,insert(X,T,NT).
insert(X,[Y|T],[X,Y|T]):-X=<Y.
insert(X,[],[X]).


%����� ���������� ����������
set_count(N, StartVal):-retractall(count(N,_)),
                        assert(count(N,StartVal)).
increase_count(N, Delta):-retract(count(N, OldVal)),
                          NewVal = OldVal+Delta,
                          assert(count(N, NewVal)),!.
delete_count(N, FinalVal):-retract(count(N,FinalVal)).

%���������� member
member1(X, [Y|T]):-X=Y; member1(X,T).

%���������� findall
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

%����� ������
���_����������(L):-findall(X,���������(X,_,_,_,_,_),L).
���_�������_�����������(L):-findall(X,�������_����������(X,_,_,_),L).
���_����������_�����������(L):-findall(V,���������_����������(V,_,_,_),L).


����(���,����):-���������(���,_,����,_,_,_).

����������_�����������(���_����������,�����������������):-
    ���������(���_����������,�����������������,A,B,C,D),
    write("���� ���������:  "),write(A),nl,
    write("��� ��������:  "),write(B),nl,
    write("��� ������:   "),write(C),nl,
    write("����������������� ��������� (� ����):    "),write(D),nl.


������_�������_����������:-
    �������_����������(B,A,_,_),
    write("���������:  "),write(A),nl,
    write("�������_����������:  "),write(B),nl.


%����������������� ��������� ������ � ������ �������� ���
������_������(���):-���������(���,_,_,_,_,���), ���>11000.
������_�����(���):-���������(���,_,_,_,_,���),���=<11000.



/*��������� � �����*/
��������:-
    ���������(���,�����������,_,_,_,_),
    �����������=����������_�������,
    write(���),nl,
    fail.
    main:-new(D,dialog("���������� � �����������")),
    send(D,append,button(��������_������_��_����,message(@prolog,cons)),below),
	send(D, append, button(���������_����, message(@prolog, save)),below),
	send(D, append, new(���������, dialog("���������", size(250,250)))),
    send(���������, append, label("","��������������:")),
    send(���������, append,button(��������_����������, message(@prolog, add_el_man)),below),
    send(���������, append,button(�������_����������, message(@prolog, remove_el_man)),below),
	send(���������, append,button(��������_�������_����������, message(@prolog, add_el_wom)),below),
	send(���������, append,button(�������_�������_����������, message(@prolog, remove_el_wom)),below),
	send(���������, append,button(��������_����������_����������, message(@prolog, add_el_nasl)),below),
	send(���������, append,button(�������_����������_����������, message(@prolog, remove_el_nasl)),below),

	send(D, append, label("","������:")),
    ���_����������(L),

    send(D,append,new(LB,list_browser),below),
    send(LB,width,35),
    send(LB,members(L)),
    send(LB,open_message,message(@prolog,���������,@arg1?key)),
     nb_setval(listB,LB),

    
    send(D, append,button(�����, message(D,destroy)),below),
    send(D, open).
	
cons:-consult('C:/Users/User/PROJECTS/������/lab1/data.pl').

save:-tell('C:/Users/User/PROJECTS/������/lab1/save.pl'),
    listing(���������),
    listing(�������_����������),
    listing(���������_����������),
    told.

add_el_man:-new(Add, frame('�������� ����������')),
    send(Add,append(new(D, dialog))),
    send(D, append, new(TMan, text_item(���������,''))),
    send(D, append, button(ok, message(@prolog,��������_����������,TMan?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Add,open).

��������_����������(���������):-
    term_string(N, ���������),
    assert(���������(N,_,_,_,_,_)),
      ���_����������(List),
    nb_getval(listB, LB),
    send(LB, members(List)).

remove_el_man:-new(Delete, frame('������� ����������')),
    send(Delete, append(new(D, dialog))),
    send(D, append, new(TMan, text_item(���������,''))),
    send(D, append, button(ok, message(@prolog,delete_element_man,TMan?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Delete,open).

delete_element_man(Man):-term_string(N,Man),
    retract(���������(N,_,_,_,_,_)),
    ���_����������(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
add_el_wom:-new(Add, frame('�������� ������� ����������')),
    send(Add,append(new(D, dialog))),
    send(D, append, new(TWoman, text_item(�������_����������,''))),
    send(D, append, button(ok, message(@prolog,��������_�������_����������,
                                                   TWoman?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Add,open).

��������_�������_����������(�������_����������):-
    term_string(Woman, �������_����������),
    assert(�������_����������(Woman,_,_,_)),
      ���_�������_�����������(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
remove_el_wom:-new(Delete, frame('������� ������� ����������')),
    send(Delete, append(new(D, dialog))),
    send(D, append, new(TWoman, text_item(�������_����������,''))),
    send(D, append, button(ok, message(@prolog,�������_�������_����������,TWoman?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Delete,open).

�������_�������_����������(�������_����������):-
	term_string(Woman, �������_����������),
    retract(�������_����������(Woman,_,_,_)),
		���_�������_�����������(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
add_el_nasl:-new(Add, frame('�������� ���������� ����������')),
    send(Add,append(new(D, dialog))),
    send(D, append, new(TNasl, text_item(���������_����������,''))),
    send(D, append, button(ok, message(@prolog,��������_����������_����������,
                                                   TNasl?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Add,open).

��������_����������_����������(���������_����������):-
    term_string(Nasl, ���������_����������),
    assert(���������_����������(Nasl,_,_,_)),
      ���_����������_�����������(List),
    nb_getval(listB, LB),
    send(LB, members(List)).
	
remove_el_nasl:-new(Delete, frame('������� ���������� ����������')),
    send(Delete, append(new(D, dialog))),
    send(D, append, new(TNasl, text_item(���������_����������,''))),
    send(D, append, button(ok, message(@prolog,�������_����������_����������,TNasl?selection))),
    send(D, append(button(cancel, message(D, return, @nil)))),
    get(D,confirm,Answer),
    free(D),
    Answer \== @nil,
    send(Delete,open).

�������_����������_����������(���������_����������):-
	term_string(Nasl, ���������_����������),
    retract(���������_����������(Nasl,_,_,_)),
		���_����������_�����������(List),
    nb_getval(listB, LB),
    send(LB, members(List)).