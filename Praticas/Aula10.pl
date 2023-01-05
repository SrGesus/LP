substitui_el_lst([], _, _, []).
substitui_el_lst([El| L1], El, NovoEl, [NovoEl|L2]):-
    substitui_el_lst(L1, El, NovoEl, L2), !.
substitui_el_lst([P| L1], El, NovoEl, [P|L2]):-
    substitui_el_lst(L1, El, NovoEl, L2).

substitui_arg(T_c, Arg, Novo_Arg, Novo_T_c):-
    T_c =.. [F|ListaT_c],
    substitui_el_lst(ListaT_c, Arg, Novo_Arg, L2),
    Novo_T_c =.. [F|L2].
