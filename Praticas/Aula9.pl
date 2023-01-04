p(2, 3).
p(X,Y) :- q(X), r(Y).
q(1).
q(2):- !.
q(3).
r(1).
r(3).

% pergunta frequente AL:
% imaginem que eu dava em P2 considere o produto interno para o qual a base B=(1+t, 1-t, t²) é ortonormal
% a) Calcule a projeção ortogonal do veto 1+t+t² sobre 1-t-t² 