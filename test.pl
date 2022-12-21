
% Factos
% caracteristica(Pessoa, Caracteristica)
caracteristica(fernandes, 'Tem Skill Issue').
caracteristica(anaPaiva, 'Tem Skill Issue').
caracteristica(patrickBateman, 'Gosta de Jantar no Dorsia').

pessoasComSkillIssue(ListaPessoas) :- 
    setof(Pessoa, caracteristica(Pessoa, 'Tem Skill Issue'), ListaPessoas).

