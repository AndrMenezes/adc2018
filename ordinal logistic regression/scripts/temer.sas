proc delete data = _all_; run;

%let path = C:\Users\User\Dropbox\5° Série\Análise de Dados Categóricos\Regressão Multinomial\dados;
%include "C:\Users\User\Dropbox\4° Série\Modelos Lineares Generalizados\trabalho\scripts\tablatex.sas";

*******************************************************************************************************;
proc import out = dados datafile = "&&path/dados_datafolha.csv" dbms = csv replace;
			delimiter = ";";
			getnames = yes;
run;

data dados;
	set dados;
	if(y3 = 1 or y3 = 2) then temer = 1;
	if(y3 = 3) then temer = 2;
	if(y3 = 4 or y3 = 5) then temer = 3;
run;

proc freq data = dados;
	tables y3 * denuncia / nocol nopercent;
run;

proc sort data = dados;	by y3; run;

*******************************************************************************************************;
* Modelo 1;
proc logistic data = dados order = data;
	model y3 = / link = clogit aggregate scale = none;
run;
* Modelo 2;
proc logistic data = dados order = data;
	class sexo(ref = "2") / param = ref;
	model y3 = sexo / link = clogit aggregate scale = none;
run;
* Modelo 3;
proc logistic data = dados order = data;
	class sexo(ref = "2") votou2010(ref = "2") / param = ref;
	model y3 = sexo votou2010 / link = clogit aggregate scale = none;
run;

* Modelo 4 - final;
proc logistic data = dados order = data;
	class sexo(ref = "2") votou2010(ref = "2") denuncia(ref = "2") / param = ref;
	model y3 = sexo votou2010 denuncia  / link = clogit aggregate scale = none;
	ods output ParameterEstimates = estimativas(drop = _ESTTYPE_ df);
	ods output OddsRatios = razaochances;
run;


%uplatex(x = "table1.tex");
proc print data = estimativas noobs;
	format _numeric_ comma10.4;
run;
%downlatex;

%uplatex(x = "table2.tex");
proc print data = razaochances noobs;
	format _numeric_ comma10.4;
run;
%downlatex;

data grid;
	infile cards;
	input sexo denuncia votou2010;
	cards;
     1        1         1
     2        1         1
     1        2         1
     2        2         1
     1        1         2
     2        1         2
     1        2         2
     2        2         2
     1        1         3
     2        1         3
     1        2         3
     2        2         3
	 ;
run;

proc logistic data = dados order = data;
	class sexo(ref = "2") votou2010(ref = "2") denuncia(ref = "2") / param = ref;
	model y3 = sexo votou2010 denuncia  / link = clogit aggregate scale = none influence;
	score data = grid out = prob clm;
	score data = grid out = acum cumulative clm;
*	output out = residuos reschi = pearson resdev = deviance;
run;

proc export data = prob outfile = "&&path/prob.csv" dbms = csv replace; run;
proc export data = acum outfile = "&&path/acum.csv" dbms = csv replace; run;

*a chance de achar o governo temer ótimo/bom, (em relação ao governo ser regular ou ruim/pessímo),
entre as pessoas que acreditam que os deputados não deveriam aceitar a denúncia do MP contra o temer, 
é 9.122 vezes a das pessoas que acreditam que os deputados deveriam aceitar a denúncia do MP contra o temer;

proc logistic data = dados order = data;
	class sexo(ref = "1") lula(ref = "1") votou2010(ref = "2") corrup_ou_incomp(ref = "1") / param = ref;
	model temer = sexo idade1 escola votou2010 lula corrup_ou_incomp / link = glogit;
run;
