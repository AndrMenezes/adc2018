proc delete data = _all_; run;
data Eleicao;
	input Category $21. xi Count;
	datalines;
	Bush             1 727
	Dukakis          2 583
	Others           3 137
	;
run;

* Construindo a verossimilhança;
proc nlmixed data = eleicao df = 999999;
	parms p1 = 0.5, p2 = 0.4;
	bounds 0 < p1 < 1, 0 < p2 < 1;

	p3 		= 1 - (p1 + p2);
	llike 	= (category="Bush") * log(p1) + (category="Dukakis") * log(p2) + (category="Others") * log(p3);

	model xi ~ general(llike);
	replicate count;

	contrast "H0: p1 = p2" p1 - p2;

	estimate "p3" p3;
	estimate "p1 - p2" p1 - p2;
	estimate "p1/p2" p1 / p2;
run;

* Forma alternativa;
proc logistic data = eleicao ;
	freq count;
	model category = / link = glogit;
	output out = probs(where = (xi = 1) drop = category count) p = pi lower = lower upper = upper;
run;

proc print data = probs;
	var _LEVEL_ lower pi upper;
run;
	











