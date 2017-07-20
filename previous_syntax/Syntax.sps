1. Absolute Difference: pm1, pm2, pm3 (park-moon). 

FRE pv254 pv255.
COMPUTE pm1=abs(pv254-pv255).
FRE pm1.
DESC pm1.

FRE kv37 kv38.
COMPUTE pm2=abs(kv37-kv38).
FRE pm2.
DESC pm2.

FRE hv62 hv63.
COMPUTE pm3=abs(hv62-hv63).
FRE pm3.
DESC pm3.


2. Relative Candidate Preference based on Wave 1 Candidate Choice: pref1, pref2, pref3.
>>> Undecided voters (pv189=3; N=83) excluded. So total N=258 (P 95 + M 163). 

FRE pv189.
IF (pv189=1) pref1=(pv254-pv255).
IF (pv189=2) pref1=(pv255-pv254).
FRE pref1.
DESC pref1.

IF (pv189=1) pref2=(kv37-kv38).
IF (pv189=2) pref2=(kv38-kv37).
FRE pref2.
DESC pref2.

IF (pv189=1) pref3=(hv62-hv63).
IF (pv189=2) pref3=(hv63-hv62).
FRE pref3.
DESC pref3.


3. Relative Candidate Preference including undecided voters: pref1a, pref2a, pref3a.
>>> Total N=319: P 125, M 194, Undecided 22
>>> # of Undecided was initially 83 but reduced to 22 after imputation: Leaning (N=61; P 30 + M 31) and No Leaning (N=22). 

COMPUTE w1diff=pv254-pv255.
FRE w1diff. 

RECODE pv189 (1=1) (2=2) (3=3) into w1choice.
IF ((pv189=3) and (w1diff<0)) w1choice=2.
IF ((pv189=3) and (w1diff>0)) w1choice=1.
FRE w1choice.  

CROSSTABS
  /TABLES=pv189 BY w1choice
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

IF (w1choice=1) pref1a=(pv254-pv255).
IF (w1choice=2) pref1a=(pv255-pv254).
FRE pref1a.
DESC pref1a.

IF (w1choice=1) pref2a=(kv37-kv38).
IF (w1choice=2) pref2a=(kv38-kv37).
FRE pref2a.
DESC pref2a.

IF (w1choice=1) pref3a=(hv62-hv63).
IF (w1choice=2) pref3a=(hv63-hv62).
FRE pref3a.
DESC pref3a.


4. Swing Voters (N=17). 

COMPUTE swing=0.
IF ((pref1<0) or (pref2<0) or (pref3<0)) swing=1.
FRE swing.

SORT CASES BY swing(D).

COMPUTE swinga=0.
IF ((pref1a<0) or (pref2a<0) or (pref3a<0)) swinga=1.
FRE swinga.

SORT CASES BY swinga(D).


5. Problematic Cases (N=7). 

>>> Diagnostic analysis (N=5). 

COMPUTE error=0.
IF ((pv189=1) and (w1diff<0)) error=1.
IF ((pv189=2) and (w1diff>0)) error=2.
FRE error. 

SORT CASES BY error(D).

>>> Handcoding (N=7). 

RECODE handcoding (1=1) (else=0).
FRE handcoding. 

SORT CASES BY handcoding(D).


6. ANALYSIS: Should exclude error caess? Refer to "handcoding".  

USE ALL.
COMPUTE filter_$=(handcoding = 0).
VARIABLE LABELS filter_$ 'handcoding = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

=======

DATA TRANSFORMATION FOR READING & POSTING: LN and SQRT

FRE reading_sum posting_sum/histogram normal. .
DESC reading_sum posting_sum.

COMPUTE reading_ln=ln(reading_sum). 
COMPUTE reading_sqrt=sqrt(reading_sum). 
COMPUTE posting_ln=ln(posting_sum+1). 
COMPUTE posting_sqrt=sqrt(posting_sum). 
FRE reading_sum reading_ln reading_sqrt posting_sum posting_ln posting_sqrt/histogram normal. 
DESC reading_sum reading_ln reading_sqrt posting_sum posting_ln posting_sqrt.
CORR reading_sum reading_ln reading_sqrt posting_sum posting_ln posting_sqrt.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT posting_ln
  /METHOD=ENTER reading_ln
  /SAVE RESID.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT posting_sqrt
  /METHOD=ENTER reading_sqrt
  /SAVE RESID.


