* Encoding: UTF-8.
DATA LIST FREE/ide7c sqrtindg post_ln pamo3f. 
BEGIN DATA. 
 
     2.3773     1.4142     2.0840   -40.6534 
     3.6962     1.4142     2.0840   -26.6950 
     5.0152     1.4142     2.0840   -12.7367 
     2.3773     1.4142     2.8562   -42.8205 
     3.6962     1.4142     2.8562   -20.5628 
     5.0152     1.4142     2.8562     1.6950 
     2.3773     1.4142     3.6284   -44.9877 
     3.6962     1.4142     3.6284   -14.4305 
     5.0152     1.4142     3.6284    16.1266 
     2.3773    15.9382     2.0840   -47.8119 
     3.6962    15.9382     2.0840   -22.7705 
     5.0152    15.9382     2.0840     2.2710 
     2.3773    15.9382     2.8562   -47.3090 
     3.6962    15.9382     2.8562   -17.8869 
     5.0152    15.9382     2.8562    11.5352 
     2.3773    15.9382     3.6284   -46.8061 
     3.6962    15.9382     3.6284   -13.0033 
     5.0152    15.9382     3.6284    20.7995 
     2.3773    31.6418     2.0840   -55.5519 
     3.6962    31.6418     2.0840   -18.5271 
     5.0152    31.6418     2.0840    18.4976 
     2.3773    31.6418     2.8562   -52.1620 
     3.6962    31.6418     2.8562   -14.9936 
     5.0152    31.6418     2.8562    22.1747 
     2.3773    31.6418     3.6284   -48.7722 
     3.6962    31.6418     3.6284   -11.4601 
     5.0152    31.6418     3.6284    25.8519 
 
END DATA. 
GRAPH/SCATTERPLOT=ide7c WITH pamo3f BY post_ln/PANEL ROWVAR=sqrtindg.

DATA LIST FREE/ide7c sqrtoudg post_ln pamo3f. 
BEGIN DATA. 
 
     2.3773     6.5891     2.0840   -21.5567 
     3.6962     6.5891     2.0840    -9.6587 
     5.0152     6.5891     2.0840     2.2394 
     2.3773     6.5891     2.8562    -4.2929 
     3.6962     6.5891     2.8562     6.6736 
     5.0152     6.5891     2.8562    17.6401 
     2.3773     6.5891     3.6284    12.9710 
     3.6962     6.5891     3.6284    23.0059 
     5.0152     6.5891     3.6284    33.0408 
     2.3773    18.2155     2.0840   -79.7474 
     3.6962    18.2155     2.0840   -43.2545 
     5.0152    18.2155     2.0840    -6.7617 
     2.3773    18.2155     2.8562   -53.6222 
     3.6962    18.2155     2.8562   -22.6183 
     5.0152    18.2155     2.8562     8.3856 
     2.3773    18.2155     3.6284   -27.4969 
     3.6962    18.2155     3.6284    -1.9820 
     5.0152    18.2155     3.6284    23.5329 
     2.3773    29.8419     2.0840  -137.9380 
     3.6962    29.8419     2.0840   -76.8504 
     5.0152    29.8419     2.0840   -15.7628 
     2.3773    29.8419     2.8562  -102.9514 
     3.6962    29.8419     2.8562   -51.9102 
     5.0152    29.8419     2.8562     -.8689 
     2.3773    29.8419     3.6284   -67.9649 
     3.6962    29.8419     3.6284   -26.9699 
     5.0152    29.8419     3.6284    14.0250 
 
END DATA. 
GRAPH/SCATTERPLOT=ide7c WITH pamo3f BY post_ln/PANEL ROWVAR=sqrtoudg.
>>>> Replicating S1 with S2 data. 

FRE pv330.
RECODE pv330 (1=1) (2 thru 4=0) (else=sysmis) into hanelse.
FRE hanelse.
FRE hanmin.

CORR hanmin ide7c pre_4.
DESC hanmin ide7c
/SAVE. 

COMPUTE pidideo1=mean.1(Zhanmin, Zide7c).
COMPUTE pidideo2=(Zhanmin+Zide7c)/2.
COMPUTE pidideo3=(Zhanmin+Zide7c).
DESC pidideo1 pidideo2.

COMPUTE idepost=ide7c*post_ln.
COMPUTE pidpost=hanmin*post_ln.
COMPUTE pid1post=hanelse*post_ln.
COMPUTE pi1post=pidideo1*post_ln.
COMPUTE pi2post=pidideo2*post_ln.
COMPUTE pi3post=pidideo3*post_ln.
EXECUTE.

COMPUTE indideo=indegree*ide7c.
COMPUTE postind=post_ln*indegree.
COMPUTE idepostind=ide7c*post_ln*indegree.
EXECUTE.

COMPUTE indsideo=sqrtindg*ide7c.
COMPUTE postinds=post_ln*sqrtindg.
COMPUTE idepostinds=ide7c*post_ln*sqrtindg.
EXECUTE.



REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pamo3f
  /METHOD=ENTER age gender educ income eff12 offtalk2 ontalk1 part16 post_ln hanmin
  /METHOD=ENTER pidpost.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT pamo3f
  /METHOD=ENTER age gender educ income knowall eff12 offtalk1 ontalk1 read_ln part16 post_ln ide7c sqrtindg
  /METHOD=ENTER indsideo idepost postinds idepostinds.

DESC pamo3f.

FRE pv2. 
FRE pv330.
RECODE pv330 (1=1) (2=0) (else=sysmis) into hanmin.
FRE hanmin.


Corr offline_diss pv168.

FRE pv1.

FRE 교육 소득.

COMPUTE age.n=나이.
COMPUTE female.n=성별. 
COMPUTE edu.n=교육.
COMPUTE income.n=소득.


FRE eff12.

DESC age gender income education eff12 part16 offline_diss online_diss pv168 hanmin ideoa ideob posting_ln posting_sqrt posting_sum. 

FRE hv84 hv85 hv86 hv87 hv88 hv89.
CORR hv84 hv85 hv86 hv87 hv88 hv89 ideoa ideob.


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

CORR pref3a pamo3.

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



CONTROLS.

>>> Demographics: age, sex, income, education. 

FRE age gender income education. 
DESC  age gender income education. 
COMPUTE educ=education.

>>> Communication: offline and online. 

FRE pv168 pv1.
DESC  pv168 pv1.

CORR offline_diss pv168.
COMPUTE offtalk1=offline_diss.
COMPUTE offtalk2=pv168.
COMPUTE ontalk1=pv1.


>>> Ideology. 

FRE poli_orient.
DESC poli_orient.
COMPUTE ide7c=poli_orient.


FRE poli_orient 정치성향.
Corr poli_orient 정치성향.
RECODE poli_orient (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) into ideoa.
COMPUTE ideob=정치성향.
Corr ideoa ideob ide7c.



>>> Interest.

FRE poli_interest pv166.
RELIABILITY
  /VARIABLES=poli_interest pv166
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE polint12=mean(poli_interest, pv166).
FRE polint12.
DESC polint12.

>>> Efficacy.

FRE pv163 pv164.
RELIABILITY
  /VARIABLES=pv163 pv164
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RECODE pv163 pv164 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1) into eff1 eff2.
COMPUTE eff12=mean(eff1, eff2).
FRE eff12.
DESC eff12.

>>> Knowledge. 

FRE pv177 pv178 pv179 pv180 pv181 pv182 pv183 pv184 pv185 pv185_1.

RECODE pv177 (2=1) (else=0) into kn1. 
RECODE pv178 (1=1) (else=0) into kn2.
RECODE pv179 (2=1) (else=0) into kn3.
RECODE pv180 (2=1) (else=0) into kn4.  
RECODE pv181 (1=1) (else=0) into kn5. 
RECODE pv182 (3=1) (else=0) into kn6. 
RECODE pv183 (4=1) (else=0) into kn7. 
RECODE pv184 (5=1) (else=0) into kn8. 
RECODE pv185 (2=1) (else=0) into kn9. 
RECODE pv185_1 (4=1) (else=0) into kn10. 
FRE kn1 kn2 kn3 kn4 kn5 kn6 kn7 kn8 kn9 kn10.
RELIABILITY
  /VARIABLES= kn1 kn2 kn3 kn4 kn5 kn6 kn7 kn8 kn9 kn10
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE knowtotal=kn1 + kn2 + kn3 + kn4 + kn5 + kn6 + kn7 + kn8 + kn9 + kn10. 
FRE knowtotal. 
DESC knowtotal.
COMPUTE knowall=knowtotal. 

>>> Participation. 

FRE pv170 pv171 pv172 pv174 pv175 pv176. 
RELIABILITY
  /VARIABLES= pv170 pv171 pv172 pv174 pv175 pv176
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE part16=mean(pv170, pv171, pv172, pv174, pv175, pv176).
FRE part16.
DESC part16.  


FRE 박_문_15평가차이_3차.
COMPUTE pamo3=박_문_15평가차이_3차.
EXECUTE.

COMPUTE post_ln=posting_ln.
COMPUTE read_ln=reading_ln.

FRE hv62 hv63.
COMPUTE pamo3f=hv62-hv63. 
CORR pamo3 pamo3f.



