CONTROLS.

>>> Demographics: age, sex, income, education. 

FRE age gender income education. 
DESC  age gender income education. 

>>> Communication: offline and online. 

FRE pv168 pv1.
DESC  pv168 pv1.

CORR offline_diss pv168.

>>> Ideology. 

FRE poli_orient.
DESC poli_orient.

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


