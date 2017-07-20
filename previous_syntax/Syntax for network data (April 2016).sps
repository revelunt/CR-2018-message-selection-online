﻿* Encoding: UTF-8.
FRE pv189 canpref1.
FRE pv191/

DATASET ACTIVATE DataSet1.
CROSSTABS
  /TABLES=pv189 BY canpref1
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

FRE pv258 ideo1 poli_orient.
CORR pv258 ideology.
FRE pv258 pv259 pv260. 

RECODE ide_self (sysmis=9) (else=copy).
FRE ide_self.
COMPUTE ide_fami=pv259.
IF (ide_self=9) ide_fami=9.
COMPUTE ide_frie=pv260.
IF (ide_self=9) ide_frie=9.
CORR ide_self ide_fami ide_frie. 

CROSSTABS
  /TABLES=pv258 BY poli_orient
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT
  /COUNT ROUND CELL.

DATASET ACTIVATE DataSet2.
FACTOR
  /VARIABLES pv13 pv14 pv15 pv16 pv18 pv19 pv20 pv21 pv23 pv24 pv27 pv28 pv29 pv30 pv31
  /MISSING LISTWISE 
  /ANALYSIS pv13 pv14 pv15 pv16 pv18 pv19 pv20 pv21 pv23 pv24 pv27 pv28 pv29 pv30 pv31
  /PRINT INITIAL EXTRACTION ROTATION
  /FORMAT SORT BLANK(.30)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.

RELIABILITY
  /VARIABLES=pv18 pv19 pv20 pv21 pv23 pv24
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv13 pv14 pv15 pv16 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv27 pv28 pv29 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv30 pv31 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

COMPUTE motv1=mean(pv18, pv19, pv20, pv21, pv23, pv24).
COMPUTE motv2=mean(pv13, pv14, pv15, pv16).
COMPUTE motv3=mean(pv27, pv28, pv29).
COMPUTE motv4=mean(pv30, pv31).
DESC motv1 motv2 motv3 motv4. 
CORR motv1 motv2 motv3 motv4. 

CORR pv167 knowtotal.

COMPUTE p_image = mean(pv194, pv195, pv196, pv197, pv198, pv199, pv200, pv201, pv202, pv203, pv204, pv205, pv206, pv207, pv208). 
COMPUTE m_image = mean(pv209, pv210, pv211, pv212, pv213, pv214, pv215, pv216, pv217, pv218, pv219, pv220, pv221, pv222, pv223). 
DESC p_image m_image. 

FRE pv254 pv255.
COMPUTE pm_pref=pv254-pv255.
DESC pm_pref. 
COMPUTE pm_pfab=abs(pv254-pv255).
DESC pm_pfab.


FRE pv267 pv268 pv269 pv270 pv271 pv272 pv273 pv274 pv275. 

RELIABILITY
  /VARIABLES=pv267 pv268 pv269 pv270
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=pv271 pv272 pv273 pv274 pv275
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

COMPUTE evalcrit1=mean(pv267, pv268, pv269, pv270). 
COMPUTE evalcrit2=mean(pv271, pv272, pv273, pv274, pv275). 
DESC evalcrit1 evalcrit2.

RECODE sex (1=0) (2=1) into female.
FRE female sex. 

>>>>>>>>>>>> POLICY STANCE.

FACTOR
  /VARIABLES pv299 pv300 pv303 pv304
  /MISSING LISTWISE 
  /ANALYSIS pv299 pv300 pv303 pv304
  /PRINT INITIAL EXTRACTION ROTATION
  /FORMAT SORT BLANK(.30)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.
RELIABILITY
  /VARIABLES=pv299 pv303
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv300 pv304
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE policy_c=mean(pv300, pv304). 
COMPUTE policy_l=mean(pv299, pv303).
CORR policy_c policy_l ide_self. 

>>>>>>>>>>>> TOLERANCE.

FRE pv142 pv143 pv144 pv145 pv146 pv147. 
FACTOR
  /VARIABLES pv142 pv143 pv144 pv145 pv146 pv147
  /MISSING LISTWISE 
  /ANALYSIS pv142 pv143 pv144 pv145 pv146 pv147
  /PRINT INITIAL EXTRACTION ROTATION
  /FORMAT SORT BLANK(.30)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.
RELIABILITY
  /VARIABLES=pv142 pv143 pv144 pv145 pv146 pv147
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE tolerance=mean(pv142, pv143, pv144, pv145, pv146, pv147).
DESC tolerance. 

>>>>>>>>>>>> ONLINE DISCUSSION STYLE.

FACTOR
  /VARIABLES pv50 pv51 pv52 pv53 pv63 pv64 pv69 pv70
  /MISSING LISTWISE 
  /ANALYSIS pv50 pv51 pv52 pv53 pv63 pv64 pv69 pv70
  /PRINT INITIAL EXTRACTION ROTATION
  /FORMAT SORT BLANK(.30)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.
RELIABILITY
  /VARIABLES=pv51 pv53 
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv63 pv64
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv51 pv53 pv63 pv64
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv69 pv70
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv50 pv52
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE homoge=mean(pv50, pv52).
COMPUTE heteroge=mean(pv51, pv53). 
CORR homoge heteroge.

>>>>>>>>>>>> OPINIONATION.

FACTOR
  /VARIABLES pv45 pv47 pv118 pv156
  /MISSING LISTWISE 
  /ANALYSIS pv45 pv47 pv118 pv156
  /PRINT INITIAL EXTRACTION ROTATION
  /FORMAT SORT BLANK(.30)
  /CRITERIA MINEIGEN(1) ITERATE(25)
  /EXTRACTION PC
  /CRITERIA ITERATE(25) DELTA(0)
  /ROTATION OBLIMIN
  /METHOD=CORRELATION.
RELIABILITY
  /VARIABLES=pv45 pv118
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
RELIABILITY
  /VARIABLES=pv47 pv156
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE opleader=mean(pv45, pv118).
COMPUTE follower=mean(pv47, pv156). 
CORR opleader follower.

>>>>>>>>>>>> INTEREST.

RELIABILITY
  /VARIABLES=pv165 pv166
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.
COMPUTE interest=mean(pv165, pv166).
DESC interest.


