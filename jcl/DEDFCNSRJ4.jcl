* See Pan member (DEDFCNSRTI)                                           00001   
*                                                                       00002   
 OPTION SDB=INPUT                * (so we don't have to change the JCL) 00003   
 JOINKEYS F1=TYPE0406,FIELDS=(SEQUENCE_NUMBER,A)                        00004   
 JOINKEYS F2=MISSING,FIELDS=(SEQUENCE_NUMBER,A)                         00005   
 JOIN UNPAIRED,F1,ONLY                 *** remove the unpaired records  00006   
