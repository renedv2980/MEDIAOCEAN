* See Pan member (DEDFCNSRTI)                                           00001   
*                                                                       00002   
 OPTION SDB=INPUT                * (so we don't have to change the JCL) 00003   
 JOINKEYS F1=TYPE06C,FIELDS=(MIT_KEY,A)                                 00004   
 JOINKEYS F2=TYPE06P,FIELDS=(MIT_KEY,A)                                 00005   
 JOIN UNPAIRED,F2,ONLY                 *** catch unpaired "06" records  00006   
