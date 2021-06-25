* See Pan member (DEDFCNSRTI)                                           00001   
*                                                                       00002   
 OPTION SDB=INPUT                * (so we don't have to change the JCL) 00003   
 JOINKEYS F1=TYPE04D,FIELDS=(MIT_KEY_SHORTER,A)  *** SHORTER !!!        00004   
 JOINKEYS F2=TYPE04P,FIELDS=(MIT_KEY_SHORTER,A)                         00005   
 JOIN UNPAIRED,F2,ONLY                 *** catch unpaired "04" records  00006   
