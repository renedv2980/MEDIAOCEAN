* See Pan member (DEDFCNSRTI)                                           00001   
*                                                                       00002   
 OPTION SDB=INPUT                * (so we don't have to change the JCL) 00003   
 SORT FIELDS=(SEQUENCE_NUMBER,A)       *** reconstruct the file         00004   
 OUTREC BUILD=(MITREC)                 *** remove our sequence number   00005   
