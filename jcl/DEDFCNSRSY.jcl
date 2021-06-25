* See Pan member (DEDFCNSRTI)                                           00001   
*                                                                       00002   
* These fields are described in the Nielsen MIT record spec.            00003   
MITREC,1,400,CH                                                         00004   
MIT_SEQUENCE_CODE,=,2,CH                                                00005   
PROGRAM_DATA,C'04'                                                      00006   
COMMERCIAL_AUDIENCE_DATA,C'06'                                          00007   
MIT_KEY,=,84,CH                                                         00008   
MIT_KEY_SHORTER,=,82,CH                                                 00009   
MIT_RECORD_TYPE,85,1,CH                                                 00010   
PROGRAM_DESCRIPTOR_RECORD,C'D'                                          00011   
COMMERCIAL_DESCRIPTOR_RECORD,C'C'                                       00012   
HOUSEHOLD_RECORD,C'H'                                                   00013   
PERSONS_DETAIL_RECORD,C'P'                                              00014   
*                                                                       00015   
SEQUENCE_NUMBER,401,8,ZD       *** our internal sequence number         00016   
