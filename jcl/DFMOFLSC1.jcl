* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLSTI (ICETOOL).                              00003   
*                                                                       00004   
 INCLUDE COND=(2,1,CH,EQ,C'"')              ONLY KEEP DATA RECORDS      00005   
 OUTREC BUILD=(2,132)                       REMOVE CARRIAGE CONTROL     00006   
