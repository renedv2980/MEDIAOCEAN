* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLSTI (ICETOOL).                              00003   
*                                                                       00004   
* LIMIT THE NUMBER OF INVALID RECORDS WHICH GET WRITTEN TO SPOOL. THIS  00005   
* WILL PREVENT THE SPOOL FROM FILLING UP IF THERE IS A SERIOUS BUG, IN  00006   
* WHICH CASE EVERY RECORD MIGHT GET KICKED OUT!                         00007   
*                                                                       00008   
 OPTION STOPAFT=1000                                                    00009   
*                                                                       00010   
