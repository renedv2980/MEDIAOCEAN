*********************************************************************** 00001   
*** SEE PAN MEMBER DELDCHKDTI FOR DETAILS.                              00002   
*** COP1CNTL ***                                                        00003   
*********************************************************************** 00004   
*                                                                       00005   
* IGNORE PASSIVES (AND EXTENDED PASSIVES) WHEN CHECKING FOR             00006   
* DUPLICATE DISK ADDRESSES.                                             00007   
*                                                                       00008   
 OMIT COND=(24,4,BI,EQ,X'00000000',OR,23,1,BI,BO,X'40')                 00009   
