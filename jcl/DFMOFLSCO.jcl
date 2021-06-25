* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLSTI (ICETOOL).                              00003   
*                                                                       00004   
* REMOVE ALL RECORDS FOR A FIXED LIST OF AGENCY-SPECIFIC CLIENT CODES.  00005   
 OMIT COND=(1,2,CH,EQ,C'H7',AND,                 AGENCY CODE            00006   
            (27,3,CH,EQ,C'ASC',OR,                                      00007   
*****        27,3,CH,EQ,C'ASD',OR,  *** REMOVED MAR/2020 AS PER JEANNE  00008**6
*****        27,3,CH,EQ,C'ATB',OR,  *** REMOVED MAR/2020 AS PER JEANNE  00009**6
*****        27,3,CH,EQ,C'ATH',OR,  *** REMOVED APR/2019 AS PER JEANNE  00010**5
             27,3,CH,EQ,C'CBI',OR,                                      00011   
*****        27,3,CH,EQ,C'CBL',OR,  *** REMOVED JAN/2018 AS PER JEANNE  00012**4
             27,3,CH,EQ,C'TAR',OR,                                      00013**2
             27,3,CH,EQ,C'A2C',OR,                                      00014**5
             27,3,CH,EQ,C'A2D',OR,                                      00015**5
             27,3,CH,EQ,C'UBE',OR,                                      00016**5
*****        27,3,CH,EQ,C'SCJ',OR,  *** REMOVED APR/2019 AS PER JEANNE  00017**5
*****        27,3,CH,EQ,C'SJD'))    *** REMOVED APR/2019 AS PER JEANNE  00018**5
             27,3,CH,EQ,C'IB8'))                                        00019**5
