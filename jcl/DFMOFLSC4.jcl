* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLSTI (ICETOOL).                              00003   
*                                                                       00004   
* ONLY KEEP RECORDS WITH A VALID MOS (E.G., KICK OUT "*AFTER").         00005   
* ALSO KEEP THE HEADER RECORD.                                          00006   
*                                                                       00007   
 OUTFIL FNAMES=T4,REMOVECC,                                             00008   
        INCLUDE=(3,3,CH,EQ,C'HDR',OR,                                   00009   
                 87,4,CH,EQ,C'JAN/',OR,                                 00010   
                 87,4,CH,EQ,C'FEB/',OR,                                 00011   
                 87,4,CH,EQ,C'MAR/',OR,                                 00012   
                 87,4,CH,EQ,C'APR/',OR,                                 00013   
                 87,4,CH,EQ,C'MAY/',OR,                                 00014   
                 87,4,CH,EQ,C'JUN/',OR,                                 00015   
                 87,4,CH,EQ,C'JUL/',OR,                                 00016   
                 87,4,CH,EQ,C'AUG/',OR,                                 00017   
                 87,4,CH,EQ,C'SEP/',OR,                                 00018   
                 87,4,CH,EQ,C'OCT/',OR,                                 00019   
                 87,4,CH,EQ,C'NOV/',OR,                                 00020   
                 87,4,CH,EQ,C'DEC/')                                    00021   
*                                                                       00022   
* SAVE THE RECORDS THAT WERE IN ERROR, SO THAT WE CAN EXAMINE THEM.     00023   
*                                                                       00024   
 OUTFIL FNAMES=BADRECS,SAVE                                             00025   
*                                                                       00026   
