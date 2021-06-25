* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLNTI (ICETOOL).                              00003   
*                                                                       00004   
 OUTFIL FNAMES=(TALEND,TALENDP),REMOVECC,FTOV,VLTRIM=C' ',              00005**3
    TRAILER1=(C'TRL',X'05',COUNT=(M11,LENGTH=10))                       00006   
 OUTFIL FNAMES=EXCEL,REMOVECC,FTOV,VLTRIM=C' ',                         00007   
    HEADER1=(C'ACCOFF',X'05',                                           00008**2
             C'OFFICE',X'05',                                           00009   
             C'MEDIA',X'05',                                            00010   
             C'CLIENT',X'05',                                           00011   
             C'CLIENT NAME',X'05',                                      00012   
             C'VENDOR',X'05',                                           00013**2
             C'VENDOR',X'05',                                           00014**2
             C'MOS',X'05',                                              00015**2
             C'PRODUCT',X'05',                                          00016   
             C'PRODUCT',X'05',                                          00017   
             C'EST',X'05',                                              00018   
             C'ESTIMATE NAME',X'05',                                    00019   
             C'CLT BILL VT',X'05',                                      00020**2
             C'CLT INV #',X'05',                                        00021**2
             C'CLT NET BLD',X'05',                                      00022**2
             C'CLT CK DT',X'05',                                        00023   
             C'FLAG',X'05',                                             00024**2
             C'CLT DEP DT',X'05',                                       00025**2
             C'FLAG',X'05',                                             00026**2
             C'CLT CK#',X'05',                                          00027**2
             C'CLT NET RCVD',X'05',                                     00028   
             C'VDR PAYEE',X'05',                                        00029**2
             C'VDR INV #',X'05',                                        00030   
             C'VDR CK DT',X'05',                                        00031   
             C'FLAG',X'05',                                             00032**2
             C'VDR CLR',X'05',                                          00033   
             C'FLAG',X'05',                                             00034**2
             C'VDR CK#',X'05',                                          00035   
             C'VDR NET PD',X'05',                                       00036   
             C'DAILY BAL',X'05',                                        00037**6
             C'DUE DATE'),                                              00038**6
    IFTHEN=(WHEN=(1,3,CH,NE,C'HDR'),   DATA RECS: FORCE MOS TO MM/01/YY 00039**4
                  OVERLAY=(84:82,2,     SHIFT YEAR TO THE RIGHT         00040**2
                           78:78,4,     CHANGE MMM/ TO MM/01/           00041**2
                     CHANGE=(6,C'JAN/',C'01/01/',                       00042**2
                               C'FEB/',C'02/01/',                       00043**2
                               C'MAR/',C'03/01/',                       00044**2
                               C'APR/',C'04/01/',                       00045**2
                               C'MAY/',C'05/01/',                       00046**2
                               C'JUN/',C'06/01/',                       00047**2
                               C'JUL/',C'07/01/',                       00048**2
                               C'AUG/',C'08/01/',                       00049**2
                               C'SEP/',C'09/01/',                       00050**2
                               C'OCT/',C'10/01/',                       00051**2
                               C'NOV/',C'11/01/',                       00052**2
                               C'DEC/',C'12/01/'),                      00053**2
** THESE DATE FIELDS CONTAIN TWO PIECES OF DATA: A DATE, AND A FLAG.    00054**2
** FOR EACH FIELD, WE SHIFT THE RECORD TO THE RIGHT TO EFFECTIVELY      00055**2
** SPLIT THE DATE FIELD, THEN INSERT A TAB BETWEEN THE DATE AND FLAG.   00056**2
** THIS CREATES TWO COLUMNS WHERE THERE WAS PREVIOUSLY ONLY ONE.        00057**2
                           260:259,200,  SPLIT "VDR CLR"                00058**2
                           259:X'05',                                   00059**2
                           250:249,200,  SPLIT "VDR CK DT"              00060**2
                           249:X'05',                                   00061**2
                           200:199,200,  SPLIT "CLT DEP DT"             00062**2
                           199:X'05',                                   00063**2
                           190:189,200,  SPLIT "CLT CK DT"              00064**2
                           189:X'05'))                                  00065**2
*                                                                       00066**2
