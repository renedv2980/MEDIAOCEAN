* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLPTI (ICETOOL).                              00003   
*                                                                       00004   
 OUTFIL FNAMES=(TALEND,TALENDP),REMOVECC,FTOV,VLTRIM=C' ',              00005**3
    TRAILER1=(C'TRL',X'05',COUNT=(M11,LENGTH=10))                       00006   
 OUTFIL FNAMES=EXCEL,REMOVECC,FTOV,VLTRIM=C' ',                         00007   
    HEADER1=(C'MEDIA',X'05',                                            00008**2
             C'ACC OFF',X'05',                                          00009**2
             C'OFFICE',X'05',                                           00010**2
             C'CLIENT',X'05',                                           00011**2
             C'VENDOR CODE',X'05',                                      00012**2
             C'VENDOR',X'05',                                           00013**2
             C'MOS',X'05',                                              00014**2
             C'INSERTION DATE',X'05',                                   00015   
             C'PROD CODE',X'05',                                        00016   
             C'PRODUCT NAME',X'05',                                     00017   
             C'EST #',X'05',                                            00018   
             C'ESTIMATE NAME',X'05',                                    00019   
             C'CLT BILL DT',X'05',                                      00020**2
             C'CLT INV #',X'05',                                        00021**2
             C'CLT NET BLD',X'05',                                      00022**2
             C'CLT CHK DT',X'05',                                       00023**2
             C'FLAG',X'05',                                             00024**2
             C'CLT DEP DT',X'05',                                       00025**2
             C'FLAG',X'05',                                             00026**2
             C'CLT CHK #',X'05',                                        00027**2
             C'CLT NET RCVD',X'05',                                     00028**2
             C'VDR PAYEE',X'05',                                        00029**2
             C'VDR INV #',X'05',                                        00030**2
             C'VDR CHK DT',X'05',                                       00031**2
             C'FLAG',X'05',                                             00032**2
             C'VDR CLR DT',X'05',                                       00033**2
             C'FLAG',X'05',                                             00034**2
             C'VDR CHK #',X'05',                                        00035**2
             C'VDR NET PAID',X'05',                                     00036**2
             C'DAILY BALANCE',X'05',                                    00037**5
             C'DUE DATE'),                                              00038**5
    IFTHEN=(WHEN=(1,3,CH,NE,C'HDR'),   DATA RECS: FORCE MOS TO MM/01/YY 00039**4
            OVERLAY=(93:C'/',94:91,2,91:C'01',    MM/YY --> MM/01/YY    00040**2
** SOME DATE FIELDS CONTAIN TWO PIECES OF DATA: A DATE, AND A FLAG.     00041**2
** FOR THOSE FIELDS, WE SHIFT THE RECORD TO THE RIGHT TO EFFECTIVELY    00042**2
** SPLIT THE DATE FIELD, THEN INSERT A TAB BETWEEN THE DATE AND FLAG.   00043**2
** THIS CREATES TWO COLUMNS WHERE THERE WAS PREVIOUSLY ONLY ONE.        00044**2
                     284:283,300,  SPLIT "VDR CLR DT"                   00045**2
                     283:X'05',                                         00046**2
                     274:273,300,  SPLIT "VDR CHK DT"                   00047**2
                     273:X'05',                                         00048**2
                     224:223,300,  SPLIT "CLT DEP DT"                   00049**2
                     223:X'05',                                         00050**2
                     214:213,300,  SPLIT "CLT CHK DT"                   00051**2
                     213:X'05',                                         00052**2
** FORCE THE INSERTION DATE FIELD TO BE IMPORTED AS A TEXT FIELD. WE    00053**2
** DO THIS BY SURROUNDING THE FIELD WITH DOUBLE-QUOTES, AND PREFIXING   00054**2
** IT WITH AN EQUALS SIGN.                                              00055**2
                     116:115,300,  "INSERTION DATE" TO TEXT FORMAT      00056**2
                     115:C'"',                                          00057**2
                     104:102,300,                                       00058**2
                     102:C'="'))                                        00059**2
*                                                                       00060**2
