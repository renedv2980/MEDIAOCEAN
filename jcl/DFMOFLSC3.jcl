* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLSTI (ICETOOL).                              00003   
*                                                                       00004   
 OUTFIL FNAMES=(TALEND,TALENDP),REMOVECC,FTOV,VLTRIM=C' ',              00005**3
    BUILD=(3,526),                               REMOVE AGENCY          00006   
    TRAILER1=(C'TRL',X'05',COUNT=(M11,LENGTH=10))                       00007   
 OUTFIL FNAMES=EXCEL,REMOVECC,FTOV,VLTRIM=C' ',                         00008   
    HEADER1=(C'MEDIA',X'05',                                            00009   
             C'ACC OFF',X'05',                                          00010**2
             C'OFFICE',X'05',                                           00011**2
             C'CLIENT',X'05',                                           00012   
             C'VENDOR',X'05',                                           00013**2
             C'VENDOR',X'05',                                           00014**2
             C'MOS',X'05',                                              00015**2
             C'PRODUCT',X'05',                                          00016   
             C'PRODUCT',X'05',                                          00017   
             C'ESTIMATE',X'05',                                         00018   
             C'VDR INV #',X'05',                                        00019**2
********     C'VDR INV DT',X'05',           OMITTED AS PER JEANNE       00020**2
             C'CLT BILL DT',X'05',                                      00021**2
********     C'INVOICE DUE DATE',X'05',     OMITTED AS PER JEANNE       00022**2
             C'CLT INV #',X'05',                                        00023**2
             C'CLT NET BLD',X'05',                                      00024**2
             C'CLT CHK DT',X'05',                                       00025**2
             C'FLAG',X'05',                                             00026**2
             C'CLT DEP DT',X'05',                                       00027**2
             C'FLAG',X'05',                                             00028**2
             C'CLT CHK #',X'05',                                        00029**2
             C'CLT NET RCVD',X'05',                                     00030**2
             C'VDR PAYEE',X'05',                                        00031**2
             C'VDR CLR DT',X'05',                                       00032**2
             C'FLAG',X'05',                                             00033**2
             C'VDR CHK DT',X'05',                                       00034**2
             C'FLAG',X'05',                                             00035**2
             C'VDR CHK #',X'05',                                        00036**2
             C'VDR NET PAID',X'05',                                     00037**2
********     C'CASH DAYS',X'05',            OMITTED AS PER JEANNE       00038**2
             C'DAILY BALANCE'),                                         00039**2
    IFTHEN=(WHEN=INIT,BUILD=(3,526)),   SHIFT RECORD LEFT BY TWO BYTES  00040**4
    IFTHEN=(WHEN=(1,3,CH,NE,C'HDR'),    DATA RECS: FORCE MOS TO MM/1/YY 00041**4
                  OVERLAY=(90:89,2,     SHIFT YEAR TO THE RIGHT         00042**2
                           85:85,4,     CHANGE MMM/ TO MM/1/            00043**2
                     CHANGE=(5,C'JAN/',C'01/1/',                        00044**2
                               C'FEB/',C'02/1/',                        00045**2
                               C'MAR/',C'03/1/',                        00046**2
                               C'APR/',C'04/1/',                        00047**2
                               C'MAY/',C'05/1/',                        00048**2
                               C'JUN/',C'06/1/',                        00049**2
                               C'JUL/',C'07/1/',                        00050**2
                               C'AUG/',C'08/1/',                        00051**2
                               C'SEP/',C'09/1/',                        00052**2
                               C'OCT/',C'10/1/',                        00053**2
                               C'NOV/',C'11/1/',                        00054**2
                               C'DEC/',C'12/1/'),                       00055**2
** THESE DATE FIELDS CONTAIN TWO PIECES OF DATA: A DATE, AND A FLAG.    00056**2
** FOR EACH FIELD, WE SHIFT THE RECORD TO THE RIGHT TO EFFECTIVELY      00057**2
** SPLIT THE DATE FIELD, THEN INSERT A TAB BETWEEN THE DATE AND FLAG.   00058**2
** THIS CREATES TWO COLUMNS WHERE THERE WAS PREVIOUSLY ONLY ONE.        00059**2
                           277:276,200,  SPLIT "VDR CHK DT"             00060**2
                           276:X'05',                                   00061**2
                           267:266,200,  SPLIT "VDR CLR DT"             00062**2
                           266:X'05',                                   00063**2
                           230:229,200,  SPLIT "CLT DEP DT"             00064**2
                           229:X'05',                                   00065**2
                           220:219,200,  SPLIT "CLT CHK DT"             00066**2
                           219:X'05',                                   00067**2
                           157:9X,       BLANK OUT "VDR INV DT"         00068**2
                           175:9X,       BLANK OUT "INVOICE DUE DATE"   00069**2
                           304:11X))     BLANK OUT "CASH DAYS"          00070**2
*                                                                       00071**2
