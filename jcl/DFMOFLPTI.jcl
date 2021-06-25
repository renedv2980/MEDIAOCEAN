* SYSTEM PRINT:                                                         00001   
* THESE ARE ICETOOL CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB    00002   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00003   
*                                                                       00004   
* DISCARD ALL UNWANTED RECORDS. BUILD TWO OUTPUT FILES: ONE CONTAINING  00005   
* DATA RECORDS, THE OTHER CONTAINING THE HEADER RECORD.                 00006   
COPY FROM(IN) USING(COP1)                                               00007   
*                                                                       00008   
* ATTACH MULTIPLE LOGICALLY-CONNECTED WRITER OUTPUT RECORDS TOGETHER    00009   
* INTO SINGLE RECORDS. NOTE: THE "TOLEN" VALUE IS DERIVED BY            00010   
* MULTIPLYING THE LRECL (132) TIMES THE NUMBER OF LOGICALLY-CONNECTED   00011   
* RECORDS IN THE ORIGINAL WRITER OUTPUT DATASET.                        00012   
RESIZE FROM(T1) TO(T2) TOLEN(396)                                       00013   
*                                                                       00014   
* REFORMAT THE DATA RECORDS TO TAB-DELIMITED FORMAT, AND APPEND THEM    00015   
* TO THE HEADER RECORD.                                                 00016   
COPY FROM(T2) TO(T3) USING(COP2)                                        00017   
*                                                                       00018   
* CONVERT THE OUTPUT FILE TO FORMAT VB, STRIP OFF TRAILING BLANKS, AND  00019   
* ADD THE TRAILER RECORD. THIS IS THE FILE FOR TALEND.                  00020   
* ALSO PRODUCE A FILE WITH A HEADER ROW FOR IMPORTING INTO EXCEL,       00021   
* SHOULD WE NEED THIS FOR DEBUGGING.                                    00022   
COPY FROM(T3) USING(COP3)                                               00023   
*                                                                       00024   
