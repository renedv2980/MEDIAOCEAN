* SYSTEM SPOT:                                                          00001   
* THESE ARE ICETOOL CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB    00002   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00003   
*                                                                       00004   
* READ THE "ALL REQUEST" INPUT FILE. BUILD TWO OUTPUT FILES: ONE        00005   
* CONTAINING DATA RECORDS, THE OTHER CONTAINING THE HEADER RECORD.      00006   
COPY FROM(INALLCLT) USING(COPA)                                         00007   
*                                                                       00008   
* READ THE "SINGLE-CLIENT REQUEST" INPUT FILE. KEEP ONLY DATA RECORDS.  00009   
COPY FROM(ININDCLT) TO(T11) USING(COP1)                                 00010   
*                                                                       00011   
* ATTACH MULTIPLE LOGICALLY-CONNECTED WRITER OUTPUT RECORDS TOGETHER    00012   
* INTO SINGLE RECORDS. NOTE: THE "TOLEN" VALUE IS DERIVED BY            00013   
* MULTIPLYING THE LRECL (132) TIMES THE NUMBER OF LOGICALLY-CONNECTED   00014   
* RECORDS IN THE ORIGINAL WRITER OUTPUT DATASET.                        00015   
RESIZE FROM(T1A) TO(T2) TOLEN(528)                                      00016   
RESIZE FROM(T11) TO(T11R) TOLEN(528)                                    00017   
*                                                                       00018   
* REFORMAT THE "ALL-CLIENT REQUEST" DATA RECORDS TO TAB-DELIMITED       00019   
* FORMAT.                                                               00020   
COPY FROM(T2) TO(T2R) USING(COP2)                                       00021   
*                                                                       00022   
* REMOVE ALL DATA RECORDS FROM THE "ALL-CLIENT REQUESTS" FOR WHICH THE  00023   
* CLIENT CODE WAS ALREADY REQUESTED IN A "SINGLE-CLIENT" REQUEST.       00024   
* APPEND THE REMAINING RECORDS TO THE HEADER RECORD FILE.               00025   
COPY FROM(T2R) TO(T3) USING(COPO)                                       00026   
*                                                                       00027   
* REFORMAT THE "SINGLE-CLIENT REQUEST" DATA RECORDS TO TAB-DELIMITED    00028   
* FORMAT, AND APPEND THEM TO THE HEADER RECORD FILE.                    00029   
COPY FROM(T11R) TO(T3) USING(COP2)                                      00030   
*                                                                       00031   
* WRITE ANY INVALID RECORDS TO A DATASET. WRITE THE REMANING (GOOD)     00032**2
* RECORDS TO A DIFFERENT DATASET FOR FURTHER PROCESSING.                00033**2
COPY FROM(T3) USING(COP4)                                               00034**2
*                                                                       00035**2
* PRINT A LISTING OF THE INVALID RECORDS.                               00036**2
COPY FROM(BADRECS) TO(SHOWBAD) USING(COP5)                              00037**2
*                                                                       00038**2
* CONVERT THE OUTPUT FILE TO FORMAT VB, STRIP OFF TRAILING BLANKS,      00039   
* ADD THE TRAILER RECORD, AND REMOVE THE AGENCY CODE WE ADDED EARLIER   00040   
* (TALEND DOES NOT NEED IT). THIS IS THE FILE FOR TALEND.               00041   
* ALSO PRODUCE A FILE WITH A HEADER ROW FOR IMPORTING INTO EXCEL,       00042   
* SHOULD WE NEED THIS FOR DEBUGGING.                                    00043   
COPY FROM(T4) USING(COP3)                                               00044**2
*                                                                       00045**2
* COUNT THE RECORDS IN THE ERROR FILE AND SET RC=8 IF NOT EMPTY.        00046**2
COUNT FROM(BADRECS) NOTEMPTY RC8                                        00047**2
*                                                                       00048**2
*                                                                       00049   
