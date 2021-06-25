*          DATA SET DMEXTFWR   AT LEVEL 110 AS OF 22/04/94                      
*********************** THIS VERSION BAD  FWR 9/2/98                            
*PHASE DMEXTFWR,*,NOAUTO                                                00001*71
*INCLUDE CARDS                                                          00002   
*INCLUDE PRINT110                                                       00003*21
*INCLUDE PRINT                                                          00004*21
*INCLUDE HEXOUT                                                         00005*40
*******E DMDADDS         USE LOADABLE DATAMANAGER   1/14/98 FWR         00006   
*INCLUDE DMDMGRL                                                        00006   
       ++INCLUDE  DMGREQUS                                                      
         TITLE 'DMEXTEND - EXTEND AND ERASE A DIRECT ACCESS FILE'       00007   
         PRINT NOGEN                                                    00008109
DMEXTEND CSECT                                                          00009   
         NBASE 0,DMEXTEND,WORK=A(DMEXWORK)                              00010   
         LA    R8,2048(RB)                                              00011*98
         LA    R8,2048(R8)                                              00012*98
         USING DMEXTEND+4096,R8                                         00013*98
*                                                                       00014*21
         L     RA,=V(CPRINT)                                            00015*21
         USING DPRINT,RA                                                00016*21
*                                                                       00017*21
         MVC   TITLE(8),=CL8'DMEXTEND'                                  00018*23
*                                                                       00019*17
         BAS   RE,DFPLVL                                                00020107
*                                                                       00021107
DME10    DS    0H                                                       00022*16
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                             00023*16
         MVC   P(80),CARD                                               00024*21
         GOTO1 =V(PRINTER)                                              00025*21
         CLI   CARD,C'*'           TEST FOR A COMMENT                   00026*21
         BE    DME10                                                    00027*21
         CLC   =C'FIND',CARD                                            00028*60
         BE    DMEFIND                                                  00029*60
         CLC   =C'DISPLAY',CARD                                         00030*61
         BE    DMEFIND                                                  00031*61
         CLC   =C'FIXOLD',CARD                                          00032*61
         BE    DMEFIX                                                   00033*61
         CLC   =C'FIXNEW',CARD                                          00034*67
         BE    DMEFIX                                                   00035*67
         SPACE 1                                                        00036*17
DME12    LA    RE,CARD             VALIDATE ACTION                      00037*16
         LA    R0,8                MAX ACTION LEN IS 8                  00038*16
DME14    CLI   0(RE),C' '                                               00039*16
         BE    DME16                                                    00040*16
         CLI   0(RE),C'='                                               00041   
         BE    DME16                                                    00042*16
         LA    RE,1(RE)                                                 00043   
         BCT   R0,DME14                                                 00044*16
         B     ACTNERR                                                  00045*19
*                                                                       00046   
DME16    ST    RE,FULL             SAVE TERMINATOR ADDRESS              00047*16
*                                                                       00048*16
         LA    R4,ACTTAB                                                00049*16
         LA    R0,CARD                                                  00050*16
         SR    RE,R0               GIVES LENGTH                         00051*16
         STC   RE,FULL             AND DATA LENGTH                      00052*16
*                                                                       00053*16
DME18    ZIC   RE,FULL             RESTORE DATA LENGTH                  00054*16
         CLM   RE,1,0(R4)          TEST EQUAL NUMBER OF CHARACTERS      00055*16
         BNE   DME20                                                    00056*16
         BCTR  RE,0                                                     00057*16
         EX    RE,*+8                                                   00058*16
         B     *+10                                                     00059*16
         CLC   CARD(0),2(R4) *EXECUTED*                                 00060*16
         BE    DME30                                                    00061*16
*                                                                       00062*16
DME20    LA    R4,L'ACTTAB(R4)     POINT TO NEXT ENTRY                  00063*16
         CLI   0(R4),X'FF'         TEST EOL                             00064*16
         BNE   DME18                                                    00065*16
         B     ACTNERR                                                  00066*16
         EJECT                                                          00067*21
****************************************************************        00068*16
* TABLE ENTRIES ARE AS FOLLOWS -                               *        00069*16
* LENGTH FOR ACTION NAME COMPARE                               *        00070*16
* ACTION DEFINITION BITS  ====  X'80' = NO OLD FILE            *        00071*16
*                         ====  X'40' = EXTEND ON SAME VOLUME  *        00072*16
* ACTION NAME IN CHARACTERS                                    *        00073*16
         SPACE 1                                               *        00074*16
ACTTAB   DS    0XL10                                           *        00075*16
         DC    AL1(6),X'80'                                    *        00076*16
ACT1NAME DC    CL8'DEFINE'                                     *        00077*16
         DC    AL1(6),X'C0'                                    *        00078*16
ACT2NAME DC    CL8'DEFVOL'                                     *        00079*16
         DC    AL1(6),X'00'                                    *        00080*16
ACT3NAME DC    CL8'EXTEND'                                     *        00081*16
         DC    AL1(6),X'40'                                    *        00082*16
ACT4NAME DC    CL8'EXTVOL'                                     *        00083*16
         DC    X'FF'                                           *        00084*16
****************************************************************        00085*16
         EJECT                                                          00086*16
DME30    DS    0H                                                       00087*16
         MVI   NEWXTNTS,1          SET DEFAULT NUMBER                   00088*16
         L     RE,FULL                                                  00089*16
         CLI   0(RE),C'='          TEST XTNTS SPECIFIED                 00090*16
         BNE   DME32               NO                                   00091*16
         MVC   NEWXTNTS,1(RE)                                           00092*16
         NI    NEWXTNTS,X'0F'      MAKE HEX                             00093*16
*                                                                       00094*16
         CLI   1(RE),C'1'                                               00095*16
         BL    XTNTERR                                                  00096*16
         CLI   1(RE),C'9'                                               00097*16
         BH    XTNTERR                                                  00098*16
*                                                                       00099*16
DME32    DS    0H                                                       00100*17
         GOTO1 =V(CARDS),DMCB,CARD,=C'RE00'                             00101*17
         MVC   P(80),CARD                                               00102*23
         GOTO1 =V(PRINTER)                                              00103*23
         CLI   CARD,C'*'                                                00104*23
         BE    DME32                                                    00105*23
*                                                                       00106*23
         CLC   CARD(9),=C'ERASE=ALL'                                    00107*19
         BE    DME32X                                                   00108*17
         CLC   CARD(9),=C'ERASE=NEW'                                    00109*17
         BE    DME32X                                                   00110*17
         CLC   CARD(8),=C'ERASE=NO'                                     00111*17
         BE    DME32X                                                   00112*17
         B     ERSERR                                                   00113*17
*                                                                       00114*17
DME32X   LA    R3,OLDFILE          POINT TO OLD FILE                    00115*17
         USING DTFPHD,R3                                                00116   
         XC    P1(24),P1           SET DADDS PARAM LIST                 00117   
         L     RE,=A(DMEXBUFF)                                          00118   
         ST    RE,P2                                                    00119   
         MVI   P2,X'FF'                                                 00120   
         ST    R3,P4                                                    00121   
         LA    RE,P6                                                    00122   
         ST    RE,P5                                                    00123   
         EJECT                                                          00124*26
DME34    TM    1(R4),X'80'         TEST OLD EXTENT EXISTS               00125*16
         BO    DME40               NO                                   00126*20
         SPACE 1                                                        00127*26
* PROCESS OLD EXTENT *                                                  00128*16
         SPACE 1                                                        00129*16
         GOTO1 =V(DADDS),P1,V(DAOPEN)                                   00130   
         SR    R1,R1                                                    00131   
         LA    RE,DMTX                                                  00132   
*                                                                       00133*16
DME36    CLI   0(RE),X'FF'         COUNT NUMBER OF EXISTING EXTENTS     00134*16
         BE    DME38                                                    00135*16
         LA    R1,1(R1)                                                 00136   
         LA    RE,14(RE)                                                00137   
         B     DME36                                                    00138*16
*                                                                       00139*16
DME38    STC   R1,OLDXTNTS                                              00140*16
         GOTO1 =V(DADDS),P1,V(DACLOSE)                                  00141   
         SPACE 2                                                        00142*26
DME40    L     R5,AJFCBA                                                00143*77
         ST    R5,EXITLST                                               00144*71
         MVI   EXITLST,HIGHBIT+JFCBEXIT                                 00145*71
         LA    R2,NEWFILE                                               00146*71
         PRINT GEN                                                      00147*72
         RDJFCB NEWFILE            GET JFCB DATA BEFORE OPEN (I.E. JCL) 00148*72
         LTR   RF,RF                                                    00149*72
         BZ    *+6                                                      00150*72
         DC    H'0'                                                     00151*72
         PRINT NOGEN                                                    00152*72
         GOTO1 =V(PRINTER)                                              00153109
         MVC   P(L'JFCBMSG1),JFCBMSG1                                   00154109
         GOTO1 =V(PRINTER)                                              00155109
         BAS   RE,DISPJFCB         GO DISPLAY THE JFCB                  00156110
         GOTO1 =V(PRINTER)         BLANK LINE                           00157*91
*                                                                       00158107
         OPEN  ((2),EXTEND)        OPEN NEW FILE AS MVS SEQUENTIAL      00159*71
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                  00160*48
         BO    *+6                                                      00161*48
         DC    H'0'                                                     00162*48
         CLI   SMSFLAG,C'A'        IS SMS ACTIVE ON THIS SYSTEM?        00163107
         BNE   DME44               NO, SKIP GETTING INFO ON THE FILE    00164107
         USING INFMJFCB,R5                                              00165109
         LA    R6,JFCBDSNM         GET ADDRESS OF DATASET NAME          00166109
         BAS   RE,DSNSMS                                                00167107
*                                                                       00169   
DME44    TM    1(R4),X'40'         ARE NEW EXTENTS ON SAME VOL          00170*16
         BO    DME50               YES                                  00171*16
         SPACE 1                                                        00172*16
* NEW EXTENTS ON NEW VOLUME - WRITE ONE RECORD FOR EACH EXTENT *        00173*16
         SPACE 1                                                        00174*16
         ZIC   R5,NEWXTNTS                                              00175*16
         TM    1(R4),X'80'         TEST OLD EXTENT EXISTS               00176*35
         BO    DME47               NO - SKIP FIRST FEOV                 00177*35
*                                                                       00178*35
DME46    FEOV  (2)                 FORCE VOLUME SWITCH                  00179*44
         BAL   RE,PRTDEB                                                00180*44
*                                                                       00181*35
DME47    L     RF,ANEWREC                                               00182*79
         PRINT GEN                                                      00183*79
         PUT   (2),(15)                                                 00184*79
         PRINT NOGEN                                                    00185*79
*                                                                       00186*16
         L     RE,NEWFILE+44       GET DEB ADDRESS                      00187*36
         SR    R0,R0                                                    00188*36
         ICM   R0,1,16(RE)         NUMBER OF EXTENTS IN DEB             00189*36
         BNZ   *+6                                                      00190*33
         DC    H'0'                                                     00191*33
         LA    RF,32(RE)           POINT TO FIRST EXTENT                00192*36
         B     DME47B                                                   00193*36
*                                                                       00194*33
DME47A   LA    RF,16(RF)           POINT TO NEXT                        00195*36
*                                                                       00196*33
DME47B   BCT   R0,DME47A                                                00197*36
         SPACE 1                                                        00198*33
* FORCE END OF FILE TO END CCHH ON LAST EXTENT *                        00199*33
         SPACE 1                                                        00200*33
         MVC   NEWFILE+8(4),10(RF) MOVE END CCHH TO DCB                 00201*36
         MVI   NEWFILE+12,99       SET HIGH RECORD NUM                  00202*33
         MVI   NEWFILE+18,X'00'    SET TRACK BALANCE TO 0               00203*33
         MVI   NEWFILE+19,X'00'    SET TRACK BALANCE TO 0               00204*33
*                                                                       00205*36
         BCT   R5,DME46                                                 00206*36
*                                                                       00207*42
         CLOSE ((2),)                                                   00208*33
*                                                                       00209*44
         BAL   RE,PRTDEB                                                00210*44
*                                                                       00211*44
         B     DME60                                                    00212*16
JFCBMSG1 DC    C'INFO FROM NEWFILE DD (BEFORE OPEN) FOLLOWS:'           00213108
JFCBMSG2 DC    C'INFO FROM NEWFILE DD (AFTER OPEN) FOLLOWS:'            00213108
         EJECT                                                          00214*16
* NEW EXTENTS ON SAME VOLUME *                                          00215*39
         SPACE 1                                                        00216*16
DME50    TM    JFCBIND2,JFCMOD     DOES JCL INDICATE DISP=MOD?          00364109
         BO    DME51               YES, GOOD                            00365109
         ABEND 21                  NO, TOO BAD WE'LL GET 614            00278*84
*                                  IF WE CONTINUE                               
         DROP  R5                  DONE WITH JFCB                       00168*76
DME51    ZIC   R5,NEWXTNTS                                              00217*16
         B     DME52PUT            SKIP OPEN FIRST TIME ONLY            00218*39
*                                                                       00219*16
DME52    OPEN  ((2),OUTPUT)                                             00220*39
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                  00221*48
         BO    *+6                                                      00222*48
         DC    H'0'                                                     00223*48
*                                                                       00224*39
DME52PUT L     RF,ANEWREC          WRITE A SINGLE RECORD                00225*79
         PUT   (2),(15)            WRITE A SINGLE RECORD                00226*79
*                                                                       00227*16
         CLOSE ((2),)                                                   00228*39
*                                                                       00229*44
         BAL   RE,PRTDEB                                                00230*44
*                                                                       00231*59
         OPEN  ((2),OUTPUT)        REOPEN DCB                           00232*59
         TM    48(R2),X'10'        TEST OPEN SUCCEEDED                  00233*59
         BO    *+6                                                      00234*59
         DC    H'0'                                                     00235*59
         MVC   8(4,R2),LASTXTNT+10 MOVE LAST CCHH TO DCB                00236*59
         MVI   12(R2),99           FORCE HIGH REC                       00237*59
         XC    18(2,R2),18(R2)     CLEAR TRACK BALANCE                  00238*59
         CLOSE ((2),)                                                   00239*59
*                                                                       00240*39
         BCT   R5,DME52                                                 00241*38
         EJECT                                                          00242*39
* ERASE FILE IF NECESSARY *                                             00243*17
         SPACE 1                                                        00244*17
DME60    CLC   CARD+6(2),=C'NO'    TEST DO NOT ERASE                    00245*17
         BE    DMEOJ                                                    00246   
         SPACE 1                                                        00247**2
* OPEN THE NEW FILE AS A DDS FILE *                                     00248**2
         SPACE 1                                                        00249**2
         L     R3,AERSFILE                                              00250*81
         ST    R3,P4                  SET DTF ADDRESS                   00251*16
         MVC   22(7,R3),=C'NEWFILE'   OVERRIDE FILE NAME                00252*16
         GOTO1 =V(DADDS),P1,V(DAOPEN)                                   00253**2
         SPACE 1                                                        00254**2
* CALL DADDS TO ERASE *                                                 00255**2
         SPACE 1                                                        00256**2
         XC    P6,P6               SET TO ERASE WHOLE FILE              00257*17
         CLC   CARD+6(3),=C'NEW'   TEST ERASE NEW EXTENT(S) ONLY        00258*17
         BNE   DME64               NO - GO ERASE ALL                    00259*21
* NEED TO CALL DADDS TO FORCE IT TO DO HITRK CALCULATION                        
         MVC   P6(4),=X'00010100'                                               
         GOTO1 =V(DADDS),P1,A(DATRNS)                                           
*                                                                       00260**2
         ZIC   R5,OLDXTNTS         GET NUMBER OF OLD EXTENTS            00261*17
         LA    RE,DMTX-14                                               00262*17
*NOP     MVC   P+1(31),=C'ERASE=NEW, NO OF OLD EXTENTS = '              00275*84
         ST    R5,P+32                                                  00276*85
         GOTO1 =V(PRINTER)                                              00277*84
*                                                                       00263*17
DME62    LA    RE,14(RE)                                                00264*17
         MVC   P6(2),12(RE)        MOVE HIGH TT                         00265*17
         BCT   R5,DME62                                                 00266*17
*                                                                       00267**2
DME64    GOTO1 =V(DADDS),P1,V(WTERASE)                                  00268*17
*                                                                       00269**9
         GOTO1 (RF),(R1),V(DACLOSE)                                     00270**9
         SPACE 2                                                        00271   
DMEOJ    XBASE                                                          00272   
LINKERR  EQU   *                                                        00273*84
         GOTO1 =V(HEXOUT),DMCB,SMSLIST,WORK,16,=C'MIX'                  00274*84
         MVC   P+1(08),WORK                                             00275*84
         MVC   P+10(08),WORK+8                                          00276*85
         GOTO1 =V(PRINTER)                                              00277*84
         ABEND 20                                                       00278*84
         EJECT                                                          00279*60
DMEFIND  L     R5,AJFCBA                                                00143*77
         ST    R5,EXITLST                                               00144*71
         MVI   EXITLST,HIGHBIT+JFCBEXIT                                 00145*71
         LA    R2,NEWFILE                                               00146*71
*                                                                       00158107
*        OPEN  ((2),EXTEND)        OPEN NEW FILE AS MVS SEQUENTIAL      00159*71
*        TM    48(R2),X'10'        TEST OPEN SUCCEEDED                  00160*48
*        BO    *+6                                                      00161*48
*        DC    H'0'                                                     00162*48
         PRINT GEN                                                      00147*72
         RDJFCB NEWFILE            GET JFCB DATA AFTER OPEN             00148*72
         LTR   RF,RF                                                    00149*72
         BZ    *+6                                                      00150*72
         DC    H'0'                                                     00151*72
         PRINT NOGEN                                                    00152*72
         GOTO1 =V(PRINTER)                                              00153109
         MVC   P(L'JFCBMSG2),JFCBMSG2                                   00154109
         GOTO1 =V(PRINTER)                                              00155109
         BAS   RE,DISPJFCB         GO DISPLAY THE JFCB                  00156110
         GOTO1 =V(PRINTER)         BLANK LINE                           00157*91
         CLI   SMSFLAG,C'A'        IS SMS ACTIVE ON THIS SYSTEM?        00163107
         BNE   DME44               NO, SKIP GETTING INFO ON THE FILE    00164107
         USING INFMJFCB,R5                                              00165109
         LA    R6,JFCBDSNM         GET ADDRESS OF DATASET NAME          00166109
         BAS   RE,DSNSMS                                                00167107
         DROP  R5                  DONE WITH JFCB                       00168*76
         BAL   RE,PRTDEB                                                00280*60
         B     DMEOJ                                                    00281*60
         LTORG                                                          00282*76
         EJECT                                                          00283*61
* THIS LOGIC TO SET HIGH WATER MARK FOR EXISTING FILES *                00284*61
         SPACE 1                                                        00285*61
DMEFIX   ABEND  21               NO-LONGER SUPPORTED                    00286*96
         EJECT                                                          00287107
**  SUBROUTINE TO DETERMINE THE DFP LEVEL OF THE SYSTEM AND IF          00288107
**  THE SMS SUBSYSTEM IS ACTIVE                                         00289107
DFPLVL   ST    RE,RETSAVE                                               00290107
         LA    R6,DFP                                                   00291107
         ST    R6,LEVEL                                                 00292107
         LINK  EP=IGWASYS,MF=(E,SYSLIST)                                00293107
         XC    RETCDE(8),RETCDE                                         00294107
         BNZ   LINKERR                                                  00295107
         L     R7,SPECIND                                               00296107
         CH    R7,=H'1'            ARE WE ON A HIGHER LEVEL?            00297107
         BNE   DISPLVL             NO - SKIP GETTING IT'S LEVEL         00298107
         LA    R6,DFSMS                                                 00299107
         ST    R6,LEVEL                                                 00300107
         LINK  EP=IGWASYS,MF=(E,SYSLIST)                                00301107
         XC    RETCDE(8),RETCDE                                         00302107
         BNZ   LINKERR                                                  00303107
         MVC   PRODNAME(09),=C'DFSMS/MVS'                               00304107
*                                                                       00305107
DISPLVL  MVC   P+2(44),=C'THE DATA MANAGEMENT LEVEL OF THIS SYSTEM IS:' 00306107
         MVC   P+47(10),PRODNAME                                        00307107
         GOTO1 =V(HEXOUT),DMCB,SYSLVL+3,P+58,1,=C'MIX'                  00308107
         GOTO1 =V(HEXOUT),DMCB,SYSLVL+7,P+60,1,=C'MIX'                  00309107
         GOTO1 =V(HEXOUT),DMCB,SYSLVL+11,P+62,1,=C'MIX'                 00310107
         GOTO1 =V(PRINTER)                                              00311107
         GOTO1 =V(PRINTER)         BLANK LINE                           00312108
*                                                                       00313107
DISPSMS  L     R7,SYSATTR                                               00314107
         MVC   P+2(06),=C'SMS IS'                                       00315107
         CH    R7,=H'1'            IS SMS ACTIVE?                       00316107
         BNE   NOSMS                                                    00317107
         MVC   P+9(06),=C'ACTIVE'                                       00318107
         MVI   SMSFLAG,C'A'                                             00319107
         B     PRTSMS                                                   00320107
NOSMS    MVC   P+9(10),=C'NOT ACTIVE'                                   00321107
         MVI   SMSFLAG,C'N'                                             00322107
PRTSMS   GOTO1 =V(PRINTER)                                              00323107
         L     RE,RETSAVE                                               00324107
         BR    RE                                                       00325107
         EJECT                                                          00326*40
**  SUBROUTINE TO GET SMS INFO FOR A SPECIFIC DATASET NAME, THE ADDRESS 00327107
**  OF WHICH IS PASSED IN REG 6                                         00328107
DSNSMS   ST    RE,RETSAVE                                               00329107
         ST    R6,DSNADDR                                               00330107
         LINK  EP=IGWASMS,MF=(E,SMSLIST)                                00331107
         LTR   RF,RF                                                    00332107
         BZ    DSNSMS1                                                          
         LA    RE,12                                                            
         CR    RF,RE                                                            
         BNE   LINKERR                                                  00333107
         MVC   P(L'SMSMSG1),SMSMSG1                                     00154109
         GOTO1 =V(PRINTER)                                              00335107
         B     DSNSMSE                                                          
DSNSMS1  MVC   P(L'SMSMSG2),SMSMSG2                                     00154109
         GOTO1 =V(PRINTER)                                              00335107
         MVC   P+3(16),=C'STORAGE CLASS =>'                             00336107
         MVC   P+20(30),SMSDATA                                         00337107
         GOTO1 =V(PRINTER)                                              00338107
         MVC   P(19),=C'MANAGEMENT CLASS =>'                            00339107
         MVC   P+20(30),SMSDATA+30                                      00340107
         GOTO1 =V(PRINTER)                                              00341107
         MVC   P+6(13),=C'DATA CLASS =>'                                00342107
         MVC   P+20(30),SMSDATA+60                                      00343107
         GOTO1 =V(PRINTER)         BLANK LINE                           00344107
DSNSMSE  L     RE,RETSAVE                                               00345107
         BR    RE                                                       00346107
         EJECT                                                          00347107
**  SUBROUTINE TO DISPLAY PERTINENT FIELDS FROM THE JFCB                00348109
**  ADDRESS OF JFCB IS PASSED IN R5                                     00349109
DISPJFCB ST    RE,RETSAVE                                               00350109
         USING INFMJFCB,R5                                              00351109
         IC    R6,JFCBNVOL         GET NUMBER OF VOLUMES                00352109
***                                ONLY 5 VOL SERS CAN BE IN JFCB       00353109
***                                REMAINDER MUST BE IN EXTENSIONS      00354109
         MVC   P+2(15),=C'DATASET NAME =>'                              00355109
         MVC   P+18(44),JFCBDSNM    GET DATASET NAME                    00356109
         GOTO1 =V(PRINTER)                                              00357109
         MVC   P+3(14),=C'STATUS BITS =>'                               00358109
         GOTO1 =V(HEXOUT),DMCB,JFCBIND2,P+18,1,=C'MIX'                  00359109
         TM    JFCBIND2,JFCNEW                                          00360109
         BNO   TESTMOD                                                  00361109
         MVC   P+25(3),=C'NEW'                                          00362109
         B     PRTSTAT                                                  00363109
TESTMOD  TM    JFCBIND2,JFCMOD                                          00364109
         BNO   TESTOLD                                                  00365109
         MVC   P+25(3),=C'MOD'                                          00366109
         B     PRTSTAT                                                  00367109
TESTOLD  TM    JFCBIND2,JFCOLD                                          00368109
         MVC   P+25(3),=C'OLD'              WHAT ELSE COULD IT BE?      00369109
PRTSTAT  GOTO1 =V(PRINTER)                                              00370109
*                                                                       00371109
         MVC   P+5(12),=C'VOLSER(S) =>'                                 00372109
         MVC   P+18(30),JFCBVOLS   5 VOL SERS                           00373109
         GOTO1 =V(PRINTER)                                              00374109
         MVC   P+5(12),=C'VOL COUNT =>'                                 00375109
         GOTO1 =V(HEXOUT),DMCB,JFCBVLCT,P+18,1,=C'MIX'                  00376109
         GOTO1 =V(PRINTER)                                              00377109
         MVC   P+5(12),=C'SPACE VAL =>'                                 00378109
         GOTO1 =V(HEXOUT),DMCB,JFCBPQTY,P+18,3,=C'MIX'                  00379109
         MVI   P+24,C','                                                00380109
         GOTO1 =V(HEXOUT),DMCB,JFCBSQTY,P+26,3,=C'MIX'                  00381109
         MVC   P+35(13),=C'VALUES IN HEX'                               00382109
         GOTO1 =V(PRINTER)                                              00383109
         L     RE,RETSAVE                                               00384109
         BR    RE                                                       00385109
         DROP  R5                  DONE WITH JFCB                       00386109
         EJECT                                                          00387109
* SUBROUTINE TO PRINT DEB *                                             00388*58
         SPACE 1                                                        00389*40
PRTDEB   NTR1                                                           00390*40
*                                                                       00391*40
* FORMAT BASIC SECTION *                                                00392*40
*                                                                       00393*40
         LA    R3,DEBFILE                                               00394*58
         MVC   22(7,R3),=C'NEWFILE'  OVERRIDE FILE NAME                 00395*58
         XC    WORK(16),WORK         USE WORK SO PARAM LIST UNTOUCHED   00396*57
         GOTO1 =V(DADDS),WORK,V(DAOPEN),,,(R3)                          00397*58
*                                                                       00398*55
ENDIN    XC    DSPL,DSPL                                                00399*55
         L     R4,DTFADCB          GET DCB ADDRESS                      00400*58
         L     R4,44(R4)           GET DEB ADDRESS                      00401*58
         ZIC   RE,16(R4)           GET NUMBER OF EXTENTS                00402*67
         SLL   RE,4                X 16                                 00403*67
         LA    RE,32(RE)                                                00404*67
         BCTR  RE,0                                                     00405*67
         EX    RE,*+8                                                   00406*67
         B     *+10                                                     00407*67
         MVC   SAVEDEB(0),0(R4)    SAVE ENTIRE DEB                      00408*67
*                                                                       00409*67
         LA    R0,2                                                     00410*46
*                                                                       00411*40
         GOTO1 =V(PRINTER)         SKIP A LINE                          00412*75
         MVC   P(26),=C'DEB AS USED BY DATAMANAGER'                     00413*75
         GOTO1 =V(PRINTER)         SKIP A LINE                          00414*75
PRTDEB2  BAL   RE,HEXPRT           PRINT BEGINNING PART OF DEB          00415*73
         LA    R4,16(R4)                                                00416*58
         LH    RE,DSPL                                                  00417*40
         LA    RE,16(RE)                                                00418*40
         STH   RE,DSPL                                                  00419*40
         BCT   R0,PRTDEB2                                               00420*40
         GOTO1 =V(PRINTER)         SKIP A LINE                          00421*73
         MVC   P+10(L'DEBMSG1),DEBMSG1                                  00154109
         GOTO1 =V(PRINTER)         PRINT A LINE                         00423*90
         MVC   P+10(L'DEBMSG2),DEBMSG2                                  00154109
         GOTO1 =V(PRINTER)         PRINT A LINE                         00425*82
*                                                                       00426*40
         L     R4,DTFADCB          GET DCB ADDRESS                      00427*58
         L     RE,44(R4)           GET DEB ADDRESS                      00428*58
         USING DEBBASIC,R4                                              00429*76
         ZIC   R0,16(RE)           GET NUMBER OF EXTENTS                00430*40
         LTR   R0,R0                                                    00431*58
         BP    *+6                                                      00432*58
         DC    H'0'                                                     00433*58
         SR    R1,R1               CLEAR COUNTER                        00434102
         LA    R4,32(RE)           POINT TO FIRST EXTENT                00435*58
         USING DEBDASD,R4          MAP THE EXTENT AREA                  00436*76
*        DEBSTRCC                  STARTING CYL  (H)                    00437*78
*        DEBSTRHH                                                       00438*76
*        DEBENDCC                  ENDING CYL (H)                       00439*78
*        DEBENDHH                                                       00440*76
*        DEBNMTRK                  H   NUMBER OF TRACKS ALLOCATED       00441*78
*                                                                       00442*78
PRTDEB4  SR    R9,R9                                                    00443*83
         ICM   R9,7,DEBUCBA        GET THE UCB ADDRESS                  00444*88
**                 UCBNAME         DEVICE NUMBER                        00445*80
         BAL   RE,HEXPRTX          PRINT AN EXTENT                      00446*83
         MVC   LASTXTNT,0(R4)      SAVE FOR POSTERITY                   00447*59
         AH    R1,DEBNMTRK         ADD UP THE NUMBER OF TRACKS          00448102
*                                                                       00449*40
         LA    R4,16(R4)           NEXT EXTENT                          00450*58
         LH    RE,DSPL                                                  00451*40
         LA    RE,16(RE)                                                00452*40
         STH   RE,DSPL                                                  00453*40
         BCT   R0,PRTDEB4                                               00454*40
         DROP  R4                                                       00455*83
         ST    R1,TRKSUSED                                              00456102
         EDIT  (R1),(10,SUMTRK),0,COMMAS=YES                                    
         MVC   P+39(7),=C'======='                                      00457106
         GOTO1 =V(PRINTER)                                              00458104
*NOP     GOTO1 =V(HEXOUT),DMCB,TRKSUSED,P+38,4,=C'MIX'                  00459106
         MVC   P+36(10),SUMTRK     DECIMAL TOTAL                                
         GOTO1 =V(PRINTER)         OUTPUT TOTAL TRACKS                  00460102
         C     R1,LIMIT                                                 00461103
         BNH   HEXDONE                                                  00462102
         MVC   P+5(35),=C'****** 65K TRACK LIMIT EXCEEDED ***'          00463102
         GOTO1 =V(PRINTER)         OUTPUT TOTAL TRACKS                  00464102
*                                                                       00465*50
HEXDONE  LA    R3,DEBFILE                                               00466102
         XC    WORK(16),WORK       USE WORK SO PARAM LIST UNTOUCHED     00467*56
         GOTO1 =V(DADDS),WORK,V(DACLOSE),,,(R3)                         00468*58
*                                                                       00469*46
         GOTO1 =V(PRINTER)         SKIP A LINE                          00470*46
         B     EXIT                                                     00471*40
TRKSUSED DS    F                                                        00473102
LIMIT    DC    F'65520'            MAX SIZE OF HALF WORD W/O SIGN       00472103
*                                  NOW MINUS 1 CYLINDER                         
SUMTRK   DS    CL10                                                             
DEBMSG1  DC    C'UCB        START    END         TRK '                  00422104
DEBMSG2  DC    C'ADDR       CC/HH    CC/HH      COUNT'                  00424104
SMSMSG1  DC    C'DATASET NOT FOUND IN CATALOG; NOT SMS MANAGED'         00334109
SMSMSG2  DC    C'ASSOCIATED SMS INFO FOR DSN:'                          00334109
*                                                                       00474*40
HEXPRT   NTR1                                                           00475*40
         MVI   P,C'+'                                                   00476*40
         GOTO1 =V(HEXOUT),DMCB,DSPL+1,P+1,1,=C'MIX'                     00477*43
         GOTO1 =V(HEXOUT),DMCB,(R4),WORK,16,=C'MIX'                     00478*58
         MVC   P+4(8),WORK                                              00479*47
         MVC   P+13(8),WORK+8                                           00480*47
         MVC   P+22(8),WORK+16                                          00481*47
         MVC   P+31(8),WORK+24                                          00482*47
         GOTO1 =V(PRINTER)                                              00483*40
EXIT     XIT1                                                           00484*40
HEXPRTX  NTR1                                                           00485*80
         MVI   P,C'+'                                                   00486*80
         GOTO1 =V(HEXOUT),DMCB,DSPL+1,P+1,1,=C'MIX'                     00487*80
         GOTO1 =V(HEXOUT),DMCB,(R4),WORK,16,=C'MIX'                     00488*80
         MVC   P+4(2),WORK                                              00489*80
         MVC   P+8(7),WORK+2           UCB ADDRESS                      00490*80
         MVC   P+16(4),WORK+8          ZERO AREA                        00491*80
         MVC   P+21(8),WORK+12         STARTING CC/HH                   00492*80
         MVC   P+30(8),WORK+20         ENDING CC/HH                     00493*80
         MVC   P+42(4),WORK+28         TOTAL TRACKS IN EXTENT(HEX)      00494104
         USING UCBOB,R9                                                 00495*83
         PRINT GEN                                                      00496109
         UCBDEVN DEVN=CHAN                                              00497101
         PRINT NOGEN                                                    00498109
         MVC   P+50(4),CHAN            DEVICE NUMBER                    00499*90
         MVC   P+56(6),UCBVOLI         VOL SER                          00500*89
         GOTO1 =V(PRINTER)                                              00501*80
         XIT1                                                           00502*81
         DROP  R9                                                       00503*83
*                                                                       00504*40
DSPL     DC    H'0'                                                     00505*41
CHAN     DS    F                                                        00506*90
         EJECT                                                          00507*17
* ERROR ROUTINES *                                                      00508*17
         SPACE 1                                                        00509*17
ACTNERR  MVC   ACTNMSG+L'ACTNMSG(12),CARD                               00510*17
         LA    RE,ACTNMSGL                                              00511*21
         LA    RF,ACTNMSG                                               00512*21
         B     ERRX                                                     00513*17
*                                                                       00514*17
XTNTERR  MVC   XTNTMSG+L'XTNTMSG(12),CARD                               00515*17
         LA    RE,XTNTMSGL                                              00516*21
         LA    RF,XTNTMSG                                               00517*21
         B     ERRX                                                     00518*17
*                                                                       00519*17
ERSERR   MVC   ERSMSG+L'ERSMSG(12),CARD                                 00520*17
         LA    RE,ERSMSGL                                               00521*21
         LA    RF,ERSMSG                                                00522*21
*                                                                       00523*17
ERRX     BCTR  RE,0                ADJUST FOR EX                        00524*21
         EX    RE,*+8                                                   00525*21
         B     *+10                                                     00526*21
         MVC   P(0),0(RF)                                               00527*21
         GOTO1 =V(PRINTER)                                              00528*21
         B     DMEOJ                                                    00529*17
         SPACE 2                                                        00530*17
ACTNMSG  DC    C'** ERROR ** INVALID ACTION  '                          00531*21
         DC    CL12' '                                                  00532*17
ACTNMSGL EQU   *-ACTNMSG                                                00533*17
*                                                                       00534*17
XTNTMSG  DC    C'** ERROR ** INVALID NUMBER OF EXTENTS'                 00535*21
         DC    CL12' '                                                  00536*17
XTNTMSGL EQU   *-XTNTMSG                                                00537*17
*                                                                       00538*17
ERSMSG   DC    C'** ERROR ** INVALID ERASE OPTION -'                    00539*21
         DC    CL12' '                                                  00540*17
ERSMSGL  EQU   *-ERSMSG                                                 00541*17
         EJECT                                                          00542   
         DS    0D                                                       00543   
         DC    C'*OLDFIL*'                                              00544   
OLDFILE  DMDA  DSKXTNT=16                                               00545   
         SPACE 2                                                        00546**6
         DS    0D                                                       00547   
         DC    C'*NEWFIL*'                                              00548   
NEWFILE  DCB   DDNAME=NEWFILE,DSORG=PS,MACRF=(PM),EXLST=(EXITLST),     X00549*71
               RECFM=FU,BLKSIZE=9500                                    00550*17
EXITLST  DS    2F                                                       00551*71
AJFCBA   DC    A(JFCBAREA)                                              00552*77
ANEWREC  DC    A(NEWREC)                                                00553*79
AERSFILE DC    A(ERSFILE)                                               00554*81
SYSLIST  DC    A(RETCDE)                                                00555*94
         DC    A(REASCDE)                                               00556*75
         DC    A(LEVEL)                                                 00557*94
         DC    A(SYSLVL)                                                00558*94
         DC    A(SYSATTR)                                               00559*94
LEVEL    DS    F                                                        00560*94
DFSMS    EQU   2                   LEVEL INDICATOR FOR DFSMS PRODUCT    00561104
DFP      EQU   1                                                        00562104
SYSLVL   DS    4F                                                       00563*97
         ORG   SYSLVL                                                   00564100
SMSVER   DS    1F                                                       00565100
SMSREL   DS    1F                                                       00566100
SMSMOD   DS    1F                                                       00567100
SPECIND  DS    1F                                                       00568100
*                                                                       00569100
SYSATTR  DS    4F                                                       00570100
PRODNAME DC    CL10'MVS/DFP'                                            00571104
*                                                                       00572*94
SMSLIST  DC    A(RETCDE)                                                00573*94
         DC    A(REASCDE)                                               00574*94
         DC    A(PROB1)                                                 00575*94
         DC    A(DSNLEN)                                                00576*94
DSNADDR  DS    A                                                        00577*75
         DC    A(SMSDATA)                                               00578*75
         DC    A(DSTYPE)                                                00579*75
RETCDE   DC    F'0'                                                     00580*75
REASCDE  DC    F'0'                                                     00581*75
PROB1    DC    2F'0'                                                    00582*75
DSNLEN   DC    F'44'                                                    00583*75
DSTYPE   DC    F'0'                                                     00584*75
SMSDATA  DC    3CL30' '                                                 00585*75
         DC    C'*JFCB***'                                              00586*71
HIGHBIT  EQU   X'80'                                                    00587*71
JFCBEXIT EQU   X'07'               DCB EXITLIST CODE VALUE (ORIGINAL)   00588*71
         SPACE 2                                                        00589*56
DEBFILE  DMDA  DSKXTNT=16                                               00590*56
         SPACE 2                                                        00591**5
BLDXTNT  DS    0C                                                       00592   
CARD     DC    CL80' '                                                  00593   
ANSWER   DC    CL8' '                                                   00594   
SMSFLAG  DC    X'00'                                                    00595105
UPSI     DC    X'00'                                                    00596105
TYPE     DC    X'00'                                                    00597   
OLDXTNTS DC    X'00'                                                    00598**2
NEWXTNTS DC    X'00'                                                    00599*17
DEBXTNTS DC    X'00'                                                    00600*63
LASTXTNT DC    XL16'00'                                                 00601*59
         SPACE 2                                                        00602   
DUB      DS    D                                                        00603   
FULL     DS    F                                                        00604*18
ACMRG    DS    A                                                        00605   
WORK     DS    CL64                                                     00606*47
DMCB     DS    6F                                                       00607   
RETSAVE  DS    F                                                        00608107
         DC    C'**PARM**'                                              00609   
P1       DS    F                                                        00610   
P2       DS    F                                                        00611   
P3       DS    F                                                        00612   
P4       DS    F                                                        00613   
P5       DS    F                                                        00614   
P6       DS    F                                                        00615   
         LTORG                                                          00616*78
SAVEDEB  DC    XL256'00'                                                00617*69
         DC    16X'00'             PRECAUTION                           00618*67
         EJECT                                                          00619   
*                                                                       00620*18
         DS    0D                                                       00621*78
         DC    C'*ERSFIL*'                                              00622*78
ERSFILE  DMDA  DSKXTNT=16                                               00623*78
         SPACE 2                                                        00624*78
NEWREC   DC    950CL10'THUNDERBAY'     1 REC/TRK ON 3350                00625*18
         SPACE 2                                                        00626   
         DS    0D                                                       00627   
         DC    C'**BUFF**'                                              00628   
DMEXBUFF DS    200D                                                     00629   
         SPACE 2                                                        00630   
         DS    0D                                                       00631   
         DC    C'**WORK**'                                              00632   
DMEXWORK DS    200D                                                     00633   
         SPACE 2                                                        00634   
DMEXLAST DS    D                                                        00635   
JFCBAREA DS    176X                                                     00636*77
         EJECT                                                          00637   
*DMDTFPH                                                                00638   
       ++INCLUDE  DMDTFPH                                                       
         SPACE 2                                                        00640   
       ++INCLUDE  DDDPRINT                                                      
         SPACE 1                                                        00001   
         PRINT   GEN                                                    00642*76
         IEFJFCBN   LIST=YES                                            00643*71
         IEZDEB     LIST=NO                                             00644*76
         DSECT                                                          00645*82
         IEFUCBOB   DEVCLAS=DA                                          00646*82
         END                                                            00647   
