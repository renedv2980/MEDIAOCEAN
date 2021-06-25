*          DATA SET ACREPAC02  AT LEVEL 008 AS OF 07/09/18                      
*PHASE ACAC02A,*                                                                
*INCLUDE AUTOAC                                                                 
*INCLUDE PRNTBL                                                                 
*INCLUDE GETBROAD                                                               
*INCLUDE BUFFERIN                                                               
*INCLUDE BINSR31                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE ACLIST                                                                 
*INCLUDE DLFLD                                                                  
*                                                                               
         TITLE 'AC - SPECIAL CASHFLOW'                                          
*                                                                               
**********************************************************************          
* PROGRAM OPTIONS :                                                  *          
*                                                                    *          
*         QOPT1 - 'Y'  DISPLAY SR ACCOUNT DETAILS                    *          
*         QOPT2 - 'Y'  EXPAND MULTIPLE TRANSACTIONS                  *          
*         QOPT4 - 'Y'  MARK APPROVED (LIVE RUN)                      *          
*         QOPT5 - 'Y'  RUN OFF SS/SR TRANSACTIONS NOT POINTERS       *          
*         QOPT6 - 'Y'  CALC CASH AVAILABILITY BY MOS NOT BY ESTIMATE *          
*         QOPT7 - 'Y'  PRINTABLES (INTERNAL USE ONLY)                *          
*         QOPT8 - 'Y'  SUPPRESS DISBURSED DETAILS/TRANSACTIONS       *          
*--------------------------------------------------------------------*          
*         QSELECT(3)   CLIENT CODE                                   *          
*           OR                                                       *          
*         QSELECT(4)   CLIENT LIST (PRODUCT NOT ALLOWED IF LIST)     *          
*         QSELECT+3(3) PRODUCT CODE                                  *          
*         QAPPL(2)     TOLERANCE LEVEL (99 MAX)                      *          
*         QAPPL+2(6)   ESTIMATE                                      *          
*         QAPPL+8(2)   MEDIA OR +N, +S, +P FOR SYSTEM                *          
**********************************************************************          
*RGUP 008 06JUL18  <SPEC-20692> NEED ADDITIONAL MEDIA FOR DIGIAL AUDIO          
**********************************************************************          
ACAA02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACAA**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACAAD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,REQLAST              LAST FOR REQUEST                       
         BE    REQL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         L     R2,ABOXRC                     SET UP BOX ROUTINE                 
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'     SET WIDTH FOR REPORT                        
*                                                                               
         CLI   MCTSTRUN,X'FF'      TEST RUN=TEST                                
         BNE   *+8                                                              
         OI    RUNOPT,TESTRUN                                                   
*                                                                               
         MVC   UPSI,MCUPSI         TEST OPTIONS                                 
         CLI   MCRECOVR,C'W'       IS THIS A SOON RUN                           
         BE    RUNF05              NO                                           
         OC    MCREMPQK,MCREMPQK   IS THIS A SOON RUN?                          
         BZ    RUNF10              NO                                           
RUNF05   OI    RUNOPT,SOONRUN      YES                                          
         MVC   USID,MCUSERID       GET USER ID                                  
         MVC   PQID,MCREMPQK       PQ ID                                        
         MVC   VSSB,MCSSB          AND ADDRESS OF SSB                           
*                                                                               
RUNF10   DS    0H                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(3,TODAY3)                                
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
REQF     DS    0H                                                               
         GOTO1 ASETMOS,DMCB,(RC)   SET START AND END MOS DATES                  
*                                                                               
         GOTOR INITABLK            INITIALIZE AUTO APPROVE BLOCK                
         GOTO1 AUTOAPP,AUTBLK      BUILD PAYABLE AND APPLY CASH                 
         BAS   RE,PRNL             PRINT REPORT                                 
*                                                                               
*        ZAP   RCRQTOT,RCENNUM                                                  
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
REQL     DS    0H                                                               
*        GOTO1 PRINT,DMCB,SPACES,=C'BC01'                                       
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
REQLX    B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
* PRINT LAST                                                         *          
**********************************************************************          
PRNL     NTR1                                                                   
         LA    RE,DLBLK                                                         
         LHI   RF,DLCBXLX                                                       
         XCEFL                                                                  
*                                                                               
         USING DLCBD,DLBLK                                                      
         OI    DLCBFLG1,DLCBFXTN                                                
*        MVC   DLCBAPR,ACREPORT                                                 
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         LA    R0,XP                                                            
         ST    R0,DLCBAPL                                                       
         BRAS  RE,CLEARXP                JUST IN CASE                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   DLCBACT,C'I'              START AND INTIALIZE REPORT             
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         USING PLINED,R5                                                        
         LA    R5,XP                                                            
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
*        GOTO1 ACREPORT                                                         
*                                                                               
         XC    TSRAWRK,TSRAWRK                                                  
         XC    TSRAWRK2,TSRAWRK2                                                
         ZAP   MOSDATOT,=P'0'                                                   
*                                                                               
         GOTO1 ABFRIN,DMCB,(RC),BUFFARDH                                        
         B     PRNL12                                                           
*                                                                               
PRNL10   GOTO1 ABFRIN,DMCB,(RC),BUFFASEQ       GET NXT RECORD                   
*                                                                               
PRNL12   TM    BCTSERRS,BUFFEEOF            IS THIS END OF FILE                 
         BO    PRNLX                                                            
*                                                                               
         LA    R2,TSRAWRK                                                       
         USING TSRAD,R2                                                         
*                                                                               
         CLI   TSRKTYP,TTREPQ                                                   
         BNE   PRNL10                                                           
*                                                                               
         CLC   TSRKMOS,MMOSSTR                                                  
         BL    PRNL10                                                           
         CLC   TSRKMOS,MMOSEND                                                  
         BH    PRNL10                                                           
*                                                                               
         OC    TSRKCDA,TSRKCDA                                                  
         BNZ   PRNL20                                                           
         ZAP   MOSDATOT,TSRADR     TOT CR FOR MOS-BILLDATE                      
         B     PRNL10                                                           
*                                                                               
PRNL20   DS    0H                                                               
         MVC   DLCBFLD,SPACES                                                   
*                                                                               
         MVC   DLCBFLD(L'TSRKSYS),TSRKSYS                                       
*                                                                               
         CLI   TSRKSYS,C'S'                                                     
         BNE   *+10                                                             
         MVC   DLCBFLD+2(8),=CL8'SPOTPAK'                                       
*                                                                               
         CLI   TSRKSYS,C'N'                                                     
         BNE   *+10                                                             
         MVC   DLCBFLD+2(8),=CL8'NETPAK'                                        
*                                                                               
         CLI   TSRKSYS,C'P'                                                     
         BNE   *+10                                                             
         MVC   DLCBFLD+2(8),=CL8'PRINTPAK'                                      
*                                                                               
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         MVC   DLCBFLD(L'TSRKMED),TSRKMED                                       
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
* READ MEDIA INTERFACE RECORD TO GET MEDIA NAME                                 
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R1,SVKEY                                                         
         USING MINRECD,R1                                                       
         MVI   MINKTYP,MINKTYPQ                                                 
         MVC   MINKCPY,RCCOMPFL                                                 
         MVC   MINKMED,TSRKSYS     LEN=2,SYSTEM+MEDIA                           
         DROP  R1                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(L'MINKTYP+L'MINKCPY+L'MINKMED),IOKEY                       
         BNE   PRNL50                                                           
*                                                                               
         MVC   DSKADDR,IOKEY+MINKDA-MINKEY                                      
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC),DSKADDR                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IO                                                            
         MVI   ELCODE,MDIELQ                                                    
         BRAS  RE,GETEL                                                         
         BNE   PRNL50                                                           
         USING MDIELD,R5                                                        
         MVC   DLCBFLD(L'MDIDESC),MDIDESC                                       
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
         DROP  R5                                                               
*                                                                               
PRNL50   DS    0H                                                               
         MVC   DLCBFLD(L'TSRKCLI),TSRKCLI                                       
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
* READ ACCOUNT RECORD TO GET ADVERTISER NAME                                    
*                                                                               
         XC    SVKEY,SVKEY                                                      
         LA    R1,SVKEY                                                         
         USING ACTRECD,R1                                                       
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVC   ACTKUNT(2),=C'SJ'   PRODUCTION U/L                               
         MVC   ACTKACT(L'TSRKCLI),TSRKCLI                                       
         OC    ACTKACT,SPACES                                                   
         DROP  R1                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(ACTKEND),IOKEY                                             
         BNE   PRNL60                                                           
*                                                                               
         MVC   DSKADDR,IOKEY+ACTKDA-ACTKEY                                      
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC),DSKADDR                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,IO                                                            
         MVI   ELCODE,NAMELQ                                                    
         BRAS  RE,GETEL                                                         
         BNE   PRNL60                                                           
*                                                                               
         USING NAMELD,R5                                                        
         LLC   RF,NAMLN                                                         
         SHI   RF,3                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DLCBFLD(0),NAMEREC                                               
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
         DROP  R5                                                               
*                                                                               
PRNL60   DS    0H                                                               
         MVC   WORK(2),TSRKMOS   YM                                             
         MVI   WORK+2,X'01'      DAY                                            
         GOTO1 DATCON,DMCB,(1,WORK),(10,DLCBFLD)                                
         MVC   DLCBFLD+3(2),DLCBFLD+6                                           
         MVC   DLCBFLD+5(3),SPACES                                              
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(1,TSRKBDA),(10,DLCBFLD)                             
*                                                                               
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         LA    R3,MOSDATOT         TOTALS FOR MOS-BILLDATE                      
         CLC   TSRAWRK2(TSRKCDA-TSRKEY),TSRAWRK SAME S,M,CLI,MOS,BDA?           
         BNE   *+8                 YES - DO NOT REPEAT THE AMOUNT               
         LA    R3,=PL8'0'                                                       
*                                                                               
         EDIT  (P8,0(R3)),(15,DLCBFLD),2,ZERO=NOBLANK,MINUS=YES,       +        
               COMMAS=YES                                                       
         MVI   DLCBTYP,C'N'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         GOTO1 DATCON,DMCB,(1,TSRKCDA),(10,DLCBFLD)                             
*                                                                               
         TM    TSRFLAG,TSRFLGNC                                                 
         BZ    *+8                                                              
         MVI   DLCBFLD+8,C'*'                                                   
*                                                                               
         MVI   DLCBTYP,C'T'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         EDIT  (P8,TSRACR),(15,DLCBFLD),2,ZERO=NOBLANK,MINUS=YES,      +        
               COMMAS=YES                                                       
         MVI   DLCBTYP,C'N'                                                     
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         MVI   DLCBACT,DLCBEOL           SEND END OF LINE                       
         GOTO1 =V(DLFLD),DLBLK                                                  
*                                                                               
         MVC   TSRAWRK2,TSRAWRK                                                 
         B     PRNL10                                                           
*                                                                               
PRNLX    DS    0H                                                               
         LA    R5,XP                                                            
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
         MVI   DLBLK+DLCBACT-DLCBD,C'R'                                         
         GOTO1 =V(DLFLD),DLBLK                                                  
         B     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
*                                                                               
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
ATCHOOK  DC    A(TRCHK)                                                         
ACCMST   DC    CL8'ACCMST'                                                      
PKROUND  DC    XL2'010C'                                                        
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(TSRTAB)           SS TRANSACTIONS TABLE                        
         DC    A(PSSATAB)          SS TRANSACTIONS TABLE                        
         DC    A(SETMOS)           SET START AND END MOS DATES                  
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
         DC    A(MAINTAB)          MAIN TABLE                                   
         DC    A(LDSYTAB)          LEDGER SYSTEM TABLE                          
         DC    A(MBUFF)            BUFFERIN ROUTINE                             
         DC    V(AUTOAC)           AUTO APPROVE MODULE                          
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(BINSRCH)          BINSRCH (31 BIT MODE)                        
         DC    V(HELLO)            HELLO                                        
         DC    V(ACLIST)           CLIENT LIST                                  
         DC    V(BUFFERIN)         BUFFERIN                                     
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R5,56,ELCODE                                                     
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         SPACE 1                                                                
         GETEL2 R5,DATADISP,ELCODE,2                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE UPDATES VENOR PASSIVE POINTER WITH THE APPROVED BIT        *          
* R2=(ADDRESS OF THE CURRENT TABLE ENTRY)                            *          
**********************************************************************          
         USING PSSAD,R2                                                         
         SPACE 1                                                                
UPDVPAS  NTR1  BASE=*,LABEL=*                                                   
         USING AAVPASD,R3          AUTO APPROVE VENDOR PASSIVE PTR              
         XC    SVKEY,SVKEY         BUILD KEY TO READ SR RECDS                   
         LA    R3,SVKEY                                                         
         MVC   AAVPKEY,SPACES                                                   
         MVI   AAVPTYP,AAVPTYPQ    X'24'                                        
         MVI   AAVPSUB,AAVPSUBQ    X'01'                                        
         MVC   AAVPCPY,RCCOMPFL                                                 
         MVC   AAVPCLT,PSSKCLI     CLIENT CODE                                  
         MVC   AAVPPRD,PSSKPRO     PRODUCT CODE                                 
         MVC   AAVPEST,PSSKEST     ESTIMATE                                     
         MVC   AAVPMOS,PSSKMOS     MOS                                          
         MVC   AAVPSYS,PSSKSYS     SYSTEM                                       
         MVC   AAVPOFF,PSSKOFF     OFFICE                                       
         MVC   AAVPACCT,PSSKSTN+1  ACCOUNT                                      
         MVC   AAVPKDA,DSKADDR     DISK ADDRESS                                 
         MVI   AAVPFLG1,0          WANT ALL TYPES OF ESTIMATES                  
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   SVKEY(AAVPFLG1-AAVPKEY),IOKEY                                    
         BNE   UPDVX                                                            
         LA    R3,IOKEY                                                         
*                                                                               
         MVC   MSG,=CL10'VENDOR BEF'                                            
         GOTO1 ADUMP,DMCB,(RC),(R3),L'IOKEY+10                                  
*                                                                               
         OI    AAVPSTAT,AAVPAPP    TURN ON APPROVE BIT                          
*                                                                               
         MVC   MSG,=CL10'VENDOR AFT'                                            
         GOTO1 ADUMP,DMCB,(RC),(R3),L'IOKEY+10                                  
*                                                                               
         CLI   RCWRITE,C'N'                                                     
         BE    UPDVX                                                            
         CLI   QOPT4,C'Y'          DO WE WANT TO MARK RECORD                    
         BNE   UPDVX                                                            
         GOTO1 =A(DMWRTDR),DMCB,(RC)                                            
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDVX    J     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS UPDVPAS                                                   *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE INITIALIZES AUTO APPROVE BLOCK TO PASS TO ROUTINE          *          
**********************************************************************          
         SPACE 1                                                                
INITABLK NTR1  BASE=*,LABEL=*                                                   
         XC    AUTBLK(AUTBLKL),AUTBLK                                           
*                                                                               
         MVC   AUTALST,ADLSTREC                                                 
         OC    ADLSTREC,ADLSTREC                                                
         BNZ   *+10                                                             
         MVC   AUTCLNT,QCLI        CLIENT CODE                                  
*                                                                               
         MVC   AUTSMOS,MMOSSTR     START MOS DATE                               
         MVC   AUTEMOS,MMOSEND     END MOS DATE                                 
         MVC   AUTCOMF,ADCOMFAC    COMFACS ADDRESS                              
         MVC   AUTBFRIN,VBUFFRIN   ADDRESS OF BUFFERIN                          
         MVC   AUTABIN,BINSRC31    ADDRESS OF 31 BIT BINSEARCH RTN.             
         MVC   ADYNALOC,DYNALLOC   ADDRESS OF DYNAMIC ALLOCATION                
         MVI   AUTOFSW,C'Y'        RUNNING OFFLINE TSAR                         
         MVC   AUTCPY,RCCOMPFL     COMPANY CODE                                 
         MVC   AUTALPHA,ALPHAID    COMPANY ALPHAID                              
         MVC   AUTLDGR,QLEDGER     LEDGER                                       
*                                  ALL MEDIAS OR SPECIFIC MEDIA                 
         MVC   AUTSYSTM,QSYSTEM    SYSTEM CODE S/P/N                            
         MVC   AUTMEDIA,QMEDIA     MEDIA CODE FOR A GIVEN SYSTEM                
*                                                                               
         MVC   AUTOPT1,QOPT1                                                    
         MVC   AUTOPT2,QOPT2                                                    
         MVC   AUTOPT5,QOPT5                                                    
         MVC   AUTOPT6,QOPT6                                                    
         MVC   AUTOPT8,QOPT8                                                    
         MVC   AUTPROF,PROFILES    PASS PROGRAM CONTROL PROFILES                
         ZAP   AUTPCNT,=P'100'                                                  
*                                                                               
         MVC   AUTACCT,QACCOUNT                                                 
         MVC   AUTTODAY,TODAYP                                                  
*                                                                               
         L     R2,=A(TRCHK)                                                     
         ST    R2,TRCHOOK                                                       
         J     EXIT                                                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS INITABLK                                                  *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CLEAR XP TO SPACES                                                            
**********************************************************************          
         SPACE 1                                                                
CLEARXP  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,XP                                                            
         MVI   XP,C' '             FILL XP WITH SPACES                          
         MVC   XP+1(L'XP-1),XP                                                  
         J     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
         J     EXIT                                                             
**********************************************************************          
* LITERALS FOR PRINTIT  ROUTINE                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1  BASE=*,LABEL=*                                                   
         MVC   XHEAD3+1(10),=CL10'USER ID:  '                                   
         MVC   XHEAD3+12(L'SVCLOGO),SVCLOGO  COMPANY'S MAIN ID                  
*                                                                               
         MVC   XHEAD3+88(4),=C'MOS='                                            
         MVC   XHEAD3+92(6),PRTSTART                                            
         MVI   XHEAD3+98,C'-'                                                   
         MVC   XHEAD3+99(6),PRTEND                                              
*                                                                               
         MVC   XHEAD4+88(17),=C'*** DRAFT RUN ***'                              
         CLI   QOPT4,C'Y'                                                       
         BNE   *+10                                                             
         MVC   XHEAD4+88(17),=C'*** LIVE RUN *** '                              
*                                                                               
         CLC   QTOLRNCE,SPACES                                                  
         BE    HEADX                                                            
         MVC   XHEAD4+159(10),=C'TOLERANCE='                                    
         MVC   XHEAD4+169(2),QTOLRNCE                                           
         MVI   XHEAD4+171,C'%'                                                  
*                                                                               
HEADX    J     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS FOR HEADUP   ROUTINE                                      *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE ZAPS SYS/MED/CLI/PROD TOTAL BUCKETS                        *          
* PASS ADDRESS OF BUCKET TO ZAP IN R1   R1=A(SYS/MED/CLI/PROD BUCKET)*          
**********************************************************************          
         SPACE 1                                                                
ZAPTOTBK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,CLITBKCT         NO. OF BUCKETS                               
         ZAP   0(CLITBKLN,R1),=P'0'     CLEAR FIELDS                            
         LA    R1,CLITBKLN(R1)                                                  
         BCT   R0,*-10             NUMBER OF BUCKETS TO LOOP                    
ZAPTBX   J     EXIT                                                             
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
*&&DO                                                                           
**********************************************************************          
* ROUTINE TO PRINT TOTALS AT SYS/MED/CLI/PROD LEVEL CHANGES          *          
**********************************************************************          
         SPACE 1                                                                
PSMCPTOT NTR1  BASE=*,LABEL=*                                                   
         USING PLINED,R5                                                        
         LA    R5,XP                                                            
*                                                                               
         TM    FLAG,FLGPRNT        WAS ANYTHING PRINTED ?                       
         BNO   PSMCPTX                                                          
         MVC   PPRO,LSTPRO         MOVE PRODUCT CODE TO PRINT                   
         MVC   PEST,=C'TOTALS'     MOVE TOT IN NEXT FIELD                       
         LA    R1,PROTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR PRDCT                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PCLI,LSTCLI                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,CLITBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR CLIENT               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PMED,LSTMED                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,MEDTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR MEDIA                
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PSYS,LSTSYS                                                      
         MVC   PEST,=C'TOTALS'                                                  
         LA    R1,SYSTBKT                                                       
         BRAS  RE,PRNTOTBK         PRINT TOTAL BUCKETS FOR SYSTEM               
         BRAS  RE,ZAPTOTBK         CLEAR TOTALS BUCKETS                         
         BAS   RE,PRINTIT                                                       
PSMCPTX  J     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
*&&                                                                             
**********************************************************************          
* ROUTINE ADDS TO SYS/MED/CLI/PROD TOTAL BUCKETS                     *          
* PASS ADDRESS OF BUCKET TO ADD IN R1   R1=A(SYS/MED/CLI/PROD BUCKET)*          
**********************************************************************          
         SPACE 1                                                                
         USING PSSAD,R2                                                         
ADDTOTBK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,CLITBKCT         NO. OF BUCKETS                               
         LA    R3,PSSABKT                                                       
         AP    0(CLITBKLN,R1),0(PSSABKLN,R3)    ADD TO TOTALS BUCKET            
         LA    R1,CLITBKLN(R1)                                                  
         LA    R3,PSSABKLN(R3)                                                  
         BCT   R0,*-14             NUMBER OF BUCKETS TO LOOP                    
ADDTBX   J     EXIT                                                             
         DROP  R2                                                               
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ROUTINE PRINTS  SYS/MED/CLI/PROD TOTAL BUCKETS                     *          
* PASS ADDRESS OF BUCKET TO ADD IN R1   R1=A(SYS/MED/CLI/PROD BUCKET)*          
**********************************************************************          
*&&DO                                                                           
         USING PLINED,R5                                                        
PRNTOTBK NTR1  BASE=*,LABEL=*                                                   
         LA    R0,CLITBKCT         NO. OF BUCKETS                               
         LA    R6,PBUCKET                                                       
PRNTB10  EDIT  (P8,(R1)),(PBUCLEN,(R6)),2,ZERO=BLANK,MINUS=YES,        +        
               COMMAS=YES                                                       
         LA    R1,CLITBKLN(R1)                                                  
*                                                                               
         LA    RE,PUNDISB          BUMP LITTLE EXTRA FOR THIS FLD               
         CR    R6,RE                                                            
         BNE   *+8                                                              
         LA    R6,L'PMARKA(R6)     BUMP TO PRINT 'A' IN APPROVED ITEMS          
*                                                                               
         LA    R6,L'PBUCKET(R6)                                                 
         BCT   R0,PRNTB10          NUMBER OF BUCKETS TO LOOP                    
PRNTTBX  J     EXIT                                                             
         DROP  R5                                                               
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
***********************************************************************         
* SET START AND END MOS DATES                                         *         
***********************************************************************         
         SPACE 1                                                                
SETMOS   DS    0D                                                               
         NMOD1 0,*SETMOS*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   PRTSTART,SPACES                                                  
         MVC   PRTEND,SPACES                                                    
         XC    MMOSSTR,MMOSSTR                                                  
         XC    MMOSEND,MMOSEND                                                  
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         L     R3,ADQSTACK                                                      
         USING ACQD,R3                                                          
         CLI   ACQCONT1,C'C'       DO WE HAVE CONTINUATION CHARACTER            
         BNE   SETMS60                                                          
         CLI   ACQCONT2,C'C'                                                    
         BNE   SETMS60             AND MOS DATE FILTER IN TYPE FIELDS           
*                                                                               
         LHI   R0,2                CHECK 3RD AND 4TH CARDS FOR MOS              
         LHI   R2,4                4 ENTRIES TO CHECK                           
         LA    R4,ACQTYP1          POINT TO 1ST TYPE                            
SETMS10  DS    0H                                                               
         CLI   0(R4),ACQDATE       DATE FILTER?                                 
         BNE   SETMS20                                                          
         CLI   1(R4),ACQDTMOS      ACQDTTYP                                     
         BE    SETMS30             WHAT FOLLOWS IS A MOS                        
SETMS20  LA    R4,L'ACQTYP1+L'ACQFLT1(R4)   BUMP TO NEXT ENTRY                  
         BCT   R2,SETMS10                                                       
*                                                                               
         CLI   ACQCONT3,C'C'                                                    
         BNE   SETMS60                                                          
         LHI   R2,4                                                             
         LA    R4,ACQCARD4                                                      
         BCT   R0,SETMS10           CHECK 4TH REQUEST CARD FOR MOS              
         B     SETMS60                                                          
*                                                                               
SETMS30  CLC   (ACQDTSTR-ACQTYP1)(L'ACQDTSTR,R4),SPACES                         
         BE    SETMS40                                                          
         MVC   WORK(L'ACQDTSTR),(ACQDTSTR-ACQTYP1)(R4)                          
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,MMOSSTR)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,PRTSTART)                                
*                                                                               
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+6,-1  MOS START MINUS 1 YEAR         
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,LASTYEAR)                              
*                                                                               
SETMS40  MVC   MMOSEND,=X'FFFFFF'                                               
         CLC   (ACQDTEND-ACQTYP1)(L'ACQDTEND,R4),SPACES                         
         BE    SETMS20                                                          
         MVC   WORK(L'ACQDTEND),(ACQDTEND-ACQTYP1)(R4)                          
         CLC   WORK+4(2),SPACES                                                 
         BNE   SETMS50                                                          
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
SETMS50  GOTO1 DATCON,DMCB,(0,WORK),(1,MMOSEND)                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,PRTEND) FOR PRINTING ON REPORT           
*                                                                               
         OC    MMOSSTR,MMOSSTR     IF REQUEST COMES FROM RLP THEN               
         BNZ   SETMS20             THERE IS NO START DATE SPECIFIED             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,-11                                
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,MMOSSTR)                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(6,PRTSTART)                              
         B     SETMS20                                                          
*                                                                               
SETMS60  DS    0H                                                               
         OC    MMOSSTR,MMOSSTR                                                  
         BNZ   *+6                                                              
         DC    H'0'                NO MOS START DATE PROVIDED                   
         OC    MMOSEND,MMOSEND                                                  
         BNZ   *+6                                                              
         DC    H'0'                NO MOS END DATE PROVIDED                     
SETMSX   XIT1                                                                   
**********************************************************************          
* LITERALS SETMOS  NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
***********************************************************************         
* TRACE HOOK                                                          *         
*                                                                               
***********************************************************************         
TRCHK    NMOD1 0,*TRCHK                                                         
         LR    R2,R1               A(SRC)                                       
         LR    R6,R0               A(COMMAND)                                   
*                                                                               
         BRAS  RE,CLEARXP          CLEAR XP TO SPACES                           
*                                                                               
         MVC   XP(6),0(R6)                                                      
*                                                                               
         LR    R3,R1                                                            
         SRL   R3,24                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   XP+7(0),0(R1)                                                    
         OC    XP(L'SPACES),SPACES                                              
         OC    XP+L'SPACES(L'XP-L'SPACES),SPACES                                
*                                                                               
         LA    R4,XP+13(R3)                                                     
         AHI   R3,1                                                             
         GOTO1 HEXOUT,DMCB,0(R2),(R4),(R3),=C'N'                                
*                                                                               
         GOTO1 ACREPORT                                                         
*                                                                               
         XMOD1 1                                                                
*                                                                               
*                                                                               
*                                                                               
DWNHOOK  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'N'                                                    
         MVI   LINE,0                                                           
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
BXHOOK   NMOD1 0,*BHOOK                                                         
         XMOD1 1                                                                
BOXRC    DC    A(0)                                                             
*                                                                               
*&&DO                                                                           
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
         USING PLINED,R5                                                        
         LA    R5,XP                                                            
*                                                                               
         CLI   RCSUBPRG,4          IS THIS TOTALS PAGE?                         
         BE    BXHK30                                                           
*                                                                               
         CLC   PEST,=C'TOTALS'     PRINTING CLI/PROD/EST/SYS/MED TOTALS         
         BE    BXHK20                                                           
*                                                                               
         MVC   PSYS,LSTSYS                                                      
*                                                                               
         CLI   PSYS,C'N'           IS THIS NET SYSTEM ?                         
         BNE   *+12                                                             
         CLI   AUTPRF2,C'S'        RUN BY SYSTEM OR SYS/MEDIA ?                 
         BE    *+10                                                             
         MVC   PMED,LSTMED                                                      
*                                                                               
         MVC   PCLI,LSTCLI                                                      
         MVC   PPRO,LSTPRO                                                      
         MVC   PEST,LSTEST                                                      
*                                                                               
BXHK10   CLC   LSTSTN,SPACES                                                    
         BNH   BXHK20                                                           
         MVC   PSTA,LSTSTN                                                      
         DROP  R5                                                               
*                                                                               
BXHK20   MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+8,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PMED-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PCLI-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PPRO-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PEST-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PMOS-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PSTA-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PINV-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PCLEAR-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PBILLED-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PRCVD-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PDISB-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PCASH-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PUNDISB-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PAVAIL-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         B     BXXIT                                                            
*                                                                               
BXHK30   DS    0H                                                               
         MVC   BOXCOLS(165),XSPACES                                             
         MVC   BOXROWS,XSPACES                                                  
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
*        MVI   BOXCOLS+(PTOTMSG-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PTOT-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
         B     BXXIT                                                            
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
         XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
*&&                                                                             
**********************************************************************          
* LITERALS BXHOOK  NMOD1                                             *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 APRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),        +        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS  DUMP NMOD1                                               *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BUFFERIN INTERFACE ROUTINE                                          *         
*                                                                     *         
* NTRY - P1 = (RC)                                                    *         
*      - P2 = BUFFERIN ACTION (DEFINED IN DDBUFFD)                    *         
*                                                                     *         
* EXIT - CC=EQU - ACTION COMPLETED WITHOUT ERRORS                     *         
*        CC=NEQ - BCTSERRS=BUFFERIN ERROR CODE                        *         
***********************************************************************         
         SPACE 1                                                                
MBUFF    DS    0D                                                               
         NMOD1 0,**MBUFF*                                                       
         L     RC,0(R1)            P1=A(LOCAL WORKING)                          
         SR    R0,R0                                                            
         IC    R0,7(R1)            P2=BUFFERIN ACTION                           
*                                                                               
         LA    R1,TSRAWRK          RECEIVABLES WORK AREA                        
         ST    R1,BUFFREC          SET A(INPUT RECORD)                          
         ICM   R3,15,AUTPBUFF                                                   
         MVC   TSARKSAV,0(R1)      SAVE CURRENT RECORD KEY                      
         USING BUFFD,R3            R3=A(BUFFERIN CONTROL BLOCK)                 
*                                                                               
         CHI   R0,BUFFAINI         TEST INITIALIZATION CALL                     
         JNE   BUFFER10                                                         
*                                                                               
         USING BIND,R5                                                          
*        L     R5,APSSATAB         POINT TO PAYABLES TABLE                      
         L     R5,ATSRTAB          POINT TO RECEIVABLES TABLE                   
         SR    RE,RE                                                            
         ICM   RE,7,BINKEY         KEY LENGTH                                   
         SR    RF,RF                                                            
         IC    RF,BINFST           RECORD LENGTH=RECLEN-BUCKETS                 
*        L     RF,BINLEN           RECORD LENGTH                                
         SR    RF,RE               DATA LENGTH                                  
         STCM  RE,3,BUFFLKEY       SET KEY LENGTH                               
         STCM  RF,3,BUFFLCOM       SET COMMENT LENGTH                           
         MVC   BUFFNCOL,BINNUM     NUMBER OF BUCKETS/COLUMNS                    
*                                                                               
BUFFER10 GOTOR AUTBFRIN,DMCB,((R0),BUFFD),BUFFREC,AUTCOMF                       
         MVC   BUFFRET,BUFFERRS-BUFFPARM(R1)                                    
         CHI   R0,BUFFARDH         EMULATE TSAR NOT FOUND ON READ HIGH          
         JNE   BUFFER20                                                         
         L     RF,BUFFREC                                                       
         LH    R1,BUFFLKEY         LENGTH OF THE KEY                            
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         JE    BUFFER20                                                         
         CLC   TSARKSAV(0),0(RF)                                                
         OI    BUFFRET,BUFFERNF                                                 
                                                                                
BUFFER20 MVC   BCTSERRS,BUFFRET    PASS BACK ERRORS                             
         CLI   BUFFRET,0           SET CONDITION CODE FOR CALLER                
         J     EXIT                                                             
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         L     R2,4(R1)                                                         
         LTR   R2,R2               DO WE HAVE DISK ADDRESS                      
         BNZ   DMGETR10            YES                                          
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         LA    R2,ACCKDA                                                        
DMGETR10 GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',(R2),IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',ACCKDA,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES                                                             *          
**********************************************************************          
*                                                                               
* BINTABLE 1 - SR TRANSACTION TABLE BUILT IN PROC TRANSACTION                   
*                                                                               
MAINTAB  DS    0F                                                               
         DC    C'**TSRACCT*'                                                    
TSRTAB   DS    0C                                                               
         DC    AL4(0)                  NUMBER IN TABLE                          
         DC    AL4(TSRALNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TSRKLNQ)            KEY LENGTH                               
         DC    AL4(TSRAMAX)            MAX IN TABLE                             
         DC    AL1(TSRABKCT)           NUMBER OF BUCKETS                        
         DC    AL1(TSRABKT-TSRAD)      DISPLACEMENT TO FIRST BUCKET             
         DC    AL4(TSRASIZE)                                                    
*                                                                               
TSRAMAX  EQU   50000                                                            
         SPACE 1                                                                
*                                                                               
* BINTABLE 2 - SS ACCOUNT TABLE BUILT IN ACCOUNT LAST                           
*                                                                               
         DC    C'**PSSACCT*'                                                    
PSSATAB  DS    0C                                                               
         DC    AL4(0)                  NUMBER IN TABLE                          
         DC    AL4(PSSALNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(PSSAKLNQ)           KEY LENGTH                               
         DC    AL4(PSSAMAX)            MAX IN TABLE                             
         DC    AL1(PSSABKCT)           NUMBER OF BUCKETS                        
         DC    AL1(PSSABKT-PSSAD)      DISPLACEMENT TO FIRST BUCKET             
         DC    AL4(PSSASIZE)                                                    
*                                                                               
*MAINNUM  EQU   (*-MAINTAB)/MAINLNQ NUMBER OF BINARY TABLES                     
*                                                                               
*SSAMAX  EQU   99000                                                            
         SPACE 1                                                                
TSRASIZE EQU   (TSRAMAX*TSRALNQ)                                                
LENBUFF1 EQU   TSRASIZE                                                         
LENBUFF  EQU   LENBUFF1+LENBUFF2                                                
*                                                                               
*                                  TABLE OF LEDGER AND SYSTEM                   
LDSYTAB  DC    C'SS'               LEDGER S - SYS SPOT (BUT MAYBE NET)          
         DC    C'TS'               LEDGER T - SYS SPOT                          
         DC    C'PP'               LEDGER P - SYS PRINT                         
         DC    C'QP'               LEDGER Q - SYS PRINT                         
         DC    C'UN'               LEDGER U - SYS NET                           
         DC    X'FF'                                                            
         SPACE 2                                                                
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACAAD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
VTYPES   DS    0A                                                               
ATSRTAB  DS    A                   SS TRANSACTION TABLE                         
APSSATAB DS    A                   SS ACC TABLE BUILT FROM FILE                 
ASETMOS  DS    A                   SETS START AND END MOS DATES                 
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
AMAINTAB DS    A                   ADDRESS OF MAIN TABLE                        
ALDSYTAB DS    A                   LEDGER SYSTEM TABLE                          
ABFRIN   DS    A                   ADDRESS OF TSAR MAINTENANCE ROUTINE          
AUTOAPP  DS    V                   AUTO APPROVE MODULE                          
APRNTBL  DS    V                   PRINT DATA                                   
BINSRC31 DS    V                   BINSRCH (31 BIT MODE)                        
VHELLO   DS    V                                                                
ACLIST   DS    V                                                                
VBUFFRIN DS    V                   ADDRESS OF BUFFERIN                          
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
VSSB     DS    V                   NEEDED FOR SOON                              
AELMNTS  DS    0A                                                               
AGDAELD  DS    A                   X'E5'                                        
AMBIELD  DS    A                   X'F3'                                        
AOTHELD  DS    A                   X'23'                                        
AXPYELD  DS    A                   X'46'                                        
AMDTELD  DS    A                   X'1A'                                        
AMDPELD  DS    A                   X'6A'                                        
AELMNTSQ EQU   *-AELMNTS                                                        
*                                                                               
REQPCNT  DS    PL8        IF THIS % OF UNAPPROVED IS AVL THEN AUTO APPR         
DSKADDR  DS    XL4                 DISK ADDRESS OF CURRENT TRANSACTION          
DISP2    DS    H                                                                
PTOTAPP  DS    PL8        TOTALS APPROVED IN THIS RUN.                          
PTOTPRV  DS    PL8        PREVIOUSLY APPROVED TOTALS.                           
***********************************************************************         
* ORDER OF THE NEXT FOUR BUCKET TABLES SHOULD NOT CHANGE OR PROGRAM   *         
* WILL FAIL CLITBKT-> PROTBKT-> SYSTBKT-> MEDTBKT.                    *         
***********************************************************************         
         DC    C'****CLI TOT TAB****'                                           
CLITBKT  DS    0PL8            ****CLIENT TOTALS BUCKET****                     
CLIBILLB DS    PL8         TOTAL   AMOUNT BILLED                                
CLITBKLN EQU   *-CLITBKT           BUCKET LENGTH                                
CLIRCVDB DS    PL8         TOTAL   CASH RECEIVED                                
CLIDISBB DS    PL8         TOTAL   DISBURSED AMOUNT                             
CLICASHB DS    PL8         TOTAL   CASH POSITION                                
CLICLRB  DS    PL8         TOTAL   CLEARED AMOUNT TRANSACTION AMOUNT            
CLIUDISB DS    PL8         TOTAL   ALL OF UNDISBURSED AMOUNT                    
CLIAVLB  DS    PL8         TOTAL   CASH AVAILABLE TO APPROVE                    
CLITBKCT EQU   (*-CLITBKT)/CLITBKLN NUMBER OF BUCKETS                           
*                                                                               
PROTBKT  DS    (CLITBKCT)PL8   ****PRODUCT TOTALS BUCKET****                    
SYSTBKT  DS    (CLITBKCT)PL8   ****SYSTEM TOTALS BUCKET****                     
MEDTBKT  DS    (CLITBKCT)PL8   ****MEDIA TOTALS BUCKET****                      
***********************************************************************         
*                                                                               
LASTS    DS    0X                  **LAST TIME VALUES**                         
LSTSYS   DS    CL1                 LAST SYSTEM CODE                             
LSTMED   DS    CL1                 LAST MEDIA CODE                              
LSTCLI   DS    CL3                 LAST CLIENT                                  
LSTPRO   DS    CL3                 LAST PRODUCT                                 
LSTEST   DS    CL6                 LAST ESTIMATE                                
LSTMOS   DS    XL2                 LAST MOS                                     
LSTSTN   DS    CL14                LAST STATION                                 
LSTSYSL  EQU   *-LSTSYS                                                         
*                                                                               
LASTL    EQU   *-LASTS                                                          
*                                                                               
BCFLAG1  DS    XL1                                                              
BCFULL   DS    F                                                                
BCDUB    DS    D                                                                
BCTSERRS DS    CL1                 TEMP TSAR ERRORS                             
*                                                                               
PROFILES DS    0XL16                                                            
PROF1    DS    XL1                                                              
PROF2    DS    XL1                                                              
PROF3    DS    XL1                                                              
PROF4    DS    XL1                                                              
PROF5    DS    XL1                                                              
PROF6    DS    XL1                                                              
PROF7    DS    XL1                                                              
PROF8    DS    XL1                                                              
PROF9    DS    XL1                                                              
PROF10   DS    XL1                                                              
PROF11   DS    XL1                                                              
PROF12   DS    XL1                                                              
PROF13   DS    XL1                                                              
PROF14   DS    XL1                                                              
PROF15   DS    XL1                                                              
PROF16   DS    XL1                                                              
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
ELCODE   DS    CL1                                                              
ELEM     DS    CL255                                                            
TODAYP   DS    PL3                 TODAY'S DATE-PACKED                          
CURTIME  DS    XL3                 CURRENT TIME                                 
TODAY3   DS    XL3                                                              
*                                                                               
RUNOPT   DS    XL1                                                              
SOONRUN  EQU   X'80'                RUNNING SOON                                
TESTRUN  EQU   X'40'                RUN=TEST IS SET                             
UPSI     DS    XL1                                                              
KPFACWK  EQU   X'80'                KEEP WORKER FILE                            
DSPNIOS  EQU   X'40'                DISPLAY NO. OF IOS (BUFFERIN)               
*                                                                               
USID     DS    XL(L'FWRUSID)        ID                                          
PQID     DS    XL(L'FWRPQID)        PQID                                        
LUID     DS    XL(L'FWRLUID)        LUID                                        
*                                                                               
FLAG     DS    XL1                                                              
FLGPRNT  EQU   X'10'               MARK SOMETHING WAS PRINTED                   
FLGHIGH  EQU   X'08'               PRINTING TOTAL BUCKETS                       
FLGPRN03 EQU   X'01'               PRINT FULLY DISB MOS TOTAL LINE              
*                                                                               
FLAG1    DS    XL1                                                              
FLGSTN   EQU   X'08'               SKIPPED PRINTING STATION                     
*                                                                               
SVKEY    DS    CL42                                                             
*                                                                               
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
*                                                                               
SVMOS    DS    XL3                                                              
DTE1     DS    CL6                 YYMMDD (EBCDIC)                              
DTE2     DS    CL6                 YYMMDD (EBCDIC)                              
MMOSSTR  DS    XL3                 MEDIA MONTH OF SVC START FROM REQ            
MMOSEND  DS    XL3                 MEDIA MONTH OF SVC END FROM REQ              
START    DS    XL3                                                              
END      DS    XL3                                                              
PRTSTART DS    CL6                                                              
PRTEND   DS    CL6                                                              
LASTYEAR DS    XL3                 MMOSSTR-(ONE YEAR)                           
*                                                                               
TSRAWRK  DS    CL(TSRALNQ)      BINSEARCH WORK AREA - SR TRNS TABLE             
TSRAWRK2 DS    CL(TSRALNQ)      BINSEARCH WORK AREA - SR TRNS TABLE             
MOSDATOT DS    PL8              $ TOTALS FOR MOS-BILLDATE                       
BUFFREC  DS    A                   BUFFER S/R A(RECORD)                         
BUFFRET  DS    X                   BUFFER S/R RETURN INDICATORS                 
TSARKSAV DS    XL150               TSAR RECORD KEY SAVE AREA                    
*                                                                               
DLBLK    DS    (DLCBXLX)X                                                       
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
       ++INCLUDE ACAUTOAPD                                                      
       ++INCLUDE DDDLCB                                                         
*                                                                               
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
*                                                                               
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL2                                                              
PSYS     DS    CL1                 SYSTEM                                       
         DS    CL1                                                              
PMED     DS    CL1                 MEDIA                                        
         DS    CL1                                                              
PCLI     DS    CL3                 CLIENT CODE                                  
         DS    CL1                                                              
PMOS     DS    CL6                 MONTH OF SERVICE                             
         DS    CL1                                                              
PBDAT    DS    CL8                 BILL DATE                                    
         DS    CL1                                                              
PDR      DS    CL16                BILLED AMOUNT FROM SR                        
         DS    CL1                                                              
PCDAT    DS    CL8                 CHECK DATE                                   
         DS    CL1                                                              
PCR      DS    CL16                CASH FROM CLIENT                             
         DS    CL1                                                              
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* DSECT FOR ENTRY IN SS TRANSACTION TABLE                             *         
***********************************************************************         
TSRAD    DSECT                                                                  
TSRKEY   DS    0X                                                               
*                                                                               
TSRKSYS  DS    CL1                 SYSTEM SPOT/NET/PRINT                        
TSRKMED  DS    CL1                 MEDIA FOR SYSTEM ABOVE                       
TSRKCLI  DS    CL3                 CLIENT                                       
*                                                                               
TSRKBNO  DS    CL6             *** SR REF/BILL NO. ***                          
TSRKACT  DS    CL12                ACCOUNT NUMBER                               
*                                                                               
TSRKMOS  DS    CL2                 TRANSACTION MOS PACKED (YYMM)                
TSRKBDA  DS    CL3                 BILL DATE                                    
*                                                                               
TSRKCDA  DS    XL3                 CHECK DATE                                   
*                                                                               
TSRKTYP  DS    XL1                 RECORD TYPE                                  
TTDRQ    EQU   0                   DEBIT                                        
TTCRQ    EQU   1                   CREDIT                                       
TTDRREFQ EQU   2                   DR/CR TOTALS BY REF#                         
TTREPQ   EQU   3                   REPORT LINE - TOTALLED BY                    
*                                  BILL DATES AND CHECK DATES                   
TTACCTQ  EQU   4                   ACCOUNT-REF#, FOR DUPLICATE CHECKS           
*                                                                               
TSRFLAG  DS    XL1                 FLAG IN KEY PART WILL MAKE SURE THAT         
*                                  CR IS ALWAYS AFTER DR IN TABLE               
TSRFLGCR EQU   X'80'               CREDIT TRANSACTION                           
TSRFLGNC EQU   X'40'               DEBIT WITH NO MATCHING CREDIT                
*                                                                               
TSRKLNQ  EQU   *-TSRAD             LENGTH OF KEY                                
TSRKLNQ2 EQU   *-TSRKMOS           LENGTH OF KEY FROM MOS ONWARD                
*                                                                               
TSRABKT  DS    0PL8                BUCKET                                       
*                                                                               
TSRADR   DS    PL8                 DEBIT AMOUNT                                 
TSRABKLN EQU   *-TSRABKT           BUCKET LENGTH                                
TSRACR   DS    PL8                 CREDIT AMOUNT                                
TSRACOM  DS    PL8                 COMMISSION AMOUNT                            
*                                                                               
TSRABKCT EQU   (*-TSRABKT)/TSRABKLN NUMBER OF BUCKETS                           
TSRALNQ  EQU   *-TSRAD             LENGTH OF ENTRY                              
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*&&DO                                                                           
TSRAD    DSECT                                                                  
TSRKEY   DS    0CL36                                                            
TSRKSYS  DS    CL1                                                              
TSRKACT  DS    CL14                SR ACCOUNT                                   
TSRKCAC  DS    CL14                SR CONTRA U/L/AC                             
TSRKBNO  DS    CL6                 SR REF/BILL NO.                              
TSRKBDA  DS    PL3                 SR TRNDATE/BILL DATE                         
TSRKCLI  DS    CL3                 CLEINT FROM CONTRA OF SS                     
TSRKPRO  DS    CL3                 PRODUCT FROM 1ST 3 BYTES OF REF              
         DS    CL1                 SPARE                                        
TSRKEST  DS    CL6                 ESTIMATE                                     
TSRKRTYP DS    X                   REC TYPE 00=BILL#TOTALS,01=EST TOTS          
*                                  02=DETAILS BY MOS                            
TSRKMOS  DS    PL2                 TRANSACTION MOS PACKED (YYMM)                
*TSRKOFF  DS    CL2                                                             
TSRKLNQ2 EQU   *-TSRKRTYP                                                       
TSRFLAG  DS    XL1                 FLAG IN KEY PART WILL MAKE SURE THAT         
*                                  DR IS ALWAYS AFTER CR IN TABLE               
TSRFLGDR EQU   X'80'               DEBIT TRANSACTION                            
TSRKLNQ1 EQU   *-TSRKCLI                                                        
TSRKLNQ  EQU   *-TSRAD             LENGTH OF KEY                                
*                                                                               
TSRABKT  DS    0PL8                BUCKET                                       
*                                                                               
TSRADR   DS    PL8                 DEBIT AMOUNT                                 
TSRABKLN EQU   *-TSRABKT           BUCKET LENGTH                                
TSRACR   DS    PL8                 CREDIT AMOUNT                                
*SRACCR  DS    PL8                 CALCULATED CREDIT                            
TSRAPCNT DS    PL8                 CALCULATED PERCENT                           
TSRAWRTF DS    PL8                 WRITE OFF                                    
TSRATXFR DS    PL8                 TRANSFER                                     
TRAOFFST DS    PL8                 OFFSET                                       
*                                                                               
TSRABKCT EQU   (*-TSRABKT)/TSRABKLN NUMBER OF BUCKETS                           
TSRALNQ  EQU   *-TSRAD             LENGTH OF ENTRY                              
*&&                                                                             
***********************************************************************         
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACAUTOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACAUTOD                                                        
         PRINT ON                                                               
* ACBIGPRINTD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
         ORG   QSELECT                                                          
QCLI     DS    CL3                 CLIENT                                       
QPROD    DS    CL3                 PRODUCT                                      
         ORG                                                                    
         ORG   QAPPL                                                            
QTOLRNCE DS    CL2                 TOLERANCE LEVEL                              
QEST     DS    CL6                 ESTIMATE                                     
QMED     DS    CL1                 MEDIA CODE OR '+' FOR SYSTEM                 
QSYSTEM  DS    CL1                 SYTEM S/N/P OR BLANK                         
         ORG                                                                    
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDBOXEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBOXEQUS                                                      
         PRINT ON                                                               
* ACREPPROFD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
*SRUPDD                                                                         
         PRINT OFF                                                              
       ++INCLUDE SRUPDD                                                         
         PRINT ON                                                               
*                                                                               
*FASSBOFF                                                                       
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
*                                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACREPAC02 07/09/18'                                      
         END                                                                    
