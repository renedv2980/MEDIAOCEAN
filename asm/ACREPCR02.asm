*          DATA SET ACREPCR02  AT LEVEL 164 AS OF 10/14/20                      
*PHASE ACCR02A,*                                                                
*INCLUDE CASHVAL                                                                
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE SMTP                                                                   
         TITLE 'CASH  RECONCILIATION'                                           
***********************************************************************         
* INPUTS:                                                             *         
*   QOPT1: Y - MARK RECORD                                            *         
*                                                                     *         
*   QOPT2: Y - MARK PREVIOUSLY MARKED ITEMS                           *         
*                                                                     *         
*   QOPT7: Y - DUMP RECORDS                                           *         
*                                                                     *         
***********************************************************************         
* HISTORY                                                             *         
*  APR22/02 LEV007 RGUP:SKIP PRINTING SPACES AT THE BEGINNING OF THE  *         
*                       ACCOUNT - CHANGE ZENY TO ZENYA                *         
*  JUL19/02        RGUP:ADD CODE TO SEND EMAIL (SENDMAIL ROUTINE)     *         
*  NOV18/02        JSHA:ADDED BANK TLDA INFO TO REPORT                *         
*  JAN28/14 LEV125 MNAS:REMOVE JSHA MNAS FROM OV EMAIL NOTIFICATIONS  *         
*                       DSFTK-32 GROUPM CANADA ADDITIONAL SETUP       *         
*  FEB25/14 LEV126 MNAS:DSFTK-32/DSFTK-28                             *         
***********************************************************************         
ACCR02   CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 0,**ACCR**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACCRD,RC                                                         
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS                                           
*                                                                               
         XR    R4,R4                                                            
         LA    R5,CHKLNQ           GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=A(CHKMAX)                                                    
         MR    R4,RE                                                            
         STCM  R5,15,HALF          SAVE LENGTH OF LATER IN HALF/HALF2           
         LA    R3,BKALNQ                                                        
         L     RE,=A(BKAMAX)                                                    
         MR    R2,RE                                                            
         AR    R5,R3                                                            
         AHI   R5,20               ADD IN HEADERS                               
         ST    R5,FULL                                                          
         L     R0,FULL                                                          
         GETMAIN R,LV=(0)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(10,R1),=C'*CHECKTAB*'                                          
         LA    R1,10(R1)                                                        
         ST    R1,ACHKTAB          START OF AREA                                
         ICM   RF,15,HALF                                                       
         AR    R1,RF                                                            
         MVC   0(10,R1),=C'*BKACCTAB*'                                          
         LA    R1,10(R1)                                                        
         ST    R1,ABKATAB                                                       
*                                                                               
         USING BIND,RE                                                          
         L     RE,ACHKTAB          SET UP CHECK TABLE                           
         XC    BININ,BININ         CLEAR BIN TABLE                              
         LA    R1,CHKLNQ                                                        
         STCM  R1,15,BINLEN        ENTRY LENGTH                                 
         XC    BINDISP,BINDISP     DISPLACEMENT TO KEY                          
         LA    R1,CHKKLNQ                                                       
         STC   R1,BINKEY+2         LENGTH OF KEY                                
         L     R1,=A(CHKMAX)                                                    
         STCM  R1,15,BINMAX        MAXIMUM NUMBER OF ENTRIES                    
         XC    BINNUM,BINNUM       NUMBER OF BUCKETS                            
         XC    BINFST,BINFST       DISPLACEMENT TO FIRST BUCKET                 
*                                                                               
         L     RE,ABKATAB          SET UP BANK ACCOUNT TABLE                    
         XC    BININ,BININ         CLEAR BIN TABLE                              
         LA    R1,BKALNQ                                                        
         STCM  R1,15,BINLEN        ENTRY LENGTH                                 
         XC    BINDISP,BINDISP     DISPLACEMENT TO KEY                          
         LA    R1,BKALNQ                                                        
         STC   R1,BINKEY+2         LENGTH OF KEY                                
         L     R1,=A(BKAMAX)                                                    
         STCM  R1,15,BINMAX        MAXIMUM NUMBER OF ENTRIES                    
         XC    BINNUM,BINNUM       NUMBER OF BUCKETS                            
         XC    BINFST,BINFST       DISPLACEMENT TO FIRST BUCKET                 
         DROP  RE                                                               
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
REQF     DS    0H                                                               
         GOTO1 VSORTER,DMCB,SORTCARD,RECCARD                                    
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY)                                 
         GOTO1 DATCON,DMCB,(4,RCDATE),(1,TODAYP)                                
         GOTO1 ADDAY,DMCB,(C'Y',TODAY),WORK,-1                                  
         GOTO1 DATCON,DMCB,(0,WORK),(1,LSTYR)                                   
         XC    FLAG,FLAG           CLEAR STATUS FLAG                            
*                                                                               
         ZAP   PKDMPMAX,=P'750'                                                 
*                                                                               
         USING AGYTBD,R4                                                        
         L     R4,AAGYTAB          GET CORRECT SPBACCT FOR THIS ACCOUNT         
REQF10   CLI   0(R4),EOF           END OF TABLE                                 
         BE    REQFX                                                            
         CLC   ORIGINUM,AGYOID     MATCH ON ORIGIN ID                           
         BNE   REQF30                                                           
*                                                                               
         ZAP   PKDUMP,=P'0'                                                     
*                                                                               
         USING BIND,R1                                                          
         L     R1,ACHKTAB                                                       
         XC    BININ,BININ         CLEAR BINTABLE #1 - CHK TABLE                
         L     R1,ABKATAB                                                       
         XC    BININ,BININ         CLEAR BINTABLE #2 - BNK ACC TABLE            
         DROP  R1                                                               
*                                                                               
         MVC   DDNAME,AGYDDNM      MOVE IN DDNAME FOR DYNALLOC                  
         MVC   DSBKNM,AGYBNKN      MOVE IN BANK NAME TO DSN                     
*        MVC   DSBKGEN(9),=C'.G0057V00'                                         
*        CLC   AGYBNKN,=C'CHS00'                                                
*        BNE   *+10                                                             
*        MVC   DSBKGEN(9),=C'.G0066V00'                                         
         MVC   SVBKNAME,AGYBKNM    SAVE OFF BANK NAME                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,AGYDTF                                                      
         LA    RF,ACCR02(RF)                                                    
         ST    RF,ADCB             SET A(BANK TAPE DCB)                         
         SR    RF,RF                                                            
         ICM   RF,3,AGYRTE                                                      
         LA    RF,ACCR02(RF)                                                    
         ST    RF,ARTN             SET A(BANK ROUTINE)                          
         MVC   FORMAT,AGYFRM                                                    
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'FF',DDNAME),(X'00',DSNAME)                      
         L     R2,ADCB                                                          
         OPEN  ((R2),(INPUT))      OPEN BANKS DTF TAPE                          
         L     RF,ARTN                                                          
         BASR  RE,RF               GOTO BANK ROUTINE                            
*                                                                               
REQF20   DS    0H                                                               
         L     R2,ADCB                                                          
         CLOSE ((R2))                                                           
* JPMPCARD ROUTINE WILL ALREADY HAVE DONE THIS CLOSE - CAUTION IF               
* PUTTING IN CODE TO CHECK THE RETURN CODE ON THE PRIOR CLOSE                   
*                                                                               
         BAS   RE,MARKIT           MARK THE TRANSACTIONS                        
         BAS   RE,CHKERR           CHECK IF THERE WERE ANY ERRORS               
*                                                                               
REQF30   LA    R4,AGYLNQ(R4)                                                    
         B     REQF10                                                           
*                                                                               
REQFX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* REQ LAST                                                           *          
**********************************************************************          
         USING PLINED,R7                                                        
REQL     DS    0H                                                               
         LA    R7,XP                                                            
         MVC   PRTLINE(PRLNQ),XSPACES                                           
         MVI   RCSUBPRG,1                                                       
*                                                                               
         LA    RE,PKFLDS           PACKED FILEDS                                
         LA    RF,PKFLDLNQ         # OF PACKED FIELDS                           
         ZAP   0(L'PKFLDS,RE),=P'0'                                             
         LA    RE,L'PKFLDS(RE)                                                  
         BCT   RF,*-10                                                          
*                                                                               
         USING SRTD,R2                                                          
         LA    R2,SRTWRK                                                        
*                                                                               
         XC    LSTBANK,LSTBANK     CLEAR BANK INFORMATION AREA                  
REQL10   GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R5,15,DMCB+4        LAST RECORD FROM SORT                        
         BZ    REQLX                                                            
         MVC   SRTREC(SRTLNQ),0(R5)                                             
*                                                                               
*        MVC   MSG,=CL10'SORTER-GET'                                            
*        GOTO1 ADUMP,DMCB,(RC),SRTREC,SRTLNQ                                    
*                                                                               
         OC    LSTBANK,LSTBANK ANY INFO?                                        
         BZ    REQL20                                                           
         CLC   LSTBANK,SRTBKNM     SAME BANK INFO AS BEFORE?                    
         BE    REQL20                                                           
         BAS   RE,CLOSEBX                                                       
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM,=CL6'TOTAL'                                              
         EDIT  PKCRAMT,PAMNTCR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-                
         EDIT  PKDRAMT,PAMNTDR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-                
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM(13),=C'RECORD COUNT :'                                   
         EDIT  PKCNT,PAMNTCR,ZERO=NOBLANK,COMMAS=YES                            
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         ZAP   PKCNT,=P'0'         RESET CHANGE COUNTER                         
         ZAP   PKCRAMT,=P'0'       RESET CREDIT ACCUMULATOR                     
         ZAP   PKDRAMT,=P'0'       RESET DEBIT ACCUMULATOR                      
         NI    FLAG,X'FF'-FLGPRNT  RESET ANYTHING PRINTED FLAG.                 
*                                                                               
REQL20   OC    SRTESTAT,SRTESTAT   ARE UP UP TO THE ERRORS?                     
         BZ    REQL30                                                           
         CLI   RCSUBPRG,2          ARE WE ALREADY RUNNING ERROR REPORT          
         BE    REQL30                                                           
         TM    FLAG,FLGPRNT        WAS ANYTHING PRINTED                         
         BNO   REQL30              NOTHING PRINTED-NO TOTALS                    
         BAS   RE,CLOSEBX                                                       
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM,=CL6'TOTAL'                                              
         EDIT  PKCRAMT,PAMNTCR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-                
         EDIT  PKDRAMT,PAMNTDR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-                
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM(13),=C'RECORD COUNT :'                                   
         EDIT  PKCNT,PAMNTCR,ZERO=NOBLANK,COMMAS=YES                            
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         ZAP   PKCNT,=P'0'         RESET CHANGE COUNTER                         
         ZAP   PKCRAMT,=P'0'       RESET CREDIT ACCUMULATOR                     
         ZAP   PKDRAMT,=P'0'       RESET DEBIT ACCUMULATOR                      
*                                                                               
REQL30   OI    FLAG,FLGPRNT        SHOW THAT SOMETHING WAS PRINTED              
         AP    PKCNT,=P'1'         INCREMENT ACCOUNT COUNT                      
         AP    PKCNTTOT,=P'1'      INCREMENT TOTAL   COUNT                      
         LA    RE,PKCRAMT          ADD UP CREDIT AMOUNT (CHECK)                 
         LA    RF,PKCRAMTT         ADD UP TOTAL CREDIT AMOUNT (CHECK)           
         TM    SRTSTAT,SRTDR       IS THIS A DEBIT?                             
         BZ    *+12                                                             
         LA    RE,PKDRAMT          ADD UP DEBIT AMOUNT (DEPOSIT)                
         LA    RF,PKDRAMTT         ADD UP TOTAL DEBIT AMOUNT (DEPOSIT)          
         AP    0(L'PKCRAMT,RE),SRTAMNT                                          
         AP    0(L'PKCRAMTT,RF),SRTAMNT                                         
*                                                                               
         MVC   LSTBANK,SRTBKNM     SAVE OFF PREVIOUS BANK INFO                  
         MVC   PCHKNUM,SRTCHKNM    MOVE IN CHECK NUMBER                         
         LA    RF,PAMNTCR          ASSUME IT'S A CREDIT (CHECK)                 
         TM    SRTSTAT,SRTDR       IS IT A DEBIT (DEPOSIT)?                     
         BZ    *+8                                                              
         LA    RF,PAMNTDR          PRINT IN DEBIT FIELD                         
         EDIT  SRTAMNT,(L'PAMNTCR,0(RF)),2,ZERO=NOBLANK,FLOAT=-,       X        
               COMMAS=YES                                                       
         GOTO1 DATCON,DMCB,(1,SRTCLRDT),(21,PCLRDTE)                            
         MVC   P2CACC,SRT2CA       CASH ACCOUNT                                 
         MVC   PCONTRA(L'SRTCONT),SRTCONT     CONTRA ACCOUNT                    
         CLC   SRTTRNDT,SPACES     ANY TRANSACTION DATE?                        
         BNH   REQL40              NO - SKIP IT                                 
         GOTO1 DATCON,DMCB,(1,SRTTRNDT),(21,PCHKDTE)                            
*                                                                               
REQL40   OC    SRTESTAT,SRTESTAT   ARE UP UP TO THE ERRORS?                     
         BZ    REQL60                                                           
         CLI   RCSUBPRG,2          ARE WE ALREADY RUNNING ERROR REPORT          
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
         MVI   RCSUBPRG,2          ERROR REPORT                                 
*                                                                               
         USING ERRTBD,R3                                                        
         L     R3,AERRTAB                                                       
         SR    R0,R0                                                            
REQL50   DS    0H                                                               
         CLI   0(R3),EOF           END OF TABLE?                                
         BE    REQL60                                                           
         MVC   BYTE,SRTESTAT       SAVE OFF SRTESTAT                            
         NC    BYTE,ERRTYP         MATCH ON ERROR                               
         BZ    REQL55                                                           
         MVC   PERR,ERRMESS        MOVE IN MESSAGE                              
         CHI   R0,3                AT THE END OF THE PRINT LINES?               
         BL    REQL52                                                           
         BAS   RE,PRINTIT                                                       
         SR    R0,R0               RESET COUNTER                                
         LA    R7,XP               RESET XP                                     
*                                                                               
REQL52   LA    R7,L'XP(R7)                                                      
         AHI   R0,1                                                             
*                                                                               
REQL55   LA    R3,ERRLNQ(R3)                                                    
         B     REQL50                                                           
         DROP  R3                                                               
*                                                                               
REQL60   LA    R7,XP               RESET XP                                     
         BAS   RE,PRINTIT                                                       
         B     REQL10                                                           
*                                                                               
REQLX    CP    PKCNT,=P'0'         ANY ACCOUNT TOTALS?                          
         BE    REQLXX                                                           
         BAS   RE,CLOSEBX                                                       
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM,=CL6'TOTAL'                                              
         EDIT  PKCRAMT,PAMNTCR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-                
         EDIT  PKDRAMT,PAMNTDR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-                
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM(13),=C'RECORD COUNT :'                                   
         EDIT  PKCNT,PAMNTCR,ZERO=NOBLANK,COMMAS=YES                            
         BAS   RE,PRINTIT                                                       
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
REQLXX   CP    PKCNTTOT,=P'0'                                                   
         BE    EXIT                                                             
         MVI   RCSUBPRG,9                                                       
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM(10),=C'RUN TOTALS'                                       
         MVC   PSECOND+5(10),=C'----------'                                     
         EDIT  PKCRAMTT,PAMNTCR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-               
         EDIT  PKDRAMTT,PAMNTDR,2,ZERO=NOBLANK,COMMAS=YES,FLOAT=-               
         MVC   EMLINE1,XP                                                       
         BAS   RE,PRINTIT                                                       
         MVC   PCHKNUM(20),=C'TOTAL RECORD COUNT :'                             
         EDIT  PKCNTTOT,PAMNTCR,ZERO=NOBLANK,COMMAS=YES                         
         MVC   EMLINE2,XP                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
*        REMOVE INTERNAL EMAIL NOTIFICATIONS PER TICKET ITMF-49870              
*        BAS   RE,SENDMAIL   NOTIFY PEOPLE REPORT IS GENERATED                  
*                                                                               
         B     EXIT                                                             
         DROP  R2,R7                                                            
         EJECT                                                                  
**********************************************************************          
* MARK THE TRANSACTIONS AS RECONCILED                                *          
**********************************************************************          
MARKIT   NTR1                                                                   
         USING BIND,R1                                                          
         L     R1,ABKATAB                                                       
         ICM   R0,15,BININ                                                      
         BZ    MRKITX                                                           
         USING BKATBD,R2                                                        
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         USING ACCTBD,R3                                                        
MRKIT10  L     R3,AACCTAB          ACCOUNT AND USER ID TABLE                    
         SR    R4,R4               TEST TABLE IS SETUP RIGHT                    
         LAY   R5,ACCTABX                                                       
         LA    RE,ACCTBLNQ                                                      
         DR    R4,RE                                                            
         LTR   R4,R4                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MRKIT20  CLI   0(R3),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    MRKIT999            READ NEXT ACCOUNT IF ANY                     
         CLC   BKAACC,ACCNUM       MATCH ON THE ACCOUNT NUMBER                  
         BNE   MRKIT99                                                          
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BNE   MRKIT99                                                          
*                                                                               
         USING TRNRECD,R4                                                       
         LA    R4,SVKEY                                                         
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCPY,RCCOMPFL           COMPANY CODE                          
         MVC   TRNKUNT(2),=C'SC'          ALWAYS SC                             
         MVC   TRNKACT(L'ACCSCA),ACCSCA   READ ONLY FOR SC ACCT IN TAB          
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     MRKIT40                                                          
*                                                                               
MRKIT30  GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
MRKIT40  CLC   SVKEY(ACTKEND),IOKEY                                             
         BNE   MRKIT99                                                          
*                                                                               
         LA    R4,IO                                                            
         CLC   IOKEY,TSTKEY                                                     
         BNE   *+8                                                              
         B     *+4                                                              
         CLC   TRNKREF,SPACES      ARE WE AT THE TRANSACTION LEVEL?             
         BNH   MRKIT30                                                          
         CLC   TRNKDATE,LSTYR      DO NOT READ PAST LAST YEAR                   
         BL    MRKIT30                                                          
*                                                                               
*        MVC   MSG,=CL10'TRNS KEY'                                              
*        GOTO1 ADUMP,DMCB,(RC),TRNKEY,L'TRNKEY                                  
*                                                                               
         USING BIND,R1                                                          
         L     R1,ACHKTAB                                                       
         ICM   R6,15,BININ                                                      
         BZ    MRKITX                                                           
         USING CHKTBD,R7                                                        
         LA    R7,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
MRKIT50  DS    0H                                                               
*        MVC   MSG,=CL10'TRNKREF'                                               
*        GOTO1 ADUMP,DMCB,(RC),TRNKREF,L'TRNKREF                                
*        MVC   MSG,=CL10'CHKNUM'                                                
*        GOTO1 ADUMP,DMCB,(RC),CHKNUM,L'CHKNUM                                  
*                                                                               
         CLC   BKAACC,CHKACC       MATCH ON THE ACCOUNT NUMBER FIRST            
         BNE   MRKIT60                                                          
         CLC   TRNKREF,CHKNUM      MATCH ON CHECK NUMBER?                       
         BE    MRKIT70                                                          
         NI    FLAG,X'FF'-FLGSKGET NEED TO DO GETREC                            
*                                                                               
MRKIT60  LA    R7,CHKLNQ(R7)                                                    
         BCT   R6,MRKIT50                                                       
         B     MRKIT30             NOT IN TABLE - READ SEQ                      
*                                                                               
MRKIT70  DS    0H                                                               
         TM    FLAG,FLGSKGET       DO WE WANT TO SKIP DOING GETREC              
         BO    MRKIT75                                                          
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         OI    FLAG,FLGSKGET       SKIP GETREC NXT TIME FOR SAME RECD           
*                                                                               
MRKIT75  NI    FLAG,X'FF'-FLGMRK   TURN OFF MARKED FLAG                         
*                                                                               
         USING TRNELD,R5                                                        
         LR    R5,R4               POINT R5 TO ELEMENTS                         
         AH    R5,DISP2            BUMP TO FIRST ELEMET                         
         CP    TRNAMNT,CHKAMNT     MAKE SURE THAT THE AMNT IS THE SAME          
         BNE   MRKIT60             IF NOT READ NEXT                             
         NI    FLAG,X'FF'-FLGSKGET NEED TO DO GETREC NEXT TIME                  
*                                                                               
*        MVC   MSG,=CL10'BINTAB #3'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R7),L'CHKWRK                                    
*                                                                               
*        MVC   MSG,=CL10'TRNS IN'                                               
*        SR    RE,RE                                                            
*        ICM   RE,3,TRNRLEN                                                     
*        GOTO1 ADUMP,DMCB,(RC),(R4),(RE)                                        
*                                                                               
*                                                                               
* FOUND THE RIGHT TRANSACTION MARK IT RECONCILED IN THE FOLLOWING SPOTS         
*       - TURN ON THE TRNSBREC BIT IN TRNSTAT IN THE X'44'                      
*       - SAVE THE STATEMENT DATE IN THE TRSBSTDT FIELD IN THE X'60'            
*       - SAVE RECONCILED DATE IN GDADATE FIELD IN X'E5'                        
*       - SAVE CLEARED (PAID) DATE IN GDADATE2 FIELD IN X'E5'                   
*                                                                               
         TM    TRNSTAT,TRNSBREC    WAS THIS MARKED                              
         BNO   MRKIT80             IF ALREADY MARKED - CONTINUE                 
         OI    FLAG,FLGMRK         ELSE CHECK TO MAKE SURE DATES MATCH          
MRKIT80  OI    TRNSTAT,TRNSBREC    MARKED RECONCILED                            
         OI    CHKSTAT,CHKMRK      MARK TABLE ENTRY AS RECONCILED               
         DROP  R5                                                               
*                                                                               
         TM    CHKSTAT,CHKVDER     SKIP VOIDS?                                  
         BO    MRKIT130                                                         
*                                                                               
         LR    R5,R4               RESET R5 BACK TO KEY                         
         MVI   ELCODE,TRSELQ       X'60'-TRANSACTION STATUS ELM                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRSELD,R5                                                        
         TM    FLAG,FLGMRK         WAS TRN MARKED ALREADY?                      
         BNO   *+12                                                             
         CLI   QOPT2,C'Y'          SKIP PREVIOUSLY MARKED ITEMS                 
         BNE   *+14                                                             
         MVC   TRSBSTDT,CHKSTDTE   SAVE OFF STATEMENT DATE                      
         B     MRKIT90                                                          
         CLC   TRSBSTDT,CHKSTDTE   IS STATEMENT DATE THE SAME?                  
         BE    MRKIT90                                                          
         OI    CHKSTAT,CHKDTERR    MARK AS ERROR                                
         B     MRKIT130                                                         
         DROP  R5                                                               
*                                                                               
MRKIT90  LR    R5,R4                                                            
         MVI   ELCODE,GDAELQ       X'E5'-GENERAL DATE ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    MRKIT100                                                         
         TM    FLAG,FLGMRK         WAS TRN MARKED ALREADY?                      
         BNO   *+12                                                             
         OI    CHKSTAT,CHKDTERR    MARK AS ERROR                                
         B     MRKIT130                                                         
         BAS   RE,BLDE5            BUILD DATE ELELMENT                          
         B     MRKIT120                                                         
*                                                                               
         USING GDAELD,R5                                                        
MRKIT100 TM    FLAG,FLGMRK         WAS TRN MARKED ALREADY?                      
         BNO   MRKIT110                                                         
         CLI   QOPT2,C'Y'          DO WE WANT TO MARK IT NOW                    
         BE    MRKIT110                                                         
         CLC   GDADATE,TODAYP      WAS IT MARKED TODAY                          
         BNE   *+14                                                             
         CLC   GDADATE2,CHKPDDTE   IS PAID DATE THE SAME                        
         BE    MRKIT110                                                         
*                                                                               
         OI    CHKSTAT,CHKDTERR    MARK AS ERROR                                
         B     MRKIT130                                                         
*                                                                               
MRKIT110 MVC   GDADATE,TODAYP      SAVE OFF RECONCILED DATE                     
         MVC   GDADATE2,CHKPDDTE   SAVE OFF CLEARED DATE                        
         DROP  R5                                                               
*                                                                               
MRKIT120 DS    0H                                                               
         MVC   MSG,=CL10'TRNS OUT'                                              
         SR    RE,RE                                                            
         ICM   RE,3,TRNRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R4),(RE)                                        
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   MRKIT130                                                         
         CLI   RCWRITE,C'N'                                                     
         BE    MRKIT130                                                         
         GOTO1 =A(DMPUTREC),DMCB,(RC)                                           
*                                                                               
* PUT TO SORTER                                                                 
*                                                                               
         USING SRTD,R5                                                          
MRKIT130 LA    R5,SRTWRK                                                        
         MVC   SRTREC(SRTLNQ),SPACES      CLEAR WORK AREA                       
         MVC   SRTSTAT,CHKSTAT     TURN ON VOIDS AND OTHERS                     
         MVI   SRTESTAT,0                                                       
         TM    SRTSTAT,CHKDTERR    DID WE GET A DATE ERROR ON THIS?             
         BNO   *+8                                                              
         OI    SRTESTAT,SRTNDTE    SET STATUS TO SHOW DATE ERROR                
         TM    SRTSTAT,CHKVDER     VOIDS AS ERRORS? (FOR FMFM)                  
         BNO   *+8                                                              
         OI    SRTESTAT,SRTVDER                                                 
         TM    SRTSTAT,CHKMRK      WAS THIS MARKED                              
         BO    *+8                                                              
         OI    SRTESTAT,SRTNMRK    MARK UNMARKED                                
         MVC   SRTBKNM,CHKBKNM     BANK NAME                                    
         MVC   SRTBKACC,CHKACC     BANK ACCOUNT NUMBER                          
         MVC   SRTBKDTE,CHKSTDTE   BANK STATEMENT DATE                          
         MVC   SRTCHKNM,CHKNUM     CHECK NUMBER                                 
         MVC   SRTCLRDT,CHKPDDTE   BANK CHECK DATE                              
         MVC   SRT2CA,TRNKACT      CASH ACCOUNT                                 
         MVC   SRTCONT,TRNKULC     CONTRA ACCOUNT W/ U/L                        
         MVC   SRTTRNDT,TRNKDATE   TRANSACTION DATE                             
         ZAP   SRTAMNT,CHKAMNT     CHECK AMOUNT                                 
*                                                                               
*        MVC   MSG,=CL10'SORTER-PUT'                                            
*        GOTO1 ADUMP,DMCB,(RC),SRTWRK,SRTLNQ                                    
*                                                                               
         GOTO1 VSORTER,DMCB,=C'PUT',SRTREC                                      
         B     MRKIT30                                                          
*                                                                               
MRKIT99  LA    R3,ACCTBLNQ(R3)     BUMP TO NEXT ENTRY                           
         B     MRKIT20                                                          
*                                                                               
MRKIT999 LA    R2,BKALNQ(R2)       BUMP TO NEXT ENTRY                           
         BCT   R0,MRKIT10                                                       
*                                                                               
MRKITX   B     EXIT                                                             
         DROP  R2,R3,R4,R5,R7                                                   
         EJECT                                                                  
**********************************************************************          
* CHECK THE CHECK TABLE FOR ANY ERRORS - UNRECONCILED CHECKS         *          
**********************************************************************          
CHKERR   NTR1                                                                   
         USING BIND,R1                                                          
         L     R1,ACHKTAB                                                       
         ICM   R0,15,BININ                                                      
         BZ    CHKERRX                                                          
         USING CHKTBD,R2                                                        
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
CHKERR10 TM    CHKSTAT,CHKMRK      WAS CHECK MARKED RECONCILED?                 
         BO    CHKERR20                                                         
*                                                                               
* PUT TO SORTER                                                                 
*                                                                               
         USING SRTD,R5                                                          
         LA    R5,SRTWRK                                                        
         MVC   SRTREC(SRTLNQ),SPACES      CLEAR WORK AREA                       
         MVI   SRTESTAT,SRTNMRK    MARK IT AS AN ERROR                          
         MVC   SRTSTAT,CHKSTAT     TURN ON VOIDS AND OTHERS                     
         MVC   SRTBKNM,CHKBKNM     BANK NAME                                    
         MVC   SRTBKACC,CHKACC     BANK ACCOUNT NUMBER                          
         MVC   SRTBKDTE,CHKSTDTE   BANK STATEMENT DATE                          
         MVC   SRTCLRDT,CHKPDDTE   BANK CHECK DATE                              
         MVC   SRTCHKNM,CHKNUM     CHECK NUMBER                                 
         ZAP   SRTAMNT,CHKAMNT     CHECK AMOUNT                                 
         GOTO1 VSORTER,DMCB,=C'PUT',SRTREC                                      
*                                                                               
CHKERR20 LA    R2,CHKLNQ(R2)       BUMP TO NEXT ENTRY                           
         BCT   R0,CHKERR10                                                      
*                                                                               
CHKERRX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD E5 ELEMENT (IF NOT ON RECORD)                                *          
*       R7 - ADDRESS F CHECK TABLE ENTRY                             *          
**********************************************************************          
         USING GDAELD,R4                                                        
         USING CHKTBD,R7                                                        
BLDE5    NTR1                                                                   
         LA    R4,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   GDAEL,GDAELQ        X'E5' ELEMENT                                
         MVI   GDALN,GDALN2Q                                                    
         MVI   GDATYPE,GDATRECN    BANKREC GDADATE=DATE RECONCILED              
         MVC   GDADATE,TODAYP      SAVE OFF RECONCILED DATE                     
         MVC   GDADATE2,CHKPDDTE   SAVE OFF CLEARED DATE                        
         BAS   RE,ADDL                                                          
*                                                                               
BLDE5X   B     EXIT                                                             
         DROP  R4,R7                                                            
         EJECT                                                                  
***********************************************************************         
* CLOSE BOX                                                           *         
***********************************************************************         
         USING BOXD,R4                                                          
CLOSEBX  NTR1                                                                   
         L     R4,ADBOX                                                         
         MVI   BOXREQ,C'C'          CLOSE BOX                                   
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ADD ELEMENT                                                         *         
*     R4 = A(ELEMENT)                                                 *         
***********************************************************************         
ADDL     NTR1                                                                   
         LA    R2,IO                                                            
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),(R2),(R4)                              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* PRINT THE DATA                                                      *         
***********************************************************************         
PRINTIT  NTR1                                                                   
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* NOTIFY PEOPLE AUTOMATICALLY THAT REPORT WAS GENERATED              *          
**********************************************************************          
         USING MAILTBD,R2                                                       
SENDMAIL NTR1                                                                   
*                                                                               
         LA    R2,MAILTAB                                                       
SENDM10  CLI   MAILAGY,EOF                                                      
         BE    SENDMX                                                           
         CLC   MAILAGY,ORIGINUM    ARE WE AT THE RIGHT AGENCY                   
         BE    SENDM20                                                          
         LA    R2,MAILNQ(R2)       BUMP TO NEXT ENTRY                           
         B     SENDM10                                                          
*                                                                               
SENDM20  DS    0H                                                               
         CLI   MAILADD,C' '        IF NO EMAIL RECIPIENTS EXIT                  
         BE    SENDMX                                                           
         MVC   SUBJUSID,MAILUID              CONNECT ID OF THE AGENCY           
         GOTO1 DATCON,DMCB,(0,TODAY),(5,SUBJDATE)                               
*                                                                               
         LA    RF,MAILADD                                                       
         LA    RF,L'MAILADD-1(RF)                                               
         LHI   R1,L'MAILADD        GET LENGTH OF THE ADDRESS FIELD              
SENDM30  CLI   0(RF),C' '          IS IT A SPACE                                
         BNE   SENDM40                                                          
         BCTR  RF,0                SUBTRACT ONE                                 
         BCT   R1,SENDM30          DECREASE LENGTH                              
         B     SENDM60             NO ADDRESS FOUND IN TABLE ENTRY              
*                                                                               
SENDM40  DS    0H                                                               
*        LA    R3,TOWHO            POINT TO DEFAULT RECIPIENT LIST              
*        LA    R3,L'TOWHO-1(R3)    POINT TO ':' IN TOWHO                        
*        MVI   0(R3),C','          REPLACE ':' WITH ','                         
         LA    R3,TOWHO1                                                        
         BCTR  R1,0                LENGTH TO MOVE MINUS 1                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),MAILADD                                                  
         LA    R3,1(R1,R3)         BUMP R3                                      
SENDM50  MVI   0(R3),COLON                                                      
*                                                                               
SENDM60  L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         MVC   BODYJBNM(8),MCJOB   PRINT JOB NAME                               
         DROP  RF                                                               
*                                                                               
         CLI   QOPT1,C'Y'                                                       
         BNE   SENDMX                                                           
         CLI   RCWRITE,C'N'                                                     
         BE    SENDMX                                                           
*                                                                               
         GOTOR VSMTP,DMCB,('SMTPAINI',JESMAIL)                                  
         GOTOR VSMTP,DMCB,('SMTPAPRS',TOWHO1),(L'SUBJDSC,SUBJDSC)               
         GOTOR VSMTP,DMCB,('SMTPAPTL',BODYLIN1)                                 
*        GOTOR VSMTP,DMCB,('SMTPAPTL',EMLINE1)                                  
*        GOTOR VSMTP,DMCB,('SMTPAPTL',EMLINE2)                                  
         GOTOR VSMTP,DMCB,('SMTPASND',0)                                        
         GOTOR VSMTP,DMCB,('SMTPAEND',0) DETACH SMTP                            
         MVC   EMLINE1,XSPACES                                                  
         MVC   EMLINE2,XSPACES                                                  
SENDMX   B     EXIT                                                             
*                                                                               
JESMAIL  DC    CL8'JESMAIL '                                                    
COLON    EQU   C':'                                                             
*                                                                               
TOWHO    DC    C'JSHA,SSHI,MNAS:'                                               
TOWHO1   DC    CL45' '                                                          
*                                                                               
BODYLIN1 DC    0CL80                                                            
BODYJBNM DC    CL8'        '                                                    
SUBJDSC  DC    0CL72                                                            
         DC    CL28'ACR REPORT WAS GENERATED ON '                               
SUBJDATE DC    CL8' '                      TODAY'S DATE                         
         DC    CL5' FOR '                                                       
SUBJUSID DC    CL8' '                      CONNECT ID                           
         DC    CL(L'BODYLIN1-(*-BODYLIN1))' '     SPARE SPACES                  
*                                                                               
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
**********************************************************************          
* SORTER DEFINITIONS                                                 *          
**********************************************************************          
SORTCARD DC    CL80'SORT FIELDS=(1,49,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=90'                                    
         EJECT                                                                  
**********************************************************************          
* CONSTANTS & BUFFER                                                 *          
**********************************************************************          
ABNDTF   DCB   DDNAME=ABNDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=180,BLKSIZE=27900                                          
*                                                                               
CHSDTF   DCB   DDNAME=CHSDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=67,BLKSIZE=27939                                           
*                                                                               
CITDTF   DCB   DDNAME=CITDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=58,BLKSIZE=5800                                            
*                                                                               
CITIDTF  DCB   DDNAME=CITIDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,   X        
               LRECL=120,BLKSIZE=27000                                          
*                                                                               
CT80DTF  DCB   DDNAME=CT80DTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,   X        
               LRECL=80,BLKSIZE=8000                                            
*                                                                               
CT200DTF DCB   DDNAME=CT200DTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,  X        
               LRECL=200,BLKSIZE=27800                                          
*                                                                               
FUNDTF   DCB   DDNAME=FUNDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=82,BLKSIZE=27962                                           
*                                                                               
* COMDTF IS A COMMONDTF DCB STATEMENT FOR ALL DYNALLOC                          
COMDTF   DCB   DDNAME=COMDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=80,BLKSIZE=27920                                           
*                                                                               
CTZDTF   DCB   DDNAME=CTZDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=81,BLKSIZE=27945                                           
*                                                                               
FLTDTF   DCB   DDNAME=FLTDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=80,BLKSIZE=8000                                            
*                                                                               
FRKDTF   DCB   DDNAME=FRKDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=80,BLKSIZE=1600                                            
*                                                                               
SCODTF   DCB   DDNAME=SCODTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=80,BLKSIZE=27920                                           
*                                                                               
WACDTF   DCB   DDNAME=WACDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=82,BLKSIZE=28700                                           
*                                                                               
HSBDTF   DCB   DDNAME=HSBDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,    X        
               LRECL=57,BLKSIZE=27987                                           
*                                                                               
HS300DTF DCB   DDNAME=HS300DTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,  X        
               LRECL=300,BLKSIZE=27900                                          
*                                                                               
CH200DTF DCB   DDNAME=CH200DTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,  X        
               LRECL=200,BLKSIZE=27800                                          
*                                                                               
CH40DTF  DCB   DDNAME=CH40DTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,   X        
               LRECL=40,BLKSIZE=6200                                            
*                                                                               
AOPSDTF  DCB   DDNAME=AOPSDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,   X        
               LRECL=120,BLKSIZE=27000                                          
*                                                                               
KEYBDTF  DCB   DDNAME=KEYBDTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,   X        
               LRECL=90,BLKSIZE=9990                                            
*DSFTK-150                                                                      
CH108DTF DCB   DDNAME=CH108DTF,RECFM=FB,DSORG=PS,EODAD=EXIT,MACRF=GM,  X        
               LRECL=108,BLKSIZE=0                                              
*                                                                               
OUT      DCB   DDNAME=OUT,RECFM=FB,DSORG=PS,EODAD=JPMXIT,MACRF=GM,     X        
               LRECL=108,BLKSIZE=0                                              
*                                                                               
*DSFTK-150                                                                      
*                                                                               
*                                                                               
DSNAME   DS    0CL30               DATASET NAME FOR DYNALLOC                    
         DC    C'ACCTAPE.ACR'                                                   
DSBKNM   DC    CL5' '              BANK NAME                                    
*        DC    CL15' '                                                          
DSBKGEN  DC    C'(0)'                                                           
         DC    CL11' '                                                          
*                                                                               
ACCMST   DC    CL8'ACCMST  '                                                    
ABOXRC   DC    A(BOXRC)                                                         
ABXHOOK  DC    A(BXHOOK)                                                        
*                                                                               
ADCONS   DS    0F                                                               
         DC    A(DUMP)             DUMP ROUTINE                                 
         DC    A(BINADD)           ADD TO THE BIN TABLE                         
         DC    A(AGNCYTAB)         ADDRESS OF AGENCY TABLE                      
         DC    A(ERRTAB)           ADDRESS OF ERROR TABLE                       
         DC    A(ACCTAB)           ADDRESS OF BANK ACCOUNT TABLE                
         DC    A(FRMTAB)           ADDRESS OF FORMAT TABLE                      
*                                                                               
         DC    V(CASHVAL)                                                       
         DC    V(PRNTBL)                                                        
         DC    V(HELLO)                                                         
         DC    V(SORTER)                                                        
         DC    V(HEXIN)                                                         
         EJECT                                                                  
***********************************************************************         
* GETEL                                                               *         
***********************************************************************         
         GETEL R5,DISP2,ELCODE                                                  
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TEST KEY FOR IDF                                                   *          
**********************************************************************          
TSTKEY   DC    0CL42                                                            
         DC    X'5E'                                                            
         DC    CL16'SCM002'                                                     
         DC    X'5E'                                                            
         DC    CL14'SPM86763716'                                                
         DC    XL3'A50121'                                                      
TSTNUM   DC    CL6'205013'                                                      
         DC    X'01'                                                            
         EJECT                                                                  
**********************************************************************          
* MAIL ID TABLE FOR AUTOMATIC NOTIFICATION                           *          
**********************************************************************          
MAILTAB  DS    0CL(MAILNQ)                                                      
*                                                                               
         DC    AL2(0909)                 ORIGIN ID                              
         DC    CL8'MCLO'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(6029)                 ORIGIN ID                              
         DC    CL8'YNME'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14741)                ORIGIN ID                              
         DC    CL8'MGNYF'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15116)                ORIGIN ID                              
         DC    CL8'MGCTF'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15152)                ORIGIN ID                              
         DC    CL8'MSDAF'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14742)                ORIGIN ID                              
         DC    CL8'SENYF'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(10653)                ORIGIN ID                              
         DC    CL8'FMNYCK'               CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14884)                ORIGIN ID                              
         DC    CL8'HMNY'                 CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(6884)                 ORIGIN ID                              
         DC    CL8'CARNY'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7348)                 ORIGIN ID                              
         DC    CL8'ICCNJ'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(3680)                 ORIGIN ID                              
         DC    CL8'DNTOA'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14782)                ORIGIN ID                              
         DC    CL8'URBAN'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7602)                 ORIGIN ID                              
         DC    CL8'OMDTOA'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14666)                ORIGIN ID CL#0107361T                  
         DC    CL8'FPHDTOA'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(18546)                ORIGIN ID DSFTK-268                    
         DC    CL8'FHSCHAC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14669)                ORIGIN ID CL#0107361T                  
         DC    CL8'FTOUMO'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(3309)                 ORIGIN ID                              
         DC    CL8'MKEXPD'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(2826)                 ORIGIN ID                              
         DC    CL8'MKTOAB'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(13493)                ORIGIN ID                              
         DC    CL8'MKJPAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(9184)                 ORIGIN ID                              
         DC    CL8'PNMDAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11554)                ORIGIN ID                              
         DC    CL8'PNPPAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(9183)                 ORIGIN ID                              
         DC    CL8'PNNLAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(9945)                 ORIGIN ID                              
         DC    CL8'PWCTOAC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7603)                 ORIGIN ID                              
         DC    CL8'OMDCOR'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7621)                 ORIGIN ID                              
         DC    CL8'BBOMAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7623)                 ORIGIN ID                              
         DC    CL8'TBOMAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7624)                 ORIGIN ID                              
         DC    CL8'PJOMTAC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7626)                 ORIGIN ID                              
         DC    CL8'PNTOAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7627)                 ORIGIN ID                              
         DC    CL8'PJOMVAC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7629)                 ORIGIN ID                              
         DC    CL8'ANOMAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8442)                 ORIGIN ID                              
         DC    CL8'FMMPI'                CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8566)                 ORIGIN ID                              
         DC    CL8'FMMC'                 CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8602)                 ORIGIN ID                              
         DC    CL8'MSHTOA'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14108)                ORIGIN ID CL#0105014T                  
         DC    CL8'MSHQU'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15123)                ORIGIN ID CL#0109072T                  
         DC    CL8'MSHUNI'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8722)                 ORIGIN ID CL#0111793T                  
         DC    CL8'MSHTOAC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15898)                ORIGIN ID CL#0111793T                  
         DC    CL8'MAXUS'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16817)                ORIGIN ID DSFTK-32                     
         DC    CL8'MAXTOA'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8685)                 ORIGIN ID                              
         DC    CL8'OMDUSA'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8594)                 ORIGIN ID                              
         DC    CL8'MSNYA'                CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8880)                 ORIGIN ID                              
         DC    CL8'TOTMAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8945)                 ORIGIN ID                              
         DC    CL8'MSCON'                CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(9211)                 ORIGIN ID                              
         DC    CL8'MVSEC'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(0031)                 ORIGIN ID                              
         DC    CL8'TLDA'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(4617)                 ORIGIN ID                              
         DC    CL8'ZENYA'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(9640)                 ORIGIN ID                              
         DC    CL8'ZDCA'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(9968)                 ORIGIN ID                              
         DC    CL8'DNQT'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(10482)                ORIGIN ID                              
         DC    CL8'OSIN'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(10596)                ORIGIN ID                              
         DC    CL8'FMFM'                 CONNECT ID                             
         DC    CL45'MBLA'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(10690)                ORIGIN ID                              
         DC    CL8'DAVACC'               CONNECT ID                             
         DC    CL45'JCOR'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(10986)                ORIGIN ID                              
         DC    CL8'TOTTAC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11054)                ORIGIN ID                              
         DC    CL8'ZDCACK'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11335)                ORIGIN ID                              
         DC    CL8'MSNYMONE'             CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11348)                ORIGIN ID                              
         DC    CL8'HNKNY'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11397)                ORIGIN ID                              
         DC    CL8'MSCONMO'              CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11553)                ORIGIN ID                              
         DC    CL8'MCLOU'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11592)                ORIGIN ID                              
         DC    CL8'MCACC'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15027)                ORIGIN ID  CL#0268480N                 
         DC    CL8'OMLOUCK'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11604)                ORIGIN ID                              
         DC    CL8'MSNYMAX'              CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11682)                ORIGIN ID                              
         DC    CL8'OSZNYA'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11698)                ORIGIN ID                              
         DC    CL8'MSCONMX'              CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11796)                ORIGIN ID                              
         DC    CL8'OMDQEAC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11797)                ORIGIN ID                              
         DC    CL8'OMDMTAC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11864)                ORIGIN ID                              
         DC    CL8'HNKDO'                CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11875)                ORIGIN ID                              
         DC    CL8'MSME'                 CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11876)                ORIGIN ID                              
         DC    CL8'MSCONME'              CONNECT ID                             
         DC    CL45'JSHI'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11900)                ORIGIN ID                              
         DC    CL8'MVNYCK'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11901)                ORIGIN ID                              
         DC    CL8'HALCK'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11905)                ORIGIN ID                              
         DC    CL8'BROMCK'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11906)                ORIGIN ID                              
         DC    CL8'STARCK'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11908)                ORIGIN ID                              
         DC    CL8'STACHCK'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11909)                ORIGIN ID                              
         DC    CL8'GMMCK'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11910)                ORIGIN ID                              
         DC    CL8'KLKCK'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11981)                ORIGIN ID                              
         DC    CL8'OSSECRE'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(12036)                ORIGIN ID                              
         DC    CL8'OSDNYRE'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(12161)                ORIGIN ID                              
         DC    CL8'OSNYCK'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(12319)                ORIGIN ID                              
         DC    CL8'MUWS'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*        DC    AL2(12361)          CL#0144329N    ORIGIN ID                     
*        DC    CL8'BEAP'                          CONNECT ID                    
*        DC    CL45' '                            E-MAIL ADDR                   
*                                                                               
         DC    AL2(12449)                ORIGIN ID                              
         DC    CL8'MCMOMCK'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(12607)                ORIGIN ID                              
         DC    CL8'OGCK'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(12615)                ORIGIN ID                              
         DC    CL8'ICCPA'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(13013)                ORIGIN ID                              
         DC    CL8'MECK'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15330)                ORIGIN ID                              
         DC    CL8'MCNYF'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(13484)                ORIGIN ID                              
         DC    CL8'ICCAL'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(13609)                ORIGIN ID                              
         DC    CL8'EUROMED'              CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*        DC    AL2(7567)                 ORIGIN ID                              
*        DC    CL8'JWTOAC'               CONNECT ID                             
*        DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14215)                ORIGIN ID CL#0106470T                  
         DC    CL8'NEOTO'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(3426)                 ORIGIN ID                              
         DC    CL8'HLTOH'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(6885)                 ORIGIN ID                              
         DC    CL8'CARSEC'               CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15111)                ORIGIN ID                              
         DC    CL8'CARWY'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15507)                ORIGIN ID                              
         DC    CL8'CARRML'               CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8800)                 ORIGIN ID                              
         DC    CL8'CARPF'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15099)                ORIGIN ID                              
         DC    CL8'CARDB'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15233)                ORIGIN ID                              
         DC    CL8'CARDBNY'              CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(7033)                 ORIGIN ID                              
         DC    CL8'CARLA'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15663)                ORIGIN ID DSFTK-104                    
         DC    CL8'CARTHD'               CONNECT ID                             
         DC    CL45'MBLA'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17458)                ORIGIN ID DSFTK-104                    
         DC    CL8'CARCSI'               CONNECT ID                             
         DC    CL45'MBLA'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(12156)                ORIGIN ID                              
         DC    CL8'VIZLA'                CONNECT ID                             
         DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(10368)                ORIGIN ID                              
         DC    CL8'MVCTOA'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(9388)                 ORIGIN ID                              
         DC    CL8'STWTO'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(13380)                ORIGIN ID                              
         DC    CL8'ZOTOA'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(3586)                 ORIGIN ID                              
         DC    CL8'SOTOT'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15224)                ORIGIN ID                              
         DC    CL8'POSTACC'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17228)                ORIGIN ID   DSFTK-76                   
         DC    CL8'YRAKD'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17537)                ORIGIN ID DSFTK-148/DSFTK-149          
         DC    CL8'YRBRD'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(11587)                ORIGIN ID                              
         DC    CL8'YRYTD'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14168)                ORIGIN ID                              
         DC    CL8'YRYQD'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(8441)                 ORIGIN ID SHGMO                        
         DC    CL8'SHGMO'                CONNECT ID #0114178T/0111707T          
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
                                                                                
         DC    AL2(13646)                ORIGIN ID SHMF                         
         DC    CL8'SHMF'                 CONNECT ID #0114178T/0111707T          
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(13915)                ORIGIN ID CL#0309345N                  
         DC    CL8'GGNY'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*        DC    AL2(13504)                ORIGIN ID                              
*        DC    CL8'PHDSAF'               CONNECT ID                             
*        DC    CL45'RBLI,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14836)                ORIGIN ID CL#0309851N                  
         DC    CL8'GROUPMCO'             CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15676)                ORIGIN ID CL#0326725N                  
         DC    CL8'FMAM'                 CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15672)                ORIGIN ID CL#0325289N                  
         DC    CL8'CARDIS'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16201)                ORIGIN ID CL#0361103N                  
         DC    CL8'CARGMCK'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16477)                ORIGIN ID CL#0378135N                  
         DC    CL8'CARDECK'              CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16487)                ORIGIN ID CL#0385154N                  
         DC    CL8'CARMCCK'              CONNECT ID                             
         DC    CL45'MBLA'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15850)                ORIGIN ID CL#0339640N                  
         DC    CL8'FMMT'                 CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15911)                ORIGIN ID CL#0345342N                  
         DC    CL8'FMMCMT'               CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15803)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGCK'                CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15793)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGERME'              CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15794)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGHMI'               CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15800)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGMPI'               CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15795)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGMC'                CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15790)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGAM'                CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16379)                ORIGIN ID CL#0373065N                  
         DC    CL8'MPGIGNCK'             CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15851)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGMT'                CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15953)                ORIGIN ID CL#0341108N                  
         DC    CL8'MPGMCMT'              CONNECT ID                             
         DC    CL45'BREL,MBLA'           COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16613)                ORIGIN ID CL#0396050N                  
         DC    CL8'HVMSOC'               CONNECT ID HVMSOC                      
         DC    CL45'MBLA'                COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16238)                ORIGIN ID CL#0114774T                  
         DC    CL8'GMXAS'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*        DC    AL2(14810)                ORIGIN ID CL#0114774T                  
*        DC    CL8'GRPMTO'               CONNECT ID                             
*        DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16210)                ORIGIN ID CL#0114988T                  
         DC    CL8'MCMMP'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16208)                ORIGIN ID CL#0114988T                  
         DC    CL8'MCMMC'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16209)                ORIGIN ID CL#0114988T                  
         DC    CL8'MCMMB'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(12612)                ORIGIN ID CL#0117826T                  
         DC    CL8'YRTOMC'               CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16557)                ORIGIN ID CL#0392997N                  
         DC    CL8'TPPS'                 CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14346)                ORIGIN ID DSFTK-67                     
         DC    CL8'OUTTO'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16820)                ORIGIN ID DSFTK-67                     
         DC    CL8'MECEL'                CONNECT ID                             
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14810)                ORIGIN ID DSFTK-84                     
         DC    CL8'GRPMTO'               CONNECT ID GRPMTO                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*        DC    AL2(16817)                ORIGIN ID DSFTK-84                     
*        DC    CL8'MAXTOA'               CONNECT ID MAXTOA                      
*        DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16209)                ORIGIN ID DSFTK-84                     
         DC    CL8'MCMMB'                CONNECT ID MCMMB                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(10823)                ORIGIN ID DSFTK-84                     
         DC    CL8'YRTOME'               CONNECT ID YRTOME                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16208)                ORIGIN ID DSFTK-84                     
         DC    CL8'MCMMC'                CONNECT ID MCMMC                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16243)                ORIGIN ID DSFTK-84                     
         DC    CL8'MCMMF'                CONNECT ID MCMMF                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(08602)                ORIGIN ID DSFTK-84                     
         DC    CL8'MSHTOA'               CONNECT ID  MSHTOA                     
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14108)                ORIGIN ID DSFTK-84                     
         DC    CL8'MSHQU'                CONNECT ID MSHQU                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(15123)                ORIGIN ID DSFTK-84                     
         DC    CL8'MSHUNI'               CONNECT ID MSHUNI                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(14346)                ORIGIN ID DSFTK-84                     
         DC    CL8'OUTTO'                CONNECT ID OUTTO                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16210)                ORIGIN ID DSFTK-84                     
         DC    CL8'MCMMP'                CONNECT ID MCMMP                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16819)                ORIGIN ID DSFTK-84                     
         DC    CL8'MECEM'                CONNECT ID MECEM                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17242)                ORIGIN ID DSFTK-125                    
         DC    CL8'MNSTOA'               CONNECT ID MNSTOA                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17243)                ORIGIN ID DSFTK-125                    
         DC    CL8'MNSQU'                CONNECT ID MNSQU                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17254)                ORIGIN ID DSFTK-125                    
         DC    CL8'MNSUNI'               CONNECT ID MNSUNI                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(18795)                ORIGIN ID SPEC-10303                   
         DC    CL8'MIEXCA'               CONNECT ID MIEXCA                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(18626)                ORIGIN ID SPEC-10303                   
         DC    CL8'SIXTOA'               CONNECT ID SIXTOA                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17345)          DSFTK-132-MDCMP/DSFTK-168 ASMAC              
         DC    CL8'ASMAC'                CONNECT ID MDCMP/ASMAC                 
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(18837)                SPEC-13531                             
         DC    CL8'ALCHS'                CONNECT ID ALCHS                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19059)                SPEC-14486                             
         DC    CL8'BRNAC'                CONNECT ID BRNAC                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
                                                                                
*                                                                               
         DC    AL2(17435)                ORIGIN ID DSFTK-109                    
         DC    CL8'PATOWD'               CONNECT ID PATOWD                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(16354)                ORIGIN ID DSFTK-156                    
         DC    CL8'WNPO'                 CONNECT ID WNPO                        
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(17355)                ORIGIN ID DSFTK-164                    
         DC    CL8'TRXAC'                CONNECT ID TRXAC                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19545)                SPEC-17408                             
         DC    CL8'WAVEL'                CONNECT ID WAVEL                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19546)                SPEC-17408                             
         DC    CL8'WAVELCO'              CONNECT ID WAVELCO                     
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19487)                SPEC-17409                             
         DC    CL8'WAVETO'               CONNECT ID WAVETO                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19541)                SPEC-17409                             
         DC    CL8'WAVETCO'              CONNECT ID WAVETCO                     
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19542)                SPEC-17409                             
         DC    CL8'WAVETOE'              CONNECT ID WAVETOE                     
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19468)                SPEC-17582                             
         DC    CL8'ESSTOA'               CONNECT ID ESSTOA                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19690)                SPEC-21410                             
         DC    CL8'NWOTOA'               CONNECT ID NWOTOA                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(19543)                SPEC-19359                             
         DC    CL8'WAVETBA'              CONNECT ID WAVETBA                     
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*SPEC-28991                                                                     
         DC    AL2(02635)                SPEC-28991                             
         DC    CL8'DDSB'                 CONNECT ID DDSB                        
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
         DC    AL2(02651)                SPEC-28991                             
         DC    CL8'DDS2'                 CONNECT ID DDS2                        
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*SPEC-28991                                                                     
*SPEC-34556                                                                     
         DC    AL2(18375)                SPEC-34556                             
         DC    CL8'TSMCARD'              CONNECT ID TSMCARD                     
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*SPEC-34556                                                                     
         DC    AL2(20103)                SPEC-31990                             
         DC    CL8'FOMGAC'               CONNECT ID FOMGAC                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*MN SPEC-46427                                                                  
         DC    AL2(20867)                SPEC-46427                             
         DC    CL8'OPNTOA'               CONNECT ID OPNTOA                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
*                                                                               
*MN SPEC-46427                                                                  
*MN SPEC-37368                                                                  
         DC    AL2(14664)                SPEC-37368                             
         DC    CL8'PHDTOA'               CONNECT ID PHDTOA                      
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
                                                                                
         DC    AL2(14659)                SPEC-37368                             
         DC    CL8'TOUMO'                CONNECT ID TOUMO                       
         DC    CL45' '                   COMMA SEPARATED E-MAIL ADDR            
                                                                                
*MN SPEC-37368                                                                  
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* ERROR TABLE                                                        *          
**********************************************************************          
ERRTAB   DS    0CL26                                                            
         DC    X'80',CL25'CHECK WAS NOT MARKED'                                 
         DC    X'40',CL25'TRNS MARKED/DATE MISMATCH'                            
         DC    X'20',CL25'CHECK IS A VOID'                                      
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* AGENCY TABLE                                                       *          
*        CONSISTS OF ORIGIN ID AND BANK ROUTINE                      *          
**********************************************************************          
AGNCYTAB DS    0H                                                               
         DC    AL2(17)             ORIGIN ID - SJR                              
         DC    AL2(FORK-ACCR02)    FORK BANK ROUTINE                            
         DC    AL2(FRKDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'FRK00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'FRKDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'FORK'                 BANK NAME                             
*                                                                               
         DC    AL2(0031)           ORIGIN ID - TLDA                             
         DC    AL2(CHASE-ACCR02)   CHASE BANK ROUTINE                           
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE SYRACUSE'       BANK NAME                             
*                                                                               
         DC    AL2(0031)           ORIGIN ID - TLDA                             
         DC    AL2(CHASE-ACCR02)   CHASE BANK ROUTINE                           
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE DELAWARE'       BANK NAME                             
*                                                                               
         DC    AL2(0031)           ORIGIN ID - TLDA                             
         DC    AL2(USBK-ACCR02)    US BANK ROUTINE                              
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'USB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'US BANK'              BANK NAME                             
*                                                                               
         DC    AL2(3141)           ORIGIN ID - DWSEC  IMDB#1971681              
         DC    AL2(ABNAMRO-ACCR02) ABNAMRO BANK ROUTINE                         
         DC    AL2(ABNDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'ABN00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'ABNDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'ABN AMRO'             BANK NAME                             
*                                                                               
         DC    AL2(10653)          ORIGIN ID - FMNYCK                           
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15850)          ORIGIN ID - FMMT CL#0339640N                 
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15911)          ORIGIN ID - FMMCMT CL#0345342N               
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(14884)          ORIGIN ID - HMNY CL#0262398N                 
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT07'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(13609)          ORIGIN ID - EUROMED                          
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(8442)           ORIGIN ID - FMMPI                            
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(8566)           ORIGIN ID - FMMC                             
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(4617)           ORIGIN ID - ZENYA                            
         DC    AL2(HSBC-ACCR02)    HSBC BANK ROUTINE                            
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'MMB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'                 BANK NAME                             
*                                                                               
         DC    AL2(11054)          ORIGIN ID - ZDCACK  IMDB#2224221             
         DC    AL2(ABNAMRO-ACCR02) ABNAMRO BANK ROUTINE                         
         DC    AL2(ABNDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'ABN01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'ABNDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'ABN AMRO'                                                   
*                                                                               
         DC    AL2(6029)           ORIGIN ID - YNME                             
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'BOA02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(14741)          ORIGIN ID - MGNYF CL# 0256886N               
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'BOA02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(15116)          ORIGIN ID - MGCTF CL# 0309852N               
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'BOA02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(15152)          ORIGIN ID - MGCTF CL# 0309851N               
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'BOA02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(14742)          ORIGIN ID - SENYF CL# 0256886N               
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'BOA02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(6235)           ORIGIN ID - ACBO                             
         DC    AL2(FLEET-ACCR02)   FLEET BANK ROUTINE                           
         DC    AL2(FLTDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'FLT00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'FLTDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'FLEET'                BANK NAME                             
*                                                                               
         DC    AL2(6884)           ORIGIN ID - CARNY                            
         DC    AL2(FUNB-ACCR02)    FIRST UNION NATIONAL ROUTINE                 
         DC    AL2(FUNDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'FUN00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'FUNDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'FIRST UNION NATIONAL'      BANK NAME                        
*                                                                               
         DC    AL2(6885)           ORIGIN ID - CARSEC  IMDB#2442631             
         DC    AL2(FUNB-ACCR02)    FIRST UNION NATIONAL ROUTINE                 
         DC    AL2(FUNDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'FUN00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'FUNDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'FIRST UNION NATIONAL'      BANK NAME                        
*                                                                               
         DC    AL2(07033)          ORIGIN ID DSFTK-104  CARLA                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(15663)          ORIGIN ID DSFTK-104  CARTHD                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(16487)          ORIGIN ID DSFTK-104  CARMCCK                 
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(16201)          ORIGIN ID DSFTK-104  CARGMCK                 
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(15672)          ORIGIN ID DSFTK-104  CARDIS                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(15507)          ORIGIN ID DSFTK-104  CARRML                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(08800)          ORIGIN ID DSFTK-104  CARPF                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(15111)          ORIGIN ID DSFTK-104  CARWY                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
         DC    AL2(17458)          ORIGIN ID DSFTK-104  CARCSI                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CSI ENTERPRISES'      BANK NAME                             
*                                                                               
*                                                                               
         DC    AL2(07033)          ORIGIN ID DSFTK-104  CARLA                   
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(15663)          ORIGIN ID DSFTK-104  CARTHD                  
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(16487)          ORIGIN ID DSFTK-104  CARMCCK                 
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(16201)          ORIGIN ID DSFTK-104  CARGMCK                 
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(15672)          ORIGIN ID DSFTK-104  CARDIS                  
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(15507)          ORIGIN ID DSFTK-104  CARRML                  
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(08800)          ORIGIN ID DSFTK-104  CARPF                   
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(15111)          ORIGIN ID DSFTK-104  CARWY                   
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
         DC    AL2(17458)          ORIGIN ID DSFTK-104  CARWY                   
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPM CHASE      '      BANK NAME                             
*                                                                               
*                                                                               
         DC    AL2(7348)           ORIGIN ID - ICCNJ CL#0179810N                
         DC    AL2(CT80-ACCR02)    CITIBANK 80 ROUTINE                          
         DC    AL2(CT80DTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT80DTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(7602)           ORIGIN ID - OMDTOA  IMDB#419063              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(3680)           ORIGIN ID - DNTOA   CL#0105137T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(3309)           ORIGIN ID - MKEXPD  CL#0105927T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(2826)           ORIGIN ID - MKTOAB  CL#0105927T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(13493)          ORIGIN ID - MKJPAC  CL#0105927T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(9184)           ORIGIN ID - PNMDAC  CL#0105927T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(11554)          ORIGIN ID - PNPPAC  CL#0105927T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(9183)           ORIGIN ID - PNNLAC  CL#0105927T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(9945)           ORIGIN ID - PWCTOAC CL#0105927T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(14666)          ORIGIN ID - FPHDTOA CL#0107361T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(18546)          ORIGIN ID - FHSCHAC DSFTK-268                
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(20103)          ORIGIN ID - FOMGAC SPEC-31990                
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
*MN SPEC-37368                                                                  
         DC    AL2(14664)          ORIGIN ID - PHDTOA SPEC-37368                
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(14664)          ORIGIN ID - PHDTOA SPEC-37368                
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(14659)          ORIGIN ID - TOUMO  SPEC-37368                
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(14659)          ORIGIN ID - TOUMO  SPEC-37368                
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
*MN SPEC-37368                                                                  
         DC    AL2(14669)          ORIGIN ID - FTOUMO CL#0107361T               
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO06'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(14782)          ORIGIN ID - URBAN   CL#0108623T              
         DC    AL2(BANK-ACCR02)    SCOTIA ROUTINE                               
         DC    AL2(SCODTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(SCO80)          ADDRESS OF FORMAT                            
         DC    CL5'SCO07'          BANK NAME FOR DYNALLOC                       
         DC    CL18'SCODTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'SCOTIABANK'    BANK NAME                                    
*                                                                               
         DC    AL2(7603)           ORIGIN ID - OMDCOR  CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(7621)           ORIGIN ID - BBOMAC  CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(7623)           ORIGIN ID - TBOMAC  CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(7624)           ORIGIN ID - PJOMTAC CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(7626)           ORIGIN ID - PNTOAC  CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(7627)           ORIGIN ID - PJOMVAC CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(7629)           ORIGIN ID - ANOMAC  CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(8442)           ORIGIN ID - FMMPI TICKET#0108640N            
         DC    AL2(CHASE-ACCR02)   HSBC BANK ROUTINE                            
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE SYRACUSE'       BANK NAME                             
*                                                                               
         DC    AL2(8566)           ORIGIN ID - FMMC  TICKET#0108640N            
         DC    AL2(CHASE-ACCR02)   HSBC BANK ROUTINE                            
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE SYRACUSE'       BANK NAME                             
*                                                                               
         DC    AL2(8594)           ORIGIN ID - MSNYA                            
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(8602)           ORIGIN ID - MSHTOA                           
         DC    AL2(ROYAL-ACCR02)   ROYAL BANK BANK ROUTINE                      
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'ROY00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'ROYAL BANK'           BANK NAME                             
*                                                                               
         DC    AL2(14108)          ORIGIN ID - MSHQ CL#0105014T                 
         DC    AL2(ROYAL-ACCR02)   ROYAL BANK                                   
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'ROY02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'ROYAL BANK'           BANK NAME                             
*                                                                               
         DC    AL2(15123)          ORIGIN ID - MSHUNI CL#0109072T               
         DC    AL2(ROYAL-ACCR02)   ROYAL BANK                                   
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'ROY03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'ROYAL BANK'           BANK NAME                             
*                                                                               
         DC    AL2(8602)           ORIGIN ID - MSHTOA IMDB#414543               
         DC    AL2(RBNK-ACCR02)    ROYAL BANK SECOND SPEC                       
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'ROY01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'ROYAL BANK'           BANK NAME                             
*                                                                               
         DC    AL2(8685)           ORIGIN ID - OMDUSA                           
         DC    AL2(CHASE-ACCR02)   CHASE BANK ROUTINE                           
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE'                BANK NAME                             
*                                                                               
         DC    AL2(8880)           ORIGIN ID - TOTMAC  CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(8945)           ORIGIN ID - MSCON                            
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(9211)           ORIGIN ID - MVSEC                            
         DC    AL2(BKONE-ACCR02)   BANK ONE BANK ROUTINE                        
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BKO00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK ONE'             BANK NAME                             
*                                                                               
         DC    AL2(9640)           ORIGIN ID - ZDCA                             
         DC    AL2(HSBC-ACCR02)    HSBC BANK ROUTINE                            
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'MMB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'                 BANK NAME                             
*                                                                               
         DC    AL2(9968)           ORIGIN ID - DNQT                             
         DC    AL2(CHASE-ACCR02)   CHASE BANK ROUTINE                           
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE'                BANK NAME                             
*                                                                               
         DC    AL2(10482)          ORIGIN ID - OSIN                             
         DC    AL2(NACT-ACCR02)    HSBC BANK ROUTINE                            
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'NCB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'NATIONAL CITY BANK'   BANK NAME                             
*                                                                               
         DC    AL2(10596)          ORIGIN ID - FMFM  IMDB#2169921               
         DC    AL2(CHASE-ACCR02)   HSBC BANK ROUTINE                            
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE SYRACUSE'       BANK NAME                             
*                                                                               
         DC    AL2(10690)          ORIGIN ID - DAVACC                           
         DC    AL2(ABUS-ACCR02)    AMERICAN BUSINESS BANK ROUTINE               
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'ABU00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'AMERICAN BUSINESS'    BANK NAME                             
*                                                                               
         DC    AL2(10986)          ORIGIN ID - TOTTAC  CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(11335)          ORIGIN ID - MSNYMONE                         
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(11348)          ORIGIN ID - HNKNY                            
         DC    AL2(CITI-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11397)          ORIGIN ID - MSCONMO                          
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(11553)          ORIGIN ID - MCLOU                            
         DC    AL2(CHASE-ACCR02)   CHASE BANK ROUTINE                           
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE SYRACUSE'       BANK NAME                             
*                                                                               
         DC    AL2(11592)          ORIGIN ID - MCACC                            
         DC    AL2(CHASE-ACCR02)   CHASE BANK ROUTINE                           
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE SYRACUSE'       BANK NAME                             
*                                                                               
         DC    AL2(11592)          ORIGIN ID - MCACC                            
         DC    AL2(CT80-ACCR02)    CITIBANK 80 ROUTINE                          
         DC    AL2(CT80DTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT80DTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15027)          ORIGIN ID - OMLOUCK CL#0268480N              
         DC    AL2(CT80-ACCR02)    CITIBANK 80 ROUTINE                          
         DC    AL2(CT80DTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT80DTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(0909)           ORIGIN ID - MCLO                             
         DC    AL2(CT80-ACCR02)    CITIBANK 80 ROUTINE                          
         DC    AL2(CT80DTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT80DTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(13484)          ORIGIN ID - ICCAL CL#0158758N                
         DC    AL2(CT80-ACCR02)    CITIBANK 80 ROUTINE                          
         DC    AL2(CT80DTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT80DTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11604)          ORIGIN ID - MSNYMAX                          
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(11682)          ORIGIN ID - OSZNYA                           
         DC    AL2(HSBC-ACCR02)    HSBC BANK ROUTINE                            
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'MMB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'                 BANK NAME                             
*                                                                               
         DC    AL2(11698)          ORIGIN ID - MSCONMX                          
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(11796)          ORIGIN ID - OMDQEAC CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(11797)          ORIGIN ID - OMDMTAC CL#0103607T              
         DC    AL2(BANK-ACCR02)    HSBC ROUTINE                                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(HS80)           ADDRESS OF FORMAT                            
         DC    CL5'HSB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(11864)          ORIGIN ID - HNKDO                            
         DC    AL2(CITI-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11875)          ORIGIN ID - MSME                             
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(11876)          ORIGIN ID - MSCONME                          
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(11900)          ORIGIN ID - MVNYCK                           
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11901)          ORIGIN ID - HALCK                            
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11905)          ORIGIN ID - BROMCK                           
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11906)          ORIGIN ID - STARCK                           
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11908)          ORIGIN ID - STACHCK                          
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11909)          ORIGIN ID - GMMCK                            
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11910)          ORIGIN ID - KLKCK                            
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(11981)          ORIGIN ID - OSSECRE                          
         DC    AL2(CITI-ACCR02)    CITIBANK BANK ROUTINE                        
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(12036)          ORIGIN ID - OSDNYRE                          
         DC    AL2(BANK-ACCR02)    CITIBANK BANK ROUTINE                        
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(12161)          ORIGIN ID - OSNYCK                           
         DC    AL2(BANK-ACCR02)    CITIBANK BANK ROUTINE                        
         DC    AL2(CITDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(CIT58)          ADDRESS OF FORMAT                            
         DC    CL5'CIT02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(12319)          ORIGIN ID - MUWS  CL#0196718N                
         DC    AL2(BANK-ACCR02)    CITIBANK 120 ROUTINE                         
         DC    AL2(CITIDTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(CT120CR)        ADDRESS OF FORMAT                            
         DC    CL5'CIT06'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CITIDTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
*&&DO                                                                           
         DC    AL2(12361)          ORIGIN ID - BEAP CL#0144329N                 
         DC    AL2(UMB-ACCR02)     UNITED MISSOURI BANK                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'UMB00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'UNITED MISSOURI BANK' BANK NAME                             
*                                                                               
         DC    AL2(13504)          ORIGIN ID - PHDSAF CL#0153408N               
         DC    AL2(USBK-ACCR02)    US BANK ROUTINE                              
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'USB01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'US BANK'              BANK NAME                             
*&&                                                                             
*                                                                               
         DC    AL2(12449)          ORIGIN ID - MCMOMCK                          
         DC    AL2(CHASE-ACCR02)   CHASE BANK ROUTINE                           
         DC    AL2(CHSDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CHSDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CHASE SYRACUSE'       BANK NAME                             
*                                                                               
         DC    AL2(12607)          ORIGIN ID - OGCK   IMDB#2572401              
         DC    AL2(FUNB-ACCR02)    FIRST UNION (WACHOVIA) ROUTINE               
         DC    AL2(FUNDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'FUN01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'FUNDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'WACHOVIA'      BANK NAME                                    
*                                                                               
         DC    AL2(12615)          ORIGIN ID - ICCPA CL#0179810N                
         DC    AL2(CT80-ACCR02)    CITIBANK 80 ROUTINE                          
         DC    AL2(CT80DTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT80DTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(13013)          ORIGIN ID - MECK   CL#0113897N               
         DC    AL2(BANK-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'BOA01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'  BANK NAME                                 
*                                                                               
         DC    AL2(15330)          ORIGIN ID - MCNYF  CL#0309820N               
         DC    AL2(BANK-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'BOA01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'  BANK NAME                                 
*                                                                               
*        DC    AL2(7567)           ORIGIN ID - JWTOAC                           
*        DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#01064432                
*        DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
*        DC    AL4(0)              ADDRESS OF FORMAT                            
*        DC    CL5'HSB01'          BANK NAME FOR DYNALLOC                       
*        DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
*        DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(14215)          ORIGIN ID - NEOTO                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0106470T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB02'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17228)          ORIGIN ID - YRAKD                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-74                   
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(11587)          ORIGIN ID - YRYTD                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0108996T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(14168)          ORIGIN ID - YRYQD                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0108996T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(8441)           ORIGIN ID - SHGMO CL#0111707T                
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE PAPER CHECKS               
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(13646)          ORIGIN ID - SHMF  CL#0111707T                
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE PAPER CHECKS               
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
* COMMENT FOR TICKET#0111793T                                                   
*   ACCTAPE.ACRHSB05 DATESETS ARE EMPTY - THIS IS THE REC=300,BLK=27900         
*   AS STATED IN THE TICKET - CHECK WITH SHIRLEY TO SEE WHAT THE ACTUAL         
*   RECL/BLKSIZE WILL BE - HSB03 WITH RECL=57 TAPES HAVE INPUT DATA             
         DC    AL2(11587)          ORIGIN ID - YRYTD                            
         DC    AL2(HS3HUN-ACCR02)  HSBC BANK ROUTINE CL#0109113T                
         DC    AL2(HS300DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HS300DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(14168)          ORIGIN ID - YRYQD                            
         DC    AL2(HS3HUN-ACCR02)  HSBC BANK ROUTINE CL#0109113T                
         DC    AL2(HS300DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HS300DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17228)          ORIGIN ID - YRAKD                            
         DC    AL2(HS3HUN-ACCR02)    HSBC BANK ROUTINE DSFTK-76                 
         DC    AL2(HS300DTF-ACCR02)  DISPLACEMENT TO DTF                        
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HS300DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17537)          ORIGIN ID - YRBRD                            
         DC    AL2(HS3HUN-ACCR02)    HSBC BANK ROUTINE DSFTK-148                
         DC    AL2(HS300DTF-ACCR02)  DISPLACEMENT TO DTF DSFTK-149              
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HS300DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17537)          ORIGIN ID - YRAKD                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-74                   
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB03'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(3426)           ORIGIN ID - HLTOH                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0106443T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB06'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(15111)          ORIGIN ID - CARWY  CL#0280997N               
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(8800)           ORIGIN ID - CARPF  CL#0280997N               
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(15099)          ORIGIN ID - CARDB  CL#0280997N               
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(15233)          ORIGIN ID - CARDBNY CL#0280997N              
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(6884)           ORIGIN ID - CARNY  CL#0280997N               
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(7033)           ORIGIN ID - CARLA  CL#0280997N               
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(12156)          ORIGIN ID - VIZLA  CL#0280997N               
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(15224)          ORIGIN ID - POSTACC CL#0291038N              
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(15507)          ORIGIN ID - CARRML CL#0306487N               
         DC    AL2(CZ80-ACCR02)    CITIZEN  80 ROUTINE                          
         DC    AL2(CTZDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CTZ00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CTZDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIZEN'       BANK NAME                                    
*                                                                               
         DC    AL2(10368)          ORIGIN ID - MVCTOA CL#0108625T               
         DC    AL2(CT200-ACCR02)   CITIBANK 200 ROUTINE                         
         DC    AL2(CT200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK 200'  BANK NAME                                    
*                                                                               
         DC    AL2(9388)           ORIGIN ID - STWTO CL#0108625T                
         DC    AL2(CT200-ACCR02)   CITIBANK 200 ROUTINE                         
         DC    AL2(CT200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK 200'  BANK NAME                                    
*                                                                               
         DC    AL2(13380)          ORIGIN ID - ZOTOA CL#0108992T                
         DC    AL2(CT200-ACCR02)   CITIBANK 200 ROUTINE                         
         DC    AL2(CT200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK 200'  BANK NAME                                    
*                                                                               
         DC    AL2(3586)           ORIGIN ID - SOTOT CL#0108993T                
         DC    AL2(CT200-ACCR02)   CITIBANK 200 ROUTINE                         
         DC    AL2(CT200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CT200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK 200'  BANK NAME                                    
*                                                                               
         DC    AL2(13915)          ORIGIN ID - GGNY CL#0309345N                 
         DC    AL2(HSBC-ACCR02)    HSBC BANK ROUTINE                            
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB07'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'                 BANK NAME                             
*                                                                               
         DC    AL2(14836)          ORIGIN ID - GROUPMCO CL#0309851N             
         DC    AL2(BOA-ACCR02)     BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'BOA00'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(15676)          ORIGIN ID - FMAM                             
         DC    AL2(CI80-ACCR02)    BANK OF AMERICA BANK ROUTINE                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15672)          ORIGIN ID - CARDIS                           
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(16201)          ORIGIN ID - CARGMCK                          
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(16477)          ORIGIN ID - CARDECK                          
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(16487)          ORIGIN ID - CARMCCK CL#0385154N              
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02) DISPLACEMENT TO DTF                         
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(8722)           ORIGIN ID - MSHTOAC                          
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0111793T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(14108)          ORIGIN ID - MSHQU                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0111793T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(15123)          ORIGIN ID - MSHUNI                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0111793T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(15898)          ORIGIN ID - MAXUS                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0111793T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16817)          ORIGIN ID - MAXTOA                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-32                   
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(15803)          ORIGIN ID - MPGCK - CL#0341108N              
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15793)          ORIGIN ID - MPGERME                          
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15794)          ORIGIN ID - MPGHMI CL#0341108N               
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT07'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15800)          ORIGIN ID - MPGMPI - CL#0341108N             
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15795)          ORIGIN ID - MPGMC - CL#0341108N              
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15790)          ORIGIN ID - MPGAM - CL#0341108N              
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(16379)          ORIGIN ID - MPGIGNCK - CL#0373065N           
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15851)          ORIGIN ID - MPGMT CL#0341108N                
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(15953)          ORIGIN ID - MPGMCMT CL#0341108N              
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(16613)          ORIGIN ID - HVMSOC CL#0396050N               
         DC    AL2(CI80-ACCR02)    CITIBANK ROUTINE                             
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CIT04'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'CITIBANK'      BANK NAME                                    
*                                                                               
         DC    AL2(16238)          ORIGIN ID - GMXAS                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0114774T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(14346)          ORIGIN ID - OUTTO                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-67                   
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16820)          ORIGIN ID - MECEL                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-67                   
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(19545)          ORIGIN ID - WAVEL                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-17408                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(19546)          ORIGIN ID - WAVELCO                          
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-17408                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(19487)          ORIGIN ID - WAVETO                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-17409                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(19541)          ORIGIN ID - WAVETCO                          
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-17409                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(19542)          ORIGIN ID - WAVETEO                          
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-17409                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(19468)          ORIGIN ID - ESSTOA                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-17582                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(19690)          ORIGIN ID - NWOTOA                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-21410                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16819)          ORIGIN ID - MECEM                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-118                  
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17242)          ORIGIN ID - MNSTOA                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-125                  
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17243)          ORIGIN ID - MNSQU                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-125                  
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17254)          ORIGIN ID - MNSUNI                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE DSFTK-125                  
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
*MN SPEC-46427                                                                  
         DC    AL2(20867)          ORIGIN ID - OPNTOA                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-46427                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
*MN SPEC-46427                                                                  
         DC    AL2(18795)          ORIGIN ID - MIEXCA                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-10303                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(18795)          ORIGIN ID - MIEXCA                           
         DC    AL2(BANK-ACCR02)    HSBC BANK ROUTINE SPEC-30063                 
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(18626)          ORIGIN ID - SIXTOA                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-10303                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16243)          ORIGIN ID - MCMMF                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE SPEC-10303                 
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(17242)          ORIGIN ID - MNSTOA                           
         DC    AL2(BANK-ACCR02)    HSBC BANK ROUTINE DSFTK-126                  
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(17243)          ORIGIN ID - MNSQU                            
         DC    AL2(BANK-ACCR02)    HSBC BANK ROUTINE DSFTK-126                  
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(17254)          ORIGIN ID - MNSUNI                           
         DC    AL2(BANK-ACCR02)    HSBC BANK ROUTINE DSFTK-126                  
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(14810)          ORIGIN ID - GRPMTO                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0114774T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16210)          ORIGIN ID - MCMMP                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0114988T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16208)          ORIGIN ID - MCMMC                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0114988T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16209)          ORIGIN ID - MCMMB                            
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0114988T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(12612)          ORIGIN ID - YRTOMC                           
         DC    AL2(HS57-ACCR02)    HSBC BANK ROUTINE CL#0117826T                
         DC    AL2(HSBDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'HSB08'          BANK NAME FOR DYNALLOC                       
         DC    CL18'HSBDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'HSBC'          BANK NAME                                    
*                                                                               
         DC    AL2(16557)          ORIGIN ID - TPPS                             
         DC    AL2(CHS40-ACCR02)   CHASE BANK ROUTINE CL#0392997N               
         DC    AL2(CH40DTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS05'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH40DTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(14810)          ORIGIN ID DSFTK-84 - GRPMTO                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(16817)          ORIGIN ID DSFTK-84 - MAXTOA                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(19545)          ORIGIN ID SPEC-19129 - WAVEL                 
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(19468)          ORIGIN ID SPEC-19131 - ESSTOA                
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(19690)          ORIGIN ID SPEC-22303 - NWOTOA                
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(18626)          ORIGIN ID SPEC-22750 - SIXTOA                
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(19543)          ORIGIN ID SPEC-19359 - WAVETBA               
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
*MN SPEC-46608                                                                  
         DC    AL2(20867)          ORIGIN ID SPEC-46608 - OPNTOA                
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
*MN SPEC-46608                                                                  
         DC    AL2(16209)          ORIGIN ID DSFTK-84 - MCMMB                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(10823)          ORIGIN ID DSFTK-84 - YRTOME                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(16820)          ORIGIN ID DSFTK-84 - MECEL                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(16208)          ORIGIN ID DSFTK-84 - MCMMC                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(16243)          ORIGIN ID DSFTK-84 - MCMMF                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(08602)          ORIGIN ID DSFTK-84 - MSHTOA                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(14108)          ORIGIN ID DSFTK-84 - MSHQU                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(15123)          ORIGIN ID DSFTK-84 - MSHUNI                  
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(14346)          ORIGIN ID DSFTK-84 - OUTTO                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(16210)          ORIGIN ID DSFTK-84 - MCMMP                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(16819)          ORIGIN ID DSFTK-84 - MECEM                   
         DC    AL2(BANK-ACCR02)    GENERIC BANK ROUTINE                         
         DC    AL2(COMDTF-ACCR02)  DISPLACEMENT TO DTF                          
         DC    AL4(BOA80CR)        ADDRESS OF FORMAT                            
         DC    CL5'GPM01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'COMDTF'        DDNAME FOR DYNALLOC                          
         DC    CL20'BANK OF AMERICA'      BANK NAME                             
*                                                                               
         DC    AL2(17345)          DSFTK-132-MDCMP/DSFTK-168 ASMAC              
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02)  DISPLACEMENT TO DTF                        
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS06'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(19059)          SPEC-14486 BRNAC                             
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02)  DISPLACEMENT TO DTF                        
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS06'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(17345)          DSFTK-150 - ASMAC                            
         DC    AL2(JPMPCARD-ACCR02)  JP MORGAN CHASE 108 BANK ROUTINE           
         DC    AL2(CH108DTF-ACCR02)  DISPLACEMENT TO DTF                        
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS07'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH108DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(18837)          SPEC-13531 - ALCHS                           
         DC    AL2(JPMPCARD-ACCR02)  JP MORGAN CHASE 108 BANK ROUTINE           
         DC    AL2(CH108DTF-ACCR02)  DISPLACEMENT TO DTF                        
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS07'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH108DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
         DC    AL2(17435)          ORIGIN ID DSFTK-109 - PATOWD                 
         DC    AL2(AOPCCARD-ACCR02)  ANCHOR OPS CREDIT CARD ROUTINE             
         DC    AL2(AOPSDTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'AOP01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'AOPSDTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'ANCHOR OPS'    BANK NAME                                    
*                                                                               
         DC    AL2(16354)          ORIGIN ID DSFTK-156 - WNPO                   
         DC    AL2(KEYBNK90-ACCR02)  KEY BANK 90 BANK ROURTINE                  
         DC    AL2(KEYBDTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'KEY01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'KEYBDTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'KEY BANK'      BANK NAME                                    
*                                                                               
         DC    AL2(17355)          ORIGIN ID DSFTK-164 - TRXAC                  
         DC    AL2(CHS200-ACCR02)  JP MORGAN CHASE 200 BANK ROUTINE             
         DC    AL2(CH200DTF-ACCR02)  DISPLACEMENT TO DTF                        
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CHS06'          BANK NAME FOR DYNALLOC                       
         DC    CL18'CH200DTF'      DDNAME FOR DYNALLOC                          
         DC    CL20'JPMCHASE'      BANK NAME                                    
*                                                                               
*SPEC-28991                                                                     
         DC    AL2(02635)          ORIGIN ID SPEC-28991 - DDSB                  
         DC    AL2(CSICCARD-ACCR02)  CSI CREDIT CARD ROUTINE                    
         DC    AL2(AOPSDTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'AOPSDTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CSI CORPORATE' BANK NAME                                    
*                                                                               
         DC    AL2(02651)          ORIGIN ID SPEC-28991 - DDS2                  
         DC    AL2(CSICCARD-ACCR02)  CSI CREDIT CARD ROUTINE                    
         DC    AL2(AOPSDTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSI01'          BANK NAME FOR DYNALLOC                       
         DC    CL18'AOPSDTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CSI CORPORATE' BANK NAME                                    
*                                                                               
*SPEC-28991                                                                     
*SPEC-34556                                                                     
         DC    AL2(18375)          ORIGIN ID SPEC-34556 - TSMCARD               
         DC    AL2(CSICCARD-ACCR02)  CSI CREDIT CARD ROUTINE                    
         DC    AL2(AOPSDTF-ACCR02) DISPLACEMENT TO DTF                          
         DC    AL4(0)              ADDRESS OF FORMAT                            
         DC    CL5'CSITD'          BANK NAME FOR DYNALLOC                       
         DC    CL18'AOPSDTF'       DDNAME FOR DYNALLOC                          
         DC    CL20'CSI CORPORATE' BANK NAME                                    
*                                                                               
*SPEC-34556                                                                     
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* ACCOUNT TABLE (CONSISTS OF)                                        *          
*         - BANK ACCOUNT NUMBER                                      *          
*         - ORIGIN ID                                                *          
*         - DDS CASH ACCOUNT (SC ACCOUNT)                            *          
**********************************************************************          
ACCTAB   DS    0H                                                               
*                                                                               
*SPEC-28991                                                                     
* DDSB - TEST                                                                   
*                                                                               
         DC    CL20'CSIBANKACCO'    BANK ACCOUNT  SPEC-28991                    
         DC    AL2(02635)           ORIGIN ID - DDSB                            
         DC    CL12'CASH3001'       SC ACCOUNT                                  
*                                                                               
* DDS2 - TEST                                                                   
                                                                                
         DC    CL20'CSIBANKACCO'    BANK ACCOUNT  SPEC-28991                    
         DC    AL2(02651)           ORIGIN ID - DDS2                            
         DC    CL12'CASH3001'       SC ACCOUNT                                  
*SPEC-28991                                                                     
*                                                                               
* BEAP - UMB ( UNITED MISSOURI BANK )                                           
*                                                                               
*        DC    CL20'5008011966'     BANK ACCOUNT   CL#0144329N                  
*        DC    AL2(12361)           ORIGIN ID - BROMCK                          
*        DC    CL12'KCU03'          SC ACCOUNT                                  
*                                                                               
* ANOMAC - HSBC (NOV 2008)                                                      
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(7629)            ORIGIN ID - ANOMAC                          
         DC    CL12'HA2001'         DDS CASH ACCOUNT                            
*                                                                               
* BBOMAC - HSBC (NOV 2008)                                                      
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(7621)            ORIGIN ID - BBOMAC                          
         DC    CL12'HB2001'         DDS CASH ACCOUNT                            
*                                                                               
* BROMCK - CITIBANK                                                             
*                                                                               
         DC    CL20'38693658'       BANK ACCOUNT   IMDB#2214721                 
         DC    AL2(11905)           ORIGIN ID - BROMCK                          
         DC    CL12'CBD10849'       CITIBANK-DISBURSEMENTS-BROMLEY              
*                                                                               
* CARNY- FIRST UNION NATIONAL BANK (OCT 2002)                                   
*                                                                               
         DC    CL20'2079950062376'  BANK ACCOUNT                                
         DC    AL2(6884)            ORIGIN ID - CARNY                           
         DC    CL12'01FU2T'         FIRST UNION T&E DISB USA                    
*                                                                               
         DC    CL20'2079950062392'  BANK ACCOUNT                                
         DC    AL2(6884)            ORIGIN ID - CARNY                           
         DC    CL12'01FU2H'         FIRST UNION HOUSE PAYABLES USA              
*                                                                               
         DC    CL20'2079950065522'  BANK ACCOUNT                                
         DC    AL2(6884)            ORIGIN ID - CARNY                           
         DC    CL12'01FU2W'         FIRST UNION DISB KIA/HYUNDAI                
*                                                                               
         DC    CL20'2079950070740'  BANK ACCOUNT                                
         DC    AL2(6884)            ORIGIN ID - CARNY                           
         DC    CL12'01FU2P'         FIRST UNION DISB PFIZER                     
*                                                                               
         DC    CL20'2079950070753'  BANK ACCOUNT                                
         DC    AL2(6884)            ORIGIN ID - CARNY                           
         DC    CL12'01FU2U'         FIRST UNION DISB USA                        
*                                                                               
         DC    CL20'2079950070766'  BANK ACCOUNT                                
         DC    AL2(6884)            ORIGIN ID - CARNY                           
         DC    CL12'MMFU20766'      MEDIA MGT FIRST UNION DISB.                 
*                                                                               
* CARSEC - WACHOVIA                                                             
*                                                                               
         DC    CL20'2000028324698'  BANK ACCOUNT   IMDB#2442631                 
         DC    AL2(6885)            ORIGIN ID - CARSEC                          
         DC    CL12'01FU1V'         FIRST UNION RECEIPTS VIZEUM                 
*                                                                               
         DC    CL20'2000028328474'  BANK ACCOUNT   IMDB#2442631                 
         DC    AL2(6885)            ORIGIN ID - CARSEC                          
         DC    CL12'01FU1D'         FIRST UNION RECEIPTS HDAA                   
*                                                                               
         DC    CL20'2079951064108'  BANK ACCOUNT   IMDB#2442631                 
         DC    AL2(6885)            ORIGIN ID - CARSEC                          
         DC    CL12'01FU2V'         FIRST UNION DISB VIZEUM                     
*                                                                               
         DC    CL20'2079951063963'  BANK ACCOUNT   IMDB#2442631                 
         DC    AL2(6885)            ORIGIN ID - CARSEC                          
         DC    CL12'01FU2D'         FIRST UNION DISB HDAA                       
*                                                                               
* CARWY - CITIZEN BANK                                                          
*                                                                               
         DC    CL20'6699011212'     BANK ACCOUNT   CL#0280997N                  
         DC    AL2(15111)           ORIGIN ID                                   
         DC    CL12'01CBB110520'    SC ACCOUNT                                  
*                                                                               
* CARPF - CITIZEN BANK                                                          
*                                                                               
         DC    CL20'6699011247'     BANK ACCOUNT   CL#0280997N                  
         DC    AL2(8800)            ORIGIN ID                                   
         DC    CL12'01CBB110500'    SC ACCOUNT                                  
*                                                                               
* CARDB - CITIZEN BANK                                                          
*                                                                               
         DC    CL20'6699011255'     BANK ACCOUNT   CL#0280997N                  
         DC    AL2(15099)           ORIGIN ID                                   
         DC    CL12'01CBD110275'    SC ACCOUNT                                  
*                                                                               
* CARDBNY - CITIZEN BANK                                                        
*                                                                               
         DC    CL20'6230288218'     BANK ACCOUNT   CL#0280997N                  
         DC    AL2(15233)           ORIGIN ID                                   
         DC    CL12'01CBD110280'    SC ACCOUNT                                  
*                                                                               
* CARNY - CITIZEN BANK                                                          
*                                                                               
         DC    CL20'6699011352'     BANK ACCOUNT   CL#0280997N                  
         DC    AL2(6884)            ORIGIN ID                                   
         DC    CL12'01CBD110210'    SC ACCOUNT                                  
*                                                                               
* CARLA - CITIZEN BANK                                                          
*                                                                               
         DC    CL20'6699011271'     BANK ACCOUNT   CL#0280997N                  
         DC    AL2(7033)            ORIGIN ID                                   
         DC    CL12'01CBD110205'    SC ACCOUNT                                  
*                                                                               
* VIZLA - CITIZEN BANK                                                          
*                                                                               
         DC    CL20'6699011263'     BANK ACCOUNT   CL#0280997N                  
         DC    AL2(12156)           ORIGIN ID                                   
         DC    CL12'01CBD110255'    SC ACCOUNT                                  
*                                                                               
* CARRML - CITIZEN BANK                                                         
*                                                                               
         DC    CL20'6699011522'     BANK ACCOUNT   CL#0306487N                  
         DC    AL2(15507)           ORIGIN ID                                   
         DC    CL12'01CBB110550'    SC ACCOUNT                                  
*                                                                               
* POSTACC - CITIZEN BANK                                                        
*                                                                               
         DC    CL20'6699011360'     BANK ACCOUNT   CL#0291038N                  
         DC    AL2(15224)           ORIGIN ID                                   
         DC    CL12'PSCBD110450H'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'6699011360'     BANK ACCOUNT   CL#0291038N                  
         DC    AL2(15224)           ORIGIN ID                                   
         DC    CL12'PSCBD110450M'   SC ACCOUNT                                  
*                                                                               
* DAVACC - AMERICAN BUSINESS BANK    (OCT 2004)                                 
*                                                                               
         DC    CL20'0000001123149'        BANK ACCOUNT                          
         DC    AL2(10690)           ORIGIN ID - DAVACC                          
         DC    CL12'DSPOTCABB'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0000001123149'        BANK ACCOUNT                          
         DC    AL2(10690)           ORIGIN ID - DAVACC                          
         DC    CL12'DPRINTCABB'     DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0000001123149'        BANK ACCOUNT                          
         DC    AL2(10690)           ORIGIN ID - DAVACC                          
         DC    CL12'DVCABB'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0000001123122'        BANK ACCOUNT                          
         DC    AL2(10690)           ORIGIN ID - DAVACC                          
         DC    CL12'DSPOTABB'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0000001123122'        BANK ACCOUNT                          
         DC    AL2(10690)           ORIGIN ID - DAVACC                          
         DC    CL12'DPRINTABB'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0000001123122'        BANK ACCOUNT                          
         DC    AL2(10690)           ORIGIN ID - DAVACC                          
         DC    CL12'DVABB'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0000001125311'       BANK ACCOUNT                           
         DC    AL2(10690)           ORIGIN ID - DAVACC                          
         DC    CL12'DVGABB'         DDS CASH ACCOUNT                            
*                                                                               
* DNQT - CHASE (JAN 2005)                                                       
*                                                                               
         DC    CL20'1520874509'     BANK ACCOUNT                                
         DC    AL2(9968)            ORIGIN ID - DNQT                            
         DC    CL12'CD030'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1520882509'     BANK ACCOUNT                                
         DC    AL2(9968)            ORIGIN ID - DNQT                            
         DC    CL12'CD031'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1520866509'     BANK ACCOUNT                                
         DC    AL2(9968)            ORIGIN ID - DNQT                            
         DC    CL12'CD032'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1520866509'     BANK ACCOUNT                                
         DC    AL2(9968)            ORIGIN ID - DNQT                            
         DC    CL12'PC014'          DDS CASH ACCOUNT                            
*                                                                               
* DWSEC - ABN AMRO (FEB 2005)                                                   
*                                                                               
         DC    CL20'5598880275'     BANK ACCOUNT  IMDB#1971681                  
         DC    AL2(3141)            ORIGIN ID - DWSEC                           
         DC    CL12'7AAAD10869'     DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880259'     BANK ACCOUNT  IMDB#1971681                  
         DC    AL2(3141)            ORIGIN ID - DWSEC                           
         DC    CL12'1AAAD10862'     DDS CASH ACCOUNT                            
*                                                                               
* FMAM - CI80  CL#0326725N                                                      
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15676)           ORIGIN ID - FMAM                            
         DC    CL12'C736'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15676)           ORIGIN ID - FMAM                            
         DC    CL12'C737'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15676)           ORIGIN ID - FMAM                            
         DC    CL12'C738'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15676)           ORIGIN ID - FMAM                            
         DC    CL12'C739'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15676)           ORIGIN ID - FMAM                            
         DC    CL12'C740'           DDS CASH ACCOUNT                            
*                                                                               
* FMSEC - CI80  CL#0148366N                                                     
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(10653)           ORIGIN ID - FMNYCK                          
         DC    CL12'C701'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(10653)           ORIGIN ID - FMNYCK                          
         DC    CL12'C702'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(10653)           ORIGIN ID - FMNYCK                          
         DC    CL12'C703'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(10653)           ORIGIN ID - FMNYCK                          
         DC    CL12'C704'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(10653)           ORIGIN ID - FMNYCK                          
         DC    CL12'C705'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'C706'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'C707'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'C708'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'C709'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'C710'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756989'       BANK ACCOUNT                                
         DC    AL2(8566)            ORIGIN ID - FMMC                            
         DC    CL12'C711'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756989'       BANK ACCOUNT                                
         DC    AL2(8566)            ORIGIN ID - FMMC                            
         DC    CL12'C712'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756989'       BANK ACCOUNT                                
         DC    AL2(8566)            ORIGIN ID - FMMC                            
         DC    CL12'C713'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756989'       BANK ACCOUNT                                
         DC    AL2(8566)            ORIGIN ID - FMMC                            
         DC    CL12'C714'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(13609)           ORIGIN ID - EUROMED                         
         DC    CL12'C715'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(13609)           ORIGIN ID - EUROMED                         
         DC    CL12'C716'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(13609)           ORIGIN ID - EUROMED                         
         DC    CL12'C717'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(13609)           ORIGIN ID - EUROMED                         
         DC    CL12'C718'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(13609)           ORIGIN ID - EUROMED                         
         DC    CL12'C719'           DDS CASH ACCOUNT                            
*                                                                               
* FMMT  - CI80  CL#0339640N                                                     
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15850)           ORIGIN ID - FMMT                            
         DC    CL12'C750'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15850)           ORIGIN ID - FMMT                            
         DC    CL12'C751'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15850)           ORIGIN ID - FMMT                            
         DC    CL12'C752'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15850)           ORIGIN ID - FMMT                            
         DC    CL12'C753'           DDS CASH ACCOUNT                            
*                                                                               
* FMMCMT  - CI80  CL#0345342N                                                   
*                                                                               
         DC    CL20'38861308'       BANK ACCOUNT                                
         DC    AL2(15911)           ORIGIN ID - FMMCMT                          
         DC    CL12'C760'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38861308'       BANK ACCOUNT                                
         DC    AL2(15911)           ORIGIN ID - FMMCMT                          
         DC    CL12'C761'           DDS CASH ACCOUNT                            
*                                                                               
*                                                                               
* HMNY  - CI80  CL#0262398N                                                     
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT CL#0262398N                    
         DC    AL2(14884)           ORIGIN ID - HMNY                            
         DC    CL12'C725'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT CL#0262398N                    
         DC    AL2(14884)           ORIGIN ID - HMNY                            
         DC    CL12'C726'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT CL#0262398N                    
         DC    AL2(14884)           ORIGIN ID - HMNY                            
         DC    CL12'C727'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT CL#0262398N                    
         DC    AL2(14884)           ORIGIN ID - HMNY                            
         DC    CL12'C728'           DDS CASH ACCOUNT                            
*                                                                               
* FMFM  - CHASE    (APR 2005)                                                   
*                                                                               
         DC    CL20'1486423509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'B001'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1486423509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'B002'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1486423509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'B003'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1486423509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'B004'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1520163509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'M001'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1520163509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'M002'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1520163509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'M003'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1529651509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'A001'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1529651509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'A002'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1529651509'     BANK ACCOUNT  IMDB#2169921                  
         DC    AL2(10596)           ORIGIN ID - FMFM                            
         DC    CL12'A003'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519587509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8566)            ORIGIN ID - FMMC                            
         DC    CL12'L002'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519587509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8566)            ORIGIN ID - FMMC                            
         DC    CL12'L004'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519587509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8566)            ORIGIN ID - FMMC                            
         DC    CL12'L006'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519579509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'B105'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519579509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'B106'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519579509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'B107'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519579509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'B108'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1519579509'     BANK ACCOUNT  TICKET#0108640N               
         DC    AL2(8442)            ORIGIN ID - FMMPI                           
         DC    CL12'B109'           DDS CASH ACCOUNT                            
*                                                                               
* GMMCK - CITIBANK                                                              
*                                                                               
         DC    CL20'38693551'       BANK ACCOUNT  IMDB#2222081                  
         DC    AL2(11909)           ORIGIN ID - GMMCK                           
         DC    CL12'CBD10840'       CITIBANK DISBURSEMENTS-GM PLANWORKS         
*                                                                               
* HALCK - CITIBANK                                                              
*                                                                               
         DC    CL20'38693498'       BANK ACCOUNT  IMDB#2214721                  
         DC    AL2(11901)           ORIGIN ID - HALCK                           
         DC    CL12'CBD10831'       CITIBANK-DISBURSEMENTS-HALOGEN              
*                                                                               
* HNKNY - CITIBANK (AUG 2004)                                                   
*                                                                               
         DC    CL20'38692049'       BANK ACCOUNT                                
         DC    AL2(11348)           ORIGIN ID - CARNY                           
         DC    CL12'1COM'           FIRST UNION T&E DISB USA                    
*                                                                               
* HNKDO - CITIBANK (AUG 2004)                                                   
*                                                                               
         DC    CL20'38694028'       BANK ACCOUNT                                
         DC    AL2(11864)           ORIGIN ID - HNKDO                           
         DC    CL12'DCOM'           DDS SC CASH ACCOUNT                         
*                                                                               
* ICCAL - CITIBANK CL#0158758N                                                  
*                                                                               
         DC    CL20'0038766386'       BANK ACCOUNT                              
         DC    AL2(13484)           ORIGIN ID - ICCAL                           
         DC    CL12'D311800'        DDS SC CASH ACCOUNT                         
*                                                                               
* ICCNJ - CITIBANK CL#0179810N                                                  
*                                                                               
         DC    CL20'0038773527'     BANK ACCOUNT                                
         DC    AL2(7348)            ORIGIN ID - ICCNJ                           
         DC    CL12'D011880'        DDS SC CASH ACCOUNT                         
*                                                                               
* ICCPA - CITIBANK CL#0179810N                                                  
*                                                                               
         DC    CL20'0038773535'     BANK ACCOUNT                                
         DC    AL2(12615)           ORIGIN ID - ICCPA                           
         DC    CL12'D211860'        DDS SC CASH ACCOUNT                         
*                                                                               
* KLKCK - CITIBANK                                                              
*                                                                               
         DC    CL20'38693631'       BANK ACCOUNT  IMDB#2214721                  
         DC    AL2(11910)           ORIGIN ID - KLKCK                           
         DC    CL12'CBD10848'       CITIBANK-DISBURSEMENTS-KLEMTNER             
*                                                                               
* MCLOU- CHASE SYRACUSE BANK       (FEB 2004)                                   
*                                                                               
         DC    CL20'0601829534'     BANK ACCOUNT                                
         DC    AL2(11553)           ORIGIN ID - MCLOU                           
         DC    CL12'B004'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT                                
         DC    AL2(11553)           ORIGIN ID - MCLOU                           
         DC    CL12'B004A'          DDS CASH ACCOUNT                            
*                                                                               
* MCACC- CHASE SYRACUSE BANK       (FEB 2004)                                   
*                                                                               
         DC    CL20'0601829534'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B006'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B006A'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0601829534'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B007'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B007A'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0601853930'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B008'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B009A'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B011A'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT      (SEP 2004)                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B012'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT IMDB #2280401                  
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B014'           MCLOUCK PRINT BANK ACCOUNT                  
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT IMDB #2280401                  
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B016'           MCFACKP PRINT BANK - FITZGERALD             
*                                                                               
         DC    CL20'1544429509'     BANK ACCOUNT IMDB #2346191                  
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B017'           MCLOUSE ACCT - SONY ELECTRONICS             
*                                                                               
* MCACC- CITIBANK                  (AUG 2007)                                   
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B020'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B021'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038758554'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B022'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B024'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B025'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B026'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT                                
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B027'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT CL#0240616N                    
         DC    AL2(11592)           ORIGIN ID - MCACC                           
         DC    CL12'B028'           DDS CASH ACCOUNT                            
*                                                                               
* OMLOUCK - CITIBANK                (AUG 2007)                                  
*                                                                               
         DC    CL20'0038820362'     BANK ACCOUNT CL#0268480N                    
         DC    AL2(15027)           ORIGIN ID - OMLOUCK                         
         DC    CL12'B050'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038820362'     BANK ACCOUNT CL#0268480N                    
         DC    AL2(15027)           ORIGIN ID - OMLOUCK                         
         DC    CL12'B051'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038820362'     BANK ACCOUNT CL#0268480N                    
         DC    AL2(15027)           ORIGIN ID - OMLOUCK                         
         DC    CL12'B052'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038820362'     BANK ACCOUNT CL#0268480N                    
         DC    AL2(15027)           ORIGIN ID - OMLOUCK                         
         DC    CL12'B053'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038820362'     BANK ACCOUNT CL#0268480N                    
         DC    AL2(15027)           ORIGIN ID - OMLOUCK                         
         DC    CL12'B054'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038820362'     BANK ACCOUNT CL#0268480N                    
         DC    AL2(15027)           ORIGIN ID - OMLOUCK                         
         DC    CL12'B055'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0038820362'     BANK ACCOUNT CL#0268480N                    
         DC    AL2(15027)           ORIGIN ID - OMLOUCK                         
         DC    CL12'B056'           DDS CASH ACCOUNT                            
*                                                                               
* MCLO- CITIBANK                    (AUG 2007)                                  
*                                                                               
         DC    CL20'0038758538'     BANK ACCOUNT                                
         DC    AL2(0909)            ORIGIN ID - MCLO                            
         DC    CL12'B023'           DDS CASH ACCOUNT                            
*                                                                               
* MCMOMCK - CHASE                                                               
*                                                                               
         DC    CL20'1535880509'     BANK ACCOUNT                                
         DC    AL2(12449)           ORIGIN ID - MCMOMCK                         
         DC    CL12'B010'           DDS CASH ACCOUNT                            
*                                                                               
* MUWS - MULLEN                                                                 
*                                                                               
         DC    CL20'0038773981'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(12319)           ORIGIN ID - MUWS                            
         DC    CL12'DCIT'           DDS CASH ACCOUNT                            
*                                                                               
* OGCK - WACHOVIA (MAY 2006)                                                    
*                                                                               
         DC    CL20'2079900560860'  BANK ACCOUNT     IMDB#2572401               
         DC    AL2(12607)           ORIGIN ID - OGCK                            
         DC    CL12'112365'         DDS CASH ACCOUNT                            
*                                                                               
* OMDCOR - HSBC (NOV 2008)                                                      
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(7603)            ORIGIN ID - OMDCOR                          
         DC    CL12'HR2001'         DDS CASH ACCOUNT                            
*                                                                               
* OMDMTAC - HSBC (NOV 2008)                                                     
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(11797)           ORIGIN ID - OMDMTAC                         
         DC    CL12'HF2001'         DDS CASH ACCOUNT                            
*                                                                               
* OMDQEAC - HSBC (NOV 2008)                                                     
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(11796)           ORIGIN ID - OMDQEAC                         
         DC    CL12'HE2001'         DDS CASH ACCOUNT                            
*                                                                               
* OMDUSA - CHASE (JAN 2005)                                                     
*                                                                               
         DC    CL20'1520065509'     BANK ACCOUNT                                
         DC    AL2(8685)            ORIGIN ID - OMDUSA                          
         DC    CL12'CD025'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1520065509'     BANK ACCOUNT                                
         DC    AL2(8685)            ORIGIN ID - OMDUSA                          
         DC    CL12'CD027'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1511451509'     BANK ACCOUNT                                
         DC    AL2(8685)            ORIGIN ID - OMDUSA                          
         DC    CL12'CD029'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'1510206509'     BANK ACCOUNT                                
         DC    AL2(8685)            ORIGIN ID - OMDUSA                          
         DC    CL12'PC011'          DDS CASH ACCOUNT                            
*                                                                               
* OMDTOA - SCOTIA (JUL 2005)                                                    
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OA2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OA2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OB2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OB2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'05518-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OD2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'85421-12'       BANK ACCOUNT       CL#010513T               
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OD2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OE2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OE2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OF2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OF2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'14253-15'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OG2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10234-11'       BANK ACCOUNT       CL#0105101T              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OI2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OJ2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OJ2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OP2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OP2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OR2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OR2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OT2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OT2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OU2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OU2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OX2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OX2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OY2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       IMDB#419063              
         DC    AL2(7602)            ORIGIN ID - OMDTOA                          
         DC    CL12'OY2001'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10042-12'       BANK ACCOUNT       DSFTK-98                 
         DC    AL2(07602)           ORIGIN ID - OMDTOA                          
         DC    CL12'OS2000'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87053-13'       BANK ACCOUNT       DSFTK-98                 
         DC    AL2(07602)           ORIGIN ID - OMDTOA                          
         DC    CL12'OS2001'         DDS CASH ACCOUNT                            
*                                                                               
* FPHDTOA - SCOTIA (MAR 2010)                                                   
*                                                                               
         DC    CL20'13765-19'       BANK ACCOUNT       CL#0107361T              
         DC    AL2(14666)           ORIGIN ID - FPHDTOA                         
         DC    CL12'C10SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00869-16'       BANK ACCOUNT       CL#0107361T              
         DC    AL2(14666)           ORIGIN ID - FPHDTOA                         
         DC    CL12'C40SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03736-13'       BANK ACCOUNT       CL#0107361T              
         DC    AL2(14666)           ORIGIN ID - FPHDTOA                         
         DC    CL12'C70SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03736-13'       BANK ACCOUNT       DSFTK-113                
         DC    AL2(14666)           ORIGIN ID - FPHDTOA                         
         DC    CL12'C75SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03736-13'       BANK ACCOUNT       DSFTK-123                
         DC    AL2(14666)           ORIGIN ID - FPHDTOA                         
         DC    CL12'C80SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03615-18'       BANK ACCOUNT       DSFTK-123                
         DC    AL2(14666)           ORIGIN ID - FPHDTOA                         
         DC    CL12'C77SBO'         DDS CASH ACCOUNT                            
*                                                                               
* FOMGAC - BNS                                                                  
*                                                                               
         DC    CL20'10506-13'       BANK ACCOUNT       SPEC-31990               
         DC    AL2(20103)           ORIGIN ID - FOMGAC                          
         DC    CL12'C26SBO'         DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-37368                                                                  
*                                                                               
* PHDTOA - BNS                                                                  
                                                                                
         DC    CL20'13765-19'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C10SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'13765-19'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C10SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'13132-15'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C20SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'13132-15'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C20SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10506-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C26SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'10506-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C26SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00869-16'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C40SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00869-16'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C40SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'21326-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C45SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'21326-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C45SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03736-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C70SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03736-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C70SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03736-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C75SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03736-13'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C75SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03615-18'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C77SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'03615-18'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C77SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'13223-11'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C80SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'13223-11'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C80SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'13223-11'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C90SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'13223-11'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'C90SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'86976-12'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'U10SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'86976-12'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'U10SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'89230-19'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'U45SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'89230-19'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'U45SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'12179-17'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'U90SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'12179-17'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14664)           ORIGIN ID - PHDTOA                          
         DC    CL12'U90SBO'         DDS CASH ACCOUNT                            
*                                                                               
*                                                                               
* TOUMO  - BNS                                                                  
*                                                                               
         DC    CL20'00520-19'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14659)           ORIGIN ID - TOUMOC                          
         DC    CL12'C50SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00520-19'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14659)           ORIGIN ID - TOUMOC                          
         DC    CL12'C50SBE'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87710-15'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14659)           ORIGIN ID - TOUMOC                          
         DC    CL12'U50SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87710-15'       BANK ACCOUNT       SPEC-37368               
         DC    AL2(14659)           ORIGIN ID - TOUMOC                          
         DC    CL12'U50SBE'         DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-37368                                                                  
*                                                                               
* FHSCHAC - SCOTIA                                                              
*                                                                               
         DC    CL20'21326-13'       BANK ACCOUNT       DSFTK-268                
         DC    AL2(18546)           ORIGIN ID - FHSCHAC                         
         DC    CL12'C77SBO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'89230-19'       BANK ACCOUNT       DSFTK-268                
         DC    AL2(18546)           ORIGIN ID - FHSCHAC                         
         DC    CL12'U45SBO'         DDS CASH ACCOUNT                            
*                                                                               
* FTOUMO - SCOTIA (MAR 2010)                                                    
*                                                                               
         DC    CL20'00520-19'       BANK ACCOUNT       CL#0107361T              
         DC    AL2(14669)           ORIGIN ID - FTOUMO                          
         DC    CL12'C50SBO'         DDS CASH ACCOUNT                            
*                                                                               
* MKEXPD - SCOTIA (SEP 2009)                                                    
*                                                                               
         DC    CL20'00679-11'       BANK ACCOUNT       CL#0105927T              
         DC    AL2(3309)            ORIGIN ID - MKEXPD                          
         DC    CL12'B2000'          DDS CASH ACCOUNT                            
*                                                                               
* MKTOAB - SCOTIA (SEP 2009)                                                    
*                                                                               
         DC    CL20'05515-11'       BANK ACCOUNT       CL#0105927T              
         DC    AL2(2826)            ORIGIN ID - MKTOAB                          
         DC    CL12'R2000'          DDS CASH ACCOUNT                            
*                                                                               
* MKJPAC - SCOTIA (SEP 2009)                                                    
*                                                                               
         DC    CL20'05725-19'       BANK ACCOUNT       CL#0105927T              
         DC    AL2(13493)           ORIGIN ID - MKJPAC                          
         DC    CL12'J2000'          DDS CASH ACCOUNT                            
*                                                                               
* PNMDAC - SCOTIA (SEP 2009)                                                    
*                                                                               
         DC    CL20'01700-11'       BANK ACCOUNT       CL#0105927T              
         DC    AL2(9184)            ORIGIN ID - PNMDAC                          
         DC    CL12'OTIAPCN'        DDS CASH ACCOUNT                            
*                                                                               
* PNPPAC - SCOTIA (SEP 2009)                                                    
*                                                                               
         DC    CL20'02021-18'       BANK ACCOUNT       CL#0105927T              
         DC    AL2(11554)           ORIGIN ID - PNPPAC                          
         DC    CL12'OTIARCN'        DDS CASH ACCOUNT                            
*                                                                               
* PNNLAC - SCOTIA (SEP 2009)                                                    
*                                                                               
         DC    CL20'01791-16'       BANK ACCOUNT       CL#0105927T              
         DC    AL2(9183)            ORIGIN ID - PNNLAC                          
         DC    CL12'OTIANCN'        DDS CASH ACCOUNT                            
*                                                                               
* PWCTOAC - SCOTIA (SEP 2009)                                                   
*                                                                               
         DC    CL20'02376-12'       BANK ACCOUNT       CL#0105927T              
         DC    AL2(9945)            ORIGIN ID - PWCTOAC                         
         DC    CL12'W2000'          DDS CASH ACCOUNT                            
*                                                                               
* DNTOA  - SCOTIA (JUL 2009)                                                    
*                                                                               
         DC    CL20'00153-18'       BANK ACCOUNT   CL#0105137T                  
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'A2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'01242-14'       BANK ACCOUNT   CL#0105137T                  
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'D2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'02446-19'       BANK ACCOUNT   CL#0105137T                  
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'P2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'12488-12'       BANK ACCOUNT   CL#0105137T                  
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'R2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'04502-19'       BANK ACCOUNT   CL#0105137T                  
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'S2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'02193-12'       BANK ACCOUNT   CL#0105137T                  
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'U2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'02446-19'       BANK ACCOUNT   DSSUP-1430                   
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'J2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'11220-10'       BANK ACCOUNT   DSSUP-1430                   
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'Q2000'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87047-16'       BANK ACCOUNT   DSSUP-1430                   
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'A2001'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87046-19'       BANK ACCOUNT   DSSUP-1430                   
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'D2001'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87049-10'       BANK ACCOUNT   DSSUP-1430                   
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'P2001'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'87049-10'       BANK ACCOUNT   DSSUP-1430                   
         DC    AL2(3680)            ORIGIN ID - DNTOA                           
         DC    CL12'J2001'          DDS CASH ACCOUNT                            
*                                                                               
* OSDNYRE - CITIBANK                                                            
*                                                                               
         DC    CL20'38693674'       BANK ACCOUNT  IMDB#2226521                  
         DC    AL2(12036)           ORIGIN ID - OSDNYRE                         
         DC    CL12'CBD10855'       CITIBANK DISB-RESOURCES-PUBLICIS            
*                                                                               
* OSNYCK - CITIBANK                                                             
*                                                                               
         DC    CL20'38694159'       BANK ACCOUNT  IMDB#2226521                  
         DC    AL2(12161)           ORIGIN ID - OSNYCK                          
         DC    CL12'CBD10858'       CITIBANK DISB-RESOURCES-OPTIMEDIA           
*                                                                               
* OSIN - NATIONAL CITY BANK        (JUL 2004)                                   
*                                                                               
         DC    CL20'0758423515'     BANK ACCOUNT                                
         DC    AL2(10482)           ORIGIN ID - OSIN                            
         DC    CL12'NDPRINT'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'0758423515'     BANK ACCOUNT                                
         DC    AL2(10482)           ORIGIN ID - OSIN                            
         DC    CL12'NDSPOT'         DDS CASH ACCOUNT                            
*                                                                               
*        DC    CL20'0000693759'     BANK ACCOUNT     IMDB#2700441               
*        DC    AL2(10482)           ORIGIN ID - OSIN                            
*        DC    CL12'NDPRINTN'       DDS CASH ACCOUNT                            
*                                                                               
*        DC    CL20'0000693759'     BANK ACCOUNT     IMDB#2700441               
*        DC    AL2(10482)           ORIGIN ID - OSIN                            
*        DC    CL12'NDSPOTN'        DDS CASH ACCOUNT                            
*                                                                               
* OSSECRE - CITIBANK  (JAN 2005)                                                
*                                                                               
         DC    CL20'38693674'       BANK ACCOUNT                                
         DC    AL2(11981)           ORIGIN ID - OSSECRE                         
         DC    CL12'CBD10855'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38694159'       BANK ACCOUNT                                
         DC    AL2(11981)           ORIGIN ID - OSSECRE                         
         DC    CL12'CBD10858'       DDS CASH ACCOUNT                            
*                                                                               
* PJOMTAC - HSBC (NOV 2008)                                                     
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(7624)            ORIGIN ID - PJOMTAC                         
         DC    CL12'HP2001'         DDS CASH ACCOUNT                            
*                                                                               
* PJOMVAC - HSBC (NOV 2008)                                                     
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(7627)            ORIGIN ID - PJOMVAC                         
         DC    CL12'HJ2001'         DDS CASH ACCOUNT                            
*                                                                               
* PNTOAC - HSBC (NOV 2008)                                                      
*                                                                               
         DC    CL20'    797042628'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(7626)            ORIGIN ID - PNTOAC                          
         DC    CL12'HD2001'         DDS CASH ACCOUNT                            
*                                                                               
* STACHCK - CITIBANK                                                            
*                                                                               
         DC    CL20'38693594'       BANK ACCOUNT   IMDB#2214721                 
         DC    AL2(11908)           ORIGIN ID - STACHCK                         
         DC    CL12'CBD10844'       CITIBANK-DISBURSEMENTS-STARLINK WW          
*                                                                               
* STARCK - CITIBANK                                                             
*                                                                               
         DC    CL20'38693535'       BANK ACCOUNT   IMDB#2214721                 
         DC    AL2(11906)           ORIGIN ID - STARCK                          
         DC    CL12'CBD10838'       CITIBANK-DISBURSEMENTS-STARCOM WW           
*                                                                               
* TBOMAC - HSBC (NOV 2008)                                                      
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(7623)            ORIGIN ID - TBOMAC                          
         DC    CL12'HT2001'         DDS CASH ACCOUNT                            
*                                                                               
* TLDA - CHASE SYRACUSE BANK   (JULY 2002)                                      
*                                                                               
         DC    CL20'0601818974'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B001'           SPOT BANK                                   
*                                                                               
         DC    CL20'0601818974'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B002'           PRINTPAK BANK ACCOUNT                       
*                                                                               
         DC    CL20'0601229586'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B004'           SPOT- PUSKAR,GIBBON, CHAPIN, INC.           
*                                                                               
         DC    CL20'0601229586'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B004P'          PRINT- PUSKAR,GIBBON, CHAPIN, INC.          
*                                                                               
         DC    CL20'0601812787'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B100'           TLP                                         
*                                                                               
         DC    CL20'0601812787'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B100P'          TLP PRINT                                   
*                                                                               
* TLDA - CHASE DELAWARE BANK   (JULY 2002)                                      
*                                                                               
         DC    CL20'1530394509'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B005'           SPOT BANK                                   
*                                                                               
         DC    CL20'1530394509'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B005P'          PRINT BANK                                  
*                                                                               
         DC    CL20'1530386509'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B007'           SPOT PLUS - BROADCAST                       
*                                                                               
         DC    CL20'1530386509'     BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B007P'          SPOT PLUS - PRINT                           
*                                                                               
* TLDA - US BANK               (APRIL 2004)                                     
*                                                                               
         DC    CL20'150080686366'   BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B400'           SPOT BANK                                   
*                                                                               
         DC    CL20'150080686366'   BANK ACCOUNT                                
         DC    AL2(0031)            ORIGIN ID - TLDA                            
         DC    CL12'B400P'          SPOT BANK                                   
*                                                                               
* TOTMAC - HSBC (NOV 2008)                                                      
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(8880)            ORIGIN ID - TOTMAC                          
         DC    CL12'HX2001'         DDS CASH ACCOUNT                            
*                                                                               
* TOTTAC - HSBC (NOV 2008)                                                      
*                                                                               
         DC    CL20'    797042610'  BANK ACCOUNT     CL#0103607T                
         DC    AL2(10986)           ORIGIN ID - TOTTAC                          
         DC    CL12'HY2001'         DDS CASH ACCOUNT                            
*                                                                               
* PHDSAF - US BANK               CL#0153408N                                    
*                                                                               
*        DC    CL20'150097030400'   BANK ACCOUNT                                
*        DC    AL2(13504)           ORIGIN ID - PHDSAF                          
*        DC    CL12'B501'           SPOT BANK                                   
*                                                                               
* MSHQU  - ROYAL BANK          (JAN 2010)                                       
*                                                                               
         DC    CL20'000001133586'   ADDED JAN 2010 CL#0105014T                  
         DC    AL2(14108)           ORIGIN ID - MSHQU                           
         DC    CL12'CRBQD'          BANK SC ACCOUNT                             
*                                                                               
* MSHUNI - ROYAL BANK          (DEC 2010)                                       
*                                                                               
         DC    CL20'000001124635'   ADDED DEC 2010 CL#0109072T                  
         DC    AL2(15123)           ORIGIN ID - MSHUNI                          
         DC    CL12'CRBMU'          BANK SC ACCOUNT                             
*                                                                               
* MSHTOA - ROYAL BANK          (JUN 2002)                                       
*                                                                               
         DC    CL20'000021320340'   ADDED OCT 2004 IMDB#414543                  
         DC    AL2(8602)            ORIGIN ID - MSHTOA                          
         DC    CL12'CRBMP'          BANK-RBC1-CDN - DISBURSEMENT                
*                                                                               
         DC    CL20'000024062089'   ADDED OCT 2004 IMDB#414543                  
         DC    AL2(8602)            ORIGIN ID - MSHTOA                          
         DC    CL12'URBMP'          BANK-RBC1-CDN - DISBURSEMENT                
*                                                                               
         DC    CL20'000001209709'   BANK ACCOUNT - MUST BE 12 BYTES             
         DC    AL2(8602)            ORIGIN ID - MSHTOA                          
         DC    CL12'CRBMD'          BANK-RBC1-CDN - DISBURSEMENT                
*                                                                               
         DC    CL20'000004016531'   BANK ACCOUNT - MUST BE 12 BYTES             
         DC    AL2(8602)            ORIGIN ID - MSHTOA                          
         DC    CL12'URBMO'          US$ RBC - MINDSHARE - OPERATING             
*                                                                               
* GROUPMCO - BANK OF AMERICA                                                    
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA001'         BOA NETWORK DISBURSEMENTS                   
*                                                                               
         DC    CL20'3299986077'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA002'         BOA HOUSE DISBURSEMENT ACCT                 
*                                                                               
         DC    CL20'3299986077'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA004'         BOA HOUSE DISBURSEMENT ACCT-MANUAL          
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA006'         BOA SPOT DISBURSEMENTS                      
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA007'         BOA PRINT DISBURSEMENTS                     
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA008'         BOA MEDIA DISBURSEMENTS - MANUAL            
*                                                                               
         DC    CL20'3299029324'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA016'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299029324'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA017'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299029324'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA018'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299029324'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA019'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299029316'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA020'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299047110'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA023'         BOA HOUSE DISBURSEMENT ACCT                 
*                                                                               
         DC    CL20'3299030025'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA024'         BOA HOUSE DISBURSEMENT ACCT                 
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA026'         BOA MEDIA DISBURSEMENTS - MANUAL            
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA028'         BOA MEDIA DISBURSEMENTS - MANUAL            
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA029'         BOA MEDIA DISBURSEMENTS - MANUAL            
*                                                                               
         DC    CL20'3299986077'     BANK ACCOUNT - CL#0324949N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA030'         BOA HOUSE DISBURSEMENT ACCT                 
*                                                                               
         DC    CL20'3359327940'     BANK ACCOUNT - CL#0268131N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA043'         BOA HOUSE DISBURSEMENT ACCT-MANUAL          
*                                                                               
         DC    CL20'3359327940'     BANK ACCOUNT - CL#0268131N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA044'         BOA HOUSE DISBURSEMENT ACCT-MANUAL          
*                                                                               
         DC    CL20'3359327940'     BANK ACCOUNT - CL#0268131N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA045'         BOA HOUSE DISBURSEMENT ACCT-MANUAL          
*                                                                               
         DC    CL20'3359327940'     BANK ACCOUNT - CL#0268131N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA046'         BOA HOUSE DISBURSEMENT ACCT-MANUAL          
*                                                                               
         DC    CL20'3359327940'     BANK ACCOUNT - CL#0268131N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA047'         BOA HOUSE DISBURSEMENT ACCT-MANUAL          
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA057'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA058'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA059'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA060'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA061'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491670'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA068'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491670'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA069'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491670'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA070'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491670'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA071'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491670'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA072'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491662'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA073'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491662'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA074'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491662'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA075'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491662'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA076'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491662'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA077'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491654'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA078'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491654'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA079'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491654'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA080'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491654'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA081'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491654'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA082'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA083'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA084'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA085'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA086'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0350237N          
         DC    CL12'DBA087'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT -                              
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - SPEC-6456            
         DC    CL12'DBA152'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT -                              
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - SPEC-6456            
         DC    CL12'DBA153'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT -                              
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - SPEC-6456            
         DC    CL12'DBA154'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT -                              
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - SPEC-6456            
         DC    CL12'DBA155'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT -                              
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - SPEC-6456            
         DC    CL12'DBA156'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359871012'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA095'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359871012'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA096'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359871020'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA097'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359871020'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA098'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359871038'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA099'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359871038'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA100'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359870998'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA103'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359870998'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0389418N          
         DC    CL12'DBA104'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359872788'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0395932N          
         DC    CL12'DBA114'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359872788'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0395932N          
         DC    CL12'DBA115'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359872788'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0395932N          
         DC    CL12'DBA116'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359872788'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0395932N          
         DC    CL12'DBA117'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359872788'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0395932N          
         DC    CL12'DBA118'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359872135'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0395932N          
         DC    CL12'DBA111'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359872135'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - CL#0395932N          
         DC    CL12'DBA112'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359873950'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - #DSSUP-1221          
         DC    CL12'DBA120'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359873950'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - #DSSUP-1221          
         DC    CL12'DBA122'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359873950'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - #DSSUP-1221          
         DC    CL12'DBA123'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359873950'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - #DSSUP-1221          
         DC    CL12'DBA124'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359873950'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMCO - #DSSUP-1221          
         DC    CL12'DBA121'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA164'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA169'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA165'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA167'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299985459'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA171'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3299986077'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA172'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA180'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359491647'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA181'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359986703'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA174'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359986703'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA175'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359986703'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA176'         DDS SC CASH ACCOUNT                         
*                                                                               
         DC    CL20'3359986703'     BANK ACCOUNT - SPEC-27196                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA177'         DDS SC CASH ACCOUNT                         
*                                                                               
* MCNYF - BANK OF AMERICA                                                       
*                                                                               
         DC    CL20'3299049389'     BANK ACCOUNT     CL#0309820N                
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA048'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3299049389'     BANK ACCOUNT     CL#0309820N                
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA049'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3299049389'     BANK ACCOUNT     CL#0309820N                
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA050'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3299049389'     BANK ACCOUNT     CL#0309820N                
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA051'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3299049389'     BANK ACCOUNT     CL#0309820N                
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA052'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359870550'     BANK ACCOUNT     CL#0389418N                
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA093'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359870550'     BANK ACCOUNT     CL#0389418N                
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA094'         DDS CASH ACCOUNT                            
*                                                                               
* MGNYF - BANK OF AMERICA CL# 0256886N                                          
*                                                                               
         DC    CL20'3359165357'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA035'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359165357'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA036'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359165357'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA037'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359165357'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA038'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359165357'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA039'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359871004'     BANK ACCOUNT - CL#0389418N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA101'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359871004'     BANK ACCOUNT - CL#0389418N                  
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA102'         DDS CASH ACCOUNT                            
*                                                                               
* SENYF - BANK OF AMERICA CL# 0256886N                                          
*                                                                               
         DC    CL20'3299064958'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA040'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3299064958'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA041'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3299064958'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA042'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359482075'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA062'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359482075'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA063'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359482075'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA064'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995209'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA134'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995209'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA135'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995209'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA136'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995209'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA137'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995209'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA138'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995829'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA140'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995829'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA141'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995829'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA142'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995829'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA143'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995829'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA144'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995829'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA149'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359995829'     BANK ACCOUNT - CUSTENH-2859                 
         DC    AL2(14836)           ORIGIN ID - GROUPMC0                        
         DC    CL12'DBA150'         DDS CASH ACCOUNT                            
*                                                                               
* MSKNCK/MSKGCK/MSKWCK/MSKLCK - BANK OF AMERICA                                 
*                                                                               
         DC    CL20'3360203403'     BANK ACCOUNT - SPEC-41810                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA186'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3360203403'     BANK ACCOUNT - SPEC-41810                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA189'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3360203403'     BANK ACCOUNT - SPEC-41810                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA187'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3360203403'     BANK ACCOUNT - SPEC-41810                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA190'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3360203403'     BANK ACCOUNT - SPEC-41810                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA195'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3360203171'     BANK ACCOUNT - SPEC-41810                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA192'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3360203171'     BANK ACCOUNT - SPEC-41810                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA193'         DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-49782                                                                  
         DC    CL20'3359491647'     BANK ACCOUNT - SPEC-49782                   
         DC    AL2(14836)           ORIGIN ID - GROUPMCO                        
         DC    CL12'DBA185'         DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-49782                                                                  
*                                                                               
* MSCONMO - BANK OF AMERICA                                                     
*                                                                               
         DC    CL20'3299793796'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11397)           ORIGIN ID - MSCONMO                         
         DC    CL12'DBA010'         BOA HOUSE DISBURSEMENTS (M-ONE)             
*                                                                               
* MSNYMONE - BANK OF AMERICA                                                    
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA009'         BOA NETWORK DISBURSEMENTS (M-ONE)           
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA012'         BOA SPOT DISBURSEMENTS (M-ONE)              
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA013'         BOA PRINT DISBURSEMENTS (M-ONE)             
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA014'         BOA PROD DISBURSEMENTS (M-ONE)              
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA031'         BOA PROD DISBURSEMENTS (M-ONE)              
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA032'         BOA PROD DISBURSEMENTS (M-ONE)              
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA033'         BOA PROD DISBURSEMENTS (M-ONE)              
*                                                                               
         DC    CL20'3299026932'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(11335)           ORIGIN ID - MSNYMONE                        
         DC    CL12'DBA034'         BOA PROD DISBURSEMENTS (M-ONE)              
*                                                                               
* MVNYCK - CITIBANK                                                             
*                                                                               
         DC    CL20'38693471'       BANK ACCOUNT   IMDB#2214721                 
         DC    AL2(11900)           ORIGIN ID - MVNYCK                          
         DC    CL12'CBD10830'       CITIBANK-DISBURSEMENTS-MEDIAVEST            
*                                                                               
* MVSEC - BANK ONE                                                              
*                                                                               
         DC    CL20'0000630546'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(9211)            ORIGIN ID - MVSEC                           
         DC    CL12'B1D10150'       BANK ONE DISBURSEMENT ACCT                  
*                                                                               
         DC    CL20'0000630546'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(9211)            ORIGIN ID - MVSEC                           
         DC    CL12'CKDNET'         BANK ONE NETWORK DISBURSEMENTS              
*                                                                               
         DC    CL20'0000630546'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(9211)            ORIGIN ID - MVSEC                           
         DC    CL12'CKDPRINT'       BANK ONE PRINT   DISBURSEMENTS              
*                                                                               
         DC    CL20'0000630546'     BANK ACCOUNT - MUST BE 10 BYTES             
         DC    AL2(9211)            ORIGIN ID - MVSEC                           
         DC    CL12'CKDSPOT'        BANK ONE SPOT    DISBURSEMENTS              
*                                                                               
* SJR - FORK                                                                    
*                                                                               
         DC    CL20'123456789012'   BANK ACCOUNT                                
         DC    AL2(17)              ORIGIN ID - SJR                             
         DC    CL12'B001'                                                       
*                                                                               
         DC    CL20'123456789012'   BANK ACCOUNT                                
         DC    AL2(17)              ORIGIN ID - SJR                             
         DC    CL12'B002'                                                       
*                                                                               
         DC    CL20'123456789012'   BANK ACCOUNT                                
         DC    AL2(17)              ORIGIN ID - SJR                             
         DC    CL12'B003'                                                       
*                                                                               
         DC    CL20'123456789012'   BANK ACCOUNT                                
         DC    AL2(17)              ORIGIN ID - SJR                             
         DC    CL12'B004'                                                       
*                                                                               
         DC    CL20'123456789012'   BANK ACCOUNT                                
         DC    AL2(17)              ORIGIN ID - SJR                             
         DC    CL12'B009'                                                       
*                                                                               
* MSDAF - BANK OF AMERICA CL# 0309851N                                          
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT    CL#0309851N                 
         DC    AL2(15152)           ORIGIN ID - MSDAF                           
         DC    CL12'DBA057'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT    CL#0309851N                 
         DC    AL2(15152)           ORIGIN ID - MSDAF                           
         DC    CL12'DBA058'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT    CL#0309851N                 
         DC    AL2(15152)           ORIGIN ID - MSDAF                           
         DC    CL12'DBA059'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT    CL#0309851N                 
         DC    AL2(15152)           ORIGIN ID - MSDAF                           
         DC    CL12'DBA060'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'3359322321'     BANK ACCOUNT    CL#0309851N                 
         DC    AL2(15152)           ORIGIN ID - MSDAF                           
         DC    CL12'DBA061'         DDS CASH ACCOUNT                            
*                                                                               
* MGCTF - BANK OF AMERICA CL# 0309852N                                          
*                                                                               
         DC    CL20'3359165357'     BANK ACCOUNT    CL#0309852N                 
         DC    AL2(15116)           ORIGIN ID - MGCTF                           
         DC    CL12'DBA064'         DDS CASH ACCOUNT                            
*                                                                               
* ZDCA - HSBC                                                                   
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDEMPD'         HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDEMPI'         HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDEMPO'         HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDEXPD'         HSBC - EXPENSES (ZD)                        
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDEXPI'         HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDEXPO'         HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDPRINTD'       HSBC - PRINT (ZD)                           
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDPRINTI'       HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDPRINTO'       HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDPROD'         HSBC - PRODUCTION(ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDPRODI'        HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDPRODO'        HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDSPOTD'        HSBC - SPOT (ZD)                            
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDSPOTI'        HSBC - EMPLOYEES (ZD)                       
*                                                                               
         DC    CL20'    797300376'  BANK ACCOUNT 13 BYTES (APR 2003)            
         DC    AL2(9640)            ORIGIN ID - ZDCA                            
         DC    CL12'HDSPOTO'        HSBC - EMPLOYEES (ZD)                       
*                                                                               
* ZENYA - HSBC                                                                  
*                                                                               
         DC    CL20'    797300368'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDEMP'          HSBC - EMPLOYEES                            
*                                                                               
         DC    CL20'    797300368'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDEXP'          HSBC - EXPENSES                             
*                                                                               
         DC    CL20'    797300368'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDPRINT'        HSBC - PRINT                                
*                                                                               
         DC    CL20'    797300368'  BANK ACCOUNT                                
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDSPOT'         HSBC - SPOT                                 
*                                                                               
         DC    CL20'    797300406'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDEMP'          HSBC - EMPLOYEES                            
*                                                                               
         DC    CL20'    797300406'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDEXP'          HSBC - EXPENSES                             
*                                                                               
         DC    CL20'    797300406'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDPRINT'        HSBC - PRINT                                
*                                                                               
         DC    CL20'    797300406'  BANK ACCOUNT                                
         DC    AL2(4617)            ORIGIN ID - ZENYA                           
         DC    CL12'HDSPOT'         HSBC - SPOT                                 
*                                                                               
* ZDCACK - ABN AMRO                                                             
*                                                                               
         DC    CL20'5598880218'     BANK ACCOUNT  IMDB#1889141                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEMP'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEMPD'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEMPI'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEMPO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880218'     BANK ACCOUNT  IMDB#1889141                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEXP'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEXPD'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEXPI'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADEXPO'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880218'     BANK ACCOUNT  IMDB#1889141                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRINT'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRINTD'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRINTI'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRINTO'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPROD'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRODD'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRODI'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRODO'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADPRODUCTION'   DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880218'     BANK ACCOUNT  IMDB#1889141                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADSPOT'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADSPOTD'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADSPOTI'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'5598880234'     BANK ACCOUNT  IMDB#1889411                  
         DC    AL2(11054)           ORIGIN ID - ZDCACK                          
         DC    CL12'ADSPOTO'        DDS CASH ACCOUNT                            
*                                                                               
*        DC    CL20'5598880218'     BANK ACCOUNT  IMDB#2882931                  
*        DC    AL2(11054)           ORIGIN ID - ZDCACK                          
*        DC    CL12'ADSPOTPE'       DDS CASH ACCOUNT                            
*                                                                               
* OSZNYA - HSBC                                                                 
*                                                                               
         DC    CL20'    797300406'  BANK ACCOUNT 13 BYTES                       
         DC    AL2(11682)           ORIGIN ID - OSZNYA                          
         DC    CL12'HDPRINT'        HSBC - PRINT                                
*                                                                               
         DC    CL20'    797300406'  BANK ACCOUNT                                
         DC    AL2(11682)           ORIGIN ID - OSZNYA                          
         DC    CL12'HDSPOT'         HSBC - SPOT                                 
*                                                                               
* JWTOAC - HSBC (DEC 2009) CL#01064432                                          
*                                                                               
*        DC    CL20'002575175001'  BANK ACCOUNT     CL#01064432                 
*        DC    AL2(7567)            ORIGIN ID - JWTOAC                          
*        DC    CL12'CHSBO'          DDS CASH ACCOUNT                            
*                                                                               
*        DC    CL20'002575175070'  BANK ACCOUNT     CL#01064432                 
*        DC    AL2(7567)            ORIGIN ID - JWTOAC                          
*        DC    CL12'UHSBO'          DDS CASH ACCOUNT                            
*                                                                               
* HLTOH  - HSBC (NOV 2010)                                                      
*                                                                               
         DC    CL20'002575132001'  BANK ACCOUNT     CL#0106443T                 
         DC    AL2(3426)            ORIGIN ID - HLTOH                           
         DC    CL12'5DIS'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002575132001'  BANK ACCOUNT     CL#0106443T                 
         DC    AL2(3426)            ORIGIN ID - HLTOH                           
         DC    CL12'5MAN'           DDS CASH ACCOUNT                            
*                                                                               
* NEOTO  - HSBC (JAN 2010)                                                      
*                                                                               
         DC    CL20'002579901001'  BANK ACCOUNT     CL#0106470T                 
         DC    AL2(14215)           ORIGIN ID - NEOTO                           
         DC    CL12'CHSB112377'     DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002579901070'  BANK ACCOUNT     CL#0106470T                 
         DC    AL2(14215)           ORIGIN ID - NEOTO                           
         DC    CL12'UHSB112378'     DDS CASH ACCOUNT                            
*                                                                               
* MVCTOA - CITIBANK 200                                                         
*                                                                               
         DC    CL20'2016260004'     BANK ACCOUNT     CL#0108625T                
         DC    AL2(10368)           ORIGIN ID - MVCTOA                          
         DC    CL12'M00201'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'2016260098'     BANK ACCOUNT     CL#0108625T                
         DC    AL2(10368)           ORIGIN ID - MVCTOA                          
         DC    CL12'M00210'         DDS CASH ACCOUNT                            
*                                                                               
* STWTO - CITIBANK 200                                                          
*                                                                               
         DC    CL20'2016261058'     BANK ACCOUNT     CL#0108625T                
         DC    AL2(9388)            ORIGIN ID - STWTO                           
         DC    CL12'L00201'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'2016261042'     BANK ACCOUNT     CL#0108625T                
         DC    AL2(9388)            ORIGIN ID - STWTO                           
         DC    CL12'L00210'         DDS CASH ACCOUNT                            
*                                                                               
* SOTOT - CITIBANK 200                                                          
*                                                                               
         DC    CL20'2015690012'     BANK ACCOUNT     CL#0108993T                
         DC    AL2(3586)            ORIGIN ID - SOTOT                           
         DC    CL12'1107'           DDS CASH ACCOUNT                            
*                                                                               
* ZOTOA - CITIBANK 200                                                          
*                                                                               
         DC    CL20'2016258011'     BANK ACCOUNT     CL#0108992T                
         DC    AL2(13380)           ORIGIN ID - ZOTOA                           
         DC    CL12'CCITI'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'2016258027'     BANK ACCOUNT     CL#0108992T                
         DC    AL2(13380)           ORIGIN ID - ZOTOA                           
         DC    CL12'UCITI'          DDS CASH ACCOUNT                            
*                                                                               
* YRAKD - HSBC DSFTK-76/DSFTK-74                                                
*                                                                               
         DC    CL20'002688867001'   BANK ACCOUNT     DSFTK-76/74                
         DC    AL2(17228)           ORIGIN ID - YRAKD                           
         DC    CL12'CHS67001Q'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002688867001'   BANK ACCOUNT     DSFTK-76/74                
         DC    AL2(17228)           ORIGIN ID - YRAKD                           
         DC    CL12'CHS67001T'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002688867001'   BANK ACCOUNT     DSFTK-76/74                
         DC    AL2(17228)           ORIGIN ID - YRAKD                           
         DC    CL12'CHS67001P'      DDS CASH ACCOUNT                            
*                                                                               
* YRAKD - HSBC DSFTK-148/DSFTK-149                                              
*                                                                               
         DC    CL20'090202953001'   BANK ACCOUNT     DSFTK-148/149              
         DC    AL2(17537)           ORIGIN ID - YRBRD                           
         DC    CL12'CHS53001Q'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'090202953001'   BANK ACCOUNT     DSFTK-148/149              
         DC    AL2(17537)           ORIGIN ID - YRBRD                           
         DC    CL12'CHS53001T'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'090202953001'   BANK ACCOUNT     DSFTK-148/149              
         DC    AL2(17537)           ORIGIN ID - YRBRD                           
         DC    CL12'CHS53001P'      DDS CASH ACCOUNT                            
*                                                                               
* YRYTD - HSBC CL#0108996T                                                      
*                                                                               
         DC    CL20'002582732001'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(11587)           ORIGIN ID - YRYTD                           
         DC    CL12'CHS32001Q'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002582732001'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(11587)           ORIGIN ID - YRYTD                           
         DC    CL12'CHS32001T'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002582732001'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(11587)           ORIGIN ID - YRYTD                           
         DC    CL12'CHS32001P'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002582732070'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(11587)           ORIGIN ID - YRYTD                           
         DC    CL12'UHS32070S'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002582732070'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(11587)           ORIGIN ID - YRYTD                           
         DC    CL12'UHS32070U'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002582732070'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(11587)           ORIGIN ID - YRYTD                           
         DC    CL12'UHS32070W'      DDS CASH ACCOUNT                            
*                                                                               
* YRYQD - HSBC CL#0108996T                                                      
*                                                                               
         DC    CL20'002583119001'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(14168)           ORIGIN ID - YRYQD                           
         DC    CL12'CHS19001Q'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002583119001'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(14168)           ORIGIN ID - YRYQD                           
         DC    CL12'CHS19001T'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002583119001'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(14168)           ORIGIN ID - YRYQD                           
         DC    CL12'CHS19001P'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002583119070'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(14168)           ORIGIN ID - YRYQD                           
         DC    CL12'CHS19070S'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002583119070'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(14168)           ORIGIN ID - YRYQD                           
         DC    CL12'CHS19070U'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002583119070'   BANK ACCOUNT     CL#0108996T                
         DC    AL2(14168)           ORIGIN ID - YRYQD                           
         DC    CL12'CHS19070W'      DDS CASH ACCOUNT                            
*                                                                               
* SHGMO - HSBC CL#0114178T/0111707T                                             
*                                                                               
         DC    CL20'002575140001'   BANK ACCOUNT     CL#0114178T                
         DC    AL2(8441)            ORIGIN ID - SHGMO CL#0111707T               
         DC    CL12'CHS40001Q'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002575140001'   BANK ACCOUNT     CL#0114178T                
         DC    AL2(8441)            ORIGIN ID - SHGMO CL#0111707T               
         DC    CL12'CHS40001T'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002575140001'   BANK ACCOUNT     CL#0114178T                
         DC    AL2(8441)            ORIGIN ID - SHGMO CL#0111707T               
         DC    CL12'CHS40001P'      DDS CASH ACCOUNT                            
*                                                                               
* SHMF - HSBC CL#0114178T/0111707T                                              
*                                                                               
         DC    CL20'002588765001'   BANK ACCOUNT     CL#0114178T                
         DC    AL2(13646)           ORIGIN ID - SHMF  CL#0111707T               
         DC    CL12'CHS65001Q'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002588765001'   BANK ACCOUNT     CL#0114178T                
         DC    AL2(13646)           ORIGIN ID - SHMF  CL#0111707T               
         DC    CL12'CHS65001T'      DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002588765001'   BANK ACCOUNT     CL#0114178T                
         DC    AL2(13646)           ORIGIN ID - SHMF  CL#0111707T               
         DC    CL12'CHS65001P'      DDS CASH ACCOUNT                            
*                                                                               
* GGNY - HSBC CL#0309345N                                                       
*                                                                               
         DC    CL20'    797302344'  BANK ACCOUNT     CL#0309345N                
         DC    AL2(13915)           ORIGIN ID - GGNY                            
         DC    CL12'01D810000001'   DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'    797303260'  BANK ACCOUNT     CL#0395981N                
         DC    AL2(13915)           ORIGIN ID - GGNY                            
         DC    CL12'01D810000012'   DDS CASH ACCOUNT                            
*                                                                               
* CARDIS - JP MORGAN CHASE 200 CL#0325289N                                      
*                                                                               
         DC    CL20'00000000000967385188'  BANK ACCOUNT CL#0325289N             
         DC    AL2(15672)           ORIGIN ID - GGNY                            
         DC    CL12'01JCD110375'    DDS CASH ACCOUNT                            
*                                                                               
* CARGMCK - JP MORGAN CHASE200 CL#0361103N                                      
*                                                                               
         DC    CL20'00000000000945959781'  BANK ACCOUNT CL#0361103N             
         DC    AL2(16201)           ORIGIN ID - CARDIS                          
         DC    CL12'01JCD110380'    DDS CASH ACCOUNT                            
*                                                                               
* CARDECK - JP MORGAN CHASE200 CL#0378135N                                      
*                                                                               
         DC    CL20'00000000000945960318'  BANK ACCOUNT CL#0378135N             
         DC    AL2(16477)           ORIGIN ID - CARDECK                         
         DC    CL12'01JCD110390'    DDS CASH ACCOUNT                            
*                                                                               
* CARMCCK - JP MORGAN CHASE200 CL#0385154N                                      
*                                                                               
         DC    CL20'00000000000945960599'  BANK ACCOUNT CL#0385154N             
         DC    AL2(16487)           ORIGIN ID - CARMCCK                         
         DC    CL12'01JCD110310'    DDS CASH ACCOUNT                            
*                                                                               
* CARSEC - JP MORGAN CHASE200 DSFTK-104                                         
* CARSEC - CSI ENTERPRISES    DSFTK-104                                         
*                                                                               
         DC    CL20'00000000000939329314'  BANK ACCOUNT DSFTK-104               
         DC    AL2(07033)           ORIGIN ID - CARLA                           
         DC    CL12'01JCD110300'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000939329314'  BANK ACCOUNT DSFTK-104               
         DC    AL2(17458)           ORIGIN ID - CARCSI                          
         DC    CL12'01JCDCSI300'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000967385212'  BANK ACCOUNT DSFTK-104               
         DC    AL2(15663)           ORIGIN ID - CARTHD                          
         DC    CL12'01JCD110365'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000945960599'  BANK ACCOUNT DSFTK-104               
         DC    AL2(16487)           ORIGIN ID - CARMCCK                         
         DC    CL12'01JCD110310'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000945959781'  BANK ACCOUNT DSFTK-104               
         DC    AL2(16201)           ORIGIN ID - CARGMCK                         
         DC    CL12'01JCD110380'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000967385188'  BANK ACCOUNT DSFTK-104               
         DC    AL2(15672)           ORIGIN ID - CARDIS                          
         DC    CL12'01JCD110375'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000967385220'  BANK ACCOUNT DSFTK-104               
         DC    AL2(15507)           ORIGIN ID - CARRML                          
         DC    CL12'01JCD110360'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000967385196'  BANK ACCOUNT DSFTK-104               
         DC    AL2(08800)           ORIGIN ID - CARPF                           
         DC    CL12'01JCD110350'    DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'00000000000967385204'  BANK ACCOUNT DSFTK-104               
         DC    AL2(15111)           ORIGIN ID - CARWY                           
         DC    CL12'01JCD110355'    DDS CASH ACCOUNT                            
*                                                                               
*                                                                               
* MSHTOAC- HSBC CL#0111793T                                                     
*                                                                               
         DC    CL20'002620790001'   BANK ACCOUNT     CL#0111793T                
         DC    AL2(8722)            ORIGIN ID - MSHTOAC                         
         DC    CL12'CHSBD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790070'   BANK ACCOUNT     CL#0111793T                
         DC    AL2(8722)            ORIGIN ID - MSHTOAC                         
         DC    CL12'UHSBD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790001'   BANK ACCOUNT     DSFTK-38                   
         DC    AL2(8722)            ORIGIN ID - MSHTOAC                         
         DC    CL12'CHSBCOS'        DDS CASH ACCOUNT                            
*                                                                               
* MSHQU - HSBC CL#0111793T                                                      
*                                                                               
         DC    CL20'002620766001'   BANK ACCOUNT     CL#0111793T                
         DC    AL2(14108)           ORIGIN ID - MSHQU                           
         DC    CL12'CHSQD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620766001'   BANK ACCOUNT     DSFTK-38                   
         DC    AL2(14108)           ORIGIN ID - MSHQU                           
         DC    CL12'CHSQCOS'        DDS CASH ACCOUNT                            
*                                                                               
* MSHUNI - HSBC CL#0111793T                                                     
*                                                                               
         DC    CL20'002620790002'   BANK ACCOUNT     CL#0111793T                
         DC    AL2(15123)           ORIGIN ID - MSHUNI                          
         DC    CL12'CHSUD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790002'   BANK ACCOUNT     DSFTK-38                   
         DC    AL2(15123)           ORIGIN ID - MSHUNI                          
         DC    CL12'CHSUCOS'        DDS CASH ACCOUNT                            
*                                                                               
* MAXUS - HSBC                                                                  
*                                                                               
         DC    CL20'002220245001'   BANK ACCOUNT     CL#0111793T                
         DC    AL2(15898)           ORIGIN ID - MAXUS                           
         DC    CL12'CHKMD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002220245070'   BANK ACCOUNT     CL#0111793T                
         DC    AL2(15898)           ORIGIN ID - MAXUS                           
         DC    CL12'UHKMD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002220245001'   BANK ACCOUNT     DSFTK-32                   
         DC    AL2(15898)           ORIGIN ID - MAXUS                           
         DC    CL12'CHKMCOS'        DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-17409                                                                  
* MAXTOA - HSBC                                                                 
*                                                                               
*        DC    CL20'002220245001'   BANK ACCOUNT     DSFTK-32                   
*        DC    AL2(16817)           ORIGIN ID - MAXTOA  SPEC-17409              
*        DC    CL12'CHSAD'          DDS CASH ACCOUNT                            
*                                                                               
*        DC    CL20'002220245070'   BANK ACCOUNT     DSFTK-32                   
*        DC    AL2(16817)           ORIGIN ID - MAXTOA  SPEC-17409              
*        DC    CL12'UHSAD'          DDS CASH ACCOUNT                            
*                                                                               
*        DC    CL20'002220245001'   BANK ACCOUNT     DSFTK-71                   
*        DC    AL2(16817)           ORIGIN ID - MAXTOA  SPEC-17409              
*        DC    CL12'CHSACOS'        DDS CASH ACCOUNT                            
*MN SPEC-17409                                                                  
*                                                                               
*                                                                               
* WAVETO - HSBC                                                                 
*                                                                               
         DC    CL20'002220245001'   BANK ACCOUNT     SPEC-17409                 
         DC    AL2(19487)           ORIGIN ID - WAVETO                          
         DC    CL12'CHSAD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002220245070'   BANK ACCOUNT     SPEC-17409                 
         DC    AL2(19487)           ORIGIN ID - WAVETO                          
         DC    CL12'UHSAD'          DDS CASH ACCOUNT                            
*                                                                               
* WAVETCO - HSBC                                                                
*                                                                               
         DC    CL20'002220245001'   BANK ACCOUNT     SPEC-17409                 
         DC    AL2(19541)           ORIGIN ID - WAVETCO                         
         DC    CL12'CHSACOS'        DDS CASH ACCOUNT                            
*                                                                               
* WAVETOE - HSBC                                                                
*                                                                               
         DC    CL20'002220245001'   BANK ACCOUNT     SPEC-17409                 
         DC    AL2(19542)           ORIGIN ID - WAVETOE                         
         DC    CL12'CHSADE'         DDS CASH ACCOUNT                            
*                                                                               
* MPGCK - CL#0341108N                                                           
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15803)           ORIGIN ID - MPGCK                           
         DC    CL12'C701'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15803)           ORIGIN ID - MPGCK                           
         DC    CL12'C702'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15803)           ORIGIN ID - MPGCK                           
         DC    CL12'C703'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15803)           ORIGIN ID - MPGCK                           
         DC    CL12'C704'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15803)           ORIGIN ID - MPGCK                           
         DC    CL12'C705'           DDS CASH ACCOUNT                            
*                                                                               
* MPGERME - CL#0341108N                                                         
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15793)           ORIGIN ID - MPGERME                         
         DC    CL12'C715'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15793)           ORIGIN ID - MPGERME                         
         DC    CL12'C716'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15793)           ORIGIN ID - MPGERME                         
         DC    CL12'C717'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15793)           ORIGIN ID - MPGERME                         
         DC    CL12'C718'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38757009'       BANK ACCOUNT                                
         DC    AL2(15793)           ORIGIN ID - MPGERME                         
         DC    CL12'C719'           DDS CASH ACCOUNT                            
*                                                                               
* MPGHMI - CL#0341108N                                                          
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT                                
         DC    AL2(15794)           ORIGIN ID - MPGHMI                          
         DC    CL12'C725'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT                                
         DC    AL2(15794)           ORIGIN ID - MPGHMI                          
         DC    CL12'C726'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT                                
         DC    AL2(15794)           ORIGIN ID - MPGHMI                          
         DC    CL12'C727'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT                                
         DC    AL2(15794)           ORIGIN ID - MPGHMI                          
         DC    CL12'C728'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38814472'       BANK ACCOUNT                                
         DC    AL2(15794)           ORIGIN ID - MPGHMI                          
         DC    CL12'C729'           DDS CASH ACCOUNT                            
*                                                                               
* MPGMPI - CL#0341108N                                                          
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(15800)           ORIGIN ID - MPGMPI                          
         DC    CL12'C706'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(15800)           ORIGIN ID - MPGMPI                          
         DC    CL12'C707'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(15800)           ORIGIN ID - MPGMPI                          
         DC    CL12'C708'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(15800)           ORIGIN ID - MPGMPI                          
         DC    CL12'C709'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756997'       BANK ACCOUNT                                
         DC    AL2(15800)           ORIGIN ID - MPGMPI                          
         DC    CL12'C710'           DDS CASH ACCOUNT                            
*                                                                               
* MPGMC - CL#0341108N                                                           
*                                                                               
         DC    CL20'38756989'       BANK ACCOUNT                                
         DC    AL2(15795)           ORIGIN ID - MPGMC                           
         DC    CL12'C711'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756989'       BANK ACCOUNT                                
         DC    AL2(15795)           ORIGIN ID - MPGMC                           
         DC    CL12'C712'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38756989'       BANK ACCOUNT                                
         DC    AL2(15795)           ORIGIN ID - MPGMC                           
         DC    CL12'C713'           DDS CASH ACCOUNT                            
*                                                                               
* MPGAM - CL#0341108N                                                           
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15790)           ORIGIN ID - MPGAM                           
         DC    CL12'C736'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15790)           ORIGIN ID - MPGAM                           
         DC    CL12'C737'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15790)           ORIGIN ID - MPGAM                           
         DC    CL12'C738'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15790)           ORIGIN ID - MPGAM                           
         DC    CL12'C739'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38855733'       BANK ACCOUNT                                
         DC    AL2(15790)           ORIGIN ID - MPGAM                           
         DC    CL12'C740'           DDS CASH ACCOUNT                            
*                                                                               
*                                                                               
* MPGIGNCK - CL#0373065N                                                        
*                                                                               
         DC    CL20'38865392'       BANK ACCOUNT                                
         DC    AL2(16379)           ORIGIN ID - MPGIGNCK                        
         DC    CL12'C770'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38865392'       BANK ACCOUNT                                
         DC    AL2(16379)           ORIGIN ID - MPGIGNCK                        
         DC    CL12'C771'           DDS CASH ACCOUNT                            
*                                                                               
* MPGMT - CL#0341108N                                                           
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15851)           ORIGIN ID - MPGMT                           
         DC    CL12'C750'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15851)           ORIGIN ID - MPGMT                           
         DC    CL12'C751'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15851)           ORIGIN ID - MPGMT                           
         DC    CL12'C752'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38859734'       BANK ACCOUNT                                
         DC    AL2(15851)           ORIGIN ID - MPGMT                           
         DC    CL12'C753'           DDS CASH ACCOUNT                            
*                                                                               
* MPGMCMT - CL#0341108N                                                         
*                                                                               
         DC    CL20'38861308'       BANK ACCOUNT                                
         DC    AL2(15953)           ORIGIN ID - MPGMCMT                         
         DC    CL12'C760'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38861308'       BANK ACCOUNT                                
         DC    AL2(15953)           ORIGIN ID - MPTMCMT                         
         DC    CL12'C761'           DDS CASH ACCOUNT                            
*                                                                               
* HVMSOC - CL#0396050N                                                          
*                                                                               
         DC    CL20'38873018'       BANK ACCOUNT - CL#0396050N                  
         DC    AL2(16613)           ORIGIN ID - HVMSOC                          
         DC    CL12'C791'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'38873018'       BANK ACCOUNT - CL#0396050N                  
         DC    AL2(16613)           ORIGIN ID - HVMSOC                          
         DC    CL12'C792'           DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002600137001'   BANK ACCOUNT - CL#0114774T                  
         DC    AL2(16238)           ORIGIN ID - GMXAS                           
         DC    CL12'CHSXD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002631083001'   BANK ACCOUNT - CL#0114774T                  
         DC    AL2(14810)           ORIGIN ID - GRPMTO                          
         DC    CL12'CHSGD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002668602001'   BANK ACCOUNT : DSFTK-67                     
         DC    AL2(14346)           ORIGIN ID - OUTTO                           
         DC    CL12'CHSCD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002668602001'   BANK ACCOUNT : DSFTK-67                     
         DC    AL2(14346)           ORIGIN ID - OUTTO                           
         DC    CL12'CHSCCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002668602070'   BANK ACCOUNT : DSFTK-67                     
         DC    AL2(14346)           ORIGIN ID - OUTTO                           
         DC    CL12'UHSCD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407003'   BANK ACCOUNT : DSFTK-67                     
         DC    AL2(16820)           ORIGIN ID - MECEL                           
         DC    CL12'CHSLD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407003'   BANK ACCOUNT : DSFTK-67                     
         DC    AL2(16820)           ORIGIN ID - MECEL                           
         DC    CL12'CHSLCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407071'   BANK ACCOUNT : DSFTK-67                     
         DC    AL2(16820)           ORIGIN ID - MECEL                           
         DC    CL12'UHSLD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002631083070'   BANK ACCOUNT : DSFTK-67                     
         DC    AL2(14810)           ORIGIN ID - GRPMTO                          
         DC    CL12'UHSGD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002631083001'   BANK ACCOUNT : DSFTK-42                     
         DC    AL2(14810)           ORIGIN ID - GRPMTO                          
         DC    CL12'CHSGCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624982002'   BANK ACCOUNT - CL#0114988T                  
         DC    AL2(16210)           ORIGIN ID - MCMMP                           
         DC    CL12'CHSPGC'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624982002'   BANK ACCOUNT - DSFTK-70                     
         DC    AL2(16210)           ORIGIN ID - MCMMP                           
         DC    CL12'CHSPGCOS'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624982001'   BANK ACCOUNT - CL#0114988T                  
         DC    AL2(16208)           ORIGIN ID - MCMMC                           
         DC    CL12'CHSMCC'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624982001'   BANK ACCOUNT - DSFTK-70                     
         DC    AL2(16208)           ORIGIN ID - MCMMC                           
         DC    CL12'CHSMCCOS'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624982070'   BANK ACCOUNT - CL#0114988T                  
         DC    AL2(16208)           ORIGIN ID - MCMMC                           
         DC    CL12'UHSMCU'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624966001'   BANK ACCOUNT - CL#0114988T                  
         DC    AL2(16209)           ORIGIN ID - MCMMB                           
         DC    CL12'CHSMBC'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624966001'   BANK ACCOUNT - DSFTK-70                     
         DC    AL2(16209)           ORIGIN ID - MCMMB                           
         DC    CL12'CHSMBCOS'       DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002624966070'   BANK ACCOUNT - CL#0114988T                  
         DC    AL2(16209)           ORIGIN ID - MCMMB                           
         DC    CL12'UHSMBU'         DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407001'   BANK ACCOUNT - CL#0117826T                  
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7001P'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407001'   BANK ACCOUNT - CL#0117826T                  
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7001Q'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407001'   BANK ACCOUNT - CL#0117826T                  
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7001S'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407001'   BANK ACCOUNT - CL#0117826T                  
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7001X'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407001'   BANK ACCOUNT - DSFTK-69                     
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7001COS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407070'   BANK ACCOUNT - DSFTK-69                     
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7070P'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407070'   BANK ACCOUNT - DSFTK-69                     
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7070S'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407070'   BANK ACCOUNT - DSFTK-69                     
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7070W'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407070'   BANK ACCOUNT - DSFTK-69                     
         DC    AL2(12612)           ORIGIN ID - YRTOMC                          
         DC    CL12'7070Y'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407001'   BANK ACCOUNT - DSFTK-118                    
         DC    AL2(16819)           ORIGIN ID - MECEM                           
         DC    CL12'CHSECOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407001'   BANK ACCOUNT - DSFTK-118                    
         DC    AL2(16819)           ORIGIN ID - MECEM                           
         DC    CL12'CHSED'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002619407070'   BANK ACCOUNT - DSFTK-118                    
         DC    AL2(16819)           ORIGIN ID - MECEM                           
         DC    CL12'UHSED'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790001'   BANK ACCOUNT - DSFTK-125                    
         DC    AL2(17242)           ORIGIN ID - MNSTOA                          
         DC    CL12'CHSNCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790001'   BANK ACCOUNT - DSFTK-125                    
         DC    AL2(17242)           ORIGIN ID - MNSTOA                          
         DC    CL12'CHSND'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790070'   BANK ACCOUNT - DSFTK-125                    
         DC    AL2(17242)           ORIGIN ID - MNSTOA                          
         DC    CL12'UHSND'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620766001'   BANK ACCOUNT - DSFTK-125                    
         DC    AL2(17243)           ORIGIN ID - MNSQU                           
         DC    CL12'CHSQCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620766001'   BANK ACCOUNT - DSFTK-125                    
         DC    AL2(17243)           ORIGIN ID - MNSQU                           
         DC    CL12'CHSQD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790002'   BANK ACCOUNT - DSFTK-125                    
         DC    AL2(17254)           ORIGIN ID - MNSUNI                          
         DC    CL12'CHSUCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002620790002'   BANK ACCOUNT - DSFTK-125                    
         DC    AL2(17254)           ORIGIN ID - MNSUNI                          
         DC    CL12'CHSUD'          DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-45974                                                                  
         DC    CL20'002620790002'   BANK ACCOUNT  SPEC-45974                    
         DC    AL2(17254)           ORIGIN ID MNSUNI                            
         DC    CL12'CHSUDE'         SC ACCOUNT                                  
*                                                                               
*MN SPEC-45974                                                                  
* WAVEL  - SPEC-17408                                                           
*                                                                               
         DC    CL20'002220245002'   BANK ACCOUNT : SPEC-17408                   
         DC    AL2(19545)           ORIGIN ID - WAVEL                           
         DC    CL12'CHSKD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002220245071'   BANK ACCOUNT : SPEC-17408                   
         DC    AL2(19545)           ORIGIN ID - WAVEL                           
         DC    CL12'UHSKD'          DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-45973                                                                  
         DC    CL20'002220245002'   BANK ACCOUNT  SPEC-45973                    
         DC    AL2(19545)           ORIGIN ID WAVEL                             
         DC    CL12'CHSKDE'         SC ACCOUNT                                  
*                                                                               
*MN SPEC-45973                                                                  
* WAVELCO - SPEC-17408                                                          
*                                                                               
         DC    CL20'002220245002'   BANK ACCOUNT : SPEC-17408                   
         DC    AL2(19546)           ORIGIN ID - WAVELCO                         
         DC    CL12'CHSKCOS'        DDS CASH ACCOUNT                            
*                                                                               
* WAVELCO - SPEC-17582                                                          
*                                                                               
         DC    CL20'002847841001'   BANK ACCOUNT : SPEC-17582                   
         DC    AL2(19468)           ORIGIN ID - ESSTOA                          
         DC    CL12'CHSSD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002847841070'   BANK ACCOUNT : SPEC-17582                   
         DC    AL2(19468)           ORIGIN ID - ESSTOA                          
         DC    CL12'UHSSD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002847841001'   BANK ACCOUNT : SPEC-17582                   
         DC    AL2(19468)           ORIGIN ID - ESSTOA                          
         DC    CL12'CHSSCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002847841001'   BANK ACCOUNT : SPEC-17582                   
         DC    AL2(19468)           ORIGIN ID - ESSTOA                          
         DC    CL12'CHSSDE'         DDS CASH ACCOUNT                            
*MN SPEC-46427                                                                  
*                                                                               
* OPNTOA - SPEC-46427                                                           
*                                                                               
         DC    CL20'002575167001'   BANK ACCOUNT : SPEC-46427                   
         DC    AL2(20867)           ORIGIN ID - OPNTOA                          
         DC    CL12'CHSBE'          DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-46427                                                                  
*MN SPEC-46608                                                                  
*                                                                               
* OPNTOA - SPEC-46608                                                           
*                                                                               
         DC    CL20'OPNTBA899 '     BANK ACCOUNT : SPEC-46608                   
         DC    AL2(20867)           ORIGIN ID - OPNTOA                          
         DC    CL12'CBA BCC'        DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-46608                                                                  
*MN SPEC-47517                                                                  
*                                                                               
* OPNTOA - SPEC-47517                                                           
*                                                                               
         DC    CL20'002575167001'   BANK ACCOUNT : SPEC-47517                   
         DC    AL2(20867)           ORIGIN ID - OPNTOA                          
         DC    CL12'CHSBD'          DDS CASH ACCOUNT                            
*                                                                               
*                                                                               
         DC    CL20'002575167001'   BANK ACCOUNT : SPEC-47517                   
         DC    AL2(20867)           ORIGIN ID - OPNTOA                          
         DC    CL12'CHSBCOS'        DDS CASH ACCOUNT                            
*                                                                               
*MN SPEC-47517                                                                  
*                                                                               
* ESSTOA - SPEC-19131                                                           
*                                                                               
         DC    CL20'ESSTBA654 '     BANK ACCOUNT : SPEC-19131                   
         DC    AL2(19468)           ORIGIN ID - ESSTOA                          
         DC    CL12'CBA SCC'        DDS CASH ACCOUNT                            
*                                                                               
* NWOTOA - SPEC-22303                                                           
*                                                                               
         DC    CL20'NWOTBA650 '     BANK ACCOUNT : SPEC-22303                   
         DC    AL2(19690)           ORIGIN ID - NWOTOA                          
         DC    CL12'CBA OCC'        DDS CASH ACCOUNT                            
*                                                                               
* SIXTOA - SPEC-22750                                                           
*                                                                               
         DC    CL20'SIXTBA652 '     BANK ACCOUNT : SPEC-22750                   
         DC    AL2(18626)           ORIGIN ID - SIXTOA                          
         DC    CL12'CBA ICC'        DDS CASH ACCOUNT                            
*                                                                               
* NWOTOA - SPEC-21410                                                           
*                                                                               
         DC    CL20'002842939001'   BANK ACCOUNT : SPEC-21410                   
         DC    AL2(19690)           ORIGIN ID - NWOTOA                          
         DC    CL12'CHSOD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002842939001'   BANK ACCOUNT : SPEC-21410                   
         DC    AL2(19690)           ORIGIN ID - NWOTOA                          
         DC    CL12'CHSOCOS'        DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002842939001'   BANK ACCOUNT : SPEC-21410                   
         DC    AL2(19690)           ORIGIN ID - NWOTOA                          
         DC    CL12'CHSODE'         DDS CASH ACCOUNT                            
*                                                                               
*                                                                               
* MIEXCA - SPEC-10303                                                           
*                                                                               
         DC    CL20'002809362001'   BANK ACCOUNT                                
         DC    AL2(18795)           ORIGIN ID - MIEXCA                          
         DC    CL12'CHSDD'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002809362001'   BANK ACCOUNT                                
         DC    AL2(18795)           ORIGIN ID - MIEXCA                          
         DC    CL12'CHSDCOS'        DDS CASH ACCOUNT                            
*                                                                               
* SIXTOA - SPEC-10303                                                           
*                                                                               
         DC    CL20'002797798001'   BANK ACCOUNT                                
         DC    AL2(18626)           ORIGIN ID - SIXTOA                          
         DC    CL12'CHSID'          DDS CASH ACCOUNT                            
*                                                                               
         DC    CL20'002797798001'   BANK ACCOUNT                                
         DC    AL2(18626)           ORIGIN ID - SIXTOA                          
         DC    CL12'CHSICOS'        DDS CASH ACCOUNT                            
*                                                                               
* MCMMF  - SPEC-10303                                                           
*                                                                               
         DC    CL20'002624982001'   BANK ACCOUNT                                
         DC    AL2(16243)           ORIGIN ID - MCMMF                           
         DC    CL12'CHSMFCOS'       DDS CASH ACCOUNT                            
*                                                                               
* TPPS - JPMORGAN CHASE                                                         
*                                                                               
         DC    CL20'0163879710'     BANK ACCOUNT   CL#0392997N                  
         DC    AL2(16557)           ORIGIN ID                                   
         DC    CL12'8107'           SC ACCOUNT                                  
*                                                                               
*                                                                               
* GROUPM CANADA - BANK OF AMERICA                                               
*                                                                               
         DC    CL20'GRPMBA308'      BANK ACCOUNT  DSFTK-84                      
         DC    AL2(14810)           ORIGIN ID GRPMTO                            
         DC    CL12'CBA GCC'        SC ACCOUNT                                  
*                                                                               
*MN SPEC-19359                                                                  
*        DC    CL20'MAXTBA309'      BANK ACCOUNT  DSFTK-84                      
*        DC    AL2(16817)           ORIGIN ID MAXTOA                            
*        DC    CL12'CBA ACC'        SC ACCOUNT                                  
*                                                                               
*MN SPEC-19359                                                                  
         DC    CL20'MCMMBBA310'     BANK ACCOUNT  DSFTK-84                      
         DC    AL2(16209)           ORIGIN ID MCMMB                             
         DC    CL12'CBA MBCC'       SC ACCOUNT                                  
*                                                                               
         DC    CL20'YRTOMBA311'     BANK ACCOUNT  DSFTK-84                      
         DC    AL2(10823)           ORIGIN ID YRTOME                            
         DC    CL12'7000BA'         SC ACCOUNT                                  
*                                                                               
         DC    CL20'MECELBA312'     BANK ACCOUNT  DSFTK-84                      
         DC    AL2(16820)           ORIGIN ID MECEL                             
         DC    CL12'CBA LCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'MCMMCBA313'     BANK ACCOUNT  DSFTK-84                      
         DC    AL2(16208)           ORIGIN ID MCMMC                             
         DC    CL12'CBA MCCC'       SC ACCOUNT                                  
*                                                                               
         DC    CL20'MCMMFBA313'     BANK ACCOUNT  DSFTK-84                      
         DC    AL2(16243)           ORIGIN ID MCMMF                             
         DC    CL12'CBA MFCC'       SC ACCOUNT                                  
*                                                                               
         DC    CL20'MSHQBA315'      BANK ACCOUNT  DSFTK-84                      
         DC    AL2(14108)           ORIGIN ID MSHQU                             
         DC    CL12'CBAMQCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'MSHUBA316'      BANK ACCOUNT  DSFTK-84                      
         DC    AL2(15123)           ORIGIN ID MSHUNI                            
         DC    CL12'CBAMUCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'OUTTBA307'      BANK ACCOUNT  DSFTK-84                      
         DC    AL2(14346)           ORIGIN ID OUTTO                             
         DC    CL12'CBA CCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'MCMMPBA332'     BANK ACCOUNT  DSFTK-84                      
         DC    AL2(16210)           ORIGIN ID MCMMP                             
         DC    CL12'CBA MPCC'       SC ACCOUNT                                  
*                                                                               
         DC    CL20'MECTBA311'      BANK ACCOUNT  DSFTK-84                      
         DC    AL2(16819)           ORIGIN ID MECEM                             
         DC    CL12'CBA ECC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'MNSTBA314'      BANK ACCOUNT  DSFTK-126                     
         DC    AL2(17242)           ORIGIN ID MNSTOA                            
         DC    CL12'CBA NCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'MNSQBA315'      BANK ACCOUNT  SPEC-15455                    
         DC    AL2(17243)           ORIGIN ID MNSQU                             
         DC    CL12'CBA QCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'MNSUBA316'      BANK ACCOUNT  SPEC-15455                    
         DC    AL2(17254)           ORIGIN ID MNSUNI                            
         DC    CL12'CBA UCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'MIEXBA591'      BANK ACCOUNT  SPEC-15455                    
         DC    AL2(18795)           ORIGIN ID MIEXCA                            
         DC    CL12'CBA DCC'        SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000448180914'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID MDCMP/ASMAC                       
         DC    CL12'JPDAHOU80914'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000450037408'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDANET37408'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000450037408'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDAPRT37408'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000450037408'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDASPT37408'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428282540'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDRNET82540'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428282540'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDRPRT82540'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428282540'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDRSPT82540'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428267509'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDTNET67509'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428267509'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDTPRT67509'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428267509'  DSFTK-132-MDCMP/DSFTK-168            
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDTSPT67509'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000713828908'  DSFTK-191-ASMAC                      
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPDEMED28908'   SC ACCOUNT                                  
*                                                                               
* WAVETBA                                                                       
*                                                                               
         DC    CL20'WAVETBA309'     BANK ACCOUNT  SPEC-19359                    
         DC    AL2(19543)           ORIGIN ID WAVETBA                           
         DC    CL12'CBA ACC'        SC ACCOUNT                                  
*                                                                               
* WAVEL                                                                         
*                                                                               
         DC    CL20'WAVELBA626'     BANK ACCOUNT  SPEC-19129                    
         DC    AL2(19545)           ORIGIN ID WAVEL                             
         DC    CL12'CBA KCC'        SC ACCOUNT                                  
*                                                                               
* BRNAC                                                                         
*                                                                               
         DC    CL20'00000000000906501783'  SPEC-14486                           
         DC    AL2(19059)           ORIGIN ID BRNAC                             
         DC    CL12'JPDBHOU01873'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000906501783'  SPEC-14486                           
         DC    AL2(19059)           ORIGIN ID BRNAC                             
         DC    CL12'JPDBNET01873'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000906501783'  SPEC-14486                           
         DC    AL2(19059)           ORIGIN ID BRNAC                             
         DC    CL12'JPDBPRT01873'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000906501783'  SPEC-14486                           
         DC    AL2(19059)           ORIGIN ID BRNAC                             
         DC    CL12'JPDBSPT01873'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00JPSEMC1ALL'     DSFTK-150                                 
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPSEMC1-ALL'    SC ACCOUNT                                  
*                                                                               
         DC    CL20'00JPSAMC2ALL'     DSFTK-150                                 
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPSAMC2-ALL'    SC ACCOUNT                                  
*                                                                               
         DC    CL20'00JPSRMC3ALL'     DSFTK-150                                 
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPSRMC3-ALL'    SC ACCOUNT                                  
*                                                                               
         DC    CL20'00JPSTMC4ALL'     DSFTK-150                                 
         DC    AL2(17345)           ORIGIN ID ASMAC                             
         DC    CL12'JPSTMC4-ALL'    SC ACCOUNT                                  
*                                                                               
* ASMAC                                                                         
*                                                                               
         DC    CL20'00JPSGMC2ALL'   SPEC-13531                                  
         DC    AL2(18837)           ORIGIN ID ASMAC                             
         DC    CL12'JPSGMC2-ALL'    SC ACCOUNT                                  
*                                                                               
         DC    CL20'SCBC04NW'              BANK ACCOUNT DSFTK-109               
         DC    AL2(17435)           ORIGIN ID PATOWD                            
         DC    CL12'BC04NW'         SC ACCOUNT                                  
*                                                                               
         DC    CL20'SCBC04W'               BANK ACCOUNT DSFTK-109               
         DC    AL2(17435)           ORIGIN ID PATOWD                            
         DC    CL12'BC04W'          SC ACCOUNT                                  
*                                                                               
         DC    CL20'SCBC04YW'              BANK ACCOUNT DSFTK-109               
         DC    AL2(17435)           ORIGIN ID PATOWD                            
         DC    CL12'BC04YW'         SC ACCOUNT                                  
*                                                                               
         DC    CL20'000000027550117'       BANK ACCOUNT DSFTK-156               
         DC    AL2(16354)           ORIGIN ID WNPO                              
         DC    CL12'B001'           SC ACCOUNT                                  
*                                                                               
         DC    CL20'000000027550117'       BANK ACCOUNT DSFTK-156               
         DC    AL2(16354)           ORIGIN ID WNPO                              
         DC    CL12'B002'           SC ACCOUNT                                  
*                                                                               
         DC    CL20'000000027550117'       BANK ACCOUNT DSFTK-156               
         DC    AL2(16354)           ORIGIN ID WNPO                              
         DC    CL12'B003'           SC ACCOUNT                                  
*                                                                               
         DC    CL20'000000027550117'       BANK ACCOUNT DSFTK-156               
         DC    AL2(16354)           ORIGIN ID WNPO                              
         DC    CL12'B004'           SC ACCOUNT                                  
*                                                                               
         DC    CL20'000000027550117'       BANK ACCOUNT DSFTK-156               
         DC    AL2(16354)           ORIGIN ID WNPO                              
         DC    CL12'B005'           SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428150879'  BANK ACCOUNT DSFTK-164               
         DC    AL2(17355)           ORIGIN ID TRXAC                             
         DC    CL12'JPDTHOU50879'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000428150432'  BANK ACCOUNT DSFTK-164               
         DC    AL2(17355)           ORIGIN ID TRXAC                             
         DC    CL12'JPDRHOU50432'   SC ACCOUNT                                  
*                                                                               
         DC    CL20'00000000000713828916'  BANK ACCOUNT DSFTK-179               
         DC    AL2(17355)           ORIGIN ID TRXAC                             
         DC    CL12'JPDEPRT28916'   SC ACCOUNT                                  
*                                                                               
*SPEC-34556                                                                     
*                                                                               
* TSMCARD                                                                       
*                                                                               
         DC    CL20'SCCMCRDCSI'     BANK ACCOUNT  SPEC-34556                    
         DC    AL2(18375)           ORIGIN ID TSMCARD                           
         DC    CL12'CMCRDCSI    '   SC ACCOUNT                                  
*                                                                               
ACCTABX  EQU   (*-ACCTAB)                                                       
*SPEC-34556                                                                     
         DC    AL1(EOF)                                                         
*                                                                               
         EJECT                                                                  
**********************************************************************          
* BANK - GENERIC BANK ROUTINE                                        *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
BANK     NTR1  BASE=*,LABEL=*                                                   
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
         MVC   SVBKACC,SPACES      ACCOUNT DEFAULT IS SPACES                    
*                                                                               
BANK000  L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
         CLC   0(50,R3),SPACES     VALID RECORD?                                
         BNH   BANK000                                                          
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
         MVC   CHKWRK,SPACES                                                    
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVI   RECNUM,DTL          ASSUME DETAIL RECORD                         
*                                                                               
         USING FRMTABD,R5                                                       
         L     R5,FORMAT                                                        
BANK010  CLI   0(R5),EOF                                                        
         BNE   BANK015                                                          
         BAS   RE,RUNIT                                                         
         CLI   RECNUM,DTL          SKIP ALL OTHER RECORS OTHER THAN DTL         
         BE    BANK050                                                          
         B     BANK000                                                          
BANK015  CLI   0(R5),HDR                                                        
         BE    BANK030                                                          
         CLI   0(R5),TRL                                                        
         BE    BANK045                                                          
BANK020  AHI   R5,FRMLNQ                                                        
         B     BANK010                                                          
*                                                                               
* HEADER RECORD                                                                 
*                                                                               
         USING FLDENTD,R7                                                       
BANK030  ICM   R7,15,FRMRENT                                                    
BANK040  CLI   0(R7),EOF           DONE                                         
         BE    BANK020                                                          
         CLI   FLDNO,BK#VAR        VARIABLE                                     
         BE    *+12                                                             
         AHI   R7,FLDLNQ                                                        
         B     BANK040                                                          
*                                                                               
         TM    FRMSTAT,FRMREQ      DO WE NEED THIS RECORD?                      
         BNO   BANK000                                                          
*                                                                               
         LR    RE,R3               VARIABLE DATA                                
         SR    RF,RF                                                            
         LA    RF,FLDSRCE          SOURCE DATA                                  
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BNE   BANK020                                                          
         MVC   RECNUM,FRMRECN                                                   
         B     BANK020                                                          
*                                                                               
* TRAILER RECORD                                                                
*                                                                               
         USING FLDENTD,R7                                                       
BANK045  ICM   R7,15,FRMRENT                                                    
BANK048  CLI   0(R7),EOF           DONE                                         
         BE    BANK020                                                          
         CLI   FLDNO,BK#VAR        VARIABLE                                     
         BE    *+12                                                             
         AHI   R7,FLDLNQ                                                        
         B     BANK048                                                          
*                                                                               
         LR    RE,R3               VARIABLE DATA                                
         SR    RF,RF                                                            
         LA    RF,FLDSRCE          SOURCE DATA                                  
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BNE   BANK020                                                          
         MVC   RECNUM,FRMRECN                                                   
         B     BANK020                                                          
         DROP  R5,R7                                                            
*                                                                               
* CHKWRK FILLED IN FROM RUNIT                                                   
*                                                                               
BANK050  GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
         CLC   SVBKACC,SPACES      DID WE GET A GOOD CHECKING ACCOUNT?          
         BNH   BANK000             NO-DO NOT ADD                                
         MVC   CHKACC,SVBKACC                                                   
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
BANK060  GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    BANK070                                                          
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     BANK060             ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
BANK070  CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    BANK000                                                          
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     BANK000                                                          
         EJECT                                                                  
**********************************************************************          
* PUT ALL RECORDS TO CHKWRK BASED ON FORMAT TABLE                    *          
**********************************************************************          
         USING FRMTABD,R7                                                       
RUNIT    NTR1                                                                   
         L     R7,FORMAT                                                        
RUNIT10  CLI   0(R7),EOF           NO HEADER ROUTINE                            
         BE    RUNITX                                                           
         CLC   FRMRECN,RECNUM                                                   
         BNE   RUNIT15                                                          
         SR    R5,R5                                                            
         ICM   R5,15,FRMRENT       FORMAT RECORD ENTRY                          
         B     RUNIT20                                                          
RUNIT15  AHI   R7,FRMLNQ                                                        
         B     RUNIT10                                                          
*                                                                               
         USING FLDENTD,R5                                                       
         ICM   R5,15,FRMRENT                                                    
RUNIT20  CLI   0(R5),EOF           DONE                                         
         BE    RUNITX                                                           
         CLI   FLDNO,BK#ACC#       ACCOUNT                                      
         BE    RUNIT30                                                          
         CLI   FLDNO,BK#CHK#       CHECK NUMBER                                 
         BE    RUNIT40                                                          
         CLI   FLDNO,BK#NETA       AMOUNT FIELD                                 
         BE    RUNIT50                                                          
         CLI   FLDNO,BK#TRNDT      DATE                                         
         BE    RUNIT60                                                          
RUNIT25  AHI   R5,FLDLNQ                                                        
         B     RUNIT20                                                          
*                                                                               
* ACCOUNT                                                                       
*                                                                               
RUNIT30  MVC   WORK,SPACES                                                      
         MVC   BYTE,FLDOVR         DISPLACEMENT INTO TABLE ENTRY                
         LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)       MOVE ACC NUM TO WORK FOR FUTURE COMP         
*                                                                               
         USING ACCTBD,RF                                                        
         L     RF,AACCTAB          ACCOUNT AND USER ID TABLE                    
RUNIT35  CLI   0(RF),EOF           ARE WE AT THE END OF THE TABLE?              
         BNE   *+14                                                             
         MVC   SVBKACC,SPACES      ACCOUNT DEFAULT IS SPACES                    
         B     RUNIT25             READ NEXT RECORD                             
*                                                                               
         LA    RE,ACCNUM                                                        
         IC    R1,BYTE                                                          
         AR    RE,R1               DISPLACEMENT TO ACCOUNT NUMBER               
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(RE)       MATCH ON ACCOUNT NUMBER                      
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         AHI   RF,ACCTBLNQ         BUMP TO NEXT ENTRY                           
         B     RUNIT35                                                          
*                                                                               
         MVC   SVBKACC,WORK                                                     
         B     RUNIT25                                                          
         DROP  RF                                                               
*                                                                               
* CHECK NUMBER                                                                  
*                                                                               
RUNIT40  LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         LR    RF,R1                                                            
         SHI   RF,L'CHKNUM                                                      
         BP    *+6                                                              
         DC    H'0'                LENGTH MUST BE BAD                           
         CHI   R1,L'CHKNUM                                                      
         BNH   *+12                                                             
         LHI   R1,L'CHKNUM         LENGTH OF SOURCE FIELD IS MAX LENGTH         
         AHI   R1,-1                                                            
         TM    FLDSTAT,FLFT        IS THE FIELD LEFT JUSTIFIED                  
         BNO   RUNIT42                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CHKNUM(0),0(RE)                                                  
         B     RUNIT25                                                          
*                                                                               
RUNIT42  LA    RE,1(RF,RE)                                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CHKNUM(0),0(RE)                                                  
         B     RUNIT25                                                          
*                                                                               
* AMOUNT FIELD                                                                  
*                                                                               
RUNIT50  LR    RE,R3               AMOUNT FIELD                                 
         SR    R1,R1               (DETAIL AMNT/TRAILER AMNT)                   
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         TM    FLDSTAT2,F2DEC      DOES AMOUNT CONTAIN DECIMAL?                 
         BNO   RUNIT54                                                          
         SR    R0,R0                                                            
         LR    RF,RE                                                            
RUNIT52  CLI   0(RF),X'40'                                                      
         BNH   *+16                                                             
         AHI   RF,1                                                             
         AHI   R0,1                                                             
         BCT   R1,RUNIT52                                                       
         GOTO1 VCASHVAL,DMCB,(X'82',(RE)),(X'01',(R0))                          
         ZAP   CHKAMNT,DMCB+4(8)                                                
         B     RUNIT25                                                          
*                                                                               
RUNIT54  SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,RE)                                                      
         ZAP   CHKAMNT,DUB                                                      
         B     RUNIT25                                                          
*                                                                               
* BANK PAID DATE                                                                
*                                                                               
RUNIT60  LR    RE,R3                                                            
         SR    R1,R1                                                            
         ICM   R1,3,FLDDSP                                                      
         AR    RE,R1                                                            
         SR    R1,R1                                                            
         IC    R1,FLDLEN                                                        
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         LA    RE,WORK             REGULAR FORMATS ARE IN WORK                  
         CLI   FLDOVR,0                                                         
         BE    RUNIT68                                                          
         LA    RE,WORK+10          SPECIAL FORMAT ALL END UP IN WORK+10         
         CLI   FLDOVR,1            FORMAT MMDDYY                                
         BNE   RUNIT62                                                          
         MVC   WORK+10(2),WORK+4                                                
         MVC   WORK+12(4),WORK                                                  
         B     RUNIT68                                                          
RUNIT62  CLI   FLDOVR,2            FORMAT MMDDYYYY                              
         BNE   RUNIT64                                                          
         MVC   WORK+10(4),WORK+4                                                
         MVC   WORK+14(4),WORK                                                  
         B     RUNIT68                                                          
RUNIT64  CLI   FLDOVR,3            FORMAT YYYY/MM/DD                            
         BNE   RUNIT66                                                          
         MVC   WORK+10(5),WORK+5                                                
         MVI   WORK+15,C'/'                                                     
         MVC   WORK+16(2),WORK+2                                                
         B     RUNIT68                                                          
RUNIT66  CLI   FLDOVR,4            FORMAT DDMMYY                                
         BNE   RUNIT25                                                          
         MVC   WORK+10(2),WORK+4                                                
         MVC   WORK+12(2),WORK+2                                                
         MVC   WORK+14(2),WORK                                                  
RUNIT68  SR    RF,RF                                                            
         IC    RF,FLDFRM                                                        
         GOTO1 DATCON,DMCB,((RF),(RE)),(1,CHKPDDTE)                             
         B     RUNIT25                                                          
*                                                                               
RUNITX   XIT1                                                                   
*RUNITX   J     EXIT                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R5,R7,RB                                                   
         EJECT                                                                  
**********************************************************************          
* ABN AMRO                                                           *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
ABNAMRO  NTR1  BASE=*,LABEL=*                                                   
         USING ABND,R3                                                          
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
ABN10    L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
ABN20    CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    ABN10               READ NEXT RECORD                             
*                                                                               
         CLC   ABNACC,ACCNUM       MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     ABN20                                                            
         DROP  R1                                                               
*                                                                               
         CLI   ABNTYP,C'D'         DETAIL RECORD?                               
         BNE   ABN10                                                            
         CLI   ABNTYP2,C'R'        RECONCILED?                                  
         BE    ABN30                                                            
         CLI   ABNTYP2,C'P'        PAID NO ISSUE?                               
         BE    ABN30                                                            
         CLI   ABNTYP2,C'C'        VOID?                                        
         BNE   ABN10                                                            
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
ABN30    MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'ABNACC),ABNACC      MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,ABNCKNO+4    CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   ABNPDAT,SPACES      ANYTHING IN PAID DATE                        
         BNH   ABN50                                                            
         MVC   WORK(6),ABNPDAT+2   MOVE IN YYMMDD FROM CCYYMMDD                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
ABN50    PACK  DUB,ABNAMNT                                                      
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
ABN60    GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    ABN70                                                            
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     ABN60               ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
ABN70    CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    ABN10                                                            
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     ABN10                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* AMERICAN BUSINESS BANK TAPE ROUTINE                                *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
ABUS     NTR1  BASE=*,LABEL=*                                                   
         USING ABUSD,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
ABUS10   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
ABUS20   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    ABUS10              READ NEXT RECORD                             
         CLC   ABUSACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     ABUS20                                                           
         DROP  R1                                                               
*                                                                               
         CLC   ABUSCKNO,=10C'0'    SKIP IF CHK NO.=0000000000                   
         BE    ABUS10                                                           
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'ABUSACC),ABUSACC    MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,ABUSCKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   ABUSPDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   ABUS30                                                           
         MVC   WORK(2),ABUSPDAT+2    MOVE IN YY                                 
         MVC   WORK+2(4),ABUSPDAT+4  MOVE IN MMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
ABUS30   PACK  DUB,ABUSAMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
ABUS40   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    ABUS50                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     ABUS40              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
ABUS50   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    ABUS10                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     ABUS10                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* BANK ONE - TAPE ROUTINE                                            *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
BKONE    NTR1  BASE=*,LABEL=*                                                   
         USING BKORECD,R3                                                       
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
BKONE10  L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         CLI   BKTYPE,BKTTOT       ARE WE GOING THE TOTAL RECORD?               
         BE    BKONE10                                                          
*                                                                               
* FOR HEADER INFO CHECK THAT BANK ACCOUNT MATCHES ACCT/ID FROM TABLE            
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
BKONE20  CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    BKONE10             READ NEXT RECORD                             
         CLC   BKACCT,ACCNUM       MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     BKONE20                                                          
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC(L'BKACCT),BKACCT  ACCT NUMBER                             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,BKCHK+4      CHECK NUMBER                                 
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   BKPDDTE,SPACES      ANYTHING IN PAID DATE                        
         BNH   BKONE30                                                          
         MVC   WORK(2),BKPDDTE+4                                                
         MVC   WORK+2(4),BKPDDTE                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
BKONE30  PACK  DUB,BKAMNT                                                       
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
BKONE40  GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    BKONE50                                                          
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     BKONE40             ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
BKONE50  CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    BKONE10                                                          
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     BKONE10                                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* BANK OF AMERICA - TAPE ROUTINE                                     *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
BOA      NTR1  BASE=*,LABEL=*                                                   
         USING BOAD,R3                                                          
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
BOA10    L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         CLI   BOAIND,BOATRL       SKIP TRAILERS                                
         BE    BOA10                                                            
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
BOA20    CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    BOA10               READ NEXT RECORD                             
         CLC   BOAACC,ACCNUM       MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     BOA20                                                            
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
*        CLI   BOAIND,BOAVOID      IS THIS CHECK A VOID                         
*        BNE   *+8                                                              
*        OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'BOAACC),BOAACC       MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,BOANUM+4     CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         LA    RE,=A(NUMTAB)                                                    
BOA25    CLI   0(RE),EOF                                                        
         BE    BOA27                                                            
         CLC   CHKNUM,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,L'CHKNUM(RE)                                                  
         B     BOA25                                                            
         OI    CHKSTAT,CHKDR       MARK AS A DEBIT                              
*                                                                               
BOA27    XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   BOADATE,SPACES      ANYTHING IN PAID DATE                        
         BNH   BOA30                                                            
         MVC   WORK(2),BOADATE+4   MOVE IN YY                                   
         MVC   WORK+2(4),BOADATE   MOVE IN MMDD                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
BOA30    PACK  DUB,BOAAMT                                                       
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
BOA40    GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    BOA50                                                            
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     BOA40               ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
BOA50    CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    BOA10                                                            
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     BOA10                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* CHASE - TAPE ROUTINE CHASE                                         *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CHASE    NTR1  BASE=*,LABEL=*                                                   
         USING CHASED,R3                                                        
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
         XC    SVSTDTE,SVSTDTE     CLEAR STATEMENT DATE                         
*                                                                               
CHASE10  L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         CLC   =C'HDR',CHASACC     IS IT A HEADER RECORD                        
         BNE   CHASE15                                                          
         CLC   RCCOMPFL,=X'5E'     ONLY FOR FMFM                                
         BNE   CHASE10                                                          
         GOTO1 DATCON,DMCB,(0,CHASSDTE),(2,SVSTDTE)                             
         B     CHASE10                                                          
CHASE15  CLC   =C'EOF',CHASACC     IS IT A TRAILER RECORD                       
         BE    CHASE10                                                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CHASE20  CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CHASE10             READ NEXT RECORD                             
*                                                                               
         CLC   CHASACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CHASE20                                                          
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   CHASTYP,C'R'       IS THIS CHECK A PAID WITH ISSUE               
         BE    CHASE40            YES PROCESS CHECK                             
         CLI   CHASTYP,C'V'       IS THIS CHECK A VOID                          
         BE    CHASE25                                                          
         CLC   RCCOMPFL,=X'5E'    ONLY FOR FMFM                                 
         BNE   CHASE30                                                          
         CLI   CHASTYP,C'P'       ONLY FOR FMFM P IS SAME AS R                  
         BE    CHASE40                                                          
         CLI   CHASTYP,C'S'                                                     
         BNE   CHASE30                                                          
CHASE25  CLC   RCCOMPFL,=X'5E'    ONLY FOR FMFM                                 
         BNE   *+8                                                              
         OI    CHKSTAT,CHKVDER    REPORT VOID AS ERROR                          
         OI    CHKSTAT,CHKVOID    MARK AS VOID                                  
         B     CHASE40                                                          
*                                                                               
CHASE30  CLI   CHASTYP,C'D'        ARE THESE DEBITS                             
         BNE   CHASE10                                                          
         OI    CHKSTAT,CHKDR                                                    
*                                                                               
CHASE40  MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'CHASACC),CHASACC    MOVE IN THE ACCT NUMBER             
         MVC   CHKSTDTE,SVSTDTE                                                 
         OC    CHKSTDTE,CHKSTDTE                                                
         BNZ   CHASE45                                                          
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
CHASE45  MVC   CHKNUM,CHASCKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   CHASPDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   CHASE50                                                          
         MVC   WORK(6),CHASPDAT    MOVE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
CHASE50  PACK  DUB,CHASAMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CHASE60  DS    0H                                                               
         CLC   CHKNUM,TSTNUM                                                    
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CHASE70                                                          
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CHASE60             ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CHASE70  CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CHASE10                                                          
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CHASE10                                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* CHASE 200 - TAPE ROUTINE CHASE                                     *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CHS200   NTR1  BASE=*,LABEL=*                                                   
         USING CHS200D,R3                                                       
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CHS20010 L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CHS20020 CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CHS20010            READ NEXT RECORD                             
*                                                                               
         CLC   C200ACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CHS20020                                                         
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   C200TYP,C'P'       IS THIS CHECK A PAID WITH ISSUE               
         BE    CHS20040           YES PROCESS CHECK                             
         CLI   C200TYP,C'R'       IS THIS CHECK RECONCILED                      
         BE    CHS20040           YES PROCESS CHECK                             
         CLI   C200TYP,C'V'       IS THIS CHECK A VOID                          
         BNE   CHS20030                                                         
         OI    CHKSTAT,CHKVDER    REPORT VOID AS ERROR                          
         OI    CHKSTAT,CHKVOID    MARK AS VOID                                  
         B     CHS20040                                                         
*                                                                               
CHS20030 CLI   C200TYP,C'D'        ARE THESE DEBITS                             
         BNE   CHS20010                                                         
         OI    CHKSTAT,CHKDR                                                    
*                                                                               
CHS20040 MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'C200ACC),C200ACC    MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
CHS20045 MVC   CHKNUM,C200CKNO+12  CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   C200PDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   CHS20050                                                         
         MVC   WORK(6),C200PDAT+2  MOVE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
CHS20050 PACK  DUB,C200AMNT+6(12)                                               
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CHS20060 DS    0H                                                               
         CLC   CHKNUM,TSTNUM                                                    
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CHS20070                                                         
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CHS20060            ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CHS20070 CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CHS20010                                                         
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CHS20010                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* CHASE 40 - TAPE ROUTINE CHASE                                      *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CHS40    NTR1  BASE=*,LABEL=*                                                   
         USING CHS40D,R3                                                        
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CHS40010 L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CHS40020 CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CHS40010            READ NEXT RECORD                             
*                                                                               
         CLC   C40ACC,ACCNUM       MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CHS40020                                                         
         DROP  R1                                                               
*                                                                               
         CLI   C40TYP,C'D'        IS THIS DETAIL RECORD                         
         BNE   CHS40010                                                         
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSEQ,0                                                         
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
*        OI    CHKSTAT,CHKDR                                                    
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'C40ACC),C40ACC    MOVE IN THE ACCT NUMBER               
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,C40CKNO+4    CHECK NUMBER-SKIP NONSIGNIFICANTS            
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   C40PDAT,SPACES      ANYTHING IN PAID DATE                        
         BNH   CHS40050                                                         
         MVC   WORK(6),C40PDAT+2   MOVE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
CHS40050 PACK  DUB,C40AMNT                                                      
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CHS40060 DS    0H                                                               
         CLC   CHKNUM,TSTNUM                                                    
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CHS40070                                                         
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CHS40060            ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CHS40070 CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CHS40010                                                         
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CHS40010                                                         
         EJECT                                                                  
**********************************************************************          
* ANCHOR OPS - CREDIT CARD PAYMENTS                                  *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
AOPCCARD NTR1  BASE=*,LABEL=*                                                   
         USING AOP108D,R3                                                       
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
AOP010   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         CLC   AOP8MERC(6),=C'VENDOR'  SKIP HEADER RECORD                       
         BE    AOP010                                                           
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
AOP020   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    AOP010              READ NEXT RECORD                             
         CLC   AOP8ACC(14),ACCNUM  MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     AOP020                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSEQ,0                                                         
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'AOP8ACC),AOP8ACC  MOVE IN THE ACCT NUMBER               
                                                                                
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
                                                                                
         MVC   CHKNUM,AOP8CKNO     CHECK NUMBER-SKIP NONSIGNIFICANTS            
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   AOP8DAT,SPACES      ANYTHING IN PAID DATE                        
         BNH   AOP050                                                           
         MVC   WORK(6),AOP8DAT+2   MOVE IN YYMMDD                               
                                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
                                                                                
AOP050   DS    0H                                                               
         LA    RE,AOP8AMNT                                                      
         LA    R1,L'AOP8AMNT                                                    
         SR    R0,R0                                                            
                                                                                
AOP055   CLI   0(RE),C' '                                                       
         BNH   AOP058                                                           
         AHI   R0,1                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,AOP055                                                        
                                                                                
*        PACK  DUB,AOP8AMNT                                                     
*        ZAP   CHKAMNT,DUB                                                      
AOP058   LA    R4,AOP8AMNT                                                      
         GOTO1 VCASHVAL,DMCB,(X'82',0(R4)),(R0)                                 
         ZAP   CHKAMNT,DMCB+4(8)                                                
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
AOP060   DS    0H                                                               
         CLC   CHKNUM,TSTNUM                                                    
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    AOP070                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     AOP060              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
AOP070   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    AOP010                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     AOP010                                                           
         EJECT                                                                  
*MN SPEC-27479                                                                  
*MN SPEC-28991                                                                  
**********************************************************************          
* CSI        - CREDIT CARD PAYMENTS                                  *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CSICCARD NTR1  BASE=*,LABEL=*                                                   
         USING CSI108D,R3                                                       
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CSI010   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CSI020   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CSI010              READ NEXT RECORD                             
         CLC   CSIACC(14),ACCNUM   MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CSI020                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSEQ,0                                                         
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'CSIACC),CSIACC   MOVE IN THE ACCT NUMBER                
                                                                                
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
                                                                                
         MVC   CHKNUM,CSICKNO      CHECK NUMBER-SKIP NONSIGNIFICANTS            
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   CSIDAT,SPACES       ANYTHING IN PAID DATE                        
         BNH   CSI050                                                           
         MVC   WORK(6),CSIDAT+2    MOVE IN YYMMDD                               
                                                                                
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
                                                                                
CSI050   DS    0H                                                               
         LA    RE,CSIAMNT                                                       
         LA    R1,L'CSIAMNT                                                     
         SR    R0,R0                                                            
                                                                                
CSI055   CLI   0(RE),C' '                                                       
         BNH   CSI058                                                           
         AHI   R0,1                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,CSI055                                                        
                                                                                
*        PACK  DUB,CSIAMNT                                                      
*        ZAP   CHKAMNT,DUB                                                      
CSI058   LA    R4,CSIAMNT                                                       
         GOTO1 VCASHVAL,DMCB,(X'82',0(R4)),(R0)                                 
         SRP   DMCB+4(8),62,0                                                   
         ZAP   CHKAMNT,DMCB+4(8)                                                
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CSI060   DS    0H                                                               
         CLC   CHKNUM,TSTNUM                                                    
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CSI070                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CSI060              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CSI070   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CSI010                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CSI010                                                           
         EJECT                                                                  
*MN SPEC-28991                                                                  
*MN SPEC-27479                                                                  
*DSFTK-150                                                                      
**********************************************************************          
* JPMCHASE 108 - PCARD CREDIT CARD PAYMENTS                          *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
JPMPCARD NTR1  BASE=*,LABEL=*                                                   
*----------------------------------------------------------------               
*        GOTO1 DYNALLOC,DMCB,(X'FF',DDNAME),(X'00',DSNAME)                      
*        L     R2,ADCB                                                          
*        OPEN  ((R2),(INPUT))      OPEN BANKS DTF TAPE                          
*                                                                               
*        CLOSE (CH108DTF)     DOES ICETOOL NEED THIS CLOSED?                    
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'FD',=CL8'DFSMSG'),(X'80',=CL21' ')              
         GOTO1 DYNALLOC,DMCB,(X'FD',=CL8'TOOLMSG'),(X'80',=CL21' ')             
         GOTO1 DYNALLOC,DMCB,(X'FD',=CL8'SYMNOUT'),(X'80',=CL21' ')             
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'SYMNAMES'),                   +        
               (X'05',=CL44'DDS.ACC.PARMS(ACCRCHSY)')                           
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'TOOLIN'),                     +        
               (X'05',=CL44'DDS.ACC.PARMS(ACCRCHTI)')                           
         GOTO1 DYNALLOC,DMCB,(X'FF',=CL8'SRT1CNTL'),                   +        
               (X'05',=CL44'DDS.ACC.PARMS(ACCRCHC1)')                           
*                                                                               
         GOTO1 DYNALLOC,DMCB,(X'80',=CL8'OUT'),(X'40',=AL3(10,10)),0            
*                                                                               
* CALL ICETOOL TO PRODUCE THE CHASE FILE.                                       
         SR    R1,R1               CONTROL CARDS ARE IN TOOLIN                  
         LINK  EP=ICETOOL                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*----------------------------------------------------------------               
*                                                                               
         USING JPM108D,R3                                                       
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
         OPEN  (OUT,(INPUT))                                                    
*                                                                               
JPM010   DS    0H                                                               
         GET   OUT,(R3)           GET RECORD INTO R3                            
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
JPM020   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    JPM010              READ NEXT RECORD                             
*        CLC   J108ACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         CLC   J108ACC+2(10),ACCNUM+2  MATCH ON THE ACCOUNT NUMBER              
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     JPM020                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSEQ,0                                                         
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'J108ACC),J108ACC  MOVE IN THE ACCT NUMBER               
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,J108CKNO     CHECK NUMBER-SKIP NONSIGNIFICANTS            
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   J108DAT,SPACES      ANYTHING IN PAID DATE                        
         BNH   JPM050                                                           
         MVC   WORK(2),J108DAT+6   MOVE IN YYMMDD                               
         MVC   WORK+2(4),J108DAT     MOVE IN YYMMDD                             
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
                                                                                
JPM050   DS    0H                                                               
*        PACK  DUB,J108AMNT                                                     
*        ZAP   CHKAMNT,DUB                                                      
                                                                                
         LA    RE,J108AMNT                                                      
         LA    R1,L'J108AMNT                                                    
         AR    RE,R1                                                            
         BCTR  RE,0                                                             
         SR    R0,R0                                                            
JPM055   CLI   0(RE),C' '                                                       
         BNH   JPM058                                                           
         AHI   R0,1                                                             
         BCTR  RE,0                                                             
         BCT   R1,JPM055                                                        
                                                                                
JPM058   DS    0H                                                               
         LA    RE,1(RE)                                                         
         LR    R4,RE                                                            
         GOTO1 VCASHVAL,DMCB,(X'82',0(R4)),(R0)                                 
         ZAP   CHKAMNT,DMCB+4(8)                                                
*                                                                               
*        MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
JPM060   DS    0H                                                               
         CLC   CHKNUM,TSTNUM                                                    
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    JPM070                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     JPM060              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
JPM070   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    JPM010                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     JPM010                                                           
*                                                                               
JPMXIT   DS    0H                                                               
         CLOSE (OUT)                                                            
         GOTO1 DYNALLOC,DMCB,(C'U',=CL8'OUT'),0                                 
         XIT1                                                                   
         EJECT                                                                  
*DSFTK-150                                                                      
**********************************************************************          
* KEY BANK 90 BYTE FORMAT                                            *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
KEYBNK90 NTR1  BASE=*,LABEL=*                                                   
         USING KEYB90D,R3                                                       
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
KEYB010  L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
KEYB020  CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    KEYB010             READ NEXT RECORD                             
         CLC   KB90ACC(14),ACCNUM  MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     KEYB020                                                          
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSEQ,0                                                         
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'KB90ACC),KB90ACC  MOVE IN THE ACCT NUMBER               
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,KB90CKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   KB90PDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   KEYB050                                                          
         MVC   WORK(2),KB90PDAT+4  CHANGE MMDDYY TO YYMMDD                      
         MVC   WORK+2(4),KB90PDAT                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
KEYB050  PACK  DUB,KB90AMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
*        MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
KEYB060  DS    0H                                                               
         CLC   CHKNUM,TSTNUM                                                    
         BNE   *+8                                                              
         B     *+4                                                              
         GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    KEYB070                                                          
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     KEYB060             ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
KEYB070  CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    KEYB010                                                          
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     KEYB010                                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* CITIBANK - TAPE ROUTINE                                            *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CITI     NTR1  BASE=*,LABEL=*                                                   
         USING CITID,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CITI10   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CITI20   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CITI10              READ NEXT RECORD                             
         CLC   CITIACCT,ACCNUM     MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CITI20                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   CITIVOID,C'P'       IS THIS CHECK A RECONCILED                   
         BE    *+8                 YES PROCESS CHECK                            
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'CITIACCT),CITIACCT   MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,CITICKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   CITIPDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   CITI30                                                           
         MVC   WORK(2),CITIPDAT+4  MOVE IN YY                                   
         MVC   WORK+2(4),CITIPDAT  MMDD                                         
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
CITI30   PACK  DUB,CITIAMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CITI40   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CITI50                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CITI40              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CITI50   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CITI10                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CITI10                                                           
         EJECT                                                                  
**********************************************************************          
* CITIBANK - 80 BYTE TAPE ROUTINE SPECS ARE IN CL#0148366N WP#13*               
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CI80     NTR1  BASE=*,LABEL=*                                                   
         USING CI80D,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CI8010   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CI8020   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CI8010              READ NEXT RECORD                             
         CLC   CI80ACCT,ACCNUM     MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CI8020                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'CI80ACCT),CI80ACCT   MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,CI80CKNO+5   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   CI80PDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   CI8030                                                           
         MVC   WORK(2),CI80PDAT+4  MOVE IN YY                                   
         MVC   WORK+2(4),CI80PDAT  MMDD                                         
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
CI8030   PACK  DUB,CI80AMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CI8040   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CI8050                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CI8040              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CI8050   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CI8010                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CI8010                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* CITIBANK - 80 BYTE TAPE ROUTINE                                    *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CT80     NTR1  BASE=*,LABEL=*                                                   
         USING CT80D,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CT8010   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CT8020   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CT8010              READ NEXT RECORD                             
         CLC   CT80ACCT,ACCNUM     MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CT8020                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   CT80VOID,C'P'       IS THIS CHECK A RECONCILED                   
         BE    *+8                 YES PROCESS CHECK                            
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'CT80ACCT),CT80ACCT   MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,CT80CKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   CT80PDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   CT8030                                                           
         MVC   WORK(2),CT80PDAT+4  MOVE IN YY                                   
         MVC   WORK+2(4),CT80PDAT  MMDD                                         
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
CT8030   PACK  DUB,CT80AMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CT8040   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CT8050                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CT8040              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CT8050   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CT8010                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CT8010                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* CITIBANK - 200 BYTE TAPE ROUTINE CL#0108625T                       *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CT200    NTR1  BASE=*,LABEL=*                                                   
         USING CT200D,R3                                                        
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CT20010  L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         CLI   CT200RTY,C'1'       IS THIS A DETAIL RECORD ?                    
         BNE   CT20010                                                          
         CLC   CT200STA,=C'RE'     RECONCILED OR PAID ?                         
         BE    *+14                                                             
         CLC   CT200STA,=C'FR'     FORCED RECONCILED ?                          
         BNE   CT20010                                                          
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CT20020  CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CT20010             READ NEXT RECORD                             
         CLC   CT200ACC,ACCNUM    MATCH ON THE ACCOUNT NUMBER                   
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CT20020                                                          
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'CT200ACC),CT200ACC MOVE IN THE ACCT NUMBER              
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,CT200CKN+4  CHECK NUMBER-SKIP NONSIGNIFICANTS             
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   CT200PDT,SPACES     ANYTHING IN PAID DATE                        
         BNH   CT20030                                                          
         MVC   WORK(6),CT200PDT+2  MOVE IN YYMMDD FROM CCYYMMDD                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
CT20030  PACK  DUB,CT200AMT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CT20040  GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CT20050                                                          
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CT20040             ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CT20050  CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CT20010                                                          
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CT20010                                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* CITIZENS - 80 BYTE TAPE ROUTINE   CL#0280997N                      *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
CZ80     NTR1  BASE=*,LABEL=*                                                   
         USING CZ80D,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
CZ8010   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
CZ8020   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    CZ8010              READ NEXT RECORD                             
         CLC   CZ80ACCT,ACCNUM     MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     CZ8020                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   CZ80STAT,C'R'       IS THIS CHECK A RECONCILED                   
         BE    *+8                 YES PROCESS CHECK                            
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'CZ80ACCT),CZ80ACCT   MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,CZ80CKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   CZ80PDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   CZ8030                                                           
         MVC   WORK(2),CZ80PDAT+4  MOVE IN YY                                   
         MVC   WORK+2(4),CZ80PDAT  MMDD                                         
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
CZ8030   PACK  DUB,CZ80AMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
CZ8040   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    CZ8050                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     CZ8040              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
CZ8050   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    CZ8010                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     CZ8010                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* FLEET - TAPE ROUTINE                                               *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
FLEET    NTR1  BASE=*,LABEL=*                                                   
         USING FLEETD,R3                                                        
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
FLEET10  L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         CLI   FLRTYPE,FLRITRQ     ARE WE DOING A TRANSACTION?                  
         BNE   FLEET10                                                          
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
FLEET20  CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    FLEET10             READ NEXT RECORD                             
         CLC   FLACCN,ACCNUM       MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     FLEET20                                                          
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'FLACCN),FLACCN       MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,FLCHKNO+4    CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   FLISDT,SPACES       ANYTHING IN PAID DATE                        
         BNH   FLEET30                                                          
         MVC   WORK(2),FLISDT+4    MOVE IN YY                                   
         MVC   WORK+2(4),FLISDT    MOVE IN MMDD                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
FLEET30  PACK  DUB,FLAMT                                                        
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
FLEET40  GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    FLEET50                                                          
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     FLEET40             ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
FLEET50  CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    FLEET10                                                          
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     FLEET10                                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* FORK - TAPE ROUTINE                                               *           
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
FORK     NTR1  BASE=*,LABEL=*                                                   
         USING FORKD,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
FORK10   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         CLC   FOCODE,=C'01'       ARE WE DOING A TRANSACTION (DETAIL)?         
         BNE   FORK10                                                           
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
FORK20   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    FORK10              READ NEXT RECORD                             
         CLC   FOACC,ACCNUM        MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     FORK20                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   FOVOID,C'V'         IS THIS CHECK A VOID                         
         BNE   *+8                                                              
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'FOACC),FOACC         MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,FONUM+4      CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         LA    RE,=A(NUMTAB)                                                    
FORK25   CLI   0(RE),EOF                                                        
         BE    FORK27                                                           
         CLC   CHKNUM,0(RE)                                                     
         BE    *+12                                                             
         LA    RE,L'CHKNUM(RE)                                                  
         B     FORK25                                                           
         OI    CHKSTAT,CHKDR       MARK AS A DEBIT                              
*                                                                               
FORK27   XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   FODATE,SPACES       ANYTHING IN PAID DATE                        
         BNH   FORK30                                                           
         MVC   WORK(2),FODATE+4    MOVE IN YY                                   
         MVC   WORK+2(4),FODATE    MOVE IN MMDD                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
FORK30   PACK  DUB,FOAMT                                                        
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
FORK40   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    FORK50                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     FORK40              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
FORK50   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    FORK10                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     FORK10                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* FIRST UNION NATIONAL BANK TAPE ROUTINE                             *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
FUNB     NTR1  BASE=*,LABEL=*                                                   
         USING FUNBD,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
FUNB10   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
FUNB20   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    FUNB10              READ NEXT RECORD                             
         CLC   =C'HR',FUNBID       DON'T NEED HEADER RECD                       
         BE    FUNB10                                                           
         CLC   =C'TR',FUNBID       DON'T NEED TRAILER RECD                      
         BE    FUNB10                                                           
*                                                                               
         CLC   FUNBACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     FUNB20                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   FUNBTYPE,C'V'       IS IT VOID                                   
         BNE   *+12                NO CHECK NEXT                                
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
         B     FUNB30                                                           
*                                                                               
         CLI   FUNBTYPE,C'D'       IS IT DEBIT                                  
         BNE   *+12                                                             
         OI    CHKSTAT,CHKDR       MARK AS DEBIT                                
         B     FUNB30                                                           
*                                                                               
         CLI   FUNBTYPE,C'R'       IS IT RECONILED                              
         BE    FUNB30              PROCESS                                      
         CLI   FUNBTYPE,C'C'       IS IT CREDIT                                 
         BNE   FUNB10              GET NEXT RECORD                              
*                                                                               
FUNB30   MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'FUNBACC),FUNBACC    MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,FUNBCKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   FUNBPDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   FUNB40                                                           
         MVC   WORK(2),FUNBPDAT+2    MOVE IN YY                                 
         MVC   WORK+2(4),FUNBPDAT+4  MOVE IN MMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
FUNB40   PACK  DUB,FUNBAMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
FUNB50   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    FUNB60                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     FUNB50              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
FUNB60   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    FUNB10                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     FUNB10                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* HSBC - TAPE ROUTINE                                                *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
HSBC     NTR1  BASE=*,LABEL=*                                                   
         USING HSBCD,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
HSBC10   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
HSBC20   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    HSBC10              READ NEXT RECORD                             
         CLC   HSBCACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     HSBC20                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   HSBCVOID,C'R'       IS THIS CHECK A RECONCILED                   
         BE    *+8                 YES PROCESS CHECK                            
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'HSBCACC),HSBCACC    MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,HSBCCKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   HSBCPDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   HSBC30                                                           
         MVC   WORK(2),HSBCPDAT+6  MOVE IN YY                                   
         MVC   WORK+2(4),HSBCPDAT  MOVE IN MMDD                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
HSBC30   PACK  DUB,HSBCAMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
HSBC40   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    HSBC50                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     HSBC40              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
HSBC50   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    HSBC10                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     HSBC10                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* HSBC 57 BYTE TAPE ROUTINE CL#01064432                              *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
HS57     NTR1  BASE=*,LABEL=*                                                   
         USING HS57D,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
HS5710   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
HS5720   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    HS5710              READ NEXT RECORD                             
         CLC   HS57ACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     HS5720                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'HS57ACC),HS57ACC    MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,HS57CKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   HS57PDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   HS5730                                                           
         MVC   WORK(6),HS57PDAT    MOVE IN YY                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
HS5730   PACK  DUB,HS57AMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
HS5740   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    HS5750                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     HS5740              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
HS5750   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    HS5710                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     HS5710                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* HSBC 300 BYTE TAPE ROUTINE CL#0109113T                              *         
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
HS3HUN   NTR1  BASE=*,LABEL=*                                                   
         USING HS300D,R3                                                        
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
HS30010  L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
HS30020  CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    HS30010             READ NEXT RECORD                             
         CLI   HS300RTY,C'C'                                                    
         BNE   HS30010                                                          
         CLC   =C'01',HS300STA     IS IT STATUS TYPE 01 ?                       
         BNE   HS30010             01 =VALID, IGNORE EVERYTHING ELSE            
         CLC   HS300AC#,ACCNUM     MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     HS30020                                                          
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'HS300AC#),HS300AC#  MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,HS300CK#+1  CHECK NUMBER-SKIP NONSIGNIFICANTS             
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   HS300PDT,SPACES    ANYTHING IN PAID DATE                         
         BNH   HS30030                                                          
         MVI   HS300PDT,C'1'                                                    
         GOTO1 VHEXIN,DMCB,HS300PDT,WORK,6 CONVERT C'CYYDDD'TOX'CYYDDD'         
         GOTO1 DATCON,DMCB,(8,WORK),(1,CHKPDDTE)                                
*                                                                               
HS30030  PACK  DUB,HS300AMT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
HS30040  GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    HS30050                                                          
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     HS30040             ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
HS30050  CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    HS30010                                                          
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     HS30010                                                          
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* NATIONAL CITY  BANK TAPE ROUTINE                                   *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
NACT     NTR1  BASE=*,LABEL=*                                                   
         USING NACTD,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
NACT10   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
NACT20   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    NACT10              READ NEXT RECORD                             
         CLC   NACTACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     NACT20                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLI   NACTIND,C'R'        IS THIS CHECK A RECONCILED                   
         BE    *+8                 YES PROCESS CHECK                            
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'NACTACC),NACTACC    MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,NACTCKNO+3    CHECK NUMBER-SKIP NONSIGNIFICANTS           
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   NACTPDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   NACT30                                                           
         MVC   WORK(6),NACTPDAT    MOVE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
NACT30   PACK  DUB,NACTAMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
NACT40   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    NACT50                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     NACT40              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
NACT50   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    NACT10                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     NACT10                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* ROYAL - SECOND TAPE ROUTINE                                        *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
RBNK     NTR1  BASE=*,LABEL=*                                                   
         USING RBKD,R3                                                          
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
RBK10    L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
RBK20    CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    RBK10               READ NEXT RECORD                             
*                                                                               
         CLC   RBKDRCOD,=C'30'     IS IT A OUTSTANDING ISSUED                   
         BNE   RBK10               READ NEXT, WE DON'T NEED THIS                
*                                                                               
         CLC   RBKDACCN,ACCNUM     MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     RBK20                                                            
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
*        CLC   RBKVOID,=C'V '      IS THIS A VOID                               
*        BNE   *+8                 NO, READ NEXT                                
*        OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'RBKDACCN),RBKDACCN    MOVE IN THE ACCT NUMBER           
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,RBKCKNO+2    CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   RBKPDATE,SPACES     ANYTHING IN PAID DATE                        
         BNH   RBK30                                                            
         MVC   WORK(6),RBKPDATE    MOVE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
RBK30    PACK  DUB,RBKDAMT                                                      
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
RBK40    GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    RBK50                                                            
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     RBK40               ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
RBK50    CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    RBK10                                                            
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     RBK10                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* ROYAL - TAPE ROUTINE                                               *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
ROYAL    NTR1  BASE=*,LABEL=*                                                   
         USING ROYD,R3                                                          
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
ROY10    L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
ROY20    CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    ROY10               READ NEXT RECORD                             
*                                                                               
         CLC   ROYHTYPE,=C'10'     ARE WE AT THE HEADER RECORD                  
         BE    ROY10               READ NEXT, WE DON'T NEED THIS                
         CLC   ROYTTYPE,=C'99'     ARE WE AT THE TRAILER RECORD                 
         BE    ROY10               READ NEXT, WE DON'T NEED THIS                
         CLC   ROYDTYPE,=C'35'     IS IT OUTSTANDING                            
         BE    ROY10               READ NEXT, WE DON'T NEED THIS                
*                                                                               
         CLC   ROYDACNO,ACCNUM     MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     ROY20                                                            
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLC   ROYDTYPE,=C'79'     ARE WE DOING CREDITS/ISSUES                  
         BE    *+8                 NO, READ NEXT                                
         OI    CHKSTAT,CHKDR       MARK AS VOID                                 
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'ROYDACNO),ROYDACNO    MOVE IN THE ACCT NUMBER           
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,ROYDSNO+6    CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   ROYDDATE,SPACES     ANYTHING IN PAID DATE                        
         BNH   ROY30                                                            
         MVC   WORK(6),ROYDDATE    MOVE IN YYMMDD                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
ROY30    PACK  DUB,ROYDAMT                                                      
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
ROY40    GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    ROY50                                                            
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     ROY40               ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
ROY50    CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    ROY10                                                            
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     ROY10                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
*&&DO                                                                           
**********************************************************************          
* UMB BANK - 80 BYTE TAPE ROUTINE                                    *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
UMB      NTR1  BASE=*,LABEL=*                                                   
         USING UMBD,R3                                                          
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
UMB10    L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
UMB20    CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    UMB10               READ NEXT RECORD                             
         CLC   UMBACCT,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     UMB20                                                            
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'UMBACCT),UMBACCT     MOVE IN THE ACCT NUMBER            
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,UMBCKNO+4    CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   UMBPDAT,SPACES      ANYTHING IN PAID DATE                        
         BNH   UMB30                                                            
         MVC   WORK(L'UMBPDAT),UMBPDAT     MOVE IN YYMMDD                       
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
UMB30    PACK  DUB,UMBAMNT                                                      
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
UMB40    GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    UMB50                                                            
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     UMB40               ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
UMB50    CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    UMB10                                                            
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     UMB10                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
*&&                                                                             
         EJECT                                                                  
**********************************************************************          
* US BANK - TAPE ROUTINE                                             *          
*            AFTER READING THE LAST RECORD IN THE TAPE               *          
*            THE PROGRAM RETURNS TO REQF20                           *          
**********************************************************************          
USBK     NTR1  BASE=*,LABEL=*                                                   
         USING USBKD,R3                                                         
         LA    R3,RECWRK                                                        
         MVC   LSTBKAC,SPACES      CLEAR LAST ACCOUNT FIELD                     
*                                                                               
USBK10   L     R1,ADCB                                                          
         GET   (R1),(R3)           GET RECORD INTO R3                           
*                                                                               
*        MVC   MSG,=CL10'TAPE-GET'                                              
*        GOTO1 ADUMP,DMCB,(RC),RECWRK,L'RECWRK                                  
*                                                                               
         USING CHKTBD,R2                                                        
         LA    R2,CHKWRK           BINSEARCH WORK AREA                          
*                                                                               
         USING ACCTBD,R1                                                        
         L     R1,AACCTAB          ACCOUNT AND USER ID TABLE                    
USBK20   CLI   0(R1),EOF           ARE WE AT THE END OF THE TABLE?              
         BE    USBK10              READ NEXT RECORD                             
         CLC   USBKACC,ACCNUM      MATCH ON THE ACCOUNT NUMBER                  
         BNE   *+14                                                             
         CLC   ACCOID,ORIGINUM     MATCH ON ORIGIN ID                           
         BE    *+12                                                             
         LA    R1,ACCTBLNQ(R1)     BUMP TO NEXT ENTRY                           
         B     USBK20                                                           
         DROP  R1                                                               
*                                                                               
         MVC   CHKWRK,SPACES       CLEAR WORK AREA                              
         MVI   CHKSTAT,0           CLEAR OUT STATUS FIELD - NO DEBITS           
         MVI   CHKSEQ,0                                                         
*                                                                               
         CLC   USBKRCOD,=C'01'     IS THIS DETAIL RECORD                        
         BNE   USBK10                                                           
         CLI   USBKRECT,USBKRCN    IS THIS CHECK A RECONCILED                   
         BE    USBK25              YES PROCESS CHECK                            
         CLI   USBKRECT,USBKCANC   IS THIS CANCELLED/VOID                       
         BNE   USBK22                                                           
         OI    CHKSTAT,CHKVOID     MARK AS VOID                                 
         B     USBK25                                                           
USBK22   CLI   USBKRECT,USBKDR     IS THIS A DEBIT                              
         BNE   USBK10                                                           
         OI    CHKSTAT,CHKDR       MARK AS DEBIT                                
*                                                                               
USBK25   MVC   CHKACC,SPACES       CLEAR OUT BANK ACCOUNT NUMBER FIELD          
         MVC   CHKACC(L'USBKACC),USBKACC    MOVE IN THE ACCT NUMBER             
         GOTO1 DATCON,DMCB,(0,TODAY),(2,CHKSTDTE)  TODAY IS STMENT DATE         
         MVC   CHKNUM,USBKCKNO+4   CHECK NUMBER-SKIP NONSIGNIFICANTS            
*                                                                               
         XC    CHKPDDTE,CHKPDDTE   SET PAID DATE TO ZEROES                      
         CLC   USBKPDAT,SPACES     ANYTHING IN PAID DATE                        
         BNH   USBK30                                                           
         MVC   WORK(2),USBKPDAT+6  MOVE IN YY                                   
         MVC   WORK+2(4),USBKPDAT  MOVE IN MMDD                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,CHKPDDTE)                                
*                                                                               
USBK30   PACK  DUB,USBKAMNT                                                     
         ZAP   CHKAMNT,DUB                                                      
         MVC   CHKBKNM,SVBKNAME    FILL IN THE BANK NAME FOR REPORTING          
*                                                                               
*        MVC   MSG,=CL10'BINTAB #1'                                             
*        GOTO1 ADUMP,DMCB,(RC),CHKWRK,L'CHKWRK                                  
*                                                                               
USBK40   GOTO1 ABINADD,DMCB,(RC),CHKWRK,ACHKTAB                                 
         BE    USBK50                                                           
         SR    RE,RE                                                            
         IC    RE,CHKSEQ           GET SEQUENCE NUMBER                          
         AHI   RE,1                                                             
         STC   RE,CHKSEQ           INCREMENT SEQUENCE NUM                       
         B     USBK40              ADD ANOTHER WITH SAME CHECK NUM              
*                                                                               
USBK50   CLC   LSTBKAC,CHKACC      SAME BANK ACCOUNT NUMBER?                    
         BE    USBK10                                                           
         MVC   LSTBKAC,CHKACC      UPDATE LAST BANK ACCOUNT                     
*                                                                               
         USING BKATBD,R1                                                        
         LA    R1,BKAWRK           ADD NEW ACCOUNT TO ACCOUNT TABLE             
         MVC   BKAACC,CHKACC                                                    
*                                                                               
*        MVC   MSG,=CL10'BINTAB #2'                                             
*        GOTO1 ADUMP,DMCB,(RC),BKAWRK,L'BKAWRK                                  
*                                                                               
         GOTO1 ABINADD,DMCB,(RC),BKAWRK,ABKATAB                                 
         B     USBK10                                                           
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         DROP  R1,R2,R3,RB                                                      
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
NUMTAB   DS    0CL6                CHECK NUMBER TABLE                           
         DC    C'300163'                                                        
         DC    C'300180'                                                        
         DC    C'300200'                                                        
         DC    C'300215'                                                        
         DC    C'300235'                                                        
         DC    X'FF'                                                            
***********************************************************************         
* FORMAT TABLE                                                        *         
***********************************************************************         
FRMTAB   DS    0C                                                               
       ++INCLUDE ACFRMTAB                                                       
         EJECT                                                                  
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
CHKMAX   EQU   70000                                                            
BKAMAX   EQU   200                                                              
         EJECT                                                                  
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         CP    PKDUMP,PKDMPMAX                                                  
         BH    DUMPX                                                            
         AP    PKDUMP,=P'1'                                                     
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 VPRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),        X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         USING BIND,R5                                                          
BINADD   DS    0D                                                               
         NMOD1 0,**BINA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,8(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         LA    R6,BINTAB           A(TABLE)                                     
         L     R3,4(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*        L     R4,DMCB             A(RECORD FOUND)                              
*        SR    R0,R0                                                            
*        ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
*        BZ    BINXIT                NO BUCKETS - EXIT                          
*        SR    R6,R6                                                            
*        IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
*        AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
*        AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
*INA10   AP    0(L'NCPBKT,R4),0(L'NCPBKT,R3)   ADD TO BUCKET                    
*        LA    R3,L'NCPBKT(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
*        LA    R4,L'NCPBKT(R4)     BUMP TO NEXT ENTRY IN TABLE                  
*        BCT   R0,BINA10                                                        
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
DMCTFIL  NMOD1 0,CTF               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE ',SVKEY,IOKEY                  
         B     DMX                                                              
*                                                                               
DMWRTDR  NMOD1 0,WRT               WRITE BACK TO DIR                            
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMWRT'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMADDDR  NMOD1 0,ADD               ADD KEY TO DIR                               
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'00',=C'DMADD'),=C'ACCDIR',IOKEY,IOKEY            
         B     DMX                                                              
*                                                                               
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRSEQ),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMRDHI),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'80',DMREAD),=C'ACCDIR ',SVKEY,IOKEY,0            
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
         DROP  R3                                                               
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
*     R2 - ADDRESS OF SORT WORK AREA (SRTWRK)                         *         
***********************************************************************         
         USING SRTD,R2                                                          
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R4,ADBOX                                                         
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         CLI   RCSUBPRG,9                                                       
         BNE   BXHK10                                                           
         MVI   BOXREQ,C'C'          CLOSE BOX                                   
         GOTO1 ACREPORT                                                         
         B     BXXIT                                                            
*                                                                               
BXHK10   MVC   XHEAD2+20(L'SRTBKNM),SRTBKNM                                     
*                                                                               
         LA    RF,L'SRTBKACC       GET THE LENGTH OF ACCOUNT                    
         LA    R1,SRTBKACC         POINT TO BEGINNING OF ACCNT                  
         CLI   0(R1),X'40'         DOES ACCOUNT NUM START WITH SPACES           
         BH    *+12                NO                                           
         LA    R1,1(R1)        YES,CHECK NEXT CHARACTER OF THE ACCNT            
         BCT   RF,*-12             BUMP TO CHECK NEXT                           
         SHI   RF,1                SUB 1 FOR EX MVC                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   XHEAD3+20(0),0(R1)                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,SRTBKDTE),(21,WORK)                               
         MVC   XHEAD4+20(10),WORK                                               
*                                                                               
         CLI   RCSUBPRG,2          ARE WE DOING THE ERROR REPORT?               
         BNE   *+10                                                             
         MVC   XHEAD4+50(10),=C'**ERRORS**'                                     
*                                                                               
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
         MVI   BOXCOLS+(PCHKDTE-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PAMNTCR-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PAMNTDR-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(P2CACC-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PCONTRA-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PCLRDTE-PRTLINE-1),C'C'                                 
         CLI   RCSUBPRG,2          ARE WE DOING THE ERROR REPORT?               
         BE    *+12                                                             
         MVI   BOXCOLS+PRLNQ,C'R'                                               
         B     BXXIT                                                            
*                                                                               
         MVI   BOXCOLS+(PERR-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+PRLNQ2,C'R'                                              
*                                                                               
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXXITX   XMOD1 1                                                                
         LTORG                                                                  
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* WORKING STORAGE                                                     *         
***********************************************************************         
ACCRD    DSECT                                                                  
VTYPES   DS    0A                                                               
ADUMP    DS    A                                                                
ABINADD  DS    A                                                                
AAGYTAB  DS    A                                                                
AERRTAB  DS    A                                                                
AACCTAB  DS    A                                                                
AFRMTAB  DS    A                                                                
*                                                                               
VCASHVAL DS    V                                                                
VPRNTBL  DS    V                                                                
VHELLO   DS    V                                                                
VSORTER  DS    V                                                                
VHEXIN   DS    V                                                                
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
ACHKTAB  DS    A                                                                
ABKATAB  DS    A                                                                
*                                                                               
ADCB     DS    A                   ADDRESS OF DCB                               
ARTN     DS    A                   ADDRESS OF BANK ROUTINE                      
FORMAT   DS    A                   ADDRESS OF FORMAT                            
*                                                                               
ADBOX    DS    A                   ADDRESS OF BOX                               
SVDA     DS    F                                                                
DISP2    DS    H                   DISPLACEMENT TO ELEMENTS                     
MSG      DS    CL10                                                             
ELCODE   DS    CL1                                                              
ELEM     DS    CL255                                                            
SVKEY    DS    CL42                                                             
DMPCNT   DS    PL4                                                              
EMSG     DS    CL105               E-MAIL MESSAGE TO LOTUS NOTES                
*                                                                               
RECNUM   DS    XL1                 STORAGE FOR REC # FOR BANK ROUT              
FLAG     DS    XL1                                                              
FLGMRK   EQU   X'80'               TRNSACTION WAS MARKED PRIOR TO RUN           
FLGPRNT  EQU   X'40'               SOMETHING WAS PRINTED                        
FLGSKGET EQU   X'20'               RECORD IS IN IO SKIP GETREC                  
*                                                                               
DDNAME   DS    CL18                DDNAME FOR DYNALLOC                          
*                                                                               
TODAY    DS    CL6                 TODAY'S DATE-CHARACTER                       
TODAYP   DS    PL3                 TODAY'S DATE-PACKED                          
LSTYR    DS    PL3                 TODAY(RCDATE) - 1 YEAR                       
*                                                                               
SVBKACC  DS    CL(L'CHKACC)        BANK ACCOUNT FROM HEADER                     
SVBKNAME DS    CL20                SAVED AREA FOR BANK NAME                     
SVSTDTE  DS    XL2                 STATEMENT DATE                               
*                                                                               
RECWRK   DS    CL180               INPUT RECORD AREA                            
CHKWRK   DS    CL(CHKLNQ)          BINSEARCH WORK AREA FOR CHK TABLE            
BKAWRK   DS    CL(BKALNQ)          BINSEARCH WORK AREA FOR BANK ACC TAB         
SRTWRK   DS    CL(SRTLNQ)          WORK AREA FOR SORTER                         
*                                                                               
LSTBANK  DS    0CL42               BANK INFO                                    
LSTBKNM  DS    CL20                SAVED AREA FOR PREVIOUS BANK NAME            
LSTBKAC  DS    CL20                SAVED AREA FOR PREVIOUS BANK ACCOUNT         
LSTSDTE  DS    XL2                 SAVED AREA FOR PREVIOUS BANK S DATE          
*                                                                               
EMLINE1  DS    CL80                EMAIL BODY LINE1                             
EMLINE2  DS    CL80                EMAIL BODY LINE2                             
*                                                                               
PKFLDS   DS    0PL8                                                             
PKCNT    DS    PL8                 RECORD COUNT                                 
PKCRAMT  DS    PL8                 CREDIT AMOUNT (CHECKS)                       
PKDRAMT  DS    PL8                 DEBIT  AMOUNT (DEPOSITS)                     
PKCNTTOT DS    PL8                                                              
PKCRAMTT DS    PL8                 CREDIT AMOUNT TOTAL                          
PKDRAMTT DS    PL8                 DEBIT  AMOUNT TOTAL                          
PKFLDLNQ EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
PKDUMP   DS    PL8                 DUMP COUNT                                   
PKDMPMAX DS    PL3                 100 MAX                                      
*                                                                               
START    DS    XL3                                                              
END      DS    XL3                                                              
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL12               LEVEL CODES                                  
LEVACDE  DS    CL12                LEVEL A CODE                                 
LEVBCDE  DS    CL12                LEVEL B CODE                                 
LEVCCDE  DS    CL12                LEVEL C CODE                                 
LEVDCDE  DS    CL12                LEVEL D CODE                                 
LEVSLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                                                             
IOAREA   DS    CL2000                                                           
IOLNQ    EQU   *-IO                                                             
*                                                                               
EOF      EQU   X'FF'               END OF FILE                                  
         EJECT                                                                  
*---------------------------------------------------------------                
         DS    0D                                                               
         DC    CL8'**SSB **'                                                    
SSB      CSECT                                                                  
         DC    X'0000FF',X'00',XL252'00'                                        
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINKLN   EQU   *-BIND                                                           
BINNUM   DS    CL1                 NUMBER OF BUCKETS                            
BINFST   DS    CL1                 DISPLACEMENT TO FIRST BUCKET                 
BINTAB   DS    0C                  THE TABLE                                    
         EJECT                                                                  
**********************************************************************          
* MAIL TABLE DSECT                                                   *          
**********************************************************************          
MAILTBD  DSECT                                                                  
MAILAGY  DS    XL2      AGENCY ORIGIN ID                                        
MAILUID  DS    CL8      CONNECT ID                                              
MAILADD  DS    CL45     E-MAIL ADDRESSES COMMA SEPARATED LIST                   
MAILNQ   EQU   *-MAILTBD           LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* ERROR TABLE DSECT                                                  *          
**********************************************************************          
ERRTBD   DSECT                                                                  
ERRTYP   DS    X                   ERROR TYPE                                   
ERRMESS  DS    CL25                ERROR DESCRIPTION                            
ERRLNQ   EQU   *-ERRTBD            LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* SPECS TABLE DSECT                                                  *          
**********************************************************************          
AGYTBD   DSECT                                                                  
AGYOID   DS    XL2                 AGENCY ORIGIN ID                             
AGYRTE   DS    AL2                 DISPLACEMENT TO PROCESSING ROUTINE           
AGYDTF   DS    AL2                 DISPLACEMENT TO DTF                          
AGYFRM   DS    AL4                 ADDRESS OF FORMAT                            
AGYBNKN  DS    CL5                 BANK NAME                                    
AGYDDNM  DS    CL18                DD NAME FOR DYNALLOC                         
AGYBKNM  DS    CL20                BANK NAME                                    
AGYLNQ   EQU   *-AGYTBD            LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* ACCOUNT TABLE DSECT                                                *          
**********************************************************************          
ACCTBD   DSECT                                                                  
ACCNUM   DS    CL20                BANK ACCOUNT NUMBER                          
ACCOID   DS    XL2                 ORIGIN ID                                    
ACCSCA   DS    CL12                SC ACCOUNT ASSOCIATED WITH BANK ACCT         
ACCTBLNQ EQU   *-ACCTBD                                                         
         EJECT                                                                  
**********************************************************************          
* CHECK TABLE DSECT (BIN TABLE #1)                                   *          
**********************************************************************          
CHKTBD   DSECT                                                                  
CHKACC   DS    CL20                BANK ACCOUNT                                 
CHKSTDTE DS    XL2                 STATEMENT DATE (COMPRESSED)                  
CHKNUM   DS    CL6                 CHECK NUMBER                                 
CHKPDDTE DS    PL3                 CHECK PAID DATE (PACKED)                     
CHKSEQ   DS    XL1                 CHECK SEQ NUM FOR MULTIPLE CHKS              
CHKKLNQ  EQU   *-CHKTBD            KEY LENGTH                                   
CHKSTAT  DS    XL1                 STAUS BYTE                                   
CHKDR    EQU   X'80'               DEBIT                                        
CHKMRK   EQU   X'40'               CHECK WAS MARKED                             
CHKVOID  EQU   X'20'               CHECK IS A VOID                              
CHKDTERR EQU   X'10'               TRNS MARKED-DATES DONT MATCH                 
CHKVDER  EQU   X'08'               REPORTING VOIDS AS ERRORS (FMFM)             
CHKAMNT  DS    PL8                 CHECK AMOUNT                                 
CHKBKNM  DS    CL20                BANK NAME                                    
CHKLNQ   EQU   *-CHKTBD            LENGTH                                       
         EJECT                                                                  
**********************************************************************          
* BANK ACCOUNT TABLE DSECT (BIN TABLE #2)                            *          
**********************************************************************          
BKATBD   DSECT                                                                  
BKAACC   DS    CL20                BANK ACCOUNT                                 
BKALNQ   EQU   *-BKATBD            LENGTH OR RECORD AND KEY                     
         EJECT                                                                  
**********************************************************************          
* SORT RECORD DSECT                                                  *          
**********************************************************************          
SRTD     DSECT                                                                  
SRTREC   DS    0C                                                               
SRTESTAT DS    XL1                 SORT ERROR STATUS                            
SRTNMRK  EQU   X'80'               CHECK HAS ERROR - UNMARKED                   
SRTNDTE  EQU   X'40'               CHECK HAS ERROR - DATE MISMATCH              
SRTVDER  EQU   X'20'               CHECK HAS ERROR - VOID (FMFM)                
SRTBKNM  DS    CL20                BANK NAME                                    
SRTBKACC DS    CL20                BANK ACCOUNT NUMBER                          
SRTBKDTE DS    XL2                 BANK STATEMENT DATE                          
SRTCHKNM DS    CL6                 CHECK NUMBER                                 
SRTCLRDT DS    PL3                 CHECK CLEARED DATE                           
SRTSTAT  DS    XL1                 SORT STATUS                                  
SRTDR    EQU   X'80'                - DEBIT                                     
SRTVOID  EQU   X'20'                - CHECK IS A VOID                           
SRT2CA   DS    CL12                CASH ACCOUNT                                 
SRTCONT  DS    CL14                CASH ACC CONTRA ACCOUNT WITH U/L             
SRTTRNDT DS    PL3                 TRANSACTION DATE                             
SRTAMNT  DS    PL8                 TRANSACTION AMOUNT                           
SRTLNQ   EQU   *-SRTREC            LENGTH OR RECORD                             
         EJECT                                                                  
**********************************************************************          
* PRINT DSECT                                                        *          
**********************************************************************          
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
         DS    CL2                                                              
PCHKNUM  DS    CL6                 CHECK NUMBER                                 
         DS    CL5                                                              
PCHKDTE  DS    CL10                TRANSACTION DATE                             
         DS    CL5                                                              
PAMNTCR  DS    CL16                CREDIT AMOUNT (CHECK)                        
         DS    CL5                                                              
PAMNTDR  DS    CL16                DEBIT  AMOUNT (DEPOSIT)                      
         DS    CL5                                                              
P2CACC   DS    CL12                CASH ACCOUNT                                 
         DS    CL5                                                              
PCONTRA  DS    CL17                CASH ACC CONTRA ACCOUNT WITH U/L             
         DS    CL5                                                              
PCLRDTE  DS    CL10                CHECK CLEARED DATE                           
         DS    CL5                                                              
PRLNQ    EQU   *-PRTLINE           LENGTH OR LINE                               
PERR     DS    CL25                ERROR MESSAGE                                
         DS    CL5                                                              
PRLNQ2   EQU   *-PRTLINE           LENGTH OR LINE WITH ERROR COL                
         EJECT                                                                  
**********************************************************************          
* BANK DSECTS                                                        *          
**********************************************************************          
*                                                                               
* DSECT FOR ABN AMRO 180 BYTE RECORD (FEB 2005)                                 
*                                                                               
ABND     DSECT                                                                  
ABNREC   DS    0CL180                                                           
ABNTYP   DS    CL1                 RECORD TYPE                                  
         DS    CL10                                                             
ABNACC   DS    CL10                ACCOUNT NUMBER                               
         DS    CL3                                                              
ABNCKNO  DS    CL10                CHECK NUMBER                                 
         DS    CL3                                                              
ABNAMNT  DS    CL10                CHECK AMOUNT                                 
ABNPDAT  DS    CL8                 PAID DATE  YYYYMMDD                          
ABNIDAT  DS    CL8                 ISSUE DATE YYYYMMDD                          
ABNTYP2  DS    CL1                 'R' - RECONCILED, 'C' - VOID                 
ABNLNQ   EQU   *-ABNREC                                                         
*                                                                               
* DSECT FOR AMERICAN BUSINESS BANK - 80 BYTE RECORDS (OCT 2004)                 
*                                                                               
ABUSD    DSECT                                                                  
ABUSREC  DS    0CL80                                                            
ABUSACC  DS    CL13                ACCOUNT NUMBER                               
ABUSCKNO DS    CL10                CHECK NUMBER                                 
ABUSAMNT DS    CL10                CHECK AMOUNT                                 
ABUSPDAT DS    CL8                 PAID DATE  MMDDYYYY                          
ABUSSPR  DS    CL(L'ABUSREC-(*-ABUSD))                                          
ABUSLNQ  EQU   *-ABUSREC                                                        
         EJECT                                                                  
*                                                                               
* DSECT FOR BANK ONE                                                            
*                                                                               
BKORECD  DSECT                                                                  
BKNUM    DS    CL3                 BANK NUMBER                                  
BKNUMM   EQU   C'021'              - MICHIGAN                                   
BKNUMD   EQU   C'101'              - DEARBORN                                   
BKACCT   DS    CL10                BANK ACCOUNT NUMBER                          
BKCHK    DS    CL10                CHECK NUMBER (ALL 9'S FOR TOTAL REC)         
         DS    CL1                 FILLER                                       
BKISDTE  DS    CL6                 ISSUE DATE - MMDDYY                          
BKPDDTE  DS    CL6                 PAID DATE - MMDDYY                           
BKSEQNUM DS    CL11                SEQUENCE NUMBER                              
         DS    CL1                 FILLER                                       
BKAMNT   DS    CL10                AMOUNT                                       
         DS    CL1                 FILLER                                       
BKADD    DS    CL15                ADDITIONAL DATA                              
         DS    CL1                 FILLER                                       
BKTYPE   DS    CL1                 BANK TRANSMISSION TYPE                       
BKTPD    EQU   C'P'                - PAID                                       
BKTREC   EQU   C'R'                - RECONCILED                                 
BKTTOT   EQU   C'T'                - TOTAL                                      
         DS    CL4                 RESERVED                                     
         ORG   BKISDTE             ***TOTAL RECORD***                           
         DS    CL12                FILLER                                       
BKRECCNT DS    CL8                 RECORD COUNT                                 
         DS    CL1                 FILLER                                       
BKAMNTOT DS    CL13                TOTAL AMOUNT                                 
         DS    CL17                FILLER                                       
         DS    CL1                 BANK TRANSMISSION TYPE (SEE ABOVE)           
         DS    CL4                 FILLER                                       
* DSECT FOR CHASE DELAWARE -  58 BYTE RECORDS (JUL 2002)                        
*                                                                               
CHDED    DSECT                                                                  
CHDEREC  DS    0CL58                                                            
CHDEACC  DS    CL10                ACCOUNT NUMBER                               
CHDECKNO DS    CL10                CHECK NUMBER                                 
CHDEAMNT DS    CL10                CHECK AMOUNT                                 
CHDEPDAT DS    CL6                 PAID DATE  MMDDYY                            
CHDEIDAT DS    CL6                 ISSUE DATE  MMDDYY                           
CHDETYP  DS    CL1                 TRANSACTION TYPE R=PAID WITH ISSUE           
*                        S=STOP,V=VOID,O=OUTSTDNG,P=PD NO ISSUE                 
         DS    CL15                 SPARES                                      
CHDELNQ  EQU   *-CHDEREC                                                        
*                                                                               
* DSECT FOR CHASE SYRACUSE -  67 BYTE RECORDS (JUL 2002)                        
*                                                                               
CHASED   DSECT                                                                  
CHASREC  DS    0CL67                                                            
         DS    CL3                 HDR                                          
         DS    CL10                ACCOUNT NUMBER                               
CHASSDTE DS    CL6                 STATEMENT DATE                               
         DS    CL48                SPACES                                       
         ORG   CHASREC                                                          
CHASACC  DS    CL10                ACCOUNT NUMBER                               
CHASCKNO DS    CL10                CHECK NUMBER                                 
CHASAMNT DS    CL10                CHECK AMOUNT                                 
CHASPDAT DS    CL6                 PAID DATE  YYMMDD                            
CHASTYP  DS    CL1                 TRANSACTION TYPE R=PAID WITH ISSUE           
*                        S=STOP,V=VOID,O=OUTSTDNG,P=PD NO ISSUE                 
         DS    CL15                ADDITIONAL DATA                              
         DS    CL9                 SEQUENCE NUMBER                              
CHASIDAT DS    CL6                 ISSUE DATE  YYMMDD                           
CHASLNQ  EQU   *-CHASREC                                                        
*                                                                               
* DSECT FOR JP MORGAN CHASE - 200 BYTE RECORDS (JUN 2011-0325289N)              
*                                                                               
CHS200D  DSECT                                                                  
C200REC  DS    0CL200                                                           
C200TYP  DS    CL1                 TYPE                                         
         DS    CL1                 SPARE                                        
C200ACC  DS    CL20                ACCOUNT NUMBER                               
         DS    CL1                 SPARE                                        
C200CKNO DS    CL18                CHECK NUMBER                                 
         DS    CL1                 SPARE                                        
C200AMNT DS    CL18                CHECK AMOUNT                                 
         DS    CL1                 SPARE                                        
C200CDAT DS    CL8                 CHECK DATE  YYYYMMDD                         
         DS    CL1                 SPARE                                        
C200PDAT DS    CL8                 PAID DATE  YYYYMMDD                          
         DS    CL(200-(*-C200REC))    FILLER SPACES                             
C200LNQ  EQU   *-C200REC                                                        
*                                                                               
* DSECT FOR JP MORGAN CHASE - 40 BYTE RECORDS (#0392997N)                       
*                                                                               
CHS40D   DSECT                                                                  
C40REC   DS    0CL40                                                            
C40TYP   DS    CL1                 TYPE                                         
C40ACC   DS    CL10                ACCOUNT NUMBER                               
C40CKNO  DS    CL10                CHECK NUMBER                                 
C40AMNT  DS    CL10                CHECK AMOUNT                                 
C40PDAT  DS    CL8                 PAID DATE  YYYYMMDD                          
C40LNQ   EQU   *-C40REC                                                         
*                                                                               
* DSECT FOR ANCHOR OPS - DSFTK-109                                              
*                                                                               
AOP108D  DSECT                                                                  
AOP8REC  DS    0CL108                                                           
AOP8MERC DS    CL12                MERCHANT CODE  (N/A)                         
AOP8AMNT DS    CL12                CHECK AMOUNT                                 
AOP8CUR  DS    CL3                 CURRENCY       (N/A)                         
AOP8DAT  DS    CL8                 PAID DATE  YYYYMMDD                          
AOP8ACC  DS    CL20                ACCOUNT NUMBER (LEFT JUSTIFIED)              
AOP8AGY  DS    CL40                AGENCY NAME    (N/A)                         
AOP8CMP  DS    CL2                 COMPANY CODE   (N/A)                         
AOP8OFF  DS    CL2                 OFFICE         (N/A)                         
AOP8CKNO DS    CL9                 CHECK NUMBER (LEFT JUSTIFIED)                
AOP8LNQ  EQU   *-AOP8REC                                                        
*                                                                               
*SPEC-27479                                                                     
*SPEC-28991                                                                     
*                                                                               
* DSECT FOR CSI SPEC-28991                                                      
*                                                                               
CSI108D  DSECT                                                                  
CSIREC   DS    0CL117                                                           
CSIMERC  DS    CL12                MERCHANT CODE  (N/A)                         
CSIAMNT  DS    CL13                CHECK AMOUNT                                 
CSICUR   DS    CL3                 CURRENCY       (N/A)                         
CSIDAT   DS    CL8                 PAID DATE  YYYYMMDD                          
CSIACC   DS    CL20                ACCOUNT NUMBER (LEFT JUSTIFIED)              
CSIAGY   DS    CL40                AGENCY NAME    (N/A)                         
CSICMP   DS    CL2                 COMPANY CODE   (N/A)                         
CSIOFF   DS    CL2                 OFFICE         (N/A)                         
CSICKNO  DS    CL9                 CHECK NUMBER (LEFT JUSTIFIED)                
CSIUSER  DS    CL9                 USER ID                                      
CSILNQ   EQU   *-CSIREC                                                         
*SPEC-28991                                                                     
*SPEC-27479                                                                     
*DSFTK-150                                                                      
*                                                                               
* DSECT FOR JP MORGAN CHASE - DSFTK-150                                         
* THIS DESECT TIES TO RECORD LAYOUT IN DDS.ACC.PARMS(ACCRCHSY)                  
* CHANGES MUST BE COORDINATED WITH THESE DATASETS                               
*                                                                               
JPM108D  DSECT                                                                  
J108REC  DS    0CL200                                                           
J108MERC DS    CL12                MERCHANT CODE  (N/A)                         
         DS    CL3                                                              
J108AMNT DS    CL15                CHECK AMOUNT                                 
J108CUR  DS    CL3                 CURRENCY       (N/A)                         
         DS    CL14                                                             
J108DAT  DS    CL8                 PAID DATE  MMDDYYYY                          
         DS    CL14                                                             
J108ACC  DS    CL12                ACCOUNT NUMBER (LEFT JUSTIFIED)              
J108AGY  DS    CL7                 AGENCY NAME    (N/A)                         
J108CMP  DS    CL2                 COMPANY CODE   (N/A)                         
J108CKNO DS    CL6                 CHECK NUMBER (LEFT JUSTIFIED)                
         DS    CL3                                                              
J108LNQ  EQU   *-J108REC                                                        
*DSFTK-150                                                                      
*                                                                               
* DSECT FOR KEY BANK - DSFTK-156                                                
*                                                                               
KEYB90D  DSECT                                                                  
KB90REC  DS    0CL90                                                            
KB90ACC  DS    CL15                BANK ACCOUNT NUMBER                          
KB90CKNO DS    CL10                CHECK NUMBER                                 
KB90AMNT DS    CL10                CHECK AMOUNT                                 
KB90PDAT DS    CL6                 DATE MMDDYY                                  
KB90LNQ  EQU   *-KB90REC                                                        
*                                                                               
* DSECT FOR BANK OF AMERICA                                                     
*                                                                               
BOAD     DSECT                                                                  
BOAACC   DS    CL10                ACCOUNT NUMBER                               
BOASPARE DS    CL3                 SPARE                                        
BOANUM   DS    CL10                CHECK NUMBER                                 
BOAAMT   DS    CL12                CHECK AMOUNT                                 
BOADATE  DS    CL6                 CHECK DATE  (MMDDYY)                         
         DS    CL39                ADDITIONAL DATA (NOT DEFINED)                
         ORG   BOASPARE                                                         
BOAIND   DS    CL1                 INDICATOR BYTE                               
BOATRL   EQU   C'T'                TRAILER                                      
         DS    CL2                 SPARE                                        
BOATCNT  DS    CL10                ITEM COUNT                                   
BOATAMT  DS    CL12                ITEM COUNT                                   
         DS    CL45                ADDITIONAL DATA (NOT DEFINED)                
*                                                                               
* DSECT FOR CITIBANK                                                            
*                                                                               
CITID    DSECT                                                                  
CITIREC  DS    0CL80                                                            
CITIHDID DS    CL35                HEADER ID $$ADD                              
         ORG   CITIREC                                                          
CITIACCT DS    CL8                 ACCOUNT NUMBER                               
CITICKNO DS    CL10                CHECK NUMBER                                 
CITIAMNT DS    CL11                CHECK AMOUNT                                 
CITIIDAT DS    CL6                 ISSUE DATE  MMDDYY                           
CITIPDAT DS    CL6                 PAID DATE  MMDDYY                            
         DS    CL30                FILLER                                       
CITIVOID DS    CL1                 '5' = VOID INDICATOR                         
CITILEN  EQU   *-CITID             LENGTH OF RECORD                             
*                                                                               
* DSECT FOR CITIBANK 80 BYTE ROUTINE                                            
*                                                                               
CI80D    DSECT                                                                  
CI80REC  DS    0CL80                                                            
CI80HDID DS    CL35                HEADER ID $$ADD                              
         ORG   CI80REC                                                          
CI80ACCT DS    CL8                 ACCOUNT NUMBER                               
CI80CKNO DS    CL11                CHECK NUMBER                                 
CI80AMNT DS    CL11                CHECK AMOUNT                                 
CI80IDAT DS    CL6                 ISSUE DATE  MMDDYY                           
CI80PDAT DS    CL6                 PAID DATE  MMDDYY                            
         DS    CL2                                                              
CI80OUTC DS    CL1                 OUTPUT CONSTANT PAID-P                       
CI80LEN  EQU   *-CI80D             LENGTH OF RECORD                             
CI80SPR  DS    CL(L'CI80REC-CI80LEN)                                            
*                                                                               
* DSECT FOR CITIBANK 80 BYTE ROUTINE                                            
*                                                                               
CT80D    DSECT                                                                  
CT80REC  DS    0CL80                                                            
CT80HDID DS    CL35                HEADER ID $$ADD                              
         ORG   CT80REC                                                          
CT80ACCT DS    CL10                ACCOUNT NUMBER                               
CT80CKNO DS    CL10                CHECK NUMBER                                 
CT80AMNT DS    CL10                CHECK AMOUNT                                 
CT80PDAT DS    CL6                 PAID DATE  MMDDYY                            
CT80VOID DS    CL1                 '5' = VOID INDICATOR                         
         DS    CL9                 SEQUENCE NUMBER                              
CT80IDAT DS    CL6                 ISSUE DATE  YYMMDD                           
CT80LEN  EQU   *-CT80D             LENGTH OF RECORD                             
CT80SPR  DS    CL(L'CT80REC-CT80LEN)                                            
*                                                                               
* DSECT FOR CITIBANK 200 BYTE ROUTINE CL#0108625T                               
*                                                                               
CT200D   DSECT                                                                  
CT200REC DS    0CL200                                                           
CT200RTY DS    CL1                 RECORD TYPE 0=HEADER 1=DETAIL                
CT200ACC DS    CL10                ACCOUNT NUMBER                               
CT200CKN DS    CL10                CHECK NUMBER                                 
CT200AMT DS    CL12                CHECK AMOUNT                                 
         DS    CL3                 CURRENCY CODE                                
CT200IDT DS    CL8                 ISSUE DATE CCYYMMDD                          
CT200PDT DS    CL8                 PAID DATE  CCYYMMDD                          
         DS    CL8                 RECONCILEMENT DATE CCYYMMDD                  
         DS    CL8                 RULE OFF DATE CCYYMMDD                       
CT200STA DS    CL2                 RE=RECONCILED AND FR=FORCED RECON.           
CT200LEN EQU   *-CT200D            LENGTH OF RECORD                             
CT200SPR DS    CL(L'CT200REC-CT200LEN)                                          
*                                                                               
CZ80D    DSECT                     CITIZENS BANK CL#0280997N                    
CZ80REC  DS    0CL81                                                            
CZ80ACCT DS    CL10                ACCOUNT NUMBER                               
CZ80CKNO DS    CL10                CHECK NUMBER                                 
CZ80STAT DS    CL1                 R=RECONCILED P=PAID                          
CZ80AMNT DS    CL12                CHECK AMOUNT                                 
CZ80PDAT DS    CL6                 PAID DATE  MMDDYY                            
CZ80LEN  EQU   *-CZ80D             LENGTH OF RECORD                             
CZ80SPR  DS    CL(L'CZ80REC-CZ80LEN)                                            
*                                                                               
* FLEET BANK REC DSECT  12/08/97                                                
*                                                                               
FLEETD   DSECT                                                                  
FLRTYPE  DS    CL1                 RECORD TYPE                                  
FLRITRQ  EQU   C'V'                V - INPUT TRANSACTION RECORD                 
FLRTOTQ  EQU   C'T'                                                             
FLCHKNO  DS    CL10                CHECK SERIAL NUMBER                          
FLACCN   DS    CL14                ACCOUNT NUMBER                               
FLAMT    DS    CL10                CHECK AMOUNT                                 
FLREFN   DS    CL9                 REFERENCE NUMBER                             
FLISDT   DS    CL6                 ISSUE DATE MMDDYY                            
         DS    CL28                FILLER SPACES                                
FLDELCD  DS    CL2           DELETE CODE (C'38'=DELETE) (SPACES=ISSUE)          
FLEETLEN EQU   *-FLEETD            LENGTH OF RECORD                             
*                                                                               
* DSECT FOR FIRST BANK OF GRAND FORKS                                           
*                                                                               
FORKD    DSECT                                                                  
FOCODE   DS    CL2                 (DETAIL-01, TRAILER-02)                      
FOACC    DS    CL12                ACCOUNT NUMBER                               
FODATA   EQU   *                                                                
FONUM    DS    CL10                CHECK NUMBER                                 
FOAMT    DS    CL12                CHECK AMOUNT                                 
FODATE   DS    CL6                 CHECK DATE  (MMDDYY)                         
FOVOID   DS    CL1                 V IF VOID                                    
         DS    CL15                ADDITIONAL DATA (NOT DEFINED)                
         DS    CL22                FILLER                                       
FORKLEN  EQU   *-FORKD                                                          
         ORG   FODATA                                                           
FOTCNT   DS    CL10                NUMBER OF DETAIL RECORDS                     
FOTAMT   DS    CL12                TOTAL AMOUNT                                 
         DS    CL44                FILLER                                       
*                                                                               
* DSECT FOR FIRST UNION NATIONAL BANK - 80 BYTE RECORDS (OCT 2002)              
*                                                                               
FUNBD    DSECT                                                                  
FUNBREC  DS    0CL80                                                            
FUNBID   DS    CL2                 HR FOR HEADER, TR FOR TRAILER                
         ORG   FUNBREC                                                          
FUNBACC  DS    CL13                ACCOUNT NUMBER                               
FUNBCKNO DS    CL10                CHECK NUMBER                                 
FUNBAMNT DS    CL10                CHECK AMOUNT                                 
FUNBPDAT DS    CL8                 PAID DATE  YYYYMMDD                          
FUNBTYPE DS    CL1                 V=VOID, R=RECON,D=DEBIT,C=CREDIT             
FUNBIDAT DS    CL8                 ISSUE DATE  YYYYMMDD                         
FUNBLNQ  EQU   *-FUNBREC                                                        
FUNBSPR  DS    CL(L'FUNBREC-FUNBLNQ)                                            
*                                                                               
* DSECT FOR HSBC BANK  - 80 BYTE RECORDS (APR 2002)                             
*                                                                               
HSBCD    DSECT                                                                  
HSBCREC  DS    0CL80                                                            
HSBCACC  DS    CL13                ACCOUNT NUMBER                               
HSBCCKNO DS    CL10                CHECK NUMBER                                 
HSBCAMNT DS    CL10                CHECK AMOUNT                                 
HSBCIDAT DS    CL8                 ISSUE DATE  MMDDYYYY                         
HSBCPDAT DS    CL8                 PAID DATE  MMDDYYYY                          
         DS    0CL11                                                            
         DS    CL5                                                              
HSBCVOID DS    CL1                 VOID INDICATOR SUPPOSE TO BE POS 61          
         DS    CL5                                                              
         DS    CL1                 VOID INDICATOR ACCORDING TO SPEC             
HSBCSPR  DS    CL(L'HSBCREC-(*-HSBCD))                                          
HSBCLNQ  EQU   *-HSBCREC                                                        
         EJECT                                                                  
*                                                                               
* DSECT FOR HSBC BANK  - 57 BYTE RECORDS (DEC 2009) CL#01064432                 
*                                                                               
HS57D    DSECT                     CL#01064432                                  
HS57REC  DS    0CL57                                                            
HS57RTUP DS    CL1                                                              
HS57SEQ  DS    CL9                 SEQUENCE NUMBER                              
HS57ACC  DS    CL12                ACCOUNT NUMBER                               
HS57CKNO DS    CL10                CHECK NUMBER                                 
HS57AMNT DS    CL13                CHECK AMOUNT                                 
HS57IDAT DS    CL6                 ISSUE DATE  YYMMDD                           
HS57PDAT DS    CL6                 PAID DATE  YYMMDD                            
HS57SPR  DS    (L'HS57REC-(*-HS57D))C                                           
HS57LNQ  EQU   *-HS57REC                                                        
         EJECT                                                                  
*                                                                               
* DSECT FOR HSBC BANK  - 300 BYTE RECORDS CL#0109113T                           
*                                                                               
HS300D   DSECT                                                                  
HS300REC DS   0CL300                                                            
HS300RTY DS   CL1                 RECORD TYPE A,C,D,Z USING C ONLY              
         DS   CL9                 RECORD COUNT NOT USED                         
         DS   CL10                EFT OID NOT USED                              
         DS   CL4                 EFT OID NOT USED                              
         DS   CL30                EFT OID NOT USED                              
HS300PDT DS   CL6                 DUE DATE / PAID DATE CYYDDD JULIAN            
         DS   CL18                                                              
HS300AMT DS   CL10                CHECK AMOUNT                                  
         DS   CL51                                                              
HS300OID DS   0CL19                                                             
HS300AC# DS   CL12                ACCOUNT NUMBER                                
HS300CK# DS   CL7                 CHECK NUMBER                                  
         DS   CL93                                                              
HS300STA DS   CL2                 01=VALID IGNORE REST                          
HS300SPR DS    (L'HS300REC-(*-HS300D))C                                         
HS300LNQ EQU   *-HS300REC                                                       
         EJECT                                                                  
*                                                                               
* DSECT FOR NATIONAL CITY BANK 80 BYTE (JULY 2004)                              
*                                                                               
NACTD    DSECT                                                                  
NACTREC  DS    0CL80                                                            
NACTACC  DS    CL10                ACCOUNT NUMBER                               
         DS    CL1                                                              
NACTIND  DS    CL1                 VOID INDICATOR                               
         DS    CL1                                                              
NACTAMNT DS    CL12                CHECK AMOUNT                                 
NACTPDAT DS    CL6                 YYMMDD  PAID DATE                            
         DS    CL9                                                              
NACTCKNO DS    CL9                 CHECK NUMBER                                 
         DS    CL(L'NACTREC-(*-NACTREC))                                        
*                                                                               
* ROYAL BANK  ANOTHER SPEC (AUGUST 2004)                                        
*                                                                               
RBKD     DSECT                                                                  
RBKREC   DS    0CL80            SECTION 5.6 OF SPEC                             
*                                                                               
RBKDACCN DS    0CL12            TRANSIT+ACCOUNT NUMBER.                         
RBKDTNO  DS    CL5              TRANSIT NUMBER                                  
RBKDACNO DS    CL7              BANK ACCOUNT NUMBER                             
RBKDRCOD DS    CL2              REPORT CODE 30                                  
         DS    CL2              FILLER                                          
RBKCKNO  DS    CL8              CHECK SERIAL NUMBER                             
RBKDAMT  DS    CL11             CHECK AMOUNT                                    
RBKVOID  DS    CL2              FILLER                                          
RBKIDATE DS    CL6              DATE ISSUED  YYMMDD                             
RBKPDATE DS    CL6              DATE PAID    YYMMDD                             
RBKDTCOD DS    CL3                                                              
         DS    CL19             CLIENT REFERENCE                                
         DS    CL(80-(*-RBKREC))                                                
*                                                                               
* DSECT FOR ROYAL BANK - 80 BYTE RECORDS  (JUN 2002)                            
*                                                                               
ROYD     DSECT                                                                  
ROYREC   DS    0CL80                                                            
*                                                                               
ROYHCNO  DS    CL10             CLIENT NO.                                      
         DS    CL20             FILLER SPACES                                   
ROYHTYPE DS    CL2              10                                              
ROYHCRDT DS    CL6              CREATION DATE YYMMDD                            
ROYHCRTM DS    CL4              CURRENT TIME HHMM                               
         DS    CL(80-(*-ROYHCNO))    FILLER SPACES                              
         ORG   ROYREC           **DETAIL RECORD**                               
ROYDCNO  DS    CL10             CLIENT NO.                                      
         DS    CL3              BANK NO. CANADIAN/US                            
         DS    CL5              TRANSIT NUMBER CANADIAN/US                      
ROYDACNO DS    CL12             ACCOUNT NO.                                     
ROYDTYPE DS    CL2              35-OUTSTANDING,79-CREDIT,51-DEBIT               
ROYDSNO  DS    CL12             CHECK NO./SERIAL NO.                            
ROYDAMT  DS    CL12             CHECK AMOUNT                                    
ROYDDATE DS    CL6              DATE ISSUED YYMMDD                              
         DS    CL(80-(*-ROYDTYPE))                                              
         ORG   ROYREC              ** TRAILER RECORD**                          
ROYTCNO  DS    CL10                                                             
         DS    CL20                                                             
ROYTTYPE DS    CL2                 TYPE                                         
ROYTDCNT DS    CL7                 TOTAL NO. OF DETAIL RECORDS                  
ROYTTAMT DS    CL15                TOTAL AMOUNT                                 
ROYTCNT  DS    CL7                 TOTAL NUMBER OF RECORDS                      
         DS    CL(80-(*-ROYREC))   SPACES                                       
*&&DO                                                                           
* DSECT FOR UNITED MISSOURI BANK 80 BYTE ROUTINE                                
*                                                                               
UMBD     DSECT                      CL#0144329N                                 
UMBREC   DS     0CL80                                                           
UMBBNO   DS     CL3                 BANK NUMBER                                 
UMBACCT  DS     CL10                ACCOUNT NUMBER                              
UMBCKNO  DS     CL10                CHECK NUMBER                                
UMBIDAT  DS     CL6                 ISSUE DATE  YYMMDD                          
UMBPDAT  DS     CL6                 PAID DATE   YYMMDD                          
UMBSEQ#  DS     CL8                 UMB GENERATED SEQUENCE #                    
UMBAMNT  DS     CL10                CHECK AMOUNT                                
UMBLEN   EQU   *-UMBD              LENGTH OF RECORD                             
UMBSPR   DS    CL(L'UMBREC-UMBLEN)                                              
*&&                                                                             
*                                                                               
* DSECT FOR US BANK  - 80 BYTE RECORDS (APR 2004)                               
*                                                                               
USBKD    DSECT                                                                  
USBKREC  DS    0CL80                                                            
USBKRCOD DS    CL2                 RECORD CODE (DETAIL 01)                      
USBKACC  DS    CL12                ACCOUNT NUMBER                               
USBKCKNO DS    CL10                CHECK NUMBER                                 
USBKAMNT DS    CL12                CHECK AMOUNT                                 
USBKPDAT DS    CL8                 PAID DATE  MMDDYYYY                          
USBKIDAT DS    CL8                 ISSUE DATE  MMDDYYYY                         
         DS    CL12                                                             
         DS    CL15                                                             
USBKRECT DS    CL1                 RECORD TYPE                                  
USBKPD   EQU   C'1'                PAID NO ISSUE                                
USBKOUT  EQU   C'2'                OUTSTANDING                                  
USBKRCN  EQU   C'3'                RECONCILED                                   
USBKSTOP EQU   C'4'                STOP                                         
USBKCANC EQU   C'5'                CANCELLED                                    
USBKDR   EQU   C'7'                DEBIT                                        
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE ACFRMTABD                                                      
       ++INCLUDE ACKEYWRD                                                       
         PRINT ON                                                               
* DDSMTPD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSMTPD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'164ACREPCR02 10/14/20'                                      
         END                                                                    
