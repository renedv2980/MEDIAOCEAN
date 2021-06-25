*          DATA SET ACREP6102  AT LEVEL 067 AS OF 04/23/15                      
*PHASE AC6102A                                                                  
*INCLUDE HEXIN                                                                  
*INCLUDE ACASOF                                                                 
*                                                                               
*              PROFILES                                                         
*                                                                               
* 1            PRINT ESTIMATE VALUE                                             
* 2            PRINT INPUT TYPE ANALYSIS                                        
* 3            PRINT INPUT TYPE ANALYSIS BY CONTRA LEDGER                       
* 4            PRINT MEDIA/UNIT SUMMARY                                         
* 5            PRINT UNIT/MEDIA SUMMARY                                         
* 6            TREAT CONTRA OF SK AS TIME                                       
* 7            AGEING METHOD, N,F,O,U,M                                         
* 8            SORT CLIENT SUMMARY BY OFFICE                                    
* 9            USE TRANSACTION DATE, NOT MOA                                    
* 10           SUPRESS OPEN CHARGES I.E.- WHEN OPEN ITEM AGEING                 
*              DON'T SHOW BILLED CHARGES, EVEN THOUGH IT WILL RUIN BAL          
*                                                                               
*              REQUEST OPTIONS                                                  
*                                                                               
* 1      S     SUMMARIES ONLY                                                   
* 2      T     TRIAL BALANCE REPORT                                             
* 3    A,U,N,E JOBS WITH AN APPROVED, UNAPPROVED, NO ESTIMATE OR ETA=NO         
*        R     NEW EST, NEED EST APP, HIAPP^=0 AND HIAPP ^=HIREV                
* 4      D,C   DEBIT JOBS ONLY, CREDIT JOBS ONLY                                
* 5      Y,T   EXCLUDE TIME, REPORT ON TIME ONLY                                
* 6      Y     USE TRANSACTION TYPE OVERRIDE                                    
* 7            PROFILE AGEING METHOD OVERRIDE                                   
         TITLE 'JOB AGEING REPORT'                                              
AC6102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,AC6102,R8,R9                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC6102D,RC                                                       
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQFST                                                           
*                                                                               
         LR    RE,RC                                                            
         LH    RF,=Y(A61DLEN)                                                   
         XCEF                                                                   
*                                                                               
         BAS   RE,GETBUFF          AQUIRE STORAGE VIA GETMAIN                   
*                                                                               
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
         GOTO1 ACMAJOBL,DMCB,FLDH,ACMACOLL,ADCOMFAC                             
         CLI   DMCB+4,X'00'                                                     
         BNE   XIT                                                              
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
*        FOR JOB AGEING, SET HEADLINE DATES                                     
*        FOR TRIAL BAL, DETERMINE EFFECTIVE MONTH                               
*                                                                               
REQFST   CLI   MODE,REQFRST                                                     
         BNE   LEVAFST                                                          
*                                                                               
         LA    RE,AC61DREQ                                                      
         LH    RF,=Y(A61DLN2)                                                   
         XCEF                                                                   
*                                                                               
         L     R2,=A(SAVERC)                                                    
         ST    RC,0(R2)            SAVE REG C                                   
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R2,=A(HOOK)                                                      
         ST    R2,HEADHOOK                                                      
         L     R7,=A(BUFFALOC)                                                  
         ST    R7,ADBUFF                                                        
         GOTO1 BUFFALO,DMCB,=C'SET',(R7)                                        
         L     R7,=A(OFFTAB)                                                    
         ST    R7,AOFFTAB                                                       
         MVI   RSUMSW,C'N'         FOR SUMMARIES                                
         MVI   RCSUBPRG,0                                                       
         MVI   PROFLAG,C'Y'        FLAG TO READ PROFILES ONCE/REQUEST           
         MVI   UNBCALL,C'N'                                                     
         XC    REQSTAT,REQSTAT                                                  
*                                                                               
         CLI   QOFFICE,C' '       WILL I NEED THE OFFICE LEVEL PROFILE          
         BNE   REQF02                                                           
         CLC   QACCOUNT(3),SPACES                                               
         BNE   REQF02                                                           
         B     *+8                                                              
*                                                                               
REQF02   OI    REQSTAT,OFFICE                                                   
         CLI   QOPT2,C'T'          TRIAL BALANCE RUN                            
         BNE   *+8                 NO                                           
         MVI   RSUMSW,C'Y'         YES, PRODUCE SUMMARIES                       
         GOTO1 PROLLER,DMCB,0,VBLOCKAC,9547,5 PACK PROLLER AREA                 
*                                  1   CURRENT JOB                              
*                                  37  PRODUCT MEDIA                            
*                                  37  CLIENT MEDIA                             
*                                  255*37, OFFICE*MEDIA                         
*                                + 37  REPORT MEDIA                             
*                                  ------                                       
*                                  9547 5PL6 BUCKETS                            
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'CLEAR',ADBUFF,(X'80',1)                          
*                                                                               
         L     R3,=A(TYPETAB)                                                   
         XC    0(4,R3),0(R3)                                                    
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         LA    R5,NUMBUCKS         INIT BUCKETS                                 
         LA    R6,BUCKS            BUCKET AREA                                  
*                                                                               
REQF04   ZAP   0(6,R6),=P'0'       ZAP BUCKET                                   
         LA    R6,6(R6)            POINT TO NEXT BUCKET                         
         BCT   R5,REQF04                                                        
         NI    XJOB,X'FF'-XJINREQ                                               
*                                                                               
         BAS   RE,GETLIST          ...OF MEDIA/NAMES                            
         BAS   RE,GETOFF           ...OF TWO BYTE OFFICE                        
*                                                                               
         MVI   MYHEAD7,C' '                                                     
         MVC   MYHEAD7+1(L'MYHEAD7-1),MYHEAD7                                   
         MVI   MYHEAD7,X'00'                  FORCE HEAD7 PRINT                 
         L     R6,=A(PATTERN)                                                   
         USING PATTERD,R6                                                       
         CLI   QOPT4,C' '                                                       
         BE    REQF06                                                           
         MVC   MYHEAD7(24),JOBWDEB                                              
         CLI   QOPT4,C'D'                                                       
         BE    REQF06                                                           
         MVC   MYHEAD7(25),JOBWCRD                                              
*                                                                               
REQF06   CLI   QOPT3,C' '                                                       
         BE    REQF12                                                           
         CLI   QOPT4,C' '                                                       
         BE    REQF08                                                           
         MVC   MYHEAD7+50(3),AND                                                
*                                                                               
REQF08   MVC   MYHEAD7+60(9),JOBSW JOBS WITH...                                 
         CLI   QOPT3,C'N'                                                       
         BNE   *+14                                                             
         MVC   MYHEAD7+70(32),NOEST NO ESTIMATE                                 
         B     REQF10                                                           
*                                                                               
         CLI   QOPT3,C'R'                                                       
         BNE   *+14                                                             
         MVC   MYHEAD7+70(29),ANESTAWA AN EST AWAITING APPROVAL                 
         B     REQF10                                                           
*                                                                               
         CLI   QOPT3,C'A'                                                       
         BNE   *+14                                                             
         MVC   MYHEAD7+70(20),ANAEST AN APPROVED ESTIMATE                       
         B     REQF10                                                           
*                                                                               
         MVC   MYHEAD7+70(22),ANUEST AN UNAPPROVED                              
*                                                                               
REQF10   GOTO1 ADSQUASH,DMCB,MYHEAD7,132                                        
*                                                                               
         USING ACMD,R7        SET UP EFFECTIVE MONTH                            
REQF12   L     R7,AMONACC                                                       
         MVC   MYMEND,ACMMEND         SAVE MEND                                 
*                                                                               
         CLI   QOPT2,C'T'          TRIAL BAL                                    
         BNE   REQFX                                                            
         MVI   FCREVOVR,FCREVTRY+FCREVTRN                                       
         CLI   MYMEND,X'FF'        NO MOS END SPECIFIED                         
         BE    REQF14              USE THIS MONTH                               
         MVC   EFFMON(6),ACMCMEND  STORE EFFECTIVE MONTH FOR TRIAL BAL          
         MVC   EFFMOS(2),MYMEND    X'YYMM'                                      
         B     REQFX                                                            
*                                                                               
REQF14   GOTO1 DATCON,DMCB,(4,RCDATE),(6,WORK)                                  
         MVC   EFFMON(6),WORK                                                   
         MVC   WORK(2),RCDATE+6    YY                                           
         MVC   WORK+2(2),RCDATE    MM                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,EFFMOS)                                  
*        GOTO1 =V(HEXIN),DMCB,WORK,EFFMOS,4    X'YYMM'                          
         MVC   ACMMEND,EFFMOS        HEVE MONACC ONLY PASS GOOD MOA'S           
*                                                                               
REQFX    B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
LEVAFST  CLI   MODE,LEVAFRST       CLIENT LEVEL PROCESSING                      
         BNE   LEVBFST                                                          
         MVI   RCSUBPRG,0                                                       
         CLI   PROFLAG,C'Y'        FIRST CLIENT FOR THIS REQUEST                
         BNE   LEVAF02             NO                                           
         BAS   RE,PROFS            GET NEW PROFILES                             
         BAS   RE,SETSTATH         SET STATUS HEADERS                           
*                                                                               
LEVAF02  MVI   PROFLAG,C'N'                                                     
         CLI   AGEMETH,C'O'        OPEN AGEING?                                 
         BNE   *+8                                                              
         MVI   FCRDHIST,C'Y'       READ HISTORIES                               
*                                                                               
         CLI   AGEMETH,C'O'        OPEN AGEING?                                 
         BE    *+12                                                             
         CLI   AGEMETH,C'U'        UNBILLED AGEING                              
         BNE   *+14                                                             
         USING ACMD,R7                                                          
         L     R7,AMONACC                                                       
         MVC   ACMMEND,=X'FFFF'    DISABLE MOA FILTERING                        
         DROP  R7                                                               
*                                                                               
         LA    R5,CLIBUCKS         INIT CLIENT                                  
         LA    R6,CLICRDS          BUCKET AREA                                  
*                                                                               
LEVAF04  ZAP   0(6,R6),=P'0'       ZAP BUCKET                                   
         LA    R6,6(R6)            POINT TO NEXT BUCKET                         
         BCT   R5,LEVAF04                                                       
*                                                                               
         LA    R6,SUBTOTS                                                       
         LA    R5,2                TWO CLIENT LEVEL JOB COUNTS                  
*                                                                               
LEVAF06  ZAP   0(6,R6),=P'0'       ZAP BUCKET                                   
         LA    R6,6(R6)            POINT TO NEXT BUCKET                         
         BCT   R5,LEVAF06                                                       
*                                                                               
         NI    XJOB,X'FF'-XJINCLI                                               
         CLI   QOPT1,C'S'                                                       
         BE    LEVAFX                                                           
         MVI   FORCEHED,C'Y'       TOP OF FORM ON NEW CLIENT                    
*                                                                               
LEVAFX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
*        MOVE PRODUCT CODE, PRODUCT NAME TO PRODNAME                            
*        SAVE IN CASE THERE IS NO PRODUCT ACTIVITY                              
*                                                                               
LEVBFST  CLI   MODE,LEVBFRST       PRODUCT LEVEL PROCESSING                     
         BNE   ACCFST                                                           
*                                                                               
         MVC   THISUNIT,SPACES     SAVE OFFICE FOR THIS ACCOUNT                 
         L     R3,ADPROFIL                                                      
         USING ACPROFD,R3                                                       
         CLI   ACPROFFC,0          OFFICE X'00'                                 
         BE    *+10                LEAVE IT A SPACE THEN                        
         MVC   THISUNIT,ACPROFFC                                                
*                                                                               
         LA    R5,PROBUCKS         INIT PRODUCT                                 
         LA    R6,PROXJOB          BUCKET AREA                                  
*                                                                               
LEVBF02  ZAP   0(6,R6),=P'0'       ZAP BUCKET                                   
         LA    R6,6(R6)            POINT TO NEXT BUCKET                         
         BCT   R5,LEVBF02                                                       
*                                                                               
         NI    XJOB,X'FF'-XJINPRO                                               
         CLI   QOPT1,C'S'          NO PRODUCT LEVEL DETAIL ON SUMMARIES         
         BE    LEVBFX                                                           
         USING ACHEIRD,R4                                                       
         L     R4,ADLDGHIR         HEIRARCHY ELEMENT                            
         SR    R5,R5                                                            
         IC    R5,ACHRLEVA         LENGTH OF CLIENT                             
         MVC   PRODNAME,SPACES                                                  
         L     R2,ADHEIRB          PRODUCT LEVEL KEY                            
         LA    R5,3(R2,R5)         R5 GETS A(PRODUCT CODE)                      
         MVC   PRODNAME(6),0(R5)   MOVE PRODUCT CODE INTO WORK                  
         LA    R2,PRODNAME+6       POINT R2 TO END OF PRODUCT CODE              
*                                                                               
LEVBF04  CLI   0(R2),C' '          BACK UP R2 TILL LAST CHAR OF PROD            
         BNE   LEVBF06                                                          
         BCT   R2,LEVBF04                                                       
*                                                                               
LEVBF06  LA    RF,PRODNAME         START OF PRODUCT CODE                        
         LR    RE,R2               END OF PRODUCT CODE                          
         SR    RE,RF               DIFFERENCE IS THE LEN OF PROD CODE           
         STH   RE,PRODISP          STORE LENGTH FOR REPORTING                   
*                                                                               
         L     R4,ADLVBNAM         A(PRODUCT NAME EL) IN R4                     
         BAS   RE,GETNAME          MOVE PRODUCT NAME TO WORK                    
         MVC   2(36,R2),WORK       PROD CODE-SPACE-PROD NAME                    
*                                                                               
         LA    R2,38(R2)                                                        
LEVBF08  CLI   0(R2),C' '          BACK R2 TILL LAST CHAR OF PROD NAME          
         BNE   LEVBF10                                                          
         BCT   R2,LEVBF08                                                       
*                                                                               
LEVBF10  CLI   PROFILES,C'R'       PRINT R VALUES W/ESTIMATE                    
         BNE   LEVBF12             NO                                           
         MVC   1(6,R2),=C', OFF='  TACK OFFICE TO END OF PRODUCT                
         MVC   7(2,R2),THISUNIT                                                 
*                                                                               
LEVBF12  MVI   PRODSW,1            SET PRODUCT PRINT PENDING                    
*                                                                               
LEVBFX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
*              FIRST FOR JOB AND TRANSACTION PROCESSING                         
*                                                                               
ACCFST   CLI   MODE,PROCACC                                                     
         BNE   SUBFST                                                           
*                                                                               
         NI    XJOB,X'FF'-XJ       ASSUME NOT AN X JOB                          
         USING ACJOBD,R4                                                        
         L     R4,ADACC                                                         
         MVI   ELCODE,ACJBELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    ACJBSTAT,ACJBXJOB   IS THIS AN X-JOB                             
         BZ    *+8                                                              
         OI    XJOB,XJ             FLAG AS SUCH                                 
*                                                                               
         MVI   FCRDTRNS,C'Y'       MAKE SURE THIS IS ON (FOR NOW)               
         CLI   QOPT2,C'T'          TRIAL BAL?                                   
         BE    ACCF02              YES, READ ALL TRANSACTIONS                   
*                                                                               
         CLI   QOPT5,C' '          ARE THEY EX/INCLUDING TIME                   
         BNE   ACCF02              YES, READ ALL TRANSACTIONS                   
*                                                                               
         CLI   MYMEND,X'FF'        MOA END SPECIFIED                            
         BNE   ACCF02              YES, READ ALL TRANSACTIONS                   
*                                                                               
         CLI   AGEMETH,C'U'        UNBILLED ITEMS AGEING METHOD                 
         BE    ACCF02              YES, READ ALL TRANSACTIONS                   
*                                                                               
         USING ACBALD,R4                                                        
         MVI   ELCODE,X'32'        SEE IF JOB HAS ZERO BAL                      
         L     R4,ADACC                                                         
         BAS   RE,GETEL                                                         
         CP    ACBLDR,ACBLCR       DEBITS EQUAL CREDITS ?                       
         BNE   ACCF02              NO, READ TRANS                               
*                                                                               
         TM    XJOB,XJ             IS THIS AN XJOB                              
         BO    ACCF02              YES                                          
*                                                                               
         MVI   FCRDTRNS,C'N'       DONT READ TRANSACTIONS                       
         B     ACCFX                                                            
*                                                                               
ACCF02   L     R3,=A(TIMETAB)      RESET BINSRCH TABLES                         
         XC    0(4,R3),0(R3)                                                    
         L     R3,=A(BILLTAB)                                                   
         XC    0(4,R3),0(R3)                                                    
         L     R3,=A(OCRTAB)                                                    
         XC    0(4,R3),0(R3)                                                    
         L     R3,=A(TBLTAB)                                                    
         XC    0(4,R3),0(R3)                                                    
*                                                                               
         L     RE,=A(TBLTAB)                                                    
         LA    RE,8(RE)            BUMP PAST HEADER                             
         LH    RF,=Y(TBLTABLN)                                                  
         XCEF                                                                   
*                                                                               
         L     RE,=A(OCRTAB)                                                    
         LA    RE,8(RE)            BUMP PAST HEADER                             
         LH    RF,=A(OCRTABLN)                                                  
         XCEF                                                                   
*                                                                               
         L     RE,=A(BILLTAB)                                                   
         LA    RE,8(RE)            BUMP PAST HEADER                             
         L     RF,=A(BILTABLN)                                                  
         XCEF                                                                   
*                                                                               
         L     RE,=A(TIMETAB)                                                   
         LA    RE,8(RE)            BUMP PAST HEADER                             
         LH    RF,=Y(TIMTABLN)                                                  
         XCEF                                                                   
*                                                                               
         USING OFFTABD,R2                                                       
         LA    R0,OFFTABNM         CLEAR TABLE OF OFFSETS                       
         L     R2,AOFFTAB                                                       
*                                                                               
ACCF04   XC    OFFREF,OFFREF                                                    
         XC    OFFDATE,OFFDATE                                                  
         XC    OFFMNTH,OFFMNTH                                                  
         ZAP   OFFAMNT,=P'0'                                                    
         LA    R2,OFFTABLN(R2)                                                  
         BCT   R0,ACCF04                                                        
*                                                                               
         BAS   RE,ESTSRCH          SET JOB STAT'S FOR REPORT                    
*                                                                               
         BAS   RE,ASOFINIT         PREREAD BILLING FOR ACASOF                   
*                                                                               
         CLI   QOPT2,C'T'          TRIAL BAL RUN                                
         BE    ACCFX               YES, POST DIRECTLY TO PROLLER                
*                                                                               
         LA    R5,JOBBUCKS         NO, POST TO BUCKETS                          
         LA    R6,BUCKS            BUCKET AREA                                  
*                                                                               
ACCF06   ZAP   0(6,R6),=P'0'       ZAP BUCKET                                   
         LA    R6,6(R6)            POINT TO NEXT BUCKET                         
         BCT   R5,ACCF06                                                        
*                                                                               
ACCFX    B     XIT                                                              
*                                                                               
**********************************************************************          
*                                                                               
         EJECT                                                                  
SUBFST   CLI   MODE,PROCSBAC       NEW SUBACC?                                  
         BNE   TRANREC             NO                                           
         CLI   AGEMETH,C'O'        OPEN AGEING                                  
         BE    SUBF02              YES                                          
         CLI   AGEMETH,C'U'        UNBILLED                                     
         BNE   SUBFX               NO                                           
*                                                                               
         USING OFFTABD,R2                                                       
SUBF02   LA    R0,OFFTABNM         CLEAR TABLE OF OFFSETS                       
         L     R2,AOFFTAB                                                       
*                                                                               
SUBF04   XC    OFFREF,OFFREF                                                    
         XC    OFFDATE,OFFDATE                                                  
         XC    OFFMNTH,OFFMNTH                                                  
         ZAP   OFFAMNT,=P'0'                                                    
         LA    R2,OFFTABLN(R2)                                                  
         BCT   R0,SUBF04                                                        
*                                                                               
         MVI   OFFSTAT,C' '                                                     
*                                                                               
SUBFX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
TRANREC  CLI   MODE,PROCTRNS                                                    
         BNE   SUBLST                                                           
         MVI   TPUTSTAT,C' '         FLAG FOR CALLS TO TIMEPUT.                 
         XC    TRANSTAT,TRANSTAT                                                
         USING TRANSD,R4                                                        
         USING ACKEYD,R5                                                        
         L     R4,ADTRANS                                                       
         LR    R5,R4                                                            
         SH    R5,DATADISP                                                      
         CLI   TRNSEL,X'44'                                                     
         BNE   XIT                                                              
*                                                                               
         CLC   ACKEYWRK,=C'**'     IS THIS A PURCHASE ORDER                     
         BE    XIT                 YES                                          
*                                                                               
         ZAP   TRANAMNT,TRNSAMNT   SAVE TRNSAMNT                                
         TM    XJOB,XJ             IS THIS AN XJOB                              
         BNO   TRAN04                                                           
*                                                                               
         USING TRCASHD,R4                                                       
TRAN02   MVI   ELCODE,X'50'        USE MEMO AMOUNT FOR X JOB TRANS              
         BAS   RE,NEXTEL                                                        
         BNE   TRAN04                                                           
*                                                                               
         CLI   TRCSTYPE,C'S'                                                    
         BNE   TRAN02                                                           
         ZAP   TRANAMNT,TRCSAMNT                                                
*                                                                               
TRAN04   L     R7,AMONACC          WHERE MDATE IS FOUND                         
         USING ACMD,R7                                                          
         USING TRANSD,R4                                                        
         L     R4,ADTRANS                                                       
         MVC   POSTDATE,ACMMDTE      GET TRANSACTION MOA                        
*                                                                               
         ZAP   POSTTRAN,TRANAMNT   BALANCE=DEBITS - CREDITS                     
         TM    TRNSSTAT,X'80'      IS THIS A CREDIT (IE BILLING)                
         BO    TRAN06              NO                                           
*                                                                               
* NOTE MONACCS MOS FILTERING REMOVED FOR BURSON                                 
*                                                                               
         CLC   MYMEND,POSTDATE     IS THIS BILL. PAST MOS END                   
         BL    XIT                 YES, FILTER HERE                             
*                                                                               
         ZAP   DUB,TRANAMNT                                                     
         MP    DUB,=P'-1'          REVERSE SIGN OF CREDITS                      
         ZAP   POSTTRAN,DUB                                                     
         B     TRAN14              PROCESS BILLING BELOW                        
*                                                                               
TRAN06   CLI   QOPT2,C'T'          TRIAL BAL                                    
         BE    TRAN08              NEED TIME FOR REPORT                         
         CLI   QOPT5,C' '            AGEING WITH NO TIME ?                      
         BE    TRAN16                THEN I DON'T NEED TIME                     
*                                                                               
TRAN08   CLC   ACKEYCON+1(2),=C'1R'  CONTRA A/C 1R?                             
         BE    TRAN10                YES, ITS TIME                              
         CLI   PROFILES+5,C'Y'       CONSIDER C/A OF SK AS TIME OPTION?         
         BNE   TRAN12                NO, FORGET IT                              
         CLC   ACKEYCON+1(2),=C'SK'  CONTRA A/C SK?                             
         BNE   TRAN12                NO, ITS NOT CLIENT TIME                    
*                                                                               
TRAN10   EQU   *                     INTERNAL TIME CHARGES                      
         OI    TRANSTAT,TIME         FLAG TRAN FOR TRIAL BAL                    
         CLI   QOPT5,C' '            OPTION TO ISOLATE CLIENT TIME              
         BE    TRAN16                NO JUST PASS IT ALONG                      
*                                                                               
         MVI   TPUTSTAT,C'B'         TELL TIMEPUT WE WANT TO ...                
         BAS   RE,TIMEPUT            SAVE THE AMOUNT BILLED                     
         CLI   QOPT5,C'Y'            EXCLUDE TIME?                              
         BE    XIT                   YES,                                       
         B     TRAN16                NO PASS TIME CHARGES ALONG                 
*                                                                               
TRAN12   EQU   *                     NON TIME CHARGES                           
         CLI   QOPT5,C' '            OPTION TO ISOLATE CLIENT TIME              
         BE    TRAN16                NO JUST PASS IT ALONG                      
         CLI   QOPT5,C'T'            TIME ONLY RUN?                             
         BE    XIT                   YES, THEN EXCLUDE NON TIME                 
         B     TRAN16                NO, PASS NON TIME CHARGES                  
*                                                                               
TRAN14   EQU   *                   CHECK TABLE FOR BILLED AMOUNT                
         BAS   RE,TIMEGET          OF CHARGES WHICH HAVE BEEN                   
*                                  ISOLATED                                     
*                                                                               
         USING TRANSD,R4                                                        
         USING ACKEYD,R5                                                        
TRAN16   L     R4,ADTRANS            RESET REGISTERS                            
         LR    R5,R4                                                            
         SH    R5,DATADISP                                                      
         CLI   QOPT2,C'T'                                                       
         BE    TRIAL                                                            
*                                                                               
         CLI   PROFILES+8,C'Y'                                                  
         BNE   *+10                                                             
         MVC   POSTDATE,TRNSDATE   USE YYMM OF TRANSACTION DATE                 
         CLI   AGEMETH,C'N'        DEFAULT AGEING                               
         BNE   TRAN18                                                           
*                                                                               
*----------------------------------------------------------------------         
*        DEFAULT AGEING METHOD-PUT DR'S AND -CR'S IN ONE BUCKET                 
*                                  CR'S AND -DR'S IN ANOTHER                    
*        WHY?, SO THAT OFFSET DR'S OFFEST THEIR ORIGINAL CHARGES                
*        AND REVERSED BILLS ARE APPLIED TO THEIR REVERSALS, NOT CHARGES         
*----------------------------------------------------------------------         
         BAS   RE,CHKMOA                                                        
         BH    XIT                                                              
         CP    TRANAMNT,=P'0'                                                   
         BNL   *+8                                                              
         OI    TRANSTAT,NEGATIVE                                                
*                                                                               
         TM    TRNSSTAT,X'80'                                                   
         BO    *+8                                                              
         OI    TRANSTAT,CREDIT                                                  
*                                                                               
         LA    R6,DEBS                                                          
         TM    TRANSTAT,NEGATIVE+CREDIT                                         
         BNM   TRAN20               NEG CR'S,POS DR'S TO ONE BUCK...            
         LA    R6,CRDS                                                          
         B     TRAN20                                                           
*       - - - - - - - - - - - - - - -                                           
TRAN18   CLI   AGEMETH,C'O'        OPEN ITEM AGEING                             
         BNE   *+12                YES                                          
         BAS   RE,ISPEELED         PEELED ITEMS ARE NOT OPEN                    
         BE    XIT                                                              
*       - - - - - - - - - - - - - - -                                           
         BAS   RE,CHKMOA                                                        
         BH    TRAN22                                                           
         LA    R6,DEBS             NON DEFAULT AGEING                           
         TM    TRNSSTAT,X'80'      IS THIS A DEBIT                              
         BO    *+8                 YES,                                         
         LA    R6,CRDS             NO USE CREDITS BUCKET                        
*                                                                               
* OPEN ITEM AND UNBILLED BOTH CALL ACASOF, WHICH WILL ZERO OUT UNBILLED         
* IF THE TRANSACTION IS PAST ASOF DATE                                          
*                                                                               
TRAN20   ST    R6,POSTBUCK         SAVE ADDRESS OF POSTING BUCKETS              
         BAS   RE,AGEIT            AGE THE TRANSACTION                          
*                                                                               
TRAN22   CLI   AGEMETH,C'O'        OPEN ITEM AGEING                             
         BE    TRAN24              YES                                          
         CLI   AGEMETH,C'U'        UNBILLED ITEM                                
         BNE   XIT                 NO   EXIT                                    
*                                                                               
TRAN24   TM    TRNSSTAT,X'80'      IS THIS A DEBIT                              
         BNO   TRAN26              NO                                           
*                                                                               
         MVI   TPUTSTAT,C'S'       TELL TIMEPUT WE WANT TO ...                  
         BAS   RE,TIMEPUT          SUBTRACT BILLED PORTION OF CHARGE            
*                                                                               
         CLC   QUESTOR,=CL12'POUNDPUP'                                          
         BNE   *+8                                                              
         BAS   RE,PRINTIT                                                       
*                                                                               
         TM    TRNSSTAT,X'20'      OFFSET ITEM                                  
         BO    PUTOFFS             OFFSETS SAVED IN A TABLE - OPEN AGE          
*                                                                               
         LA    R6,OPENDEBS         ADD TO OPEN DEBITS BUCKET                    
         ST    R6,POSTBUCK                                                      
         BAS   RE,AGEIT                                                         
         B     XIT                                                              
*                                                                               
*        SAVE OPEN CREDITS                                                      
*                                                                               
TRAN26   CLI   AGEMETH,C'O'        OPEN ITEM AGEING?                            
         BNE   XIT                 NO                                           
*                                                                               
         MVI   ELCODE,X'60'        GET DA DATE                                  
         BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TRSTATD,R4                                                       
         MVC   POSTDADT,TRSTDATE   SAVER DA DATE                                
         USING TRNELD,R4                                                        
         L     R4,ADTRANS          RESTORE R4                                   
*                                                                               
         L     R3,=A(BILLTAB)      TABLE OF BILL #'S FROM THE 4B'S              
         L     R5,0(R3)            # IN TABLE                                   
         LTR   R5,R5               ANYTHING THERE                               
         BZ    TRAN32              NO, BILL DIDN'T MARK CHARGES                 
         LA    R3,8(R3)            POINT TO DATA                                
*                                                                               
         GOTO1 DATCON,DMCB,(1,TRNDATE),(2,BINDTE)                               
*                                                                               
TRAN28   CLC   0(6,R3),TRNREF      DID THIS BILL MARK CHARGES                   
         BNE   TRAN30              YES                                          
         CLC   6(2,R3),TRN2DAY     TRY DATE MATCH FOR PROGRESSIVE               
         BE    TRAN34              YES                                          
         CLC   8(2,R3),BINDTE      TRY DATE MATCH FOR CLIENT                    
         BE    TRAN34              YES                                          
*                                                                               
TRAN30   LA    R3,TIMELEN(R3)      GET NEXT BILL NUM/DATE                       
         BCT   R5,TRAN28                                                        
*                                                                               
*        THIS BILL DIDNT MARK CHARGES, SAVE IT                                  
TRAN32   CLI   QOPT5,C'T'          TIME ONLY                                    
         BE    XIT                 YES, FORGET THIS BILL                        
         MVI   OFFSTAT,C'B'                                                     
*                                                                               
         TM    TRNSTAT,X'20'       OFFSET ITEM                                  
         BO    PUTOFFS             OFFSETS SAVED IN A TABLE - OPEN AGE          
*                                                                               
         BAS   RE,SVCR             SAVE OPEN CREDITS BY DA DATE                 
         B     XIT                                                              
*                                                                               
*        THIS BILL DID MARK CHARGES, IF ITS A TOTAL BILL, SAVE THE              
*              AMOUNT OF NON MARKING BILLS IT REVERSED                          
*              TOTAL BILL AMOUNT = CHARGES MARKED - PREV BILLING                
*        THUS  PREVIOUS BILLS = MARKED - TOTAL AMOUNT OF BILL                   
*                                                                               
TRAN34   CLC   =C'TOTAL',TRNNARR   IS THIS A TOTAL BILL                         
         BNE   XIT                 NO                                           
*                                                                               
         BAS   RE,SVCR             SAVE TOTAL BILLS BY DA DATE                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              SPECIAL ROUTINES FOR POSTING TRIAL BALANCE OPTION      *         
***********************************************************************         
*                                                                               
TRIAL    SR    R3,R3               ZERO COLUMN COUNTER                          
         LA    R3,1                                                             
         CLC   EFFMOS,POSTDATE                                                  
         BL    XIT                 IGNORE NEXT MONTH.                           
         BH    TRIALX              OPENING BALANCE STUFF.                       
*                                                                               
*                                  PROCESS TRANS FOR THIS MONTH                 
         BAS   RE,ADDTTBL          ADD TO TYPE-TABLE FOR THIS MONTH             
*                                                                               
         LA    R3,4                BILLING                                      
         CLC   TRNANAL,=C'99'                                                   
         BE    TRIALX                                                           
*                                                                               
         CLI   TRNTYPE,33          33'S ARE INTERNAL AND/OR EXTERNAL            
         BE    TRIAL3                                                           
*                                                                               
         LA    R3,3                INTERNAL INCOME                              
*&&UK*&& B     TRIAL4                                                           
*&&US                                                                           
         TM    TRANSTAT,TIME       TIME CHARGE                                  
         BO    TRIAL2              GOTTA BE INTERNAL                            
         CLC   ACKEYCON+1(2),=C'SK'  CONTRA A/C SK?                             
         BE    TRIAL2                INTERNAL INCOME                            
         CLC   ACKEYCON+1(2),=C'SI'  CONTRA A/C SI?                             
         BNE   TRIAL4                NO, MUST BE EXTERNAL                       
*&&                                                                             
TRIAL2   BAS   RE,PROLLIT         ADD TO REPORT                                 
         BAS   RE,UNBINT          SEE IF ITS UNBILLED                           
         B     XIT                                                              
*                                                                               
TRIAL3   MVI   ELCODE,X'50'        SPLIT TYPE 33'S INTO EXT AND INT             
         ZAP   DUB(6),POSTTRAN                                                  
TRIAL3A  BAS   RE,NEXTEL                                                        
         BNE   TRIAL3B                                                          
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'C'       COMMISSION ITEM                              
         BNE   TRIAL3B                                                          
         SP    POSTTRAN,TRCSAMNT                                                
         B     TRIAL3A                                                          
*                                                                               
TRIAL3B  LA    R3,2                ADD EXTERNAL INCOME                          
         BAS   RE,PROLLIT                                                       
         SP    DUB(6),POSTTRAN    ANY INTERNAL INCOME SUBTRACTED                
         BZ    XIT                NO                                            
*                                                                               
         ZAP   POSTTRAN,DUB(6)    YES                                           
         BAS   RE,UNBINT          ADD IT TO TABLE, IF UNBILLED                  
         ZAP   POSTTRAN,DUB(6)    ADD TO REPORT ...                             
         LA    R3,3               AS INTERNAL INCOME                            
         B     TRIALX                                                           
*                                                                               
TRIAL4   LA    R3,2                MUST BE EXTERNAL                             
*                                                                               
TRIALX   BAS   RE,PROLLIT          ADD TO TABLE                                 
         B     XIT                                                              
*                                                                               
* BECAUSE THERE MAY BE TRANS WITH MOA'S PAST ENDDATE, CHECK HERE IF YOU         
* NEED THE TRAN. RETURNS HIGH IF THIS TRAN IS PAST MYMEND                       
* TRANS PAST MOA ARE FOR OPEN ITEM AND UNBILLED AGEING METHODS                  
*                                                                               
CHKMOA   NTR1                                                                   
         L     R7,AMONACC          WHERE MDATE IS FOUND                         
         USING ACMD,R7                                                          
         CLC   ACMMDTE,MYMEND      CHECK TRAN MOA                               
         DROP  R7                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
SUBLST   CLI   MODE,SBACLAST                                                    
         BNE   ACCLST                                                           
         CLI   AGEMETH,C'O'        OPEN ITEM AGE?                               
         BE    SUBL02              YES                                          
         CLI   AGEMETH,C'U'        UNBILLED                                     
         BNE   SUBLX               NO                                           
*                                                                               
         USING OFFTABD,R2                                                       
SUBL02   L     R2,AOFFTAB                                                       
         LA    R0,OFFTABNM                                                      
*                                                                               
SUBL04   OC    OFFREF,OFFREF       IS THIS OFFTAB ENTRY DEFINED                 
         BZ    SUBL10              NO, GET NEXT                                 
*                                                                               
         LA    R6,OPENDEBS+24                                                   
         CLI   OFFSTAT,C'B'        DOING BILLING?                               
         BNE   *+8                 NO                                           
         LA    R6,OPENCRDS+24                                                   
*                                                                               
         AP    0(6,R6),OFFAMNT     UPDATE TOTAL BUCKET                          
         SH    R6,=Y(BUCKLN)       BACK UP TO MONTHLY BUCKETS                   
*                                                                               
         LA    R3,3                COLUMN NUMBER - 1                            
         LA    R5,MNTH1            TABLE OF AGEING HEADER MONTH'S               
*                                                                               
SUBL06   CLC   OFFMNTH,0(R5)       DATE OF THIS BUCKET VS. MOA OF TRAN          
         BNH   SUBL08              TRAN IS LOWER, USE THIS BUCKET               
         LA    R5,3(R5)            TRY A LATER DATE                             
         SH    R6,=Y(BUCKLN)       PREVIOUS BUCKET                              
         BCT   R3,SUBL06                                                        
*                                                                               
SUBL08   AP    0(6,R6),OFFAMNT                                                  
*                                                                               
SUBL10   LA    R2,OFFTABLN(R2)                                                  
         BCT   R0,SUBL04                                                        
*                                                                               
SUBLX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
ACCLST   CLI   MODE,ACCLAST        JOB END                                      
         BNE   LEVBLST                                                          
*                                                                               
         CLI   FCRDTRNS,C'N'       DID I READ TRANS FOR THIS JOB                
         BE    ACCLX               NOPE, DONT TRY TO GET A TOTAL                
         MVI   SWAPSTAT,C'N'                                                    
         GOTO1 PROLLER,DMCB,1,VBLOCKAC,1 GET LINE 1 OF TABLE                    
         L     R3,DMCB             LOAD A(PROLLER JOB BUCKET)                   
         ST    R3,APROLBCK         SAVE ADDRESS                                 
         CLI   QOPT2,C'T'          TRIAL BAL RUN                                
         BE    ACCL32              YES, SKIP THE AGEING                         
*                                                                               
         CLI   PROFILES+11,C'Y'    INCLUDE ZERO BAL ANCE JOBS?                  
         BNE   *+12                NO, CHECK BALANCE                            
*                                                                               
         CLI   AGEMETH,C'U'        UNBILLED ITEM AGEING                         
         BE    ACCL02              YES, ONLY CHECK FOR OPEN DEBITS              
*                                                                               
         MVC   0(30,R3),DEBS       MOVE DEBITS INTO PROLLER AREA                
         LA    R5,CRDS             CREDITS BUCKET                               
         ZAP   JOBBAL,24(6,R3)     TOTAL DEBS                                   
         AP    JOBBAL,24(6,R5)     TOTAL CRDS FOR JOB BALANCE                   
         CP    JOBBAL,=P'0'        ZERO BAL JOB                                 
         BE    ACCL44              NO NEED TO AGE                               
*                                                                               
         CLI   AGEMETH,C'F'        FIFO AGEING?                                 
         BE    ACCL06              YES                                          
         CLI   AGEMETH,C'O'        OPEN ITEM AGEING                             
         BE    ACCL24              YES                                          
         CLI   AGEMETH,C'A'        ACTIVITY DATE AGEING                         
         BE    ACCL04              YES                                          
         CLI   AGEMETH,C'U'        UNBILLED ITEM AGEING                         
         BNE   ACCL28              NO, USE DEFAULT                              
*                                                                               
ACCL02   MVC   0(30,R3),OPENDEBS   POST OPEN DEBITS                             
         CP    24(6,R3),=P'0'      ANY BAL                                      
         BE    ACCL44              NO, CLEAR PROLLER TAB                        
         B     ACCL32              FORGET ABOUT BILLING, JUST REPORT            
*                                                                               
*                                  MOA AGEING METHOD                            
*                                                                               
ACCL04   BAS   RE,ADDR3            ADD R5 TO R3 (CREDITS TO DEBITS)             
         B     ACCL32              POST TO PROLLER TABLES, PRINT                
*                                                                               
ACCL06   EQU   *                   FIFO AGEING, APPLY CREDITS TO OLDEST         
*                                  DEBITS (TO A MINIUM OF ZERO), EXTRA          
*                                  CREDITS GET APPLIED TO NEXT MONTH            
*                                                                               
         LR    R2,R3               SAVE STARTING ADDRESS OF DEBITS              
         LA    R3,24(R3)           CUMULATIVE BUCKET                            
         LR    R4,R5               SAVE STARTING ADDRESS OF CREDITS             
         LA    R5,24(R5)           CUMULATIVE CREDITS                           
         AP    0(6,R3),0(6,R5)     CUMULATIVE BALANCE                           
*                                                                               
ACCL08   CR    R5,R4               AM I AT THE START OF CREDITS                 
         BE    ACCL20              YES, I'M FINISHED                            
         SH    R5,=Y(BUCKLN)       NEWER CREDITS                                
         CR    R3,R2               ANY DR'S LEFT                                
         BE    ACCL10              NO, ADD CR'S TO THIS BUCKET                  
         SH    R3,=Y(BUCKLN)       NEWER DEBITS                                 
*                                                                               
ACCL10   EQU   *                                                                
         AP    0(6,R3),0(6,R5)     ADD IN THE CREDITS                           
         ZAP   0(6,R5),=P'0'                                                    
         CP    0(6,R3),=P'0'       HOW DOES THE BALANCE LOOK?                   
         BZ    ACCL08              ZERO!, DO THE NEXT ONE.                      
         BM    ACCL14              TO MUCH BILLING, ZAP AND CARRY.              
*                                                                               
ACCL12   EQU   *                   POSITIVE BAL IN BUCKET                       
         CR    R4,R5                                                            
         BE    ACCL20                                                           
         SH    R5,=Y(BUCKLN)       NOT ENOUGH BILLING,BORROW NEXT MNTHS         
         AP    0(6,R3),0(6,R5)                                                  
         ZAP   0(6,R5),=P'0'                                                    
         CP    0(6,R3),=P'0'       HOW DOES THE BALANCE LOOK?                   
         BP    ACCL12              STILL NOT ENOUGH BILLING                     
         BZ    ACCL08              ZERO                                         
*        LA    R5,6(R5)            TOO MUCH NOW, PUT REMAINDER IN SAME          
*                                  BUCKET YOU TOOK IT FROM  (FALL THRU)         
*                                                                               
ACCL14   EQU   *                   TO MUCH BILLING, PUT REMAINDER BACK          
         AP    0(6,R5),0(6,R3)                                                  
         ZAP   0(6,R3),=P'0'                                                    
         GOTO1 AGETST,DMCB,(R3),(R2) ANY DEBITS LEFT TO OFFSET                  
         BE    ACCL16              NO, POST REM'NING CRS                        
         CR    R3,R2               MOST CURRENT DR BUCKET                       
         BE    ACCL16              YES, POST                                    
         SH    R3,=Y(BUCKLN)       CHECK OUT NEXT MONTH                         
         B     ACCL10                                                           
*                                                                               
*                                   SUM UP CR'S AND POST IN MOA OF THE          
*                                   MOST CURRENT CREDIT                         
ACCL16   ZAP   DUB(6),=P'0'                                                     
         XC    DUB+6(2),DUB+6       CLEAR AREA TO STORE NON-ZERO BUCK #         
         LR    R3,R4                A(FIRST CR BUCKET)                          
         LA    R3,18(R3)            A(LAST CR BUCKET)                           
         LA    R1,4                 NUMBER OF BUCKETS TO ADD                    
*                                                                               
ACCL18   AP    DUB(6),0(6,R3)                                                   
         CP    0(6,R3),=P'0'        IS THIS A ZERO BUCKET                       
         BE    *+8                  NO                                          
         STC   R1,DUB+6             STICK BUCKET NUMBER                         
         SH    R3,=Y(BUCKLN)                                                    
         BCT   R1,ACCL18                                                        
*                                                                               
         ZIC   R7,DUB+6             NON-ZERO BUCK NUM                           
         BCTR  R7,R0                DECRIMENT TO BUILD OFFSET                   
         MH    R7,=Y(BUCKLN)        MAKE OFFSET FROM BUCKET NUMBER              
         LR    R3,R2                START OF DR'S'                              
         AR    R3,R7                PLUS OFFSET OF MOST CURRENT NON-0           
*                                   CR BUCKET                                   
         ZAP   0(6,R3),DUB(6)       IS WHERE I PUT THE SUM OF CR'S              
         B     ACCL22                                                           
*                                                                               
ACCL20   EQU   *                                                                
         GOTO1 AGETST,DMCB,(R3),(R2) ANY DEBITS LEFT TO OFFSET                  
         BE    ACCL22              NO I'M DONE                                  
         CP    0(6,R3),=P'0'                                                    
         BNM   ACCL22              BUCKET IS POSITIVE, SO I'M DONE              
         CR    R3,R2               AM I AT THE LAST DR BUCKET                   
         BE    ACCL22              THEN I'M DONE                                
         LA    R5,CRDS+18          PUT REM'DER IN OLD CRDS                      
         ZAP   0(6,R5),0(6,R3)                                                  
         ZAP   0(6,R3),=P'0'                                                    
         SH    R3,=Y(BUCKLN)                                                    
         B     ACCL10              START ALL OVER AGAIN                         
*                                                                               
ACCL22   EQU   *                   FINISH UP FIFO                               
         CLI   SWAPSTAT,C'Y'       DID I SWAP THESE BUCKETS                     
         BNE   ACCL32              NO, PRINT AND POST                           
         LR    R1,R2               SAVED ADDRESS OF BUCKETS                     
         BAS   RE,NEGATE           SWAP THEM BACK THEN                          
         B     ACCL32                                                           
*                                                                               
*                                  OPEN ITEM AGEING                             
ACCL24   EQU   *                                                                
         MVC   0(30,R3),OPENDEBS   AGE OPEN DEBITS                              
*                                                                               
         LA    R5,CRDS                                                          
         LA    R4,5                                                             
         ZAP   0(6,R5),=P'0'                                                    
         LA    R5,6(R5)                                                         
         BCT   R4,*-10                                                          
         LA    R5,CRDS             SET R5 TO ZERO BUCKS                         
*                                                                               
*---------------------------------------------------------------------+         
*        NOTE: APPLTOTS ROUTINE ATTEMPTS TO OFFSET OPEN BILLS WITH    |         
*              TOTAL BILLS WHICH OCCURED AFTER.                                 
*---------------------------------------------------------------------+         
         BAS   RE,APPLTOTS         APPLY TOTAL BILLS TO OPEN          |         
*---------------------------------------------------------------------+         
*                                                                               
         CP    JOBBAL,OPENDEBS+24(6)  JOBBAL SAME AS SUM OF OPEN DR'S           
         BE    ACCL04              YES, POST BY MOA                             
*                                                                               
         MVC   0(30,R5),OPENCRDS   BUCK OF BILLS WHICH DIDN'T MARK              
*                                                                               
*                                                                               
         CP    JOBBAL,OPENCRDS+24(6)  JOBBAL SAME AS SUM OF OPEN CR'S           
         BE    ACCL04                 YES, POST BY MOA                          
*                                                                               
         ZAP   DUB,24(6,R3)        DR TOTAL (OPEN ITEMS)                        
         AP    DUB,24(6,R5)        PLUS CR TOTAL, IS OPEN BAL                   
         SP    DUB,JOBBAL          GET DIFFERENCE                               
         SP    24(6,R5),DUB        ADJUST CR BAL                                
         SP    18(6,R5),DUB        ADJUST CR BUCKET (FIRST MONTH)               
*                                                                               
         BAS   RE,CHKSIGNS         IF EVERYTHING IS THE SAME SIGN...            
         BE    ACCL04                 ... POST BY MOA                           
*                                                                               
*---------------------------------------------------------------------+         
*        HANDLE SPECIAL CASE OF: (DR'S < CR'S) AND (ABS(DR'S) > CR)             
*---------------------------------------------------------------------+         
*                                                                               
         CP    24(6,R3),24(6,R5)   ARE DR'S LESS THAN CR'S                      
         BNL   ACCL26              NO                                           
*                                                                               
         ZAP   DUB,24(6,R3)        GET ABS(DR'S) INTO DUB                       
         BP    *+10                                                             
         MP    DUB,=P'-1'                                                       
         CP    DUB,24(6,R5)        ARE ABS(DR) > CR'S                           
         BH    ACCL30              YES, NEGATE BUCKETS                          
*                                                                               
ACCL26   B     ACCL06              AGE FIFO,                                    
*                                                                               
*                                                                               
ACCL28   EQU   *                   OLD METHOD                                   
         MVI   SWAPSTAT,C'N'                                                    
         CP    JOBBAL,=P'0'      IS THE BALANCE POSITIVE                        
         BNM   ACCL06              JUST AGE FIFO                                
         XC    0(30,R3),0(R5)      SWAP DEBITS AND CREDITS                      
         XC    0(30,R5),0(R3)                                                   
         XC    0(30,R3),0(R5)                                                   
*                                                                               
ACCL30   LR    R1,R3                                                            
         BAS   RE,NEGATE           SWAP SIGNS OF BUCKETS SO CR'S                
         LR    R1,R5               LOOK LIKE DR'S                               
         BAS   RE,NEGATE                                                        
         MVI   SWAPSTAT,C'Y'                                                    
         B     ACCL06              JUST AGE FIFO                                
*                                                                               
ACCL32   L     R3,APROLBCK         RELOAD R3                                    
         LA    R4,4                                                             
*                                                                               
ACCL34   CP    0(6,R3),=P'0'                                                    
         BNE   ACCL36                                                           
         LA    R3,6(R3)                                                         
         BCT   R4,ACCL34                                                        
         B     ACCLX              ALL ZERO BUCKETS                              
*                                                                               
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -            
*        KEEP JOBS WITH DEBIT/CREDIT BALANCE BUCKETS                            
*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -            
ACCL36   L     R5,APROLBCK                                                      
*                                                                               
         LA    R3,CLIDEBS                                                       
         LA    R6,SUBTOTS                                                       
         CP    24(6,R5),=P'0'      WHAT KIND OF BALANCE                         
         BH    *+12                POSITIVE, BRANCH                             
         LA    R3,CLICRDS                                                       
         LA    R6,6(R6)                                                         
*                                                                               
         CLI   QOPT4,C' '          DEBIT OR CREDIT BAL'S ONLY                   
         BNE   ACCL38              YES, FILTER JOBS ON TYPE OF BALANCE          
*                                                                               
         BAS   RE,ADDR3            BUMP DR OR CR TOTAL                          
         AP    0(6,R6),=P'1'       BUMP COUNT                                   
         B     ACCL42              PRINT BUCKETS, ETC                           
*                                                                               
*                                                                               
ACCL38   CP    24(6,R5),=P'0'      WHAT KIND OF BALANCE                         
         BH    ACCL40              POSITIVE, BRANCH                             
         CLI   QOPT4,C'C'          DO WE WANT NEGATIVE BAL'S                    
         BNE   ACCL44              NO, LEAVE                                    
         B     ACCL42              YES, CONTINUE                                
*                                                                               
ACCL40   CLI   QOPT4,C'D'          DO WE WANT POSITIVE BAL'S                    
         BNE   ACCL44              NO, LEAVE                                    
         B     ACCL42              YES, CONTINUE                                
*                                                                               
ACCL42   BAS   RE,JOBEND                                                        
         BAS   RE,GETNUMS                                                       
         BAS   RE,ADDLINES                                                      
         L     R3,APROLBCK                                                      
         GOTO1 CLISUM,DMCB,(R3)      BUILD CLIENT SUMMARY TABLE                 
*                                                                               
         TM    XJOB,XJ             IS THIS AN XJOB                              
         BNO   ACCL44              NO                                           
         L     R5,APROLBCK         ACCUMULATE XJOB TOTALS                       
         LA    R3,PROXJOB                                                       
         BAS   RE,ADDR3                                                         
         L     R5,APROLBCK                                                      
         LA    R3,CLIXJOB                                                       
         BAS   RE,ADDR3                                                         
         L     R5,APROLBCK                                                      
         LA    R3,REQXJOB                                                       
         BAS   RE,ADDR3                                                         
         OI    XJOB,XJINPRO+XJINCLI+XJINREQ                                     
*                                                                               
ACCL44   GOTO1 PROLLER,DMCB,2,VBLOCKAC,1 CLEAR JOB ACCUMULATOR                  
*                                                                               
ACCLX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
LEVBLST  CLI   MODE,LEVBLAST       PRODUCT END                                  
         BNE   LEVALST                                                          
         L     R6,=A(PATTERN)                                                   
         USING PATTERD,R6                                                       
         MVC   P+1(43),SPACES                                                   
         MVC   PSECOND+1(43),SPACES                                             
*                                                                               
         LA    R2,1                                                             
         GOTO1 =A(MEDIASUM)                                                     
*                                                                               
LEVBLX   B     XIT                                                              
*                                                                               
**********************************************************************          
*                                                                               
LEVALST  CLI   MODE,LEVALAST       CLIENT END                                   
         BNE   LEDGLST                                                          
*                                                                               
         L     R6,=A(PATTERN)                                                   
         USING PATTERD,R6                                                       
         CLI   QOPT4,C' '          IS THIS A DEBIT OR CREDIT ONLY RUN           
         BNE   LEVAL08             YES, DONT WRITE THESE TOTALS                 
*                                                                               
         CLI   QOPT1,C'S'          SUMMARY RUN?                                 
         BE    LEVAL08             YES, DONT WRITE THESE TOTALS                 
*                                                                               
         LA    R4,2                                                             
         LA    R5,SUBTOTS                                                       
*                                                                               
LEVAL02  CP    0(6,R5),=P'0'       CHECK CLIENT SUBTOTALS                       
         BNE   LEVAL04             YES                                          
         LA    R5,6(R5)                                                         
         BCT   R4,LEVAL02                                                       
         B     LEVAL08             NO, SKIP THESE TOTALS                        
*                                                                               
LEVAL04  MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         MVC   P+4(24),JOBWDEB                                                  
         LA    R2,CLIDEBS          BUCKET TO PRINT                              
         LA    R3,SUBTOTS          CLIENT DEBITS ARE IN 0(SUBTOTS)              
         GOTO1 SUBPRT,DMCB,(R2),(R3)                                            
         BAS   RE,PRINTEM                                                       
         LA    R4,5                ADD TO REQUEST TOTALS                        
         LA    R5,REQDEBS          AND CLEAR CLIENT TOTALS                      
         BAS   RE,LEVAL06                                                       
*                                                                               
         MVC   P+4(25),JOBWCRD                                                  
         MVI   SPACING,2                                                        
         LA    R2,CLICRDS                                                       
         LA    R3,6(R3)            CLIENT CREDITS ARE IN 6(SUBTOTS)             
         GOTO1 SUBPRT,DMCB,(R2),(R3)                                            
         BAS   RE,PRINTEM                                                       
         LA    R4,5                                                             
         LA    R5,REQCRDS                                                       
         LA    RE,LEVAL08                                                       
*                                                                               
LEVAL06  AP    0(6,R5),0(6,R2)     ADD CLIENT TOTALS TO REQUEST TOTALS          
         ZAP   0(6,R2),=P'0'                                                    
         LA    R2,6(R2)            CALLED FROM DEBIT AND CREDIT                 
         LA    R5,6(R5)                                                         
         BCT   R4,LEVAL06                                                       
*                                                                               
         AP    12(6,R3),0(6,R3)                                                 
         ZAP   0(6,R3),=P'0'                                                    
         BR    RE                  FOR FIRST CALL                               
*                                                                               
LEVAL08  LA    R2,2                                                             
         GOTO1 =A(MEDIASUM)                                                     
*                                                                               
         CLI   QOPT2,C'T'                                                       
         BNE   LEVALX                                                           
         CLI   PROFILES+1,C'Y'                                                  
         BNE   LEVALX                                                           
*                                                                               
         SR    R3,R3               PRINT TRANSACTION TYPE TABLE                 
         GOTO1 =A(PRTTYPE),DMCB,(RA),(RC),(R3)                                  
*                                                                               
LEVALX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
LEDGLST  CLI   MODE,LEDGLAST       HANDLE MEDIA BY UNIT SUMMARIES               
         BNE   REQLST                                                           
*                                                                               
         CLI   PROFILES+3,C'N'     IF THEY ARE WANTED                           
         BE    LEDGLX                                                           
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1          3                                            
         LA    R2,1                FIRST MEDIUM (FOR UNITSUM)                   
         LA    R3,MEDIANAM         START OF MEDIA LISTS                         
         LA    R4,MEDIALST                                                      
         LA    R5,37               MAX NUMBER IN MEDIA LIST                     
*                                                                               
LEDGL02  MVC   P+1(5),=C'MEDIA'                                                 
         MVC   P+7(1),0(R4)        PRINT THE MEDIA CODE                         
         MVC   P+9(15),0(R3)       PRINT THE MEDIA NAME                         
         BAS   RE,UNITSUM          GET UNIT TOTALS FOR THIS MEDIA               
         LA    R2,1(R2)            NEXT MEDIA FOR UNITSUM                       
         LA    R3,15(R3)           NEXT NAME                                    
         LA    R4,1(R4)            NEXT CODE                                    
         BCT   R5,LEDGL02          LOOP UP TILL END OF MEDIA LIST               
*                                                                               
LEDGLX   B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
REQLST   CLI   MODE,REQLAST                                                     
         BNE   RUNLST                                                           
*                                                                               
         L     R6,=A(PATTERN)                                                   
         USING PATTERD,R6                                                       
         MVC   P,SPACES            AN EXTRA 'OFFICE' FROM ABOVE                 
         XC    PRTSTA,PRTSTA       COMMUNICATE WITH PRINT ROUTINE               
*                                                                               
         MVI   FORCEHED,C'Y'       PRODUCE THE CLIENT SUMMARY                   
         MVI   RCSUBPRG,3                                                       
         XC    SAVEUNIT,SAVEUNIT   PRIME OFFICE TOTAL PRINTING                  
*                                                                               
         LA    R4,OPENDEBS         BUILD OFFICE TOTALS ON OPENDEBS              
         LA    R5,10               ZAP OPENDEBS AND CRDS                        
         ZAP   0(6,R4),=P'0'       ZAP BUCKET TO BUILD OFFICE TOTALS            
         LA    R4,6(R4)                                                         
         BCT   R5,*-10                                                          
*                                                                               
         USING CLIKEYD,R7                                                       
         LA    R7,BUFKEY                                                        
         MVC   BUFKEY,SPACES                                                    
         MVI   BUFKEY,X'FF'                                                     
         BAS   RE,BUFFHIGH                                                      
REQL02   TM    DMCB+8,X'80'                                                     
         BZ    REQL04                                                           
         OI    PRTSTA,LASTCLI      PRINT LAST CLIENT                            
         MVI   CLIOFF,X'EE'        FORCE OFFICE TOTALS                          
         B     REQL10                                                           
*                                                                               
REQL04   OI    PRTSTA,CLIENT                                                    
         CLI   PROFILES+7,C'Y'     DO THEY WANT THIS REPORT BY OFFICE           
         BNE   REQL06              NO, CONTINUE                                 
         CLC   SAVEUNIT,CLIOFF     NEW OFFICE THIS CLIENT                       
         BE    REQL06              NO, CONTINUE                                 
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(6),=C'OFFICE'   PRINT NEW OFFICE                             
         MVC   P+8(L'CLIOFF),CLIOFF                                             
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         LA    R3,OPENDEBS                                                      
         LA    R5,5                                                             
         ZAP   0(6,R3),=P'0'       ZAP BUCKET TO BUILD OFFICE TOTALS            
         LA    R3,6(R3)                                                         
         BCT   R5,*-10                                                          
*                                                                               
REQL06   MVC   P+1(3),CLICODE      PRINT CLIENT CODE                            
         MVC   P+5(36),BUFCOM      PRINT CLIENT NAME                            
         MVC   SAVEUNIT,CLIOFF                                                  
*                                                                               
         LA    R1,5                NUMBER OF BUCKETS                            
         LA    R3,DEBS                                                          
         LA    R2,BUFAMNT1                                                      
*                                                                               
REQL08   ZAP   0(6,R3),0(8,R2)     BUILD A BUCKET                               
         AP    OPENDEBS-DEBS(6,R3),0(8,R2) FOR OFFICE SUMMARY                   
         AP    CRDS-DEBS(6,R3),0(8,R2)     FOR REQUEST SUMMARY                  
*                                                                               
         LA    R2,8(R2)                                                         
         LA    R3,6(R3)                                                         
         BCT   R1,REQL08                                                        
*                                                                               
         BAS   RE,BUFFSEQ                                                       
         CLI   DMCB+8,X'80'                                                     
         BO    REQL02                                                           
*                                                                               
         MVI   SPACING,2                                                        
         LA    R3,DEBS                                                          
         GOTO1 SUBPRT,DMCB,(R3)                                                 
         BAS   RE,PRINTEM                                                       
*                                                                               
REQL10   TM    PRTSTA,CLIENT       DID I PRODUCE THE CLIENT SUMMARY             
         BZ    REQL14              NO,                                          
         CLI   PROFILES+7,C'Y'     DO THEY WANT THIS REPORT BY OFFICE           
         BNE   REQL12              NO, PROCESS NEXT RECORD                      
         TM    PRTSTA,LASTCLI      DID I COME HERE JUST TO PRINT A TOT          
         BO    *+14                THEN PRINT TOTAL REGUARDLESS                 
         CLC   SAVEUNIT,CLIOFF     NEW OFFICE THIS CLIENT                       
         BE    REQL02              NO, CONTINUE                                 
         MVC   P+1(16),=C'TOTAL FOR OFFICE'                                     
         MVC   P+18(2),SAVEUNIT                                                 
         MVI   SPACING,3                                                        
         LA    R3,OPENDEBS                                                      
         GOTO1 SUBPRT,DMCB,(R3)                                                 
         BAS   RE,PRINTEM                                                       
*                                                                               
REQL12   TM    PRTSTA,LASTCLI                                                   
         BZ    REQL02                                                           
*                                                                               
         MVC   P+10(9),=C'**TOTAL**'                                            
         LA    R3,CRDS                                                          
         GOTO1 SUBPRT,DMCB,(R3)                                                 
         BAS   RE,PRINTEM                                                       
*                                                                               
REQL14   MVI   FORCEHED,C'Y'       HANDLE UNIT BY MEDIA SUMMARIES               
         MVI   RCSUBPRG,2                                                       
         NI    PRTSTA,X'FF'-CLIENT TURN OFF CLIENT PRINT                        
         LA    R2,3                NEED RUN TOTALS FROM MEDIASUM                
         L     R3,=A(UNITLIST)     LIST OF OFFICES                              
         LA    R5,255              MAX NUMBER OF OFFICES                        
*                                                                               
REQL16   DS    0H                                                               
*&&UK                                                                           
         MVC   P+1(4),=C'UNIT'                                                  
         MVC   P+6(2),0(R3)                                                     
*&&                                                                             
*&&US                                                                           
         MVC   P+1(6),=C'OFFICE'                                                
         MVC   P+8(2),0(R3)                                                     
*&&                                                                             
         CLI   PROFILES+4,C'N'     DO WE WANT THIS SUMMARY                      
         BE    REQL18                                                           
         GOTO1 =A(MEDIASUM)                                                     
*                                                                               
REQL18   LA    R2,1(R2)            NEXT OFFICE - FOR MEDIASUM                   
         LA    R3,2(R3)            NEXT OFFICE IN UNITLIST(FOR PRINT)           
         BCT   R5,REQL16                                                        
*                                                                               
         MVC   P+1(L'P-1),SPACES                                                
         MVC   P+1(22),TWIP       TOTAL WORK IN PROGRESS                        
         BAS   RE,PRINTEM                                                       
         GOTO1 =A(MEDIASUM)                                                     
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         CLI   QOPT4,C' '          IS THIS A DEBIT OR CREDIT ONLY RUN           
         BNE   REQL24              YES, DONT WRITE THESE TOTALS                 
*                                                                               
         LA    R4,2                DID ANYTHING HAPPEN THIS RUN                 
         LA    R5,SUBTOTS+12                                                    
REQL20   CP    0(6,R5),=P'0'       CHECK JOB SUBTOTALS                          
         BNE   REQL22              YES                                          
         LA    R5,6(R5)                                                         
         BCT   R4,REQL20                                                        
         B     REQL24              NO, SKIP THESE TOTALS                        
*                                                                               
REQL22   MVC   P+10(24),JOBWDEB                                                 
         LA    R4,REQDEBS                                                       
         LA    R5,SUBTOTS+12       REQUEST DEBITS                               
         GOTO1 SUBPRT,DMCB,(R4),(R5)                                            
         BAS   RE,PRINTEM                                                       
         MVI   SPACING,2                                                        
         MVC   P+10(25),JOBWCRD                                                 
         LA    R4,REQCRDS                                                       
         LA    R5,SUBTOTS+18       REQUEST CREDITS                              
         GOTO1 SUBPRT,DMCB,(R4),(R5)                                            
         BAS   RE,PRINTEM                                                       
*                                                                               
REQL24   MVC   BUFKEY,SPACES                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,4          SUMMARY TYPE HEADERS                         
         ZAP   DOUBLE,=P'0'                                                     
         ZAP   ULTOT,=P'0'                                                      
         BAS   RE,BUFFHIGH                                                      
         MVC   ULSAVE(2),BUFKEY    PRIME U/L FOR TOTALS                         
*                                                                               
REQL26   TM    DMCB+8,X'80'                                                     
         BO    REQL32                                                           
         CLI   BUFKEY,X'FF'        CLIENT TOTAL RECORD TYPE                     
         BE    REQL32              YES, I'M DONE                                
*                                                                               
         MVC   HEAD1+44(24),=C'PRODUCTION TRIAL BALANCE'                        
         MVC   HEAD2+44(24),=24C'-'                                             
         MVC   HEAD5+73(25),=C'UNBILLED INTERNAL CHARGES'                       
*&&US*&& MVI   HEAD10+1,X'00'                                                   
*                                                                               
         CLC   ULSAVE(2),BUFKEY    NEW CLIENT                                   
         BE    REQL30              NO                                           
         CLI   ULFLAG,C'Y'         WRITE A TOTAL LINE?                          
         BNE   REQL28              NO, NEED                                     
         MVC   P+1(9),=C'TOTAL FOR'                                             
         MVC   P+11(2),ULSAVE                                                   
         EDIT  (P8,ULTOT),(10,P+46),2,MINUS=YES                                 
         BAS   RE,MYREPORT                                                      
*                                                                               
REQL28   BAS   RE,MYREPORT         DOUBLE SPACE                                 
         ZAP   ULTOT,=P'0'                                                      
         MVI   ULFLAG,C'N'                                                      
*                                                                               
REQL30   MVC   ULSAVE(2),BUFKEY                                                 
         MVC   P+1(49),BUFREC                                                   
         GOTO1 ADSQUASH,DMCB,P+1,48                                             
         EDIT  BUFAMNT1,(10,P+46),2,MINUS=YES                                   
         AP    DOUBLE,BUFAMNT1                                                  
         CP    ULTOT,=P'0'                                                      
         BE    *+8                                                              
         MVI   ULFLAG,C'Y'        WE WILL NEED LEDGER TOTALS NOW                
         AP    ULTOT,BUFAMNT1                                                   
         BAS   RE,MYREPORT                                                      
         BAS   RE,BUFFSEQ                                                       
         B     REQL26                                                           
*                                                                               
REQL32   GOTO1 BUFFALO,DMCB,=C'RESET',ADBUFF                                    
         CP    DOUBLE,=P'0'                                                     
         BE    REQL36                                                           
         CLI   ULFLAG,C'Y'         WRITE A TOTAL LINE?                          
         BNE   REQL34              NO, NEED                                     
         MVC   P+1(9),=C'TOTAL FOR'                                             
         MVC   P+11(2),ULSAVE                                                   
         EDIT  (P8,ULTOT),(10,P+46),2,MINUS=YES                                 
         BAS   RE,MYREPORT                                                      
*                                                                               
REQL34   EDIT  (P8,DOUBLE),(10,P+46),2,MINUS=YES                                
         MVC   P+10(9),=C'**TOTAL**'                                            
*&&US*&& MVI   HEAD10+1,X'00'                                                   
         BAS   RE,MYREPORT                                                      
*                                                                               
REQL36   LA    R3,1                                                             
         GOTO1 =A(PRTTYPE),DMCB,(RA),(RC),(R3)                                  
*                                                                               
REQLX    B     XIT                                                              
         EJECT                                                                  
*                                                                               
**********************************************************************          
*                                                                               
RUNLST   CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         BAS   RE,RELBUFF                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              AGING SUBROUTINES                                      *         
***********************************************************************         
*                                                                               
ADDR3    EQU   *                   ADD R5 TO R3 5 TIMES                         
         LA    R0,5                                                             
ADDR310  AP    0(6,R3),0(6,R5)                                                  
         LA    R5,6(R5)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,ADDR310                                                       
         BR    RE                                                               
*                                                                               
AGETST   NTR1                      SEE IF ANY NON-ZERO BUCKETS ARE LEFT         
         L     R7,DMCB             CHECK DEBITS                                 
         S     R7,DMCB+4           LENGTH OF REMAINING BUCKETS                  
         LTR   R7,R7                NO BUCKETS PASSED                           
         BZ    XIT                 RETURN WITHG EQUAL CC                        
         LA    R1,6                BUCKET LENGTH                                
         XR    R6,R6               CLEAR EVEN REG FOR DR COMMAND                
         DR    R6,R1               R7 HAS NUMBER OF BUCKETS LEFT                
         L     R6,DMCB+4                                                        
AGETST01 CP    0(6,R6),=P'0'       THIS BUCKET ZERO                             
         BNE   XIT                 NO, RETURN WITH NOT EQUAL CC                 
         LA    R6,6(R6)            NEXT BUCKET                                  
         BCT   R7,AGETST01         OUT OF BUCKETS                               
         CR    R7,R7               RETURN WITH EQUAL CC                         
         B     XIT                                                              
*                                                                               
NEGATE   NTR1                      ROUTINE TO NEGATE FIVE 6 BYTE PACKED         
         LA    R5,6                STRARTING AT 0(R1)                           
NEG01    ZAP   DUB(8),0(6,R1)                                                   
         MP    DUB,=P'-1'          NEGATE BUCKET                                
         ZAP   0(6,R1),DUB(8)                                                   
         LA    R1,6(R1)            POINT TO NEXT BUCKET                         
         BCT   R5,NEG01                                                         
         B     XIT                                                              
*                                                                               
*---------------------------------------------------------------------          
*        PUT POSTTRAN INTO THE POSTDATE BUCKET OF THE ACCUMULATORS AT           
*              POSTBUCK                                                         
*---------------------------------------------------------------------          
*                                                                               
AGEIT    NTR1                                                                   
         L     R2,POSTBUCK                                                      
         LA    R2,24(R2)           GO TO END AND POST TOTAL BUCKET              
         AP    0(BUCKLN,R2),POSTTRAN                                            
         SH    R2,=Y(BUCKLN)       POINT TO LAST BUCKET                         
         LA    R3,3                NUMBER OF COLS - 1                           
         LA    R5,MNTH1                                                         
AGEIT20  CLC   POSTDATE,0(R5)                                                   
         BNH   AGEIT30                                                          
*                                                                               
         LA    R5,3(R5)            GET PREVOIUS BUCKET                          
         SH    R2,=Y(BUCKLN)                                                    
         BCT   R3,AGEIT20                                                       
*                                                                               
AGEIT30  AP    0(BUCKLN,R2),POSTTRAN   POST INTO PROPER MONTH                   
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------          
*        PRINT A TRANSACTION IN THE COL YOU AGE IN INTO, FOR DEBUGGING          
*---------------------------------------------------------------------          
*                                                                               
PRINTIT  NTR1                                                                   
         LA    R4,3                NUMBER OF COLS - 1                           
         LA    R5,MNTH1                                                         
         LA    R3,P+84                                                          
PRTIT20  CLC   POSTDATE,0(R5)                                                   
         BNH   PRTIT30                                                          
*                                                                               
         LA    R5,3(R5)            GET PREVOIUS BUCKET                          
         SH    R3,=H'13'           PREV PRINT COL                               
         BCT   R4,PRTIT20                                                       
*                                                                               
PRTIT30  EDIT  POSTTRAN,(12,0(R3)),2,MINUS=YES                                  
*                                                                               
         USING TRANSD,R4                                                        
         L     R4,ADTRANS          RESTORE R4                                   
         MVC   P+5(6),TRNSREF                                                   
*                                                                               
         TM    TRNSSTAT,X'20'                                                   
         BNO   *+8                                                              
         MVI   0(R3),C'*'                                                       
*                                                                               
         BAS   RE,MYREPORT                                                      
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        AGE INTO OPENCRDS ONLY THOSE OPEN BILLS WHICH DO NOT HAVE A            
*        SUBSEQUENT TOTAL BILL WHTHIN THE DATE PARAMETERS OF THE                
*        REQUEST, SAVE TOTAL IN OPENCRDS+24                                     
*----------------------------------------------------------------------         
*                                                                               
APPLTOTS NTR1                                                                   
         LA    R5,OPENCRDS         CLEAR OPEN CREDITS BUCKETS                   
         LA    R4,5                                                             
         ZAP   0(6,R5),=P'0'                                                    
         LA    R5,6(R5)                                                         
         BCT   R4,*-10                                                          
*                                                                               
         L     R2,=A(OCRTAB)       TABLE OF OPEN CREDITS                        
         L     R4,0(R2)            NUMBER OF OPEN CREDITS                       
         LTR   R4,R4               ANY OPEN CREDITS?                            
         BZ    XIT                 NOPE                                         
         LA    R2,8(R2)            POINT TO OPEN CREDIT DATA                    
*                                                                               
         L     R3,=A(TBLTAB)       TABLE OF TOTAL BILLD                         
         L     R5,0(R3)            NUMBER OF TOTAL BILLS                        
         LTR   R5,R5               ANY TOTAL BILLS?                             
         BZ    APPL50              NO, AGE OPEN CREDITS                         
*                                                                               
         LA    R3,8(R3)            POINT TO TOTAL BILLS                         
*                                                                               
         BAS   RE,OFFCR                                                         
*              TOTBL   OPENCR                                                   
*              -----   ------                                                   
APPL20   CLC   0(2,R3),0(R2)       COMPARE DA DATE OF TBL VS OCR                
         BL    APPL30              OF THE OPEN CR IS AFTER THIS TOTAL           
*                                  BILL, LEAVE IT, AND GET NEXT TOTAL           
*                                  BILL                                         
*                                                                               
         ZAP   CRAMNT-CRKEY(BUCKLN,R2),=P'0'  CLEAR THIS OPEN CREDIT            
         LA    R2,CRLEN(R2)                   GET NEXT OPEN CREDIT              
         BCT   R4,APPL20                                                        
*                                                                               
         B     XIT                 NO MORE OPEN CR'S, SO EXIT                   
*                                                                               
APPL30   BCTR  R5,0                WHILE THERE ARE STILL TOTAL BILLS            
         LTR   R5,R5                                                            
         BZ    APPL50                                                           
*                                                                               
         LA    R3,CRLEN(R3)        GET THE NEXT ONE                             
         B     APPL20                                                           
*                                                                               
APPL50   LA    R5,OPENCRDS                                                      
         ST    R5,POSTBUCK                                                      
APPL60   ZAP   POSTTRAN,CRAMNT-CRKEY(BUCKLN,R2) POST REMAINING OPEN CRS         
         MVC   POSTDATE,CRMNTH-CRKEY(R2)                                        
         BAS   RE,AGEIT                                                         
         LA    R2,CRLEN(R2)                                                     
         BCT   R4,APPL60           WHILE THERE ARE STILL OPEN BILLS             
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        CHECK THE SIGNS OF THE 4 BUCKETS AT 0(R3) AND THE 4 AT 0(R5)           
*        IF THEY ALL HAVE THE SAME SIGN, XIT W/CC =                             
*        IF THERE ARE MIXED SIGNS, XIT W CC NOT =                               
*----------------------------------------------------------------------         
*                                                                               
CHKSIGNS NTR1                                                                   
         MVI   BYTE,C' '           INIT SIGN FLAG                               
         LR    R1,R3               CHECK DEBITS                                 
         LA    R0,4                FOR 4 BUCKETS                                
         BAS   RE,CHKS100                                                       
*                                                                               
         LR    R1,R5               CHECK CREDITS                                
         LA    R0,4                FOR 4 BUCKETS                                
         BAS   RE,CHKS100                                                       
         B     XITEQ               OK IF I RETURNED HERE                        
*                                                                               
CHKS100  EQU   *                                                                
         CP    0(6,R1),=P'0'                                                    
         BE    CHKS130             GET NEXT BUCK                                
         BP    CHKS110                                                          
*                                  NEGATIVE BUCKET                              
         CLI   BYTE,C' '           HAS FLAG BEEN SET                            
         BNE   *+12                YES,                                         
         MVI   BYTE,C'N'           NO, SET AS NEGATIVE                          
         B     CHKS130             AND GET NEXT BUCKET                          
*                                                                               
         CLI   BYTE,C'N'           HAS BYTE BEEN SET TO N                       
         BNE   XITNEQ              NO                                           
         B     CHKS130                                                          
*                                                                               
CHKS110  EQU   *                   THIS BUCKET IS POSITIVE                      
         CLI   BYTE,C' '           HAS FLAG BEEN SET                            
         BNE   *+12                YES,                                         
         MVI   BYTE,C'P'           NO, SET IT AS POSITIVE                       
         B     CHKS130             AND GET NEXT BUCKET                          
*                                                                               
         CLI   BYTE,C'P'           HAS BYTE BEEN SET TO (P)OSITIVE              
         BNE   XITNEQ              NO                                           
*                                  YES, GET NEXT                                
CHKS130  LA    R1,6(R1)                                                         
         BCT   R0,CHKS100                                                       
         BR    RE                  SO FAR SO GOOD                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*        SEE IF A TRANS ACTION HAS BEEN PEELED                                  
*        RETURN EQUAL EQ IF IT HAS                                              
*-------------------------------------------------------------------            
ISPEELED NTR1                                                                   
         USING ACKEYD,R5                                                        
*                                  YES, GET NEXT                                
         L     R4,ADTRANS                                                       
         LR    R5,R4                                                            
         SH    R5,DATADISP                                                      
*                                  YES, GET NEXT                                
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   XITEQ                                                            
*                                  YES, GET NEXT                                
         B     XITNEQ                                                           
*                                  YES, GET NEXT                                
         DROP  R5                                                               
*----------------------------------------------------------------------         
*              BUILD A LIST OF MEDIA CODES AND NAMES                            
*----------------------------------------------------------------------         
*                                                                               
GETLIST  NTR1                                                                   
         L     R4,ADCOMP                                                        
         AH    R4,DATADISP         POINT TO COMPANY DATA                        
         LA    R2,MEDIANAM         INIT AREA                                    
         LA    R3,MEDIALST                                                      
         MVC   MEDIALST,SPACES                                                  
         MVC   MEDIANAM+540(15),=CL15'ALL MEDIA'                                
*                                                                               
         LA    R6,36                                                            
         MVI   ELCODE,X'11'                                                     
GL2      BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         USING ACMEDIAD,R4                                                      
         MVC   0(1,R3),ACMDCODE                                                 
         MVC   0(15,R2),ACMDDESC                                                
         LA    R2,15(R2)           BUMP MEDIA NAMES TABLE                       
         LA    R3,1(R3)            BUMP MEDIA CODES TABLE                       
         BCT   R6,GL2                                                           
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*              BUILD A LIST OF OFFICES                                          
*              IF OLD OFFICES, FILL TABLE W/ A-9                                
*              NEW OFFICES, READ OFFICE RECORDS                                 
*----------------------------------------------------------------------         
*                                                                               
GETOFF   NTR1                                                                   
         USING ACCOMPD,R2                                                       
         L     R2,ADCOMP                                                        
         AH    R2,DATADISP                                                      
         TM    ACMPSTA4,X'01'      IS AGY ON NEW OFFICES?                       
         BNO   XIT                 NO, TABLE IS SET                             
*                                                                               
         USING ACKEYD,R7                                                        
         LA    R7,MYKEY                                                         
         L     R3,=A(UNITLIST)                                                  
         XCEF  (R3),L'UNITLIST                                                  
         LA    R3,L'UNITLIST-2(R3)                                              
         MVC   0(2,R3),SPACES      INIT OFFICE SPACE                            
         L     R3,=A(UNITLIST)                                                  
*                                                                               
         XC    ACOGKEY,ACOGKEY                                                  
         MVI   ACOGRTYP,ACOGEQU                                                 
         MVI   ACOGSREC,ACOGOFF    GO FOR                                       
         MVC   ACOGOFC,SPACES                                                   
         MVC   ACOGCUL(1),RCCOMPFL                                              
         MVC   ACOGCUL+1(2),=C'SJ'                                              
         BAS   RE,HIGH                                                          
         LA    R0,255                                                           
*                                                                               
GETO50   L     R2,=A(IOAREA)                                                    
         CLC   0(ACOGCODE-ACOGKEY,R2),ACOGKEY                                   
         BNE   GETOX                                                            
         CLC   ACOGOFC-ACOGKEY(L'ACOGOFC,R2),SPACES                             
         BNH   GETOX                                                            
*                                                                               
         MVC   0(L'ACOGOFC,R3),ACOGOFC-ACOGKEY(R2)                              
         LA    R3,L'ACOGOFC(R3)                                                 
         BCT   R0,*+6                                                           
         DC    H'0'                MORE THAN 255 OFFICES !                      
         BAS   RE,SEQ                                                           
         B     GETO50                                                           
*                                                                               
GETOX    B     XIT                                                              
         DROP  R7                                                               
         EJECT                                                                  
*---------------------------------------------------------------------+         
*              SET UP STATUS FIELD FOR PRINTING                       |         
*              PROCESS ESTIMATE DEPENDENT OPTIONS                     |         
*---------------------------------------------------------------------+         
*                                                                               
ESTSRCH  NTR1                                                                   
         MVC   STATUS,SPACES                                                    
         L     R3,ADPROFIL                                                      
         USING ACPROFD,R3                                                       
*                                                                               
         CLI   PROFILES,C'R'       PRINTING REV NUMBERS W/ ESTS                 
         BNE   EST02               NO                                           
         MVC   STATUS(1),ACPRBILL  BILL TYPE IN COL 1                           
         B     EST04               NO ACSTSTAT, OFFICE PRINTS W/PROD            
*                                                                               
EST02    MVC   STATUS+3(1),ACPRBILL BILL TYPE                                   
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         TM    ACSTSTAT,X'40'                                                   
         BZ    *+8                                                              
         MVI   STATUS+10,C'C'      CLOSED                                       
         TM    ACSTSTAT,X'20'                                                   
         BZ    *+8                                                              
         MVI   STATUS+11,C'L'      LOCKED                                       
         MVC   STATUS(2),THISUNIT  OFFICE                                       
*                                                                               
EST04    ZAP   DOUBLE,=P'0'                                                     
*                                                                               
         BAS   RE,LOOKUP                                                        
         USING JBLOCKD,R5                                                       
         MVI   HALF,C'N'                                                        
         MVC   CUREST,JBCURVER     SAVE CURRENT ESTIMATE NUMBER                 
         MVC   HIREV,JBHIREV                                                    
*                                                                               
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   EST08               NO                                           
         USING MJETABD,R3                                                       
         ZAP   DOUBLE,MJETVAL      YES, GET NET ESTIMATE                        
         ZAP   HIREVEST,MJETVAL+6(6)  AND HIGHEST REVISION                      
*                                                                               
EST06    CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    EST13                                                            
         CLI   MJETTYP,MJETTWQ     NO, LOOK FOR WORKCODE                        
         BNE   EST07                                                            
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BZ    EST12                                                            
                                                                                
EST07    XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    R3,R1                                                            
         B     EST06                                                            
         DROP  R3                                                               
*                                                                               
         USING JBCOLD,R3                                                        
EST08    ZAP   DOUBLE,JBCOLVAL                                                  
         ZAP   HIREVEST,JBCOLVAL+6(6)  SAVE HIGHER REVISION AMOUNT              
         LH    R1,JBNROWS                                                       
*                                                                               
EST10    CLI   JBCOLTYP,JBCOLTWC                                                
         BE    EST12                                                            
         AH    R3,JBLCOL                                                        
         BCT   R1,EST10                                                         
         B     *+8                                                              
         DROP  R3                                                               
*                                                                               
EST12    MVI   HALF,C'Y'           YES, WE HAVE AN ESTIMATE                     
EST13    CP    DOUBLE,=P'0'                                                     
         BE    EST16                                                            
*                                                                               
         MVC   STATUS+6(3),=C'*E*' FLAG AS HAVING AN ESTIMATE                   
         CLI   PROFILES,C'Y'       PRINT VALUE OF ESTIMATE?                     
         BE    EST14               YES                                          
         CLI   PROFILES,C'R'       PRINT VALUE WITH R VALUE                     
         BNE   EST16               NO                                           
*                                                                               
EST14    SRP   DOUBLE,64-2,5       ROUND OUT PENNIES                            
         EDIT  (P8,DOUBLE),(6,STATUS+6)                                         
*                                                                               
EST16    EQU   *                   CHECK ESTIMATE OPTION ON REQCARD             
         CLI   QOPT3,C' '          ANYTHING THERE                               
         BE    XIT                 NO. WE ARE DONE                              
         XC    ESTSTAT,ESTSTAT                                                  
         USING GOBLOCKD,R3                                                      
         L     R3,ADGOBLOC         ADDRESS OPTION DATA                          
         CLI   QOPT3,C'R'          ANYTHING THERE                               
         BE    EST30               ONLY JOBS WITH HIGHER UNAPP REVS             
*                                                                               
         CLI   JBNEWEST,C'Y'       IS JOB ON NEW ESTIMATES                      
         BE    EST17                                                            
         CLI   JBNEWEST,JBMCSQ                                                  
         BNE   EST18               NO                                           
*                                                                               
EST17    OC    JBHIAPP,JBHIAPP     HAS IT BEEN APPROVED?                        
         BNZ   *+8                 YES                                          
         OI    ESTSTAT,ESTUNAPP                                                 
*                                                                               
         CLI   GONEEDAE,C'Y'       THIS JOB NEED APPROV TO BILL                 
         BNE   *+8                NO                                            
         OI    ESTSTAT,NEEDNAPP                                                 
*                                                                               
         CLI   GONEEDES,C'Y'      NEED AN ESTIMATE                              
         BNE   *+8                NO                                            
         OI    ESTSTAT,NEEDNEST                                                 
*                                                                               
         CLI   HALF,C'Y'           HAVE AN ESTIMATE                             
         BNE   EST24               NO, SEE WHAT WE HAVT                         
         OI    ESTSTAT,HAVENEST                                                 
         B     EST24                                                            
*                                                                               
         USING ACJOBD,R4                                                        
EST18    EQU   *                                                                
         OI    ESTSTAT,NEEDNAPP    OLD EST NEED APPROVAL IF UNAPP               
         CLI   HALF,C'Y'           HAVE AN ESTIMATE                             
         BNE   EST20               NO, CANT BE UNAPPROVED                       
         OI    ESTSTAT,HAVENEST                                                 
*                                                                               
EST20    CLI   GONEEDES,C'Y'       DOES THIS JOB NEED AN ESTIMATE?              
         BNE   EST22                                                            
         OI    ESTSTAT,NEEDNEST                                                 
*                                                                               
EST22    L     R4,ADACC            JOB USES OLD ESTIMATE                        
         MVI   ELCODE,X'26'        GET JOB ELEMENT                              
         BAS   RE,GETEL            IS IT UPAPPROVED                             
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ACJBLEN,ACJBLNQ3    IS EL LONG ENUF TO HAVE UPAPP BIT            
         BL    EST24               NO, ITS APPROVED                             
         TM    ACJBSTAT,X'80'      DOES JOB HAVE AN UNAPPROVED EST              
         BNO   EST24               NO                                           
         OI    ESTSTAT,ESTUNAPP                                                 
*                                                                               
EST24    LA    R4,ESTTAB                                                        
*                                                                               
EST26    CLC   0(1,R4),ESTSTAT     PATTERN MATCH ON TABLE                       
         BNE   EST28                                                            
         CLC   1(1,R4),QOPT3       IS THIS THE PATTERN THEY WANT                
         BE    XIT                 YES                                          
         B     ESTX                NO                                           
EST28    LA    R4,L'ESTTAB(R4)                                                  
         B     EST26                                                            
*                                                                               
EST30    CLI   JBNEWEST,C'Y'                                                    
         BE    EST31                                                            
         CLI   JBNEWEST,JBMCSQ                                                  
         BNE   ESTX                                                             
*                                                                               
EST31    CLI   GONEEDAE,C'Y'                                                    
         BNE   ESTX                                                             
         CLI   JBHIAPP,0                                                        
         BE    ESTX                                                             
         CLC   JBHIAPP,JBHIREV                                                  
         BE    ESTX                JOB NOT WANTED                               
         B     XIT                                                              
*                                                                               
ESTX     MVI   FCRDTRNS,C'N'       THIS JOB NOT WANTED                          
         B     XIT                                                              
*                                                                               
ESTSTAT  DS    CL1                                                              
ESTTAB   DS    0CL2                                                             
*        NEED APPROVAL AND OLD EST JOBS                                         
         DC    B'00001111',C'U'    EST PRES, NEEDANEST,EST UNAPP                
         DC    B'00001110',C'A'                        EST APP                  
         DC    B'00001101',C'A'              DONT NEED,EST UNAPP                
         DC    B'00001100',C'A'                        EST APP                  
         DC    B'00001011',C'N'    NO ESTIMAT NEED     EST UNAPP                
         DC    B'00001010',C'N'                        EST APP                  
         DC    B'00001001',C'A'               DONT NEEDEST UNAPP                
         DC    B'00001000',C'A'                        EST APP                  
*        NEW EST JOBS WHICH DON'T NEED AN APPROVAL                              
         DC    B'00000111',C'A'    EST PRES, NEEDANEST,EST UNAPP                
         DC    B'00000110',C'A'                        EST APP                  
         DC    B'00000101',C'A'              DONT NEED,EST UNAPP                
         DC    B'00000100',C'A'                        EST APP                  
         DC    B'00000011',C'N'    NO ESTIMAT NEED     EST UNAPP                
         DC    B'00000010',C'N'                        EST APP                  
         DC    B'00000001',C'A'               DONT NEEDEST UNAPP                
         DC    B'00000000',C'A'                        EST APP                  
ESTTNUM  EQU   ((*-ESTTAB)/L'ESTTAB)                                            
NEEDNAPP EQU   X'08'                                                            
HAVENEST EQU   X'04'                                                            
NEEDNEST EQU   X'02'                                                            
ESTUNAPP EQU   X'01'                                                            
         DROP R4                                                                
         EJECT                                                                  
*        GET PROFILE FOR THIS REQUEST                                           
*                                                                               
PROFS    NTR1                                                                   
         L     R2,APROFILE           A(PROFILES AT ALL LEVELS)                  
         USING ACPROFSD,R2                                                      
         TM    REQSTAT,OFFICE        OFFICE SPECIFIC REQUEST                    
         BNO   PRO01                 NO                                         
         MVC   PROFILES(16),ACPPFOF1 YES, GET OFFICE LEVEL PROFILE              
         B     *+10                                                             
PRO01    MVC   PROFILES(16),ACPPFLD1 USE LEGDER LEVEL PROFILE                   
         DROP  R2                                                               
*                                                                               
         MVI   AGEMETH,C'N'        MAKE THE DEFAULT THE DEFAULT                 
         CLI   PROFILES+6,X'40'                                                 
         BNH   PRO03                                                            
         MVC   AGEMETH(1),PROFILES+6 AGEING METHOD FROM PROFILE                 
PRO03    CLI   QOPT7,C' '          OVERIDE AGEING PROFILE WITH REQUEST?         
         BE    *+10                NO                                           
         MVC   AGEMETH(1),QOPT7    YES GET METHOD FROM REQUEST                  
         CLI   QOPT2,C'T'                                                       
         BNE   PRO04                                                            
         MVI   PROFILES+8,C' '     NO USING TRANSACTION DATE ON TRIAL           
         B     XIT                                                              
*                                                                               
PRO04    CLI   QOPT6,C' '          OVERRIDE "USE TRANSACTION DATE"              
         BE    *+10                NO, (AGEING ONLY)                            
         MVC   PROFILES+8(1),QOPT6                                              
*                                                                               
         GOTO1 =A(BUILDPTA),DMCB,(RA),(RC)  BUILD AGEING HEADER                 
         B     XIT                                                              
         EJECT                                                                  
*        TRANSACTION PROCESSING ROUTINES                                        
*                                                                               
UNBINT   NTR1                      ADD UNBILLED INTERNAL CHARGES TO TAB         
*                                                                               
         MVI   TPUTSTAT,C'S'     TELL ROUTINE TO 'S'UBRTACT BILLED              
         MVI   UNBCALL,C'Y'                                                     
         BAS   RE,TIMEPUT                                                       
         MVI   UNBCALL,C'N'                                                     
*                                                                               
         CP    POSTTRAN,=P'0'    FULLY BILLED                                   
         BE    XIT               YES,                                           
         ZAP   BUFAMNT1,POSTTRAN  NO, FILL IN THE KEY AND DATA                  
         ZAP   BUFAMNT2,=P'0'                                                   
         ZAP   BUFAMNT3,=P'0'                                                   
         ZAP   BUFAMNT4,=P'0'                                                   
         ZAP   BUFAMNT5,=P'0'                                                   
*                                                                               
         L     R3,ADSUBAC         NO, USE NORMAL CONTRAL ACCOUNT                
         USING TRSUBHD,R3                                                       
         MVC   BUFKEY,TRSBACNT+1   KEY IS CONTRA ACCOUNT                        
         MVC   BUFCOM,SPACES                                                    
         SR    RF,RF               SAVE CONTRA ACCT NAME AS DATA                
         IC    RF,TRSBLEN                                                       
         SH    RF,=H'18'                                                        
         BM    UNB0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUFCOM(0),TRSBNAME                                               
*                                                                               
UNB0     L     RF,ADTRANS                                                       
         SH    RF,DATADISP                                                      
         USING ACKEYD,RF                                                        
         CLC   ACKEYCON+1(2),=C'1R'                                             
         BNE   UNB1               YES, USE 4C POSTING ACCOUNT FOR TABLE         
         DROP  RF                                                               
         MVI   ELCODE,X'4C'                                                     
         LR    R4,RF                                                            
         BAS   RE,GETEL                                                         
         BNE   UNB1                                                             
         USING TRSDESCD,R4                                                      
         MVC   BUFCOM,SPACES                                                    
         MVC   BUFKEY,SPACES                                                    
         SR    RF,RF                                                            
         IC    RF,TRSDLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUFKEY(0),TRSDACCS                                               
UNB1     BAS   RE,BUFFPUT                                                       
         B     XIT                                                              
         EJECT                                                                  
TIMEPUT  NTR1                                                                   
*                                                                               
*        USER HAS ASKED TO SEGREGATE CLIENT TIME                                
*        WE SAVE THE BILLED AMOUNT ON THE DEBITS TO SUBTRACT IT FROM            
*        THE BILLING.                                                           
*                                                                               
         USING TRANSD,R7                                                        
         USING ACKEYD,R5                                                        
         L     R7,ADTRANS                                                       
         LR    R5,R7                                                            
         SH    R5,DATADISP           POINT R5 TO THE KEY                        
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
*                                                                               
         XC    TIMEKEY,TIMEKEY       CLEAR THE KEY FOR BINSRCH                  
*                                                                               
         USING PTAELD,R4                                                        
         CLI   0(R4),PTAELQ                                                     
         B     TIMEP1+8                                                         
*                                                                               
TIMEP1   MVI   ELCODE,PTAELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   TIMEP7              NO MORE ELEMENTS, ALL DONE                   
*                                                                               
         TM    PTASTAT1,PTASPEND   SKIP ALL PENDING                             
         BO    TIMEP1                                                           
*                                                                               
         CLI   PTATYPE,PTATRAL     LOOK FOR BILLING, WRITE-OFFS                 
         BE    TIMEBL              AND RECOVERIES                               
         CLI   PTATYPE,PTATWOF                                                  
         BE    TIMEWO                                                           
         CLI   PTATYPE,PTATWOFR                                                 
         BNE   TIMEP1                                                           
*                                                                               
TIMEWO   CLI   TPUTSTAT,C'S'         RETURNING BILLABLE AMOUNT?                 
         BNE   TIMEP1                NO,                                        
         MVC   TIMEINV,PTAWREF       YES, GET THE INVOICE NUMBER                
         GOTO1 DATCON,DMCB,(1,PTAWDAT),(2,TIMERUN)                              
         MVC   TIMEBIL,TIMERUN                                                  
         ZAP   TIMEAMNT,PTANET                                                  
         BAS   RE,TIMEADD            ADD TO TABLE                               
         B     TIMEP1                GET NEXT                                   
*                                                                               
TIMEBL   MVC   TIMEINV,PTARBLNO      YES, SET INVOICE NUMBER                    
         MVC   TIMEBIL,PTARBLDT      BILL DATE                                  
         MVC   TIMERUN,PTARDATE      RUN DATE                                   
         ZAP   TIMEAMNT,PTANET       AMOUNT                                     
*                                                                               
* THIS CODE TO HANDLE OLD ITEMS WHERE 4B ELEMENT DID NOT CONTAIN                
* THE AMOUNT BILLED.                                                            
*                                                                               
         CP    PTANET,=P'0'        DO WE HAVE AN AMOUNT?                        
         BNE   TIMEBL2 YES                                                      
         OC    ACDTUSED,ACDTUSED   NO, IS IT FULLY BILLED?                      
         BZ    TIMEBL2             NO, TAKE WHAT YOU HAVE                       
         CLC   ACDTUSED,=X'B8B3'   YES, BEFORE 5/18/92 (NEW AC21)               
         BNL   TIMEBL2             NO                                           
         ZAP   TIMEAMNT,TRANAMNT   YES, USE TRANSACTION AMOUNT                  
*                                                                               
TIMEBL2  BAS   RE,TIMEADD            ADD TO TABLE                               
         B     TIMEP1                GET NEXT                                   
*                                                                               
TIMEP7   CLI   TPUTSTAT,C'S'       SUBTRACT BILLED AMOUNT FROM CHARGE?          
         BNE   *+8                 NO                                           
         BAS   RE,ASOFTRN          SET POSTTRAN VIA ACASOF                      
*                                                                               
TIMEPX   B     XIT                                                              
         DROP  R4,R7                                                            
*                                                                               
*                                                                               
TIMEADD  NTR1                                                                   
*                                                                               
*        ADD TIME WHICH HAS BEEN BILLED TO TABLE                                
*        FOR OPEN ITEM AGEING, SUBTRACT THE BILLED PORTION FROM THIS            
*        DEBIT AMOUNT.                                                          
*                                                                               
         CLI   TPUTSTAT,C'S'         CALLED FOR OPEN CHARGES                    
         BNE   TIMEA1                NO, ADD BILLED AMOUNT TO TABLE             
         GOTO1 DATCON,DMCB,(2,TIMERUN),(1,FULL) COMPRESSED TO PACKED            
         CLC   MYMEND(2),FULL      WAS THIS BILLED WITHIN REQ RANGE             
         BL    XIT                 NO, CONSIDER IN UNBILLED                     
         SP    POSTTRAN,TIMEAMNT   YES, SUBTRACT AMOUNT BILLED                  
*                                                                               
         L     R3,=A(BILLTAB)      AND ADD TO TABLE AS A BILL NUMBER            
         B     *+8                                                              
TIMEA1   L     R3,=A(TIMETAB)                                                   
*                                                                               
         L     R5,0(R3)            NUMBER IN TABLE                              
         L     R6,4(R3)            MAX                                          
         LA    R7,8(R3)            A(TABLE)                                     
         GOTO1 BINSRCH,DMCB,(1,TIMEKEY),(R7),(R5),TIMELEN,(0,8),(R6)            
         MVC   0(4,R3),DMCB+8                                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DMCB,1              NOT FOUND - ADDED                            
         BE    XIT                                                              
         L     R5,DMCB             RECORD FOUND (INVOICE NUM OR DATE)           
         AP    10(6,R5),TIMEAMNT   SO ADD AMOUNT TO IT                          
         XC    TIMEKEY,TIMEKEY                                                  
         ZAP   TIMEAMNT,=P'0'                                                   
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SAVE OPEN CREDITS BY DA DATE- SAVE POSTDATE, POSTTRAN                  
*              IN THE TABLE                                                     
*----------------------------------------------------------------------         
SVCR     NTR1                                                                   
*                                                                               
         XC    CRKEY,CRKEY         BUILD BINSRCH KEY                            
         MVC   CRDADT,POSTDADT     DA DATE                                      
*                                                                               
         MVC   CRMNTH,POSTDATE     MONTH TO POST IN                             
         ZAP   CRAMNT,POSTTRAN     AMOUNT                                       
*                                                                               
         L     R3,=A(OCRTAB)       TABLE OF OPEN CREDITS                        
*                                                                               
         USING TRANSD,R7                                                        
         L     R7,ADTRANS                                                       
         CLC   =C'TOTAL',TRNSNARR  IS THIS A TOTAL BILL                         
         BNE   *+8                                                              
         L     R3,=A(TBLTAB)       TABLE OF TOTAL BILLS                         
*                                                                               
         L     R5,0(,R3)           NUMBER IN TABLE                              
         STCM  R5,3,CRCTR          UNIQUE COUNTER                               
         L     R6,4(,R3)           MAX                                          
         LA    R7,8(,R3)           A(TABLE)                                     
*                                                                               
         GOTO1 BINSRCH,DMCB,(1,CRKEY),(R7),(R5),CRLEN,(0,4),(R6)                
         MVC   0(4,R3),DMCB+8                                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DMCB,1              NOT FOUND - ADDED                            
         BE    XIT                                                              
*                                  DUP KEY(DA AND UNIQUE COUNTER)               
         DC    H'0'                ABEND                                        
         EJECT                                                                  
*----------------------------------------------------------------------         
*        GIVE THE CREDIT TABLE AT 0(R3) WITH R5 ENTRIES                         
*        FIND OUT IF ANY OF THE ENTRIES OFFSET TO ZERO                          
*----------------------------------------------------------------------         
OFFCR    NTR1                                                                   
*                                                                               
         LR    R7,R3               A(TABLE) TO R7                               
         LR    R6,R5               COUNT TO R6                                  
*                                                                               
         OC    0(2,R7),0(R7)       HAS THIS BEEN OFFSET                         
         BZ    OFFC100             YES, GET NEXT                                
*                                                                               
OFFC10   LR    R1,R3               A(TABLE) TO R1                               
         LR    R0,R5               COUNT TO R0                                  
*                                                                               
OFFC30   OC    0(2,R1),0(R1)       HAS THIS BEEN OFFSET                         
         BZ    OFFC50              YES, GET NEXT                                
*                                                                               
*                                  DO THESE TWO OFFSET                          
         ZAP   DUB,CRAMNT-CRKEY(6,R1)                                           
         AP    DUB,CRAMNT-CRKEY(6,R7)                                           
         BNZ   OFFC50              NO                                           
*                                                                               
         XC    0(2,R7),0(R7)       OFFSET                                       
         XC    0(2,R1),0(R1)                                                    
         B     OFFC100             AND GET NEXT                                 
*                                                                               
OFFC50   LA    R1,CRLEN(R1)                                                     
         BCT   R0,OFFC30                                                        
*                                                                               
OFFC100  LA    R7,CRLEN(R7)                                                     
         BCT   R6,OFFC10                                                        
         B     XIT                                                              
         EJECT                                                                  
TIMEGET  NTR1                                                                   
*                                                                               
*       LOOK IN TIMETAB FOR INVOICE NUMBERS, A MATCH INDICATES PART             
*       OF THE BILL IS TIME BILLING (ALSO MATCH DATES)                          
*       QOPT5=Y - SUBTRACT TABLE AMNT FROM TRANSACTION AMNT                     
*       QOPT5=T - USE TABLE AMNT AS BILL AMNT                                   
*                                                                               
         USING TRNELD,R7                                                        
         L     R7,ADTRANS                                                       
         L     R2,=A(TIMETAB)                                                   
         L     R5,0(R2)            NUMBER IN TABLE                              
         LTR   R5,R5                                                            
         BZ    TIMEGX              TABLE EMPTY                                  
*                                                                               
         LA    R2,8(R2)            START OF TABLE                               
         GOTO1 DATCON,DMCB,(1,TRNDATE),(2,BINDTE)                               
*                                                                               
TIMEG1   CLC   0(6,R2),TRNREF      SAME INVOICE NUMBER ?                        
         BNE   TIMEG1A             NO, SKIP IT                                  
*                                                                               
         CLC   6(2,R2),TRN2DAY     MATCH ON RUN DATE                            
         BE    TIMEG2                                                           
*                                                                               
         CLC   8(2,R2),BINDTE      NO MATCH, TRY BILL DATE                      
         BE    TIMEG2                                                           
*                                                                               
TIMEG1A  LA    R2,TIMELEN(R2)                                                   
         BCT   R5,TIMEG1           LOOP TILL END OF TABLE                       
         B     TIMEGX              NOT FOUND                                    
*                                                                               
TIMEG2   EQU   *                   FOUND CLIENT TIME IS THIS BILL               
         CLI   QOPT5,C'T'                                                       
         BE    TIMEG3                                                           
         AP    POSTTRAN,10(6,R2)  ADD AMOUNT OF EXCLUDED TO THIS                
         B     TIMEG4                INVOICE (CAUSE CREDITS ARE NEG)            
*                                                                               
TIMEG3   ZAP   POSTTRAN,10(6,R2)  USE TABLE AMNT AS BILL AMNT                   
         MP    POSTTRAN,=P'-1'    REVERSE SIGN OF BILLING                       
*                                                                               
TIMEG4   MVC   0(6,R2),SPACES     ONLY USE INVOICE NUM ONCE                     
         B     XIT                                                              
*                                                                               
TIMEGX   CLI   QOPT5,C'T'         TIME ONLY REQUEST                             
         BNE   XIT                NO                                            
         ZAP   POSTTRAN,=P'0'     ZERO BILL AMNT, ITS NOT TIME                  
         B     XIT                                                              
         DROP  R7                                                               
                                                                                
*              ROUTINES AT END OF JOB                                           
*                                                                               
JOBEND   NTR1                                                                   
         CLI   QOPT1,C'S'          SUMMARY RUN                                  
         BE    JOB1                YES, NO PROD LEVEL DETAIL                    
         CLI   PRODSW,0            PRINT PRODUCT IF PENDING                     
         BE    JOB1                                                             
         MVI   PRODSW,0                                                         
*&&UK*&& MVC   P+1(60),PRODNAME                                                 
*&&US*&& GOTO1 CHOPPER,DMCB,(60,PRODNAME),(29,P+1),(C'P',2)                     
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
JOB1     EQU   *                                                                
         LA    R3,1                PRINT THE JOB PROLLER BUCKET                 
         BAS   RE,FORMAT                                                        
         L     R4,ADACCNAM                                                      
         BAS   RE,GETNAME                                                       
         MVC   SAVEPRNT(L'WORK),WORK     SHIFT NAME DOWN TO FIT IN JOB          
         MVC   WORK(13),SPACES           CLEAR FOR JOB                          
*                                                                               
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4          GET JOBS MEDIA CODE                          
         SR    R5,R5               (FIRST CHARACTER OF JOB NAME)                
         IC    R5,ACHRLEVB                                                      
         L     R2,ADACC                                                         
         LA    R5,3(R2,R5)                                                      
         MVC   THISMED,0(R5)       SAVE MEDIA TO UPDATE MEDIA TOTALS            
         MVC   WORK(6),0(R5)       SAVE JOB CODE FOR REPORT                     
         CLI   QOPT1,C'S'          SUMMARY RUN                                  
         BE    XIT                 YES, NO JOB LEVEL DETAIL                     
*                                                                               
JOB3     MVC   WORK+13(L'WORK-13),SAVEPRNT                                      
         GOTO1 ADSQUASH,DMCB,WORK,64                                            
         LH    R5,PRODISP          INDENT THE LENGTH OF THE PRODUCT             
         LA    R2,P+3(R5)          'CHOP' THE JOB NAME INTO 'P'                 
         L     R5,DMCB+4                                                        
*&&UK*&& LA    R6,32                                                            
*&&US*&& LA    R6,25               PRINT COLUMN                                 
         GOTO1 CHOPPER,DMCB,((R5),WORK),((R6),(R2)),(C'P',2)                    
*&&UK*&& MVC   P+38(12),STATUS                                                  
*&&US*&& MVC   P+32(12),STATUS                                                  
*                                                                               
         CLI   QXJOB,C'O'          XJOBS ONLY                                   
         BE    JOB10               DON'T PRINT A STAR                           
*                                                                               
         TM    XJOB,XJ             IS THIS AN XJOB                              
         BNO   *+8                                                              
         MVI   P+29,C'*'           STAR IT                                      
*                                                                               
JOB10    OC    CUREST,CUREST       IS THERE A CURRENT ESTIMATE NUM              
         BZ    JOBX                NO NO NEED FOR ESTIMATE NUMBER PRINT         
         CLI   PROFILES,C'R'       PRINT REVISION NUMBER?                       
         BNE   JOBX                NO, EXIT                                     
*                                                                               
         LA    R2,CUREST                                                        
         LA    R3,P+34                                                          
         BAS   RE,PRRNUM                                                        
*                                                                               
         CLI   QOPT3,C'R'                                                       
         BE    JOB20                                                            
         CLI   QOPT3,C'U'                                                       
         BNE   JOBX                                                             
*                                                                               
JOB20    BAS   RE,PRTHIREV         PRINT HIGHER REVISION DATA                   
*                                                                               
JOBX     BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         EJECT                                                                  
*        ROUTINE TO EXTRACT THE MEDIA AND UNIT NUMBER OF THIS JOB               
*        SO ITS TOTALS CAN BE ADDED TO THE PROPER MEDIA AND UNIT                
*        TOTALS                                                                 
*                                                                               
GETNUMS  NTR1                                                                   
         LA    R2,MEDIALST                                                      
         LA    R3,1                                                             
*                                                                               
GN2      STC   R3,MEDNUM           MEDIA FROM LIST                              
         CLC   0(1,R2),THISMED     FOUND THIS JOBS MEDIUM IN LIST               
         BE    GN4                 YES, GET UNIT NUMBER                         
         CH    R3,=H'36'           END OF LIST                                  
         BE    GN4                 YES, USE 'OTHERS'                            
         LA    R2,1(R2)            BUMP TABLE                                   
         LA    R3,1(R3)            BUMP MEDIA NUMBER                            
         B     GN2                 COMPARE TABLE WITH JOB AGAIN                 
*                                                                               
GN4      L     R2,=A(UNITLIST)     RESET TO FIND THIS JOBS OFFICE NUMB          
         LA    R3,1                                                             
*                                                                               
GN6      STH   R3,UNITNUM          STORE NUMBER IN CASE ITS IT                  
         CLC   0(2,R2),THISUNIT    IS IT                                        
         BE    XIT                 YES                                          
*                                  NOTE, BL SO IF X'00'  MATCH                  
         CH    R3,=H'255'          END OF TABLE                                 
         BE    XIT                 YES                                          
         CLI   0(R2),C' '          LAST OFFICE                                  
         BE    XIT                 YES                                          
         LA    R2,2(R2)            NEXT OFFICE                                  
         LA    R3,1(R3)            NEXT OFFICE NUMBER                           
         B     GN6                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO ADD FROM LINE 1 TO ALL RELEVANT LINES       *         
***********************************************************************         
*                                                                               
ADDLINES NTR1                                                                   
         LA    R2,1                CURRENT PRODUCT TOTAL LINE 1                 
         BAS   RE,AL2                                                           
*                                                                               
         LA    R2,2                CURRENT CLIENT TOTAL LINE  2                 
         BAS   RE,AL2                                                           
*                                                                               
         LH    R2,UNITNUM          ALL MEDIA OFFICE TOTALS  2+(1 TO 37)         
         LA    R2,2(R2)                                                         
         BAS   RE,AL2                                                           
*                                                                               
         LA    R2,258              COMPANY TOTALS                               
         BAS   RE,AL2                                                           
         B     XIT                                                              
*                                                                               
AL2      NTR1                                                                   
         BCTR  R2,R0               DECRIMENT R2                                 
         MH    R2,=H'37'           TIMES 37 AND YOUR AT AN OFFICE BLOCK         
         LA    R2,1(R2)                                                         
         LA    R3,37(R2)           POINT TO THE ALL MEDIA TOTAL                 
         GOTO1 PROLLER,DMCB,4,VBLOCKAC,1,(R3) ADD THE ALL MEDIA TOTAL           
*                                                                               
         SR    R3,R3                        CLEAR R3                            
         IC    R3,MEDNUM                    MEDIA NUMBER, THIS JOB              
         AR    R3,R2               ADDRESS OF THIS MEDIA ACCUM                  
         GOTO1 (RF),(R1),,,,(R3)   ADDIT                                        
         B     XIT                                                              
*                                                                               
*        TABLE STRUCTURE.                                                       
*                                                                               
*        CURRENT JOB                                                            
*        CURRENT PRODUCT                                                        
*           MEDIA A                                                             
*           TO                                                                  
*           MEDIA 9                                                             
*        PRODUCT TOTALS                                                         
*        CURRENT CLIENT                                                         
*           MEDIA A                                                             
*           TO                                                                  
*           MEDIA 9                                                             
*        CLIENT TOTALS                                                          
*       OFFICE A                                                                
*           MEDIA A                                                             
*           TO                                                                  
*           MEDIA 9                                                             
*       OFFICE A COMPANY TOTALS                                                 
*  FOR ALL MEDIA, BLOCK 258IS MEDIA TOTALS FOR THE COMPANY                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO PRINT UNIT SUMMARIES                        *         
***********************************************************************         
*                                                                               
UNITSUM  NTR1                                                                   
*        CALLED FROM END-O-LEDGER, R2 IS THE NUMBER OF THE MEDIA                
*        THIS ROUTINE PRODUCES AN OFFICE SUMMARY FOR THAT MEDIA                 
         MVI   ACTIVITY,C'N'                                                    
         LA    R3,75(R2)           PAST THE CLIENT AND PROD DATA                
         L     R4,=A(UNITLIST)     LIST OF OFFICES                              
         LA    R5,255              MAX OFFICES (INCLUDING OFFICE SPACE)         
*                                                                               
UNITSUM2 BAS   RE,FORMAT           PRINT  PROLLER LINE R3                       
         CLC   P+50(80),SPACES     ANYTHING HAPPEN                              
         BE    UNITSUM6            NO,                                          
         CLI   ACTIVITY,C'Y'       ANYTHING HAPPEN BEFORE                       
         BE    UNITSUM4            YES                                          
         MVC   SAVEPRNT,P          NO, DOUBLE SPACE BEFORE YOU PRINT            
         MVC   P,SPACES                                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
         MVI   ACTIVITY,C'Y'                                                    
         MVC   P,SAVEPRNT          RESTORE P                                    
*                                                                               
*                                                                               
UNITSUM4 DS    0H                                                               
*&&UK                                                                           
         MVC   P+26(4),=C'UNIT'                                                 
*&&                                                                             
*&&US                                                                           
         MVC   P+26(6),=C'OFFICE'                                               
*&&                                                                             
         MVC   P+33(2),0(R4)                                                    
         BAS   RE,PRINTEM                                                       
*                                                                               
*                                                                               
UNITSUM6 LA    R3,37(R3)           NEXT OFFICE                                  
         LA    R4,2(R4)            NEXT OFFICE CODE                             
         BCT   R5,UNITSUM2         DO FOR ALL OFFICES                           
*                                                                               
         CLI   ACTIVITY,C'Y'       ANYTHING HAPPEN AT ALL                       
         BNE   XIT                 NO, NO  NEED FOR TOTALS                      
         BAS   RE,PRINTEM                                                       
         BCTR  R2,0                                                             
         MH    R2,=H'15'                                                        
         LA    R2,MEDIANAM(R2)     NAME OF MEDIA SUMMARIZED HERE                
*&&UK                                                                           
         MVC   P+26(15),0(R2)                                                   
         LA    R5,P+41                                                          
*&&                                                                             
*&&US                                                                           
         MVC   P+22(15),0(R2)      PRINT MEDIA NAME                             
         LA    R5,P+37             POINT R5 PAST LENGTH(MAX) MEDIANAME          
*&&                                                                             
UNITSUM8 CLI   0(R5),C' '          BACK R5 INTO MEDIA NAME                      
         BNE   UNITSUM9                                                         
         BCT   R5,UNITSUM8                                                      
*                                                                               
*                                                                               
UNITSUM9 MVC   2(6,R5),=C'TOTALS'  AND PRINT THIS 2 AFTER R5                    
         BAS   RE,FORMAT           R3 NOW POINTING AT MEDIA TOTALS              
*&&US*&& BAS   RE,UNDRLINE         CLOSE THE BOX                                
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO FORMAT A PRINT LINE                         *         
***********************************************************************         
*                                                                               
FORMAT   NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,VBLOCKAC,(R3) GET THE LINE                        
         L     R2,DMCB             ADDRESS TO R2                                
*&&UK*&& LA    R3,P+50                                                          
*&&US*&& LA    R3,P+45             START PRINT COLUMN                           
         LA    R4,5                NUMBER OF ACCUMS                             
*                                                                               
*                                                                               
FORM2    CP    0(6,R2),=P'0'       THIS ACCUM ZERO?                             
         BE    FORM3               TRY THE NEXT ONE                             
         EDIT  (P6,0(R2)),(12,0(R3)),2,MINUS=YES                                
*                                                                               
FORM3    LA    R2,6(R2)            BUMP ACCUMULATOR POINTER                     
*&&UK*&& LA    R3,12(R3)                                                        
*&&US*&& LA    R3,13(R3)           BUMP PRINT COL POINTER                       
         BCT   R4,FORM2                                                         
         B     XIT                                                              
***********************************************************************         
*              ROUTINE TO PRINT VALUE OF UNAPPROVED ESTIMATES         *         
***********************************************************************         
*                                                                               
PRTHIREV NTR1                                                                   
         CLI   PROFILES,C'R'       PRINT VALUE OF ESTIMATE?                     
         BNE   PHRX                NO, EXIT                                     
         CLC   CUREST,HIREV        HIGHEST EST SAME AS CURRENT                  
         BE    PHRX                YES, DON'T PRINT                             
*                                                                               
         ZAP   DOUBLE,HIREVEST                                                  
         SRP   DOUBLE,64-2,5       ROUND OUT PENNIES                            
         LA    R3,PSECOND+38                                                    
         EDIT  (P8,DOUBLE),(6,0(R3))                                            
*                                                                               
         LA    R2,HIREV                                                         
         LA    R3,PSECOND+34                                                    
         BAS   RE,PRRNUM                                                        
PHRX     B     XIT                                                              
*                                                                               
         USING ACMD,R5                                                          
PRRNUM   L     R5,AMONACC                                                       
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
*        CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
*        BER   RE                  YES, SKIP THIS                               
         MVI   0(R3),C'R'                                                       
         EDIT  (B1,0(R2)),(3,1(R3)),ALIGN=LEFT                                  
         BR    RE                                                               
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              SET STATUS HEADERS BASED ON PROFILES/REQUEST OPTIONS   *         
***********************************************************************         
*                                                                               
SETSTATH NTR1                                                                   
         L     R6,=A(PATTERN)                                                   
         USING PATTERD,R6                                                       
*                                                                               
         LA    R2,STATH0           DEFAULT OFF/BT/EST/ST                        
         LA    R3,SPACES                                                        
         CLI   PROFILES,C'R'                                                    
         BNE   SETSX                                                            
*                                                                               
         CLI   QOPT3,C'R'          NEED HIGHER REVISIONS IN REPORT              
         BNE   SETS30                                                           
*                                                                               
SETS20   LA    R2,STATH4           BT   CUR EST                                 
         LA    R3,STATH5                UNAP REV                                
         B     SETSX                                                            
*                                                                               
SETS30   LA    R2,STATH2                                                        
         LA    R3,STATH3                                                        
         B     SETSX                                                            
*                                                                               
SETSX    MVC   MYSTAT,0(R2)                                                     
         MVC   MYSTAT2,0(R3)                                                    
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO PRINT CLIENT SUMMARY                        *         
***********************************************************************         
*                                                                               
CLISUM   NTR1                      SAVE CLIENT TOTALS FOR END                   
         L     R2,DMCB             P1 IS DOLLAR BUCKETS                         
         USING CLIKEYD,R7                                                       
         LA    R7,BUFKEY                                                        
         MVC   BUFKEY,SPACES                                                    
         MVI   CLITAG,X'FF'        CLIENT TOTAL RECORD TYPE                     
         CLI   PROFILES+7,C'Y'     SORT CLIENT SUMMARY BY OFFICE                
         BNE   CLISA                                                            
         MVC   CLIOFF,THISUNIT     THEN PUT OFFICE IN THE KEY                   
         CLI   CLIOFF,C' '         NULL OFFICE FOUND                            
         BNE   *+8                                                              
         MVI   CLIOFF,X'FF'        SORT THEM TO THE END                         
*                                                                               
CLISA    L     R4,ADHEIRA                                                       
         MVC   CLICODE(3),3(R4)    CLIENT TO KEY                                
         L     R4,ADLVANAM                                                      
         BAS   RE,GETNAME                                                       
         MVC   BUFCOM(36),WORK                                                  
*                                                                               
         LA    R1,5                                                             
         LA    R3,BUFAMNT1                                                      
*                                                                               
CLIS01   EQU   *                                                                
         ZAP   0(8,R3),0(6,R2)                                                  
         LA    R2,6(R2)                                                         
         LA    R3,8(R3)                                                         
         BCT   R1,CLIS01                                                        
*                                                                               
         BAS   RE,BUFFPUT                                                       
         B     XIT                                                              
         EJECT                                                                  
*        PRINT A FIVE BY SIX BYTE PACKED BUCKET                                 
SUBPRT   NTR1                                                                   
         L     R2,DMCB             P1 IS DOLLAR TOTALS                          
         L     R5,DMCB+4           P2 IS JOB COUNT                              
         TM    PRTSTA,CLIENT       CLIENT SUMMARY CALL                          
         BO    SUBP1               YES                                          
*                                                                               
         TM    SUBPSTAT,NOCOUNT    SKIP PRINTING RECORD COUNT                   
         BO    SUBP1               YES                                          
*                                                                               
         CP    0(6,R5),=P'0'       ANY JOBS IN THIS CALL                        
         BNE   SUBPA               YES CONTINUE                                 
         MVC   P,SPACES            NO CLEAR PRINT LINE                          
         B     XIT                 AND CHECK IF ITS A CLIENT SUM CALL           
SUBPA    LA    R3,P+36                                                          
         EDIT  (P6,0(R5)),(6,0(R3))                                             
SUBP1    EQU   *                                                                
*&&UK*&& LA    R3,P+50                                                          
*&&US*&& LA    R3,P+45             START PRINT COLUMN                           
         LA    R4,5                NUMBER OF ACCUMS                             
*                                                                               
SUBP2    CP    0(6,R2),=P'0'       THIS ACCUM ZERO?                             
         BE    SUBP3               TRY THE NEXT ONE                             
         EDIT  (P6,0(R2)),(12,0(R3)),2,MINUS=YES                                
*                                                                               
SUBP3    LA    R2,6(R2)            BUMP ACCUMULATOR POINTER                     
*&&UK*&& LA    R3,12(R3)                                                        
*&&US*&& LA    R3,13(R3)           BUMP PRINT COL POINTER                       
         BCT   R4,SUBP2                                                         
         B     XIT                                                              
*                                                                               
PROLLIT  NTR1                                                                   
*                                                                               
*        ADD AN AMOUNT TO SPECIFIC COL-(R3) AND TO A CUMMULATIVE BUCKET         
*                                                                               
         GOTO1 PROLLER,DMCB,3,VBLOCKAC,POSTTRAN,1,(R3)                          
         GOTO1 (RF),(R1),,,,,5                                                  
         B     XIT                                                              
         EJECT                                                                  
BUFFSEQ  ST    RE,SAVERE                                                        
         MVC   COMMAND,=CL8'SEQ'                                                
         BAS   RE,GOTOBUFF                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
BUFFHIGH ST    RE,SAVERE                                                        
         MVC   COMMAND,=CL8'HIGH'                                               
         BAS   RE,GOTOBUFF                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
BUFFPUT  ST    RE,SAVERE                                                        
         MVC   COMMAND,=CL8'PUT'                                                
         BAS   RE,GOTOBUFF                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
GOTOBUFF NTR1                                                                   
         GOTO1 BUFFALO,DMCB,COMMAND,ADBUFF,BUFREC,1                             
         B     XIT                                                              
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
XITEQ    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
XITNEQ   LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
*                                                                               
GETNAME  NTR1                      MOVE A VARIBLE LEN NAME INTO WORK            
         MVC   WORK,SPACES                                                      
         USING ACNAMED,R4          R4 POINTS TO THE NAME ELEMENT                
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),ACNMNAME                                                 
         EJECT                                                                  
*        SET UP HEADLINE INFO FOR TRIAL BAL AND JOB AGEING                      
*                                                                               
PRINTEM  NTR1                                                                   
         CLI   QOPT1,C'S'          SUMMARY REQUEST                              
         BNE   *+12                NO                                           
         CLI   MODE,REQLAST        YES, PRINT ONLY AT REQUEST LAST              
         BNE   XIT                                                              
*                                                                               
PRINTALL BAS   RE,MYREPORT                                                      
         B     XIT                                                              
*                                                                               
MYREPORT NTR1                                                                   
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              ADD TO TRANSACTION TYPE TABLE                                    
*                                                                               
*                                                                               
ADDTTBL  NTR1                                                                   
         USING TRANSD,R4                                                        
         TM    TRNSSTAT,X'80'                                                   
         BO    ADDT2                                                            
         CLC   TRNSBTCH+2(4),SPACES                                             
         BNH   XIT                 MANUAL BILLING ONLY                          
*                                                                               
ADDT2    DS    0H                                                               
         L     R3,=A(TYPETAB)                                                   
         L     R5,0(R3)            NUMBER IN TABLE                              
         L     R6,4(R3)            MAX                                          
         LA    R7,8(R3)                                                         
         XC    RECKEY,RECKEY                                                    
         MVC   RECKEY(1),TRNSTYPE                                               
         CLI   PROFILES+2,C'Y'     INCLUDE CONTRA LEDGER IN KEY                 
         BNE   ADDT10                                                           
         L     RF,ADSUBAC          POINT TO X'43' EL                            
         MVC   RECKEY+1(1),4(RF)   X'43', LEN, XCOMP, UNIT, LEDGER              
ADDT10   ZAP   RECCLI,POSTTRAN                                                  
         ZAP   RECREQ,POSTTRAN     FOR TOTAL                                    
         GOTO1 BINSRCH,DMCB,(1,RECKEY),(R7),(R5),14,(0,2),(R6)                  
         MVC   0(4,R3),DMCB+8                                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   DMCB,1              NOT FOUND - ADDED                            
         BE    XIT                                                              
         L     R5,DMCB             RECORD FOUND                                 
         AP    2(6,R5),POSTTRAN                                                 
         AP    8(6,R5),POSTTRAN                                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*&&US                                                                           
UNDRLINE NTR1                      UNDERLINE ROUTINE                            
         ZIC   RF,LINE                                                          
         LA    R2,MYROW(RF)                                                     
         MVI   0(R2),C'M'                                                       
         L     R2,ADBOX            ADDR OF BOX ROUTINE                          
         USING BOXD,R2                                                          
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXINIT,0                                                        
         B     XIT                                                              
*                                                                               
*&&                                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        WHEN AGEING, SAVE OFFSET TRANSACTIONS BY REFNO, T-DATE MONTH           
*        AND AMOUNT. IF A SUBSEQUENT TRAN COMES ALONG THAT WILL OFFSET          
*        AN ITEM IN THE TABLE (SAME NO, REF AND AMOUNTS NET TO ZERO)            
*        I REMOVE THE OLD ITEM FROM THE TABLE, AND IGNORE THE NEW ITEM          
*        NOTE: I AVOID HAVING TO SAVE CONTRA ACCOUNT BY INITING TABLE           
*              AT PROCSBAC, AND READING RESULTS AT SBACLAST                     
*----------------------------------------------------------------------         
PUTOFFS  EQU   *                                                                
         USING OFFTABD,R2                                                       
         USING TRANSD,R4                                                        
         L     R2,AOFFTAB                                                       
         L     R4,ADTRANS                                                       
         LA    R0,OFFTABNM                                                      
PUTO10   CLC   OFFREF,TRNSREF                                                   
         BNE   PUTO50                                                           
         CLC   OFFDATE,TRNSDATE                                                 
         BNE   PUTO50                                                           
         ZAP   WORKDUB,POSTTRAN                                                 
         AP    WORKDUB,OFFAMNT                                                  
         BNZ   PUTO50                                                           
*                                                                               
         ZAP   OFFAMNT,=P'0'       OFFSET FOUND, PURGE FROM TABLE               
         B     PUTOX               AND EXIT W/O SAVING                          
*                                                                               
PUTO50   LA    R2,OFFTABLN(R2)     LOOK FOR NEXT IN TABLE                       
         BCT   R0,PUTO10                                                        
*                                                                               
*        PUT THIS TRAN IN A FREE SPOT IN THE TABLE                              
         LA    R0,OFFTABNM                                                      
         L     R2,AOFFTAB                                                       
PUTO60   OC    OFFREF,OFFREF       IS THIS SPOT FREE                            
         BZ    PUTO70              YES                                          
         LA    R2,OFFTABLN(R2)     NO, GET NEXT SPOT                            
         BCT   R0,PUTO60                                                        
         DC    H'0'                OFFTAB IS FULL                               
*                                                                               
PUTO70   MVC   OFFREF,TRNSREF                                                   
         MVC   OFFDATE,TRNSDATE                                                 
         MVC   OFFMNTH,POSTDATE    BUCKET MONTH OF THIS TRANSACTION             
         ZAP   OFFAMNT,POSTTRAN                                                 
*                                                                               
PUTOX    B     XIT                 EXIT FROM PROCTRNS                           
         EJECT                                                                  
LOOKUP   NTR1                                                                   
         L     R3,AMONACC                                                       
         USING ACMD,R3                                                          
*                                                                               
         L     R5,ACMAJOBB                                                      
         USING JBLOCKD,R5                                                       
         MVC   JBAJOB,ADACC                                                     
         MVC   JBACOLS,ACMACOLL                                                 
         MVC   JBACOM,ADCOMFAC                                                  
         MVC   JBAGOBLK,ADGOBLOC                                                
         MVC   JBAIO,ACMAJOBI                                                   
         MVC   JBAKEY,LASTIO                                                    
         MVC   JBGETOPT,GETOPT                                                  
*                                                                               
         MVC   JBACOLTB,ACMACOL                                                 
         MVC   JBLCOLTB,ACMLCOL                                                 
         MVC   JBAOPVTB,ACMAOPV                                                 
         MVC   JBLOPVTB,ACMLOPV                                                 
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,FLDH                                                          
         ST    RE,JBORICLI                                                      
*                                                                               
         GOTO1 ACMAJOBR,DMCB,ACMAJOBB                                           
         CLI   JBERROR,X'00'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R3,ACMACOL                                                       
         XIT1  REGS=(R3,R5)                                                     
*                                                                               
FLDH     DC    AL1(8+L'FLD),4X'00',AL1(L'FLD),AL2(0)                            
FLD      DC    C'CE,HR'                                                         
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
*---------------------------------------------------------------------+         
*        DATA MANAGER INTERFACE                                       |         
*---------------------------------------------------------------------+         
*                                                                               
HIGH     MVC   COMMAND,=CL8'DMRDHI'                                             
         B     GETREC                                                           
*                                                                               
SEQ      MVC   COMMAND,=CL8'DMRSEQ'                                             
         B     GETREC                                                           
*                                                                               
GETREC   NTR1                                                                   
         L     R7,=A(IOAREA)                                                    
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',MYKEY,(R7)                      
         B     XIT                                                              
         EJECT                                                                  
ASOFINIT NTR1                                                                   
         USING ACASOFD,R4                                                       
         LA    R4,ASOFBLK                                                       
         L     RE,AASOFBLS                                                      
         ST    RE,ACABILLS                                                      
*                                                                               
         L     RE,ADACC                                                         
         ST    RE,ACAATRN                                                       
*                                                                               
         L     RE,ADCOMFAC                                                      
         ST    RE,ACACOMF                                                       
*                                                                               
         L     RE,AMONACC                                                       
         MVC   ACASAEND,ACMAEND-ACMD(RE)                                        
         L     RF,ACMAPRO2-ACMD(RE)                                             
         ST    RF,ACAPTABF                                                      
*                                                                               
         MVC   ACASOFDT,MYMEND                                                  
         MVI   ACAMODE,ACAMINIT                                                 
*                                                                               
         GOTO1 =V(ACASOF),(R4)                                                  
         B     XIT                                                              
ASOFTRN  NTR1                                                                   
         USING ACASOFD,R4                                                       
*                                                                               
         LA    R4,ASOFBLK                                                       
*                                                                               
         L     RE,ADTRANS                                                       
         SH    RE,DATADISP                                                      
         ST    RE,ACAATRN                                                       
*                                                                               
         MVI   ACAMODE,ACAMTRN                                                  
         MVI   ACAFROM,ACAFPTA                                                  
*                                                                               
         GOTO1 =V(ACASOF),(R4)                                                  
         ZAP   POSTTRAN,ACAUNBLD                                                
         B     XIT                                                              
*----------------------------------------------------------------------         
*        GET SPACE FOR THE BUFFERS                                              
*----------------------------------------------------------------------         
GETBUFF  NTR1                                                                   
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         LA    R0,MAINNUM                                                       
         LR    R5,R1               R5 IS BUFFER POINTER                         
         ST    R1,ABUFF            SAVE BUFF START                              
         SPACE 1                                                                
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
         SPACE 1                                                                
         L     R2,=A(MAINTAB)                                                   
         USING MAIND,R2                                                         
         SPACE 1                                                                
GETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         A     R5,MAINSIZE                                                      
         LA    R2,MAINLEN(R2)                                                   
         BCT   R0,GETB10                                                        
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
RELBUFF  NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
         SPACE 1                                                                
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
         USING MASTD,R2                                                         
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*----------------------------------------------------------------------         
*              LITERAL POOL                                                     
*----------------------------------------------------------------------         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         BUFF  LINES=600,ROWS=1,COLUMNS=5,FLAVOR=PACKED,               X        
               KEYLIST=(14,A),COMMENT=36                                        
         EJECT                                                                  
*              PRINT A MEDIA SUMMARY                                            
*                                                                               
MEDIASUM LR    R1,RC                                                            
         NMOD1 0,**MSUM**                                                       
         LR    RC,R1                                                            
*        THIS ROUTINE WILL PRODUCES THE MEDIA SUMMARY AT DIFFERENT              
*        LEVELS AND ZAPS THE PROLLER ACCUMULATOR FOR THAT LEVEL                 
*        R2=1  CALLED FROM END-OF-PRODUCT                                       
*        R2=2  CALLED FROM END-OF-CLIENT                                        
*        R2=3-258CALLED FROM END-OF-REQUEST (AT E-O-REQUEST, REPEATED           
*        CALLS ARE MADE FOR (OFFICE NUM+2) AND COMPANY (R2=40)                  
*                                                                               
         LR    R3,R2                                                            
         BCTR  R3,R0               DECRIMENT R3 (TABLE STARTS AT ZERO)          
         MH    R3,=H'37'          POINT TO CORRECT OFFICE BLOCK                 
         LA    R3,2(R3)            SKIP CLI, PROD TOTALS                        
*                                                                               
         MVI   ACTIVITY,C'N'       IN CASE WE DON'T FIND ANYTHING               
         LA    R4,MEDIANAM         LIST OF MEDIA NAMES                          
         LA    R5,36               MAXIMA MEDIA                                 
         LA    R6,MEDIALST         LIST OF MEDIA CODES                          
         MVI   TOTSONLY,C'N'       DEFAULT IS PRINT DETAIL                      
         CLI   QOPT1,C'S'          SUMMARY RUN                                  
         BNE   MSUM2               NO, CONTINUE                                 
         CH    R2,=H'2'                                                         
         BH    MSUM2                                                            
         MVI   TOTSONLY,C'Y'       NO DETAIL ON CLI AND PROD REQUEST            
*                                                                               
MSUM2    BAS   RE,MEDFORM          PRINT PROLLER(R3)                            
         CLC   P+50(80),SPACES     WHAT HAPPENED ?                              
         BE    MSUM6               NOTHING, BUMP R3                             
         CLI   ACTIVITY,C'Y'       FIRST ACTIVITY ?                             
         BE    MSUM4               NO, CONTINUE                                 
*                                                                               
         MVC   SAVEPRNT,P          YES, DOUBLE SPACE, THEN CONTINUE             
         MVC   P,SPACES                                                         
         MVI   SPACING,2                                                        
         CLI   TOTSONLY,C'Y'       DON'T DOUBLE IF NOT PRINTING DETAIL          
         BE    *+8                                                              
         BAS   RE,MEDPRINT                                                      
         MVI   SPACING,1                                                        
         MVI   ACTIVITY,C'Y'                                                    
         MVC   P,SAVEPRNT                                                       
*                                                                               
MSUM4    DS    0H               IS THIS THE ALL OFFICEBY MEDIA SUM              
         L     R7,=A(PATTERN)                                                   
         USING PATTERD,R7                                                       
         CLC   P+1(22),TWIP                                                     
         BNE   MSUM4A           NO, CONTINUE                                    
         MVC   SAVEPRNT,P       YES                                             
         MVC   P+23(109),SPACES DOUBLE SPACE                                    
         MVI   SPACING,2                                                        
         BAS   RE,MEDPRINT                                                      
         MVC   P,SAVEPRNT      AND SPACE OUT OFFICE FROM PRINT LINE             
         MVC   P+1(22),SPACES                                                   
         DROP  R7                                                               
MSUM4A   DS    0H                                                               
*&&US                                                                           
         CLI   RCSUBPRG,0          IS THIS A CLIENT OR PRODUCT TOTAL            
         BNE   MSUM4B              NO                                           
         MVC   P+6(5),=C'MEDIA'    YES                                          
         MVC   P+13(1),0(R6)       PRINT MEDIA CODE                             
         MVC   P+15(15),0(R4)      PRINT MEDIA NAME                             
         B     MSUM4D                                                           
*&&                                                                             
MSUM4B   MVC   P+20(5),=C'MEDIA'   PRINT MEDIA CODE AND NAME                    
         MVC   P+27(1),0(R6)                                                    
         MVC   P+29(15),0(R4)                                                   
MSUM4D   CLI   TOTSONLY,C'Y'                                                    
         BE    *+8                 DONT PRINT                                   
         BAS   RE,MEDPRINT                                                      
         MVC   P,SPACES                                                         
         GOTO1 PROLLER,DMCB,2,VBLOCKAC,(R3) CLEAR THIS PROLLER BUCKET           
*                                                                               
MSUM6    LA    R3,1(R3)            NEXT MEDIA IN PROLLER TABLE                  
         LA    R4,15(R4)           NEXT MEDIA NAME                              
         LA    R6,1(R6)            NEXT MEDIA CODE                              
         BCT   R5,MSUM2            DO UNTIL NO MORE MEDIA                       
*                                                                               
         CLI   ACTIVITY,C'Y'       ANYTHING HAPPEN                              
         BNE   XIT                 NO                                           
         CH    R2,=H'1'            YES, IS THIS PRODUCT TOTALS                  
         BNE   MSUM7               NO, PRINT                                    
         CLI   TOTSONLY,C'Y'       PRINT?                                       
         BE    *+8                 NO                                           
*                                                                               
*                                                                               
MSUM7    BAS   RE,MEDPRINT                                                      
*                                                                               
         BAS   RE,PRTXTOTS         PRINT EXPENSE TOTALS (IF NEEDED)             
*                                                                               
         CLI   RCSUBPRG,0          IS THIS A CLIENT OR PRODUCT TOTAL            
         BNE   *+14                NO, USE NEXT LITERAL                         
         MVC   P+6(10),=C'TOTALS FOR'                                           
         B     *+10                                                             
         MVC   P+20(10),=C'TOTALS FOR'                                          
*                                                                               
         MVC   TOTFOR,=CL8'PRODUCT'                                             
         CH    R2,=H'1'                                                         
         BE    MSUM7A                                                           
*                                                                               
         MVC   TOTFOR,=CL8'CLIENT'                                              
         CH    R2,=H'2'                                                         
         BE    MSUM7A                                                           
*                                                                               
         MVC   TOTFOR,=CL8'OFFICE'                                              
         CH    R2,=H'258'                                                       
         BNE   MSUM7A                                                           
         MVC   TOTFOR,=CL8'REPORT'                                              
*                                                                               
*&&US                                                                           
MSUM7A   CLI   RCSUBPRG,0          CLI OR PROD TOT                              
         BNE   *+14                NO PRINT AT P+31                             
         MVC   P+17(8),TOTFOR                                                   
         B     *+10                                                             
*&&                                                                             
         MVC   P+31(8),TOTFOR                                                   
         LA    R2,TOTFOR                                                        
*                                                                               
         BAS   RE,MEDFORM          PRINT THE PROLLER BUCKETS                    
         MVI   SPACING,3                                                        
         CLI   TOTSONLY,C'Y'       DO THEY ONLY WANT TOTALS                     
         BNE   MSUM8               NO, 'P' IS SET UP CORRECTLY                  
         MVC   P+1(43),SPACES      YES, CLEAR PRINT LINE                        
         CLC   0(7,R2),=C'PRODUCT' IS THIS IS A PRODUCT TOTAL                   
         BE    MSUM10              YES, CLEAR P, PROLLER BUCKET AND XIT         
         MVI   SPACING,1           NO, ITS A CLIENT TOTAL                       
         L     R4,ADHEIRA          MOVE CLIENT CODE TO 'P'                      
         MVC   P+1(6),3(R4)                                                     
         L     R4,ADLVANAM         MOVE CLIENT NAME TO P+8                      
         BAS   RE,GETNAME                                                       
         MVC   P+8(36),WORK                                                     
*                                                                               
*                                                                               
MSUM8    DS    0H                  DID I JUST PRINT TOTALS                      
*&&US                                                                           
         CLC   P+6(18),=C'TOTALS FOR PRODUCT'                                   
         BE    *+14                                                             
         CLC   P+20(17),=C'TOTALS FOR OFFICE'                                   
         BNE   *+8                                                              
         BAS   RE,UNDRLINE         THEN CLOSE THE BOX                           
*&&                                                                             
         BAS   RE,MEDPRINT         AND PRINT                                    
*                                                                               
*                                                                               
MSUM10   MVC   P,SPACES                                                         
         GOTO1 PROLLER,DMCB,2,VBLOCKAC,(R3) CLEAR PROLLER BUCKET                
MEDSUMX  XIT1                                                                   
*                                                                               
*----------------------------------------------------------------------         
*        PRINT EXPENSE AND NONEXPENSE TOTALS                                    
*        R2=1  CALLED FROM END-OF-PRODUCT                                       
*        R2=2  CALLED FROM END-OF-CLIENT                                        
*        R2>2  CALLED FROM END-OF-REQUEST                                       
*        R3=PROLLER INDEX FOR OVERALL TOTALS AT THIS LEVEL                      
*----------------------------------------------------------------------         
*                                                                               
PRTXTOTS NTR1                                                                   
*        OC    XJOB,XJOB           THIS WOULD PRINT X TOTS FOR THE              
*        BZ    PRTXX               FIRST AND ALL SUBSEQUENT                     
         CLI   QXJOB,C'Y'          XJOBS INCLUDED                               
         BNE   PRTXX               NO                                           
*                                                                               
         USING PATTERD,R6                                                       
         L     R6,=A(PATTERN)                                                   
         LA    R5,PROXJOB          EXPENSE TOTALS FOR PRODUCT                   
         LA    R1,XJINPRO          BIT SET IF XJOBS IN PRODUCT                  
         LA    R4,XJPROLIT         LITERAL TO PRINT IN PATTERN                  
         MVC   HALF,=H'2'                                                       
         CLM   R2,3,HALF                                                        
         BL    PRTX20                                                           
*                                                                               
         LA    R5,CLIXJOB                                                       
         LA    R1,XJINCLI                                                       
         LA    R4,XJCLILIT                                                      
         BE    PRTX20                                                           
*                                                                               
         MVC   HALF,=H'258'        IS THIS A REQUEST TOTAL                      
         CLM   R2,3,HALF                                                        
         BNE   PRTXX               NOPE                                         
*                                                                               
         LA    R5,REQXJOB                                                       
         LA    R1,XJINREQ                                                       
         LA    R4,XJREQLIT                                                      
*                                                                               
PRTX20   EQU   *                                                                
         STC   R1,BYTE                                                          
         OC    XJOB,BYTE           IS THERE EXPENSE DATA FOR THIS LEVEL         
         BZ    PRTXX               NO                                           
*                                                                               
         GOTO1 PROLLER,DMCB,1,VBLOCKAC,(R3) GET OVERALL TOTAL                   
         L     R2,DMCB                                                          
         MVC   DEBS(30),0(R2)      OVERALL TOTALS TO DEBS                       
         LA    R3,DEBS                                                          
         BAS   RE,SUBR3            GET NON-EXPENSE TOTAL                        
*                                                                               
         OI    SUBPSTAT,NOCOUNT                                                 
         LA    R6,P+6              PRINT AT P+6                                 
         CLC   HALF,=H'258'                                                     
         BNE   *+8                 UNLESS ITS A REQUEST TOTAL                   
         LA    R6,P+20                                                          
*                                                                               
         MVC   0(20,R6),0(R4)                                                   
         GOTO1 MEDSUB,DMCB,(R3)                                                 
         BAS   RE,MEDPRINT                                                      
*                                                                               
         LR    R3,R5               PRINT EXPENSE TOTALS                         
         MVC   0(15,R6),=C'EXPENSE CHARGES'                                     
         GOTO1 MEDSUB,DMCB,(R3)                                                 
         BAS   RE,MEDPRINT                                                      
*                                                                               
PRTXX    NI    SUBPSTAT,X'FF'-NOCOUNT                                           
PRTXXXX  B     XIT                                                              
*                                                                               
SUBR3    NTR1                      SUBTRACT R5 FROM R3 FIVE TIMES               
         LA    R0,5                                                             
SUBR310  SP    0(6,R3),0(6,R5)                                                  
         LA    R5,6(R5)                                                         
         LA    R3,6(R3)                                                         
         BCT   R0,SUBR310                                                       
         B     PRTXXXX                                                          
         EJECT                                                                  
*                                                                               
TOTFOR   DC    CL8' '                                                           
MEDPRINT NTR1                                                                   
         CLI   QOPT1,C'S'          SUMMARY REQUEST                              
         BNE   *+12                NO                                           
         CLI   MODE,REQLAST        YES, PRINT ONLY AT REQUEST LAST              
         BNE   MEDSUMX                                                          
*                                                                               
         GOTO1 ACREPORT                                                         
         B     MEDSUMX                                                          
         EJECT                                                                  
*              EDIT A LINE OF PACKED ACCUUMULATORS ON THE MEDIA SUMMARY         
*                                                                               
MEDFORM  NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,VBLOCKAC,(R3) GET THE LINE                        
         L     R2,DMCB             ADDRESS TO R2                                
*&&UK*&& LA    R3,P+50                                                          
*&&US*&& LA    R3,P+45             START PRINT COLUMN                           
         LA    R4,5                NUMBER OF ACCUMS                             
*                                                                               
*                                                                               
MEDF2    CP    0(6,R2),=P'0'       THIS ACCUM ZERO?                             
         BE    MEDF3               TRY THE NEXT ONE                             
         EDIT  (P6,0(R2)),(12,0(R3)),2,MINUS=YES                                
*                                                                               
MEDF3    LA    R2,6(R2)            BUMP ACCUMULATOR POINTER                     
*&&UK*&& LA    R3,12(R3)                                                        
*&&US*&& LA    R3,13(R3)           BUMP PRINT COL POINTER                       
         BCT   R4,MEDF2                                                         
         B     MEDSUMX                                                          
*                                                                               
*        SUBPRT FOR PRTXJOBS                                                    
MEDSUB   NTR1                                                                   
         L     R2,DMCB             P1 IS DOLLAR TOTALS                          
         L     R5,DMCB+4           P2 IS JOB COUNT                              
         TM    PRTSTA,CLIENT       CLIENT SUMMARY CALL                          
         BO    MEDS1               YES                                          
*                                                                               
         TM    SUBPSTAT,NOCOUNT    SKIP PRINTING RECORD COUNT                   
         BO    MEDS1               YES                                          
*                                                                               
         CP    0(6,R5),=P'0'       ANY JOBS IN THIS CALL                        
         BNE   MEDSA               YES CONTINUE                                 
         MVC   P,SPACES            NO CLEAR PRINT LINE                          
         B     XIT                 AND CHECK IF ITS A CLIENT SUM CALL           
MEDSA    LA    R3,P+36                                                          
         EDIT  (P6,0(R5)),(6,0(R3))                                             
MEDS1    EQU   *                                                                
         LA    R3,P+45             START PRINT COLUMN                           
         LA    R4,5                NUMBER OF ACCUMS                             
*                                                                               
MEDS2    CP    0(6,R2),=P'0'       THIS ACCUM ZERO?                             
         BE    MEDS3               TRY THE NEXT ONE                             
         EDIT  (P6,0(R2)),(12,0(R3)),2,MINUS=YES                                
*                                                                               
MEDS3    LA    R2,6(R2)            BUMP ACCUMULATOR POINTER                     
         LA    R3,13(R3)           BUMP PRINT COL POINTER                       
         BCT   R4,MEDS2                                                         
         XIT1                                                                   
*                                                                               
*              BOX ROUTINES (HOOK)                                              
*                                                                               
*&&US                                                                           
HOOK     NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC           RESTORE REG C                                
         MVC   HEAD7+1(L'MYHEAD7-1),MYHEAD7   OPTION 4 AND 5 HEADER             
         CLI   RCSUBPRG,0                                                       
         BNE   *+16                                                             
         MVC   HEAD9+31(L'MYSTAT),MYSTAT                                        
         MVC   HEAD10+31(L'MYSTAT),MYSTAT2                                      
*                                                                               
         CLI   RCSUBPRG,4          UNBILLED INTERNAL CHARGES                    
         BE    BOX1                                                             
         CLI   RCSUBPRG,5          INPUT TYPE REPORT                            
         BE    BOX1                                                             
*                                                                               
         L     R6,=A(PATTERN)      SET UP HEADLINES                             
         USING PATTERD,R6                                                       
         CLI   QOPT5,C' '                                                       
         BE    HED10                                                            
         MVC   HEAD5+51(9),TIMEONLY                                             
         CLI   QOPT5,C'T'                                                       
         BE    HED10                                                            
         MVC   HEAD5+47(18),OOPSRUN                                             
HED10    CLI   PROFILES+8,C'Y'                                                  
         BNE   HED20                                                            
         MVC   HEAD5+1(24),TRANAGE                                              
HED20    CLI   QOPT2,C'T'                                                       
         BE    HED30                                                            
         MVC   HEAD1+44(24),JAR    JOB AGEING REPORT                            
*&&UK                                                                           
         MVC   HEAD9+49(60),PATA                                                
         MVC   HEAD10+49(60),PATB                                               
*&&                                                                             
*&&US                                                                           
         MVC   HEAD9+44(65),PATA                                                
         MVC   HEAD10+44(65),PATB                                               
*&&                                                                             
         CLI   AGEMETH,C'U'                                                     
         BNE   *+10                                                             
         MVC   HEAD3+42(28),UNBIAGE                                             
         CLI   AGEMETH,C'O'                                                     
         BNE   *+10                                                             
         MVC   HEAD3+44(24),OPENAGE                                             
         CLI   AGEMETH,C'F'                                                     
         BNE   *+10                                                             
         MVC   HEAD3+47(18),FIFOAGE                                             
         CLI   AGEMETH,C'A'                                                     
         BNE   *+10                                                             
         MVC   HEAD3+40(30),MOSAGE                                              
HED30    CLI   QOPT2,C'T'                                                       
         BNE   HED32                                                            
         MVC   HEAD1+44(24),PRODTB PRODUCTION TRIAL BALANCE                     
*&&UK                                                                           
         MVC   HEAD9+49(60),PATC                                                
         MVC   HEAD10+49(60),PATD                                               
*&&                                                                             
*&&US                                                                           
         MVC   HEAD9+44(65),PATC                                                
         MVC   HEAD10+44(65),PATD                                               
*&&                                                                             
         MVC   HEAD3+45(16),EMON   EFFECTIVE MONTH-                             
         MVC   HEAD3+61(6),EFFMON                                               
*                                                                               
         USING ACMD,R7                                                          
HED32    L     R7,AMONACC                                                       
         CLI   ACMMEND,X'FF'                                                    
         BNE   HED40                                                            
         CLC   ACMCMSTR,SPACES     ANY START DATE?                              
         BNE   *+14                YES                                          
         CLC   ACMCMEND,SPACES     NO, SEE IF ANY END                           
         BE    HED40                                                            
         MVC   HEAD3+73(9),=C'POSTINGS '                                        
         CLC   ACMCMSTR,SPACES                                                  
         BNE   HED34                                                            
         MVC   HEAD3+82(5),=C'THRU '                                            
         MVC   HEAD3+87(L'ACMCMEND),ACMCMEND                                    
         B     HED40                                                            
*                                                                               
HED34    MVC   HEAD3+82(4),=C'FOR '                                             
         MVC   HEAD3+86(L'ACMCMSTR),ACMCMSTR                                    
         CLC   ACMMSTR,MYMEND                                                   
         BNE   HED36                                                            
         MVC   HEAD3+86+L'ACMCMSTR+1(4),=C'ONLY'                                
         B     HED40                                                            
*                                                                               
HED36    MVC   HEAD3+86+L'ACMCMSTR+1(5),=C'THRU '                               
         MVC   HEAD3+86+L'ACMCMSTR+6(L'ACMCMEND),ACMCMEND                       
         DROP  R7                                                               
*                                                                               
HED40    CLI   QXJOB,C' '                                                       
         BE    HED50                                                            
*                                                                               
         CLI   QXJOB,C'Y'                                                       
         BNE   HED45                                                            
         MVC   HEAD6+43(25),XJINC                                               
         B     HED50                                                            
*                                                                               
HED45    CLI   QXJOB,C'O'                                                       
         BNE   HED50                                                            
         MVC   HEAD6+47(17),XJONLY                                              
         B     HED50                                                            
*                                                                               
HED50    CLI   MODE,LEDGLAST                                                    
         BE    HED60                                                            
         CLI   MODE,REQLAST                                                     
         BE    HED70                                                            
         MVC   HEAD6+73(12),SUMMODE    SUMMARY MODE                             
         MVI   RCSUBPRG,3                                                       
         CLI   QOPT1,C'S'                                                       
         BE    BOX1                                                             
         MVC   HEAD6+73(12),SPACES                                              
*                                                                               
         MVC   HEAD5+70(62),SPACES                                              
         MVC   HEAD5+73(6),=C'CLIENT'   MOVE CLIENT NAME INTO HEADLINE          
         L     R2,ADHEIRA                                                       
         L     R4,ADLVANAM                                                      
         MVC   HEAD5+80(6),3(R2)                                                
         BAS   RE,GETNAME                                                       
         MVC   HEAD6+73(36),WORK                                                
         MVI   RCSUBPRG,0                                                       
         B     BOX1                                                             
*                                                                               
HED60    MVI   RCSUBPRG,1                                                       
         MVC   HEAD5+73(14),MEDANAL                                             
         B     BOX1                                                             
*                                                                               
HED70    MVI   RCSUBPRG,2                                                       
*&&UK*&& MVC   HEAD5+73(13),UNOANAL                                             
*&&US*&& MVC   HEAD5+73(15),UNOANAL                                             
         TM    PRTSTA,CLIENT                                                    
         BZ    BOX1                                                             
         MVC   HEAD5+73(15),CSUM                                                
         MVI   RCSUBPRG,3                                                       
*                                                                               
BOX1     L     R7,ADBOX                                                         
         USING BOXD,R7                                                          
         MVC   MYCOL,SPACES                                                     
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+7,C'T'        SET ROWS                                     
         MVI   MYROW+10,C'M'                                                    
         MVI   MYROW+60,C'B'                                                    
         MVI   MYCOL,C'L'          SET LH MARGIN                                
*                                                                               
*                                  FIND SPROG                                   
         CLI   RCSUBPRG,0                                                       
         BE    HOOK0                                                            
         CLI   RCSUBPRG,3                                                       
         BL    HOOK12                                                           
         BE    HOOK3               SUMMARY MODE                                 
         CLI   RCSUBPRG,5                                                       
         BL    HOOK4                                                            
         BE    HOOK5                                                            
         BH    HOOKX                                                            
*                                                                               
HOOK0    MVI   MYCOL+30,C'C'       SPROG 0                                      
         B     *+8                                                              
HOOK12   MVI   MYCOL+44,C'C'       SPROG 1,2                                    
HOOK3    LA    R1,5                SPROG 3                                      
         LA    RF,MYCOL+44                                                      
HOOK0123 MVI   0(RF),C'C'                                                       
         LA    RF,13(RF)                                                        
         BCT   R1,HOOK0123                                                      
         MVI   0(RF),C'R'                                                       
         B     HOOKX                                                            
*                                                                               
HOOK4    MVI   MYCOL+45,C'C'       SPROG 4                                      
         MVI   MYCOL+56,C'R'                                                    
         B     HOOKX                                                            
*                                                                               
HOOK5    MVI   MYCOL+13,C'C'       SPROG 5                                      
         MVI   MYCOL+28,C'R'                                                    
*                                                                               
HOOKX    MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
         XMOD1 1                                                                
*                                                                               
SAVERC   DC    A(0)                                                             
         LTORG                                                                  
*&&                                                                             
*                                                                               
TYPETAB  DC    F'0'                NUMBER IN BINSRCH TABLE                      
         DC    F'600'              MAX                                          
         DS    600CL14                                                          
*                                                                               
TIMETAB  DC    F'0'                NUMBER IN BINSRCH TABLE                      
         DC    F'600'              MAX                                          
TIMTABST EQU   *                                                                
         DS    (TIMELEN)CL600      INVNO(6)+1 BYTE STATUS+ PL6 AMOUNT           
TIMTABLN EQU   *-TIMTABST                                                       
*                                                                               
OCRTAB   DC    F'0'                NUMBER IN BINSRCH TABLE                      
         DC    F'800'              MAX                                          
OCRTABST EQU   *                                                                
         DS    800CL(CRLEN)                                                     
OCRTABLN EQU   *-OCRTABST                                                       
*                                                                               
TBLTAB   DC    F'0'                NUMBER IN BINSRCH TABLE                      
         DC    F'600'              MAX                                          
TBLTABST EQU   *                                                                
         DS    (CRLEN)CL600                                                     
TBLTABLN EQU   *-TBLTABST                                                       
*                                                                               
BILLTAB  DC    F'0'                NUMBER IN BINSRCH TABLE                      
         DC    F'3000'             MAX                                          
BILTABST EQU   *                                                                
         DS    (TIMELEN)CL3000                                                  
BILTABLN EQU   *-BILTABST                                                       
*                                                                               
UNITLIST DS    CL(256*2)                                                        
         ORG   UNITLIST                                                         
         DC    C'A B C D E F G H I J K L M N O P Q R S T U V W X Y Z '          
         DC    C'1 2 3 4 5 6 7 8 9   '                                          
         ORG   UNITLIST+L'UNITLIST                                              
*                                                                               
IOAREA   DS    (4000)C                                                          
*                                                                               
OFFTAB   DS    (OFFTABNM*OFFTABLN)C                                             
OFFTABNM EQU   2000                                                             
*                                                                               
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*--------------------------------------------------------------------*          
MAINTAB  DS    0F                                                               
         DC    S(VBLOCKAC)                                                      
         DC    H'0'                                                             
         DC    A(0)                                                             
         DC    A(PROLSIZE)                                                      
*                                                                               
         DC    S(AASOFBLS)         ACASOFS TABLE TO READ BILLS ONTO             
         DC    H'0'                                                             
         DC    A(0)                                                             
         DC    A(ACABTBSZ)                                                      
*                                                                               
MAINNUM  EQU   (*-MAINTAB)/MAINLEN                                              
*                                                                               
PROLSIZE EQU   8+30+(258*37*30)                                                 
*        DS    D                     PROLLER HEADER                             
*        DS    CL30                  WORK ACCUMULATOR (FOR CURRENT JOB)         
*        DS    (258*37)CL30          255 OFFICES*37 MEDIA+JOB,CLI&PROD          
*                                    (5 COLS*6 BYTES PACKED)                    
*                                    FOR EACH OFFICE WE STORE A TOTAL           
*                                    AND A SUB TOTAL FOR EACH MEDIA             
*                                    OFFICES AND PRODUCT, CLIENT AND            
*                                    COMPANY TOTALS                             
*                                                                               
BUFSIZE  EQU   PROLSIZE+ACABTBSZ                                                
         EJECT                                                                  
*                                  AGEING HEADERS                               
PATTERN  DS    0C                                                               
*&&UK                                                                           
         DC    CL12' '                                                          
         DC    CL12' '                                                          
         DC    CL12' '                                                          
         DC    CL12' '                                                          
         DC    C'  CUMULATIVE'                                                  
*                                                                               
         DC    C'   AND AFTER'                                                  
         DC    C'     ---    '                                                  
         DC    C'     ---    '                                                  
         DC    C'   AND PRIOR'                                                  
         DC    C'  ----------'                                                  
*                                                                               
         DC    C'     OPENING'                                                  
         DC    C'    EXTERNAL'                                                  
         DC    C'    INTERNAL'                                                  
         DC    C'     BILLING'                                                  
         DC    C'     CLOSING'                                                  
*                                                                               
         DC    C'     BALANCE'                                                  
         DC    C'     CHARGES'                                                  
         DC    C'     CHARGES'                                                  
         DC    C'     -------'                                                  
         DC    C'     BALANCE'                                                  
*&&                                                                             
*&&US                                                                           
         DC    CL13' '                                                          
         DC    CL13' '                                                          
         DC    CL13' '                                                          
         DC    CL13' '                                                          
         DC    C'  CUMULATIVE '                                                 
*                                                                               
         DC    C'   AND AFTER '                                                 
         DC    C'             '                                                 
         DC    C'             '                                                 
         DC    C'   AND PRIOR '                                                 
         DC    C'             '                                                 
*                                                                               
         DC    C'    OPENING  '                                                 
         DC    C'   EXTERNAL  '                                                 
         DC    C'   INTERNAL  '                                                 
         DC    C'    BILLING  '                                                 
         DC    C'    CLOSING  '                                                 
*                                                                               
         DC    C'    BALANCE  '                                                 
         DC    C'    CHARGES  '                                                 
         DC    C'    CHARGES  '                                                 
         DC    C'             '                                                 
         DC    C'    BALANCE  '                                                 
*&&                                                                             
         DC    C'TIME ONLY'                                                     
         DC    C'OUT-OF-POCKET ONLY'                                            
         DC    C'UNBILLED ITEMS AGEING METHOD'                                  
         DC    C'OPEN ITEMS AGEING METHOD'                                      
         DC    C'FIFO AGEING METHOD'                                            
         DC    C'DATE-OF-ACTIVITY AGEING METHOD'                                
         DC    C'AGED BY TRANSACTION DATE'                                      
         DC    C'PRODUCTION TRIAL BALANCE'                                      
         DC    CL24'   JOB AGEING REPORT'                                       
         DC    CL15'CLIENT SUMMARY'                                             
         DC    C'MEDIA ANALYSIS'                                                
*&&UK*&& DC    C'UNIT ANALYSIS'                                                 
*&&US*&& DC    C'OFFICE ANALYSIS'                                               
         DC    C'EFFECTIVE MONTH-'                                              
         DC    C'SUMMARY MODE'                                                  
         DC    C'JOBS WITH DEBIT BALANCES'                                      
         DC    C'JOBS WITH CREDIT BALANCES'                                     
         DC    C'TOTAL WORK IN PROGRESS'                                        
         DC    C'AND'                                                           
         DC    C'JOBS WITH'                                                     
         DC    C'NO ESTIMATE BUT NEED ONE TO BILL'                              
         DC    C'AN APPROVED ESTIMATE'                                          
         DC    C'AN UNAPPROVED ESTIMATE'                                        
         DC    C'AN ESTIMATE AWAITING APPROVAL'                                 
         DC    C'OFF/BT/EST/ST'                                                 
         DC    C' BT  CURRENT '                                                 
         DC    C'     ESTIMATE'                                                 
         DC    C' BT CURRENT R'                                                 
         DC    C'    HIGHEST R'                                                 
         DC    C'EXPENSE JOBS ONLY'                                             
         DC    C'EXPENSE JOBS INCLUDED (*)'                                     
         DC    C'CLIENT CHARGES      '                                          
         DC    C'PRODUCT CHARGES     '                                          
         DC    C'PRODUCTION CHARGES  '                                          
         EJECT                                                                  
BUILDPTA NMOD1 0,*BUIL*                                                         
         L     RA,0(R1)            RA IS P1                                     
         L     RC,4(R1)            RC IS P2                                     
         L     R6,=A(PATTERN)                                                   
         USING PATTERD,R6                                                       
*&&UK                                                                           
         GOTO1 DATCON,DMCB,(5,0),(0,WORK+6)                                     
*&&                                                                             
*&&US                                                                           
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK+6)                                
*&&                                                                             
         MVC   WORK+10(2),=C'01'                                                
         CLC   QEND,SPACES                     END DATE SPECIFIED               
         BE    BPAA                            NO, USE THIS MONTH               
         MVC   WORK+6(4),QEND                                                   
*                                                                               
BPAA     GOTO1 DATCON,DMCB,(0,WORK+6),(6,PATA+5)    END DATE                    
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,MNTH4)                                 
*        GOTO1 =V(HEXIN),DMCB,WORK+6,MNTH4,4                                    
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+6,WORK,F'-80' BACK 3 MONTHS FOR START            
         CLC   QSTART,SPACES                                                    
         BE    *+20                                                             
         CLC   QSTART(4),WORK                                                   
         BH    *+10                                                             
         MVC   WORK(4),QSTART                 MUST BE AT LEAST 3 MONTHS         
         MVC   WORK+4(2),=C'01'                                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(6,PATA+41)   START                         
         GOTO1 DATCON,DMCB,(0,WORK),(1,MNTH1)                                   
*        GOTO1 =V(HEXIN),DMCB,WORK,MNTH1,4                                      
*                                                                               
         MVC   WORK+18(6),WORK                SAVE START                        
         XR    R3,R3                                                            
*        L     R6,=F'35'                                                        
*&&UK                                                                           
         MVC   PATB+12(12),=C'     ------ '                                     
         MVC   PATB+24(12),PATB+12                                              
*&&                                                                             
*&&US*&& MVC   PATB+13(26),SPACES                                               
*                                                                               
BPA1     GOTO1 ADDAY,DMCB,WORK,WORK+12,F'35'                                    
         CLC   WORK+12(4),WORK+6                                                
         BE    BPA2                                                             
         LA    R3,1(R3)                       COUNT MONTHS TO END               
         MVC   WORK(4),WORK+12                                                  
         MVC   WORK+4(2),=C'01'                                                 
         B     BPA1                                                             
*                                                                               
BPA2     MVC   WORK(6),WORK+18                START                             
         XR    R2,R2                                                            
         D     R2,=F'2'                       R3 NUMBER COLUMN 3                
         AR    R2,R3                          R2 NUMBER IN COLUMN 2             
*                                                                               
*&&UK*&& LA    R4,PATA+24                                                       
*&&US*&& LA    R4,PATA+26                                                       
         LA    R7,2                                                             
         LA    R5,MNTH2                                                         
BPA3     GOTO1 ADDAY,DMCB,WORK,WORK+18,F'35' START OF NEXT MONTH                
         MVC   WORK+22(2),=C'01'                                                
         GOTO1 DATCON,DMCB,(0,WORK+18),(6,5(R4))                                
         GOTO1 DATCON,DMCB,(0,WORK+18),(1,(R5))                                 
*        GOTO1 =V(HEXIN),DMCB,WORK+18,(R5),4                                    
         BCT   R2,*+8                                                           
         B     BPA5                                                             
*                                                                               
*&&UK*&& MVC   60(5,R4),=C'  TO '                                               
*&&US*&& MVC   65(5,R4),=C'  TO '                                               
BPA4     GOTO1 ADDAY,DMCB,WORK+18,WORK,F'35'                                    
         MVC   WORK+18(4),WORK                GET END MONTH                     
         MVC   WORK+22(2),=C'01'              FOR COLUMN 2 + 3                  
         BCT   R2,BPA4                                                          
*&&UK*&& GOTO1 DATCON,DMCB,(0,WORK+18),(6,65(R4))                               
*&&US*&& GOTO1 DATCON,DMCB,(0,WORK+18),(6,70(R4))                               
         GOTO1 DATCON,DMCB,(0,WORK+18),(1,(R5))                                 
*        GOTO1 =V(HEXIN),DMCB,WORK+18,(R5),4                                    
*                                                                               
BPA5     MVC   WORK(6),WORK+18                                                  
*&&UK*&& SH    R4,=H'12'                                                        
*&&US*&& SH    R4,=H'13'                                                        
         LA    R5,3(R5)                                                         
         LR    R2,R3                          COLUMN 3                          
         BCT   R7,BPA3                                                          
         XIT1                                                                   
         EJECT                                                                  
*              PRINT TYPE TABLE                                                 
PRTTYPE  NMOD1 0,*PRTY*                                                         
         L     RA,0(R1)            RA IS P1                                     
         L     RC,4(R1)            RC IS P2                                     
         L     R3,8(R1)            R3 IS P3                                     
         CLI   QOPT1,C'S'                                                       
         BNE   FCHD                                                             
         CLI   MODE,REQLAST                                                     
         BE    FCHD                                                             
*&&US                                                                           
         CLI   QOPT2,C'T'          IF TRIAL BALANCE/SUMMARY MODE                
         BNE   *+8                                                              
         MVI   BXSW,C'Y'           THEN SET TO SWITCH BOXES                     
*&&                                                                             
         B     NOFCHD                                                           
FCHD     MVI   FORCEHED,C'Y'                                                    
NOFCHD   L     R2,=A(TYPETAB)                                                   
         L     R5,0(R2)            NUMBER IN TABLE                              
         LTR   R5,R5               TABLE EMPTY                                  
         BZ    PRXIT               YES                                          
*                                                                               
         MVI   RCSUBPRG,5          NO, SET UP HEADER INFO                       
*&&US*&& MVI   BXSW,C'N'                                                        
*                                                                               
         MVC   HEAD1+44(24),=C'PRODUCTION TRIAL BALANCE'                        
         MVC   HEAD2+44(24),=24C'-'                                             
         MVC   HEAD5+46(21),=C'THIS MONTHS ADDITIONS'                           
         MVC   HEAD3+45(16),=C'EFFECTIVE MONTH-'                                
         MVC   HEAD3+61(6),EFFMON                                               
         MVC   HEAD6+73(12),SPACES                                              
*                                                                               
         CLI   MODE,REQLAST                                                     
         BE    PRTT1                                                            
         MVC   HEAD5+70(12),SPACES                                              
         MVC   HEAD6+73(36),SPACES                                              
         MVC   HEAD5+73(6),=C'CLIENT'   MOVE CLIENT NAME INTO HEADLINE          
         L     R2,ADHEIRA                                                       
         MVC   HEAD5+80(6),3(R2)                                                
         L     R4,ADLVANAM                                                      
         USING ACNAMED,R4          R4 POINTS TO THE NAME ELEMENT                
         ZIC   R5,ACNMLEN                                                       
         SH    R5,=H'3'                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   HEAD6+73(0),ACNMNAME                                             
*                                                                               
PRTT1    L     R2,=A(TYPETAB)                                                   
         L     R5,0(R2)            NUMBER IN TABLE                              
*&&US*&& MVI   HEAD10+1,X'00'                                                   
         MVI   FSTSW,C'Y'                                                       
         LA    R2,8(R2)                                                         
         LA    R4,2(R2)                                                         
         LTR   R3,R3                                                            
         BZ    *+8                                                              
         LA    R4,6(R4)                                                         
*                                                                               
PRTT2    DS    0H                                                               
         CP    0(6,R4),=P'0'                                                    
         BE    PRTT4                                                            
         CLI   QOPT1,C'S'                                                       
         BNE   EDITIT                                                           
         CLI   FSTSW,C'Y'                                                       
         BNE   EDITIT                                                           
         MVI   FSTSW,C'N'                                                       
         CLI   MODE,REQLAST                                                     
         BE    EDITIT                                                           
         CLI   FORCEHED,C'Y'                                                    
         BE    EDITIT                                                           
*                                                                               
         BAS   RE,PRREPORT                                                      
*&&US                                                                           
         CLI   BXSW,C'Y'                                                        
         BNE   *+12                                                             
         BAS   RE,BXBOT                                                         
         BAS   RE,BXTOP                                                         
*&&                                                                             
         MVC   P+29(21),=C'THIS MONTHS ADDITIONS'                               
         MVC   P+1(10),=C'INPUT TYPE'                                           
         MVC   P+20(6),=C'AMOUNT'                                               
         BAS   RE,PRREPORT                                                      
*&&US                                                                           
         MVI   SPACING,2                                                        
         MVI   P+1,X'00'                                                        
*&&                                                                             
*&&UK                                                                           
         MVC   P+1(10),=10C'-'                                                  
         MVC   P+20(6),=6C'-'                                                   
*&&                                                                             
         BAS   RE,PRREPORT                                                      
EDITIT   EDIT  (B1,0(R2)),(2,P+5)                                               
         EDIT  (P6,0(R4)),(12,P+16),2,MINUS=YES                                 
         CLI   1(R2),0                                                          
         BE    *+16                                                             
         MVC   P+8(3),=C'( )'                                                   
         MVC   P+9(1),1(R2)                                                     
         ZAP   0(6,R4),=P'0'                                                    
         BAS   RE,PRREPORT                                                      
PRTT4    LA    R2,14(R2)                                                        
         LA    R4,14(R4)                                                        
         BCT   R5,PRTT2                                                         
*                                                                               
PRTTX    MVC   HEAD1+44(24),SPACES                                              
         MVC   HEAD2+44(24),SPACES                                              
         MVC   HEAD5+73(21),SPACES                                              
         MVI   FORCEHED,C'Y'                                                    
PRXIT    XIT1                                                                   
*                                                                               
PRREPORT ST    RE,SAVERE                                                        
         GOTO1 ACREPORT                                                         
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
BXBOT    NTR1                      POP IN A BOTTOM AT END OF JOB                
         MVC   MYROW,SPACES                                                     
         MVI   MYROW+6,C'T'                                                     
         MVI   MYROW+9,C'M'                                                     
         ZIC   RF,LINE                                                          
         LA    RE,MYROW(RF)                                                     
         BCTR  RE,0                                                             
         MVI   0(RE),C'B'                                                       
         L     R7,ADBOX                                                         
         USING BOXD,R7                                                          
         MVC   BOXROWS,MYROW                                                    
         MVI   BOXINIT,0                                                        
         BAS   RE,PRREPORT                                                      
         B     PRXIT                                                            
*                                                                               
BXTOP    NTR1                      POP IN A TOP FOR START OF SUMMARY            
         MVC   MYROW,SPACES                                                     
         MVC   MYCOL,SPACES                                                     
         MVC   P,SPACES                                                         
         ZIC   RF,LINE                                                          
         LA    RE,MYROW(RF)                                                     
         BCTR  RE,0                                                             
         MVI   0(RE),C'T'                                                       
         MVI   3(RE),C'M'                                                       
         MVI   MYROW+56,C'B'                                                    
         MVI   MYCOL,C'L'                                                       
         MVI   MYCOL+13,C'C'                                                    
         MVI   MYCOL+28,C'R'                                                    
         L     R7,ADBOX                                                         
         USING BOXD,R7                                                          
         MVC   BOXROWS,MYROW                                                    
         MVC   BOXCOLS,MYCOL                                                    
         MVI   BOXINIT,0                                                        
         BAS   RE,PRREPORT                                                      
         B     PRXIT                                                            
FSTSW    DC    C'Y'                  USED IN PRINT ROUTINE                      
         EJECT                                                                  
         LTORG                                                                  
*              DSECT FOR MODULE                                                 
*                                                                               
AC6102D  DSECT                                                                  
WORKDUB  DS    D                                                                
ADBUFF   DS    A                   BUFFALO CSECT                                
ABUFF    DS    A                   ADRESS OF GETMAINED CORE                     
VBLOCKAC DS    A                   A(PROLLER TABLE)                             
AASOFBLS DS    V                   ACASOFS TABLE OF BILLS                       
AOFFTAB  DS    A                                                                
POSTBUCK DS    A                   ADDRESS OF THE BUCKETS TO POST INTO          
PROFILES DS    4F                  RETURNED PROFILE FROM GETPROF                
ADBOX    DS    F                                                                
SAVERE   DS    F                                                                
APROLBCK DS    F                                                                
*                                                                               
AC61DREQ EQU   *                   CLEAR AT REQFRST FROM HERE ON.               
                                                                                
HEADMON  DS    CL6                                                              
MYMEND   DS    CL2                                                              
POSTDATE DS    CL2                 DATE TO POST THIS TRAN WITH                  
POSTDADT DS    CL2                 DA DATE OF THIS TRAN (IF NEEDED)             
BINDTE   DS    XL2                 TRNSDATE                                     
*                                                                               
HIREVEST DS    PL8                                                              
HIREV    DS    CL1                                                              
CUREST   DS    CL1                                                              
TPUTSTAT DS    CL1                                                              
*                                                                               
COMMAND  DS    CL8                 COMMAND FOR BUFFALO                          
MYKEY    DS    CL49                KEY FOR DATAMRG INTERFDACE                   
BUFREC   DS    0CL90                                                            
BUFKEY   DS    CL14                                                             
BUFCOM   DS    CL36                                                             
BUFAMNT1 DS    PL8                                                              
BUFAMNT2 DS    PL8                                                              
BUFAMNT3 DS    PL8                                                              
BUFAMNT4 DS    PL8                                                              
BUFAMNT5 DS    PL8                                                              
*                                                                               
MEDIANAM DS    555C                36 MEDIA PLUS 1 ALL MEDIA                    
MEDIALST DS    CL37                                                             
THISMED  DS    CL1                                                              
THISUNIT DS    CL2                                                              
SAVEUNIT DS    CL2                 CONTROL BREAKS IN CLISUM REPORT              
MEDNUM   DS    CL1                 RELATIVE MEDIA NUMBER OF CURRENT JOB         
UNITNUM  DS    H                   RELATIVE UNIT NUMBER OF CURRENT JOB          
ACTIVITY DS    CL1                                                              
TOTSONLY DS    CL1                                                              
UNBCALL  DS    CL1                 AM I CALLING TIMEPUT FROM UNBINT             
OFFSTAT  DS    CL1                                                              
STATUS   DS    CL12                                                             
SAVEPRNT DS    CL132                                                            
MYHEAD7  DS    CL132               OPTION 4&5 PRINT (SQUASHED)                  
MYSTAT   DS    CL13                STATUS HEADER                                
MYSTAT2  DS    CL13                STATUS HEADER 2                              
PRODSW   DS    C                                                                
PRODNAME DS    CL60                                                             
PRODISP  DS    H                                                                
RSUMSW   DS    CL1                                                              
PROFLAG  DS    CL1                                                              
*                                                                               
MNTH1    DS    CL3              X'YYMM' MONTH OF SERVICE                        
MNTH2    DS    CL3                                                              
MNTH3    DS    CL3                                                              
MNTH4    DS    CL3                                                              
*                                                                               
AGEMETH  DS    CL1                 AGEING METHOD FROM PROFILE OR CARD           
EFFMON   DS    CL6                 EFFECIVE MONTH FOR TRIAL BAL HEADER          
EFFMOS   DS    CL2                 EFFECTIVE MOS FOR TRIAL BAL                  
RECKEY   DS    CL2                 BINSRCH RECORD                               
RECCLI   DS    PL6                                                              
RECREQ   DS    PL6                                                              
*                                                                               
TIMEKEY  DS    0CL10               BINSRCH RECORD FOR TIME TABLE                
TIMEINV  DS    CL6                   INVOICE NUMBER                             
TIMERUN  DS    XL2                   RUN DATE                                   
TIMEBIL  DS    XL2                   BILL DATE                                  
TIMKYLEN EQU   *-TIMEKEY             KEY LENGTH                                 
TIMEAMNT DS    PL6                                                              
TIMELEN  EQU   *-TIMEKEY             RECORD LENGTH                              
*                                                                               
*                                                                               
CRKEY    DS    0CL3                BINSRCH RECORD FOR OPEN CREDIT TAB           
CRDADT   DS    CL2                  DA DATE (COMPRESSED)                        
CRCTR    DS    CL2                  COUNTER TO KEEP BILLS UNIQUE                
CRKYLEN EQU    *-CRKEY              KEY LENGTH                                  
CRMNTH   DS    CL2                  MONTH FOR POSTING                           
CRAMNT   DS    PL6                  AMOUNT TO POST                              
CRLEN    EQU   *-CRKEY              RECORD LENGTH                               
*                                                                               
MYROW    DS    CL100                                                            
MYCOL    DS    CL132                                                            
BXSW     DS    CL1                                                              
ELCODE   DS    CL1                                                              
TIMESAVE DS    PL6                   AMNOUNT OF TIME NOT INCLUDED               
POSTTRAN DS    PL6                                                              
TRANAMNT DS    PL6                 TRNSAMNT OR MEMO IF XJOB                     
JOBBAL   DS    PL6                   NET OF TRANSACTIONS ACCEPTED               
SIX      DS    CL6                                                              
*                                                                               
TRANSTAT DS    CL1                                                              
NEGATIVE EQU   1                                                                
CREDIT   EQU   2                                                                
TIME     EQU   4                  TRANSACTION IS CLIENT TIME                    
*                                                                               
PRTSTA   DS    CL1                                                              
CLIENT   EQU   1                                                                
DR       EQU   2                                                                
NEWCLI   EQU   4                                                                
NEWOFF   EQU   8                                                                
LASTCLI  EQU   16                                                               
PRTED    EQU   32                                                               
*                                                                               
REQSTAT  DS    CL1                                                              
OFFICE   EQU   1                                                                
*                                                                               
SUBPSTAT DS    CL1                 STATUS FOR SUBPRT ROUTINE                    
NOCOUNT  EQU   1                                                                
*                                                                               
SWAPSTAT DS    CL1                                                              
OFFCNT   DS    CL1                                                              
OFFSAVE  DS    CL1                  CLIENT LEVEL OFFICE                         
ULSAVE   DS    CL2                                                              
ULFLAG   DS    CL1                                                              
ULTOT    DS    CL8                                                              
XJOB     DS    CL1                 XJOB STATUS                                  
XJ       EQU   1                   CURRENT JOB IS AN XJOB                       
XJINPRO  EQU   2                   PRODUCT CONTAINS X JOBS                      
XJINCLI  EQU   4                   DITTO FOR CLIENT                             
XJINREQ  EQU   8                   AND REQUEST                                  
*                                                                               
BUCKS    DS    0C                    BUCKET AREA (WILL BE LOOP ZAPPED)          
CLITOTS  DS    CL30                  CLIENT TOTALS FOR SUMMARY                  
REETOTS  DS    CL30                  REQUEST TOTALS FOR SUMMARY                 
DEBS     DS    CL30                  PACKED BUCKETS FOR AGEING                  
OPENDEBS DS    CL30                  FOR AGEING OPTION=O                        
CRDS     DS    CL30                                                             
OPENCRDS DS    CL30                  FOR AGEING OPTION=O                        
TOTBILLS DS    CL30                  OPEN CRDS REVERSED BY A TOTAL BILL         
JOBBUCKS EQU   (*-BUCKS)/6          NUMBER OF 6 BYTE PACKED JOB BUCKETS         
*                                                                               
PROXJOB  DS    CL30                                                             
PROBUCKS EQU   (*-PROXJOB)/6                                                    
*                                                                               
CLICRDS  DS    CL30                                                             
CLIDEBS  DS    CL30                                                             
CLIXJOB  DS    CL30                                                             
CLIBUCKS EQU   (*-CLICRDS)/6                                                    
*                                                                               
REQCRDS  DS    CL30                                                             
REQDEBS  DS    CL30                                                             
REQXJOB  DS    CL30                                                             
SUBTOTS  DS    CL30                #CRD JOBS/CLI, #DEB JOBS/CLI,                
REQBUCKS EQU   (*-REQCRDS)/6                                                    
*                                       "    COMP    '      COMP &SPARE         
NUMBUCKS EQU   (*-BUCKS)/6          NUMBER OF 6 BYTE PACKED BUCKETS             
BUCKLN   EQU   6                                                                
*                                                                               
ASOFBLK  DS    CL(ACASOFLN)                                                     
*                                                                               
A61DLEN  EQU   *-AC6102D                                                        
A61DLN2  EQU   *-AC61DREQ                                                       
*                                                                               
         EJECT                                                                  
CLIKEYD  DSECT                     TO COVER CLIENT SUMMARY BUFFALO KEY          
CLITAG   DS    CL1                 X'FF' IF ITS A CLIENT SUMMARY RECORD         
CLIOFF   DS    CL2                 OFFICE                                       
CLICODE  DS    CL3                 CLIENT CODE                                  
CLITYPE  DS    CL1                 DEBIT OR CREDIT BUCKETS                      
CLINUM   DS    PL4                 NUMBER OF JOBS REPRESENTED                   
         DS    CL2                 SPARE                                        
*                                                                               
OFFTABD  DSECT                     TO COVER TABLE OF OFFSET ITEMS               
OFFREF   DS    CL6                                                              
OFFDATE  DS    CL3                 OFFICE                                       
OFFMNTH  DS    CL2                 YYMM                                         
OFFAMNT  DS    PL8                                                              
OFFTABLN EQU   *-OFFTABD                                                        
*                                                                               
PATTERD  DSECT                     TO COVER PATTERN SPACE                       
*&&UK                                                                           
PATA     DC    CL12' '                                                          
         DC    CL12' '                                                          
         DC    CL12' '                                                          
         DC    CL12' '                                                          
         DC    C'  CUMULATIVE'                                                  
*                                                                               
PATB     DC    C'   AND AFTER'                                                  
         DC    C'     ---    '                                                  
         DC    C'     ---    '                                                  
         DC    C'   AND PRIOR'                                                  
         DC    C'  ----------'                                                  
*                                                                               
PATC     DC    C'     OPENING'                                                  
         DC    C'    EXTERNAL'                                                  
         DC    C'    INTERNAL'                                                  
         DC    C'     BILLING'                                                  
         DC    C'     CLOSING'                                                  
*                                                                               
PATD     DC    C'     BALANCE'                                                  
         DC    C'     CHARGES'                                                  
         DC    C'     CHARGES'                                                  
         DC    C'     -------'                                                  
         DC    C'     BALANCE'                                                  
*&&                                                                             
*&&US                                                                           
PATA     DC    CL13' '                                                          
         DC    CL13' '                                                          
         DC    CL13' '                                                          
         DC    CL13' '                                                          
         DC    C'  CUMULATIVE '                                                 
*                                                                               
PATB     DC    C'   AND AFTER '                                                 
         DC    C'             '                                                 
         DC    C'             '                                                 
         DC    C'   AND PRIOR '                                                 
         DC    C'             '                                                 
*                                                                               
PATC     DC    C'    OPENING  '                                                 
         DC    C'   EXTERNAL  '                                                 
         DC    C'   INTERNAL  '                                                 
         DC    C'    BILLING  '                                                 
         DC    C'    CLOSING  '                                                 
*                                                                               
PATD     DC    C'    BALANCE  '                                                 
         DC    C'    CHARGES  '                                                 
         DC    C'    CHARGES  '                                                 
         DC    C'             '                                                 
         DC    C'    BALANCE  '                                                 
*&&                                                                             
TIMEONLY DC    C'TIME ONLY'                                                     
OOPSRUN  DC    C'OUT-OF-POCKET ONLY'                                            
UNBIAGE  DC    C'UNBILLED ITEMS AGEING METHOD'                                  
OPENAGE  DC    C'OPEN ITEMS AGEING METHOD'                                      
FIFOAGE  DC    C'FIFO AGEING METHOD'                                            
MOSAGE   DC    C'DATE-OF-ACTIVITY AGEING METHOD'                                
TRANAGE  DC    C'AGED BY TRANSACTION DATE'                                      
PRODTB   DC    C'PRODUCTION TRIAL BALANCE'                                      
JAR      DC    CL24'   JOB AGEING REPORT'                                       
CSUM     DC    CL15'CLIENT SUMMARY'                                             
MEDANAL  DC    C'MEDIA ANALYSIS'                                                
UNOANAL  EQU   *                                                                
*&&UK*&& DC    C'UNIT ANALYSIS'                                                 
*&&US*&& DC    C'OFFICE ANALYSIS'                                               
EMON     DC    C'EFFECTIVE MONTH-'                                              
SUMMODE  DC    C'SUMMARY MODE'                                                  
JOBWDEB  DC    C'JOBS WITH DEBIT BALANCES'                                      
JOBWCRD  DC    C'JOBS WITH CREDIT BALANCES'                                     
TWIP     DC    C'TOTAL WORK IN PROGRESS'                                        
AND      DS    C'AND'                                                           
JOBSW    DS    C'JOBS WITH'                                                     
NOEST    DS    C'NO ESTIMATE BUT NEED ONE TO BILL'                              
ANAEST   DS    C'AN APPROVED ESTIMATE'                                          
ANUEST   DS    C'AN UNAPPROVED ESTIMATE'                                        
ANESTAWA DS    C'AN ESTIMATE AWAITING APPROVAL'                                 
STATH0   DS    C'OFF/BT/EST/ST'                                                 
STATH2   DS    C' BT  CURRENT '                                                 
STATH3   DS    C'     ESTIMATE'                                                 
STATH4   DS    C' BT CURRENT R'                                                 
STATH5   DS    C'    HIGHEST R'                                                 
XJONLY   DS    C'EXPENSE JOBS ONLY'                                             
XJINC    DS    C'EXPENSE JOBS INCLUDED (*)'                                     
XJCLILIT DS    C'CLIENT CHARGES      '                                          
XJPROLIT DS    C'PRODUCT CHARGES     '                                          
XJREQLIT DS    C'PRODUCTION CHARGES  '                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
         DS    H                   SPACER FOR FULL ALLIGNMENT                   
MAINMAX  DS    A                                                                
MAINSIZE DS    A                                                                
MAINLEN  EQU   *-MAIND                                                          
*-------------------------------------------------------------------*           
GOBLOCKD DSECT                    FOR GETOPT (DSECT DEFINES GONEEDES)           
         PRINT OFF                                                              
       ++INCLUDE ACGOBLOCK                                                      
         PRINT ON                                                               
*DDBUFFALOD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDBUFFALOD                                                     
         PRINT ON                                                               
*ACREPWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*DDREPXTRAD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
*ACREPPROFD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACREPPROFD                                                     
         PRINT ON                                                               
*ACJOBBERD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACJOBBERD                                                      
         PRINT ON                                                               
*ACASOFD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACASOFD                                                        
         PRINT ON                                                               
         EJECT                                                                  
JBLOCKD  DSECT                                                                  
       ++INCLUDE ACJOBBLOCK                                                     
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067ACREP6102 04/23/15'                                      
         END                                                                    
