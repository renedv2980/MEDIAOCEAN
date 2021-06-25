*          DATA SET ACPRF03    AT LEVEL 009 AS OF 04/18/06                      
*PHASE T63003A                                                                  
*&&UK                                                                           
*INCLUDE ACPTAORD                                                               
*&&                                                                             
*&&US                                                                           
*INCLUDE CATCALL                                                                
*&&                                                                             
T63003   TITLE 'ACPRF03 - PRESTO - ORDER RES, EXPENSE ORER ANALYSIS'            
T63003   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACPRF03*,R7,RR=RE                                              
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     RF,=V(ACPTAORD)                                                  
         AR    RF,RE                                                            
         ST    RF,VPTAORD                                                       
         L     RF,=V(CATCALL)                                                   
         AR    RF,RE                                                            
         ST    RF,VCATCALL                                                      
*                                                                               
         MVI   FLAG,0                                                           
*                                                                               
         CLI   SVRCVEL+1,X'03'     ORDER RESERVATION                            
         BE    RCV03H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'04'     ORDER DELETE                                 
         BE    RCV04H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'06'     CHECK JOB STATUS FOR NEW ORDER               
         BE    RCV06H                                                           
*                                                                               
*&&US                                                                           
         CLI   SVRCVEL+1,X'09'     EXPENSE ANALYSIS CHECK                       
         BE    RCV09H                                                           
         CLI   SVRCVEL+1,X'10'     VALIDATE EXP ANALYSIS FIELDS                 
         BE    RCV10H                                                           
*&&                                                                             
         CLI   SVRCVEL+1,X'11'     DELETE ORDER RESERVATION                     
         BE    RCV11H                                                           
         B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 03 HEADER - ORDER RESERVATION                                         
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV03H   DS    0H                                                               
         BAS   RE,BLDKEY           BUILD ORDER RESERVATION KEY                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING OBRRECD,R6                                                       
         CLC   OBRKEY(OBRKEND),KEYSAVE COMPARE SIGNIFICANT DATA                 
         BNE   R03H10                                                           
*                                                                               
         GOTO1 GETREC              GET THE RESERVATION                          
         BAS   RE,TESTDUP          TEST FOR DUPLICATE REQUEST                   
         BE    SND03H              SAME SERVER MAKING EXACT REQUEST-OK          
         B     ERRDUP              ANOTHER SERVER MAKING DUP REQUEST            
*                                                                               
R03H10   GOTO1 BLDOKEY,CONTROL     READ ORDER CONTROL RECORD                    
         MVI   RDUPDATE,C'Y'       TO FORCE ORDER PROGRAM TO WAIT               
         GOTO1 READ                UNTIL THIS REQUEST COMPLETES                 
*                                                                               
         BAS   RE,BLDKEY                                                        
         XC    OBRKFRST,OBRKFRST   CLEAR FIRST NUMBER                           
         MVC   OBRKLAST,SVORSTR    PLUG START NUMBER INTO LAST IN KEY           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   OBRKEY(OBRKLAST-OBRKEY),KEYSAVE TEST SAME AGENCY                 
         BNE   R03H20              NO-CAN'T HAVE OVERLAP                        
         CLC   OBRKFRST,SVOREND    TEST IF FIRST > END OF REQUEST               
         BH    R03H20              NO OVERLAP                                   
*                                                                               
         B     ERROVER                                                          
*                                                                               
R03H20   GOTO1 BLDOKEY,SVORSTR     BUILD AN ORDER KEY W/ REQUEST START          
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         LA    R6,KEY                                                           
         USING ORDRECD,R6                                                       
         CLC   ORDKEY(ORDKORD-ORDKEY),KEYSAVE  TEST SAME COMPANY                
         BNE   R03H30              NO-ITS OK, NO OVERLAP                        
*                                                                               
         CLC   ORDKORD,SVOREND     TEST FIRST NUMBER VS. END                    
         BH    R03H30              PAST END-ITS OK                              
         CLC   ORDKORD,SVORSTR     IS ORDER RETURNED WITHIN RANGE               
         BNL   ERRORD                                                           
*                                                                               
R03H30   BAS   RE,ADDORES          ADD ORDER RESERVATION                        
*                                                                               
         B     SND03H              RETURN 03 HEADER TO PRESTO                   
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 04 HEADER - ORDER DELETE                                              
* LOGICAL DELETE ORDER RECORDS (X'20') & DELETE TRANSACTIONS (X'80')            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV04H   DS    0H                                                               
         XC    KEY,KEY             READ FOR ORDER KEY                           
         USING ORDRECD,R5                                                       
         LA    R5,KEY                                                           
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUL                                                      
         MVC   ORDKORD,SVORSTR                                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   ORDKEY,KEYSAVE      IF NOT FOUND THEN RETURN OK                  
         BNE   SND04H                                                           
*                                                                               
         TM    ORDKSTAT,ORDSDEL+ORDSLDEL  IF STATUS DELETE                      
         BNZ   SND04H                  THEN JUST SEND PRESTO OK TO DEL          
*                                                                               
         CLI   ORDKSTAT,0          IF STATUS NOT 'FULLY OPEN'                   
         BNE   ERRNFO                  THEN ERROR                               
*                                                                               
         L     R5,AIO              READ RECORD                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 GETELEM,DMCB,ACORDELQ                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ORDELD,R6                                                        
*                                                                               
*&&US*&& TM    ORDSTAT,ORDSPART    CHECK PARTIALLY PAID                         
*&&UK*&& TM    ORDSTAT,ORDSPART+ORDSMNUP  CHECK PARIAL + FULLY MATCHED          
         BNZ   ERRNFO              THEN ERROR                                   
*                                                                               
* CHECK IF AN EXPENSE ORDER                                                     
         GOTO1 GETELEM,DMCB,OAMELQ                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING OAMELD,R6                                                        
         CLC   OAMWORK,SPACES      IF WORKCODE IS SPACES                        
         BNE   *+8                                                              
         OI    FLAG,EXPORDER       THIS IS AN EXPENSE ORDER                     
*                                                                               
         LA    R5,KEY              POINT TO KEY                                 
         MVI   ORDKSTAT,ORDSLDEL   SET LOGICAL DELETE BIT                       
         GOTO1 WRITE               AND WRITE BACK KEY                           
*                                                                               
         L     R5,AIO              POINT TO RECORD                              
         MVI   ORDRSTAT,ORDSLDEL   SET LOGICAL DELETE BIT                       
*                                                                               
         LA    R6,BLOCK            ADD A DATE ELEMENT                           
         USING GDAELD,R6                                                        
         XC    BLOCK,BLOCK                                                      
         MVI   GDAEL,GDAELQ                                                     
         MVI   GDALN,GDALNQ                                                     
         MVI   GDATYPE,GDATOSTA                                                 
         MVC   GDADATE,TODAYP                                                   
         GOTO1 ADDELEM,DMCB,BLOCK                                               
*                                                                               
         GOTO1 PUTREC              WRITE BACK RECORD                            
*                                                                               
         GOTO1 GETELEM,DMCB,ACORDELQ                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ORDELD,R6                                                        
*                                                                               
         USING TRNRECD,R5                                                       
         LA    R5,KEY              READ FOR TRANSACTION KEY                     
         MVC   KEY,SPACES                                                       
         MVC   TRNKCULA,ORDJOB                                                  
*&&UK                                                                           
         CLC   CUL,TRNKCULA        IS IT PRODUCTION LEDGER                      
         BNE   *+10                                                             
*&&                                                                             
         MVC   TRNKWORK,=2C'*'                                                  
         MVC   TRNKCULC,ORDSUP                                                  
*&&US                                                                           
         TM    FLAG,EXPORDER       IF THIS IS AN EXPENSE ORDER                  
         BNO   *+10                                                             
         MVC   TRNKWORK(3),SPACES  CLEAR WORKCODE AND COMPANY CODE              
*&&                                                                             
         MVC   TRNKDATE,ORDDATE                                                 
         MVC   TRNKREF,SVORSTR                                                  
         MVI   TRNKSBR,0                                                        
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   TRNKEY,KEYSAVE      IF NOT FOUND THEN RETURN OK                  
         BNE   SND04H                                                           
*                                                                               
         MVI   TRNKSTAT,TRNSDELT   DELETE KEY                                   
         GOTO1 WRITE                                                            
*                                                                               
         L     R5,AIO              DELETE RECORD                                
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVI   TRNRSTAT,TRNSDELT   DELETE KEY                                   
         GOTO1 PUTREC                                                           
*&&UK*&& GOTO1 VPTAORD,DMCB,TRNRECD,(CTRY,ACOMFACS)                             
         B     SND04H                                                           
         DROP  R5                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* BUILD ORDER RESERVATION KEY                                                   
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BLDKEY   NTR1                                                                   
         USING OBRRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    OBRKEY,OBRKEY                                                    
         MVI   OBRKTYP,OBRKTYPQ                                                 
         MVC   OBRKCPY,CUL                                                      
         MVC   OBRKLAST,SVOREND                                                 
         MVC   OBRKFRST,SVORSTR                                                 
         B     XIT                                                              
         SPACE 2                                                                
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TEST FOR DUPLICATE RESERVATION REQUEST                                        
* AT ENTRY, AIO = AIO1 = A(RESERVATION)                                         
* RETURNS CC=EQ FOR SAME SERVER REQUESTING DUPLICATE                            
*         CC=NEQ FOR DIFFERENT SERVER REQUESTING DUPLICATE - REJECT             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
TESTDUP  NTR1  ,                                                                
         MVCDD WORK(10),AC#UNKWN    GET TRANSLATION FOR "UNKNOWN"               
         GOTO1 VDICTATE,DMCB,C'SU  ',WORK,0                                     
         MVC   DUPDATA,SPACES                                                   
*                                                                               
         GOTO1 GETELEM,DMCB,PACELQ                                              
         BNE   TDUP10                                                           
         USING PACELD,R6                                                        
         MVC   DUPPRS(L'PACPERS),PACPERS   EXTRACT PERSON                       
         CLC   DUPPRS,SPACES       IF DON'T HAVE PERSON                         
         BH    *+10                                                             
TDUP10   MVC   DUPPRS,WORK         THEN SEND "UNKNOWN"                          
*                                                                               
         MVI   BYTE,FFTTSQLI       SEARCH FOR SQL ID EL                         
         GOTO1 SRCHGET,DMCB,FFTELQ,(1,BYTE)                                     
         BNE   TDUP20                                                           
         USING FFTELD,R6                                                        
         ZIC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   DUPSRV(0),FFTDATA                                                
*                                                                               
         CLC   DUPSRV,SPACES       IF  DON'T HAVE SERVER NAME                   
         BH    *+10                                                             
TDUP20   MVC   DUPSRV(10),WORK     THEN SEND "UNKNOWN"                          
*                                                                               
         CLC   SVORSRV,DUPSRV      TEST SAME SERVER MAKING REQUEST              
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ADD ORDER RESERVATION RECORD                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
ADDORES  NTR1  ,                                                                
         BAS   RE,BLDKEY           BUILD ORDER RESERVATION KEY                  
         MVI   RDUPDATE,C'Y'        & CHECK FOR DELETED RESERVATION             
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         LA    R6,KEY                                                           
         USING OBRRECD,R6                                                       
         CLC   OBRKEY(OBRKEND),KEYSAVE COMPARE SIGNIFICANT DATA                 
         BNE   ADORES10                                                         
         TM    OBRKSTAT,OBRSDELT   TEST RESERVATION MARKED DELETED              
         BNO   ADDORESX                                                         
         OI    FLAG,RESDEL         RESERVATION MARKED DELETED                   
         NI    OBRKSTAT,X'FF'-OBRSDELT   UNDELETE                               
         GOTO1 WRITE                                                            
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC                                                           
*                                                                               
ADORES10 L     R6,AIO                                                           
         USING OBRRECD,R6                                                       
*                                                                               
         LR    RE,R6               CLEAR IO AREA                                
         LA    RF,2000                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    OBRKEY,OBRKEY       RECORD IS KEY ONLY                           
         MVI   OBRKTYP,OBRKTYPQ                                                 
         MVC   OBRKCPY,CUL                                                      
         MVC   OBRKLAST,SVOREND                                                 
         MVC   OBRKFRST,SVORSTR                                                 
         MVC   OBRRLEN,=Y(OBRRFST-OBRRECD+1)                                    
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R6,BLOCK            BUILD A PERSON BLOCK                         
         USING PACELD,R6                                                        
         MVI   PACEL,PACELQ                                                     
         MVI   PACLN,PACLNQ                                                     
         MVC   PACPERS,SVPRSN                                                   
         GOTO1 VDATCON,DMCB,(5,0),(1,PACDATE)                                   
         GOTO1 ADDELEM,DMCB,PACELD                                              
*                                                                               
         XC    BLOCK,BLOCK                                                      
         USING FFTELD,R6                                                        
         MVI   FFTEL,FFTELQ        BUILD A FREE FORM ELEMENT                    
         MVI   FFTTYPE,FFTTSQLI    WITH THE ORIGINATING SQL SERVER ID           
         MVC   FFTDATA(L'SVORSRV),SVORSRV                                       
         LA    R1,L'SVORSRV                                                     
         LA    RE,SVORSRV+L'SVORSRV-1                                           
         CLI   0(RE),C' '          FIND LAST SIGNIFICANT BYTE                   
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         STC   R1,FFTDLEN                                                       
         LA    R1,FFTDATA-FFTELD(R1) COMPUTE ELEMENT LENGTH                     
         STC   R1,FFTLN                                                         
         GOTO1 ADDELEM,DMCB,FFTELD                                              
*                                                                               
         TM    FLAG,RESDEL         TEST RESERVATION MARKED DELETED              
         BO    ADORES20                                                         
         GOTO1 ADDREC                                                           
         B     ADDORESX                                                         
*                                                                               
ADORES20 GOTO1 PUTREC                                                           
*                                                                               
ADDORESX B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* BUILD ORDER KEY FOR THE ORDER NUMBER AT 0(R1)                                 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
BLDOKEY  NTR1  ,                                                                
         USING ORDRECD,R6                                                       
         LA    R6,KEY                                                           
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUL                                                      
         MVC   ORDKORD,0(R1)                                                    
         B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 06 HEADER - CHECK JOB STATUS FOR NEW ORDER                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV06H   DS    0H                                                               
*                                                                               
         USING ACTRECD,R3          BUILD JOB KEY                                
         LA    R3,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCPY(3),CUL                                                   
         MVC   ACTKACT(12),CPJ                                                  
         OI    DMINBTS,X'08'       READ FOR DELETES                             
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'FF'-X'08' TURN OFF READ FOR DELETES                    
*                                                                               
         MVI   BYTE,C'D'           JOB DELETED                                  
         TM    ACTKSTAT,ACTSDELT   TEST JOB DELETED                             
         BO    SND06H                                                           
*                                                                               
         MVI   BYTE,C'C'           JOB CLOSED                                   
         TM    ACTKSTAT,ACTSCLOS   TEST JOB CLOSED                              
         BO    SND06H                                                           
*                                                                               
         MVI   BYTE,C'L'           JOB LOCKED                                   
         TM    ACTKSTAT,ACTSLOCK   TEST JOB LOCKED                              
         BO    SND06H                                                           
*                                                                               
         MVI   BYTE,C'0'           JOB IS OK TO ADD ORDER                       
SND06H   LA    R1,X'0006'          SEND RECORD HEADER                           
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,MCRTNCD          SEND RETURN CODE                             
         LA    R4,BYTE                                                          
         LA    R5,1                                                             
         BRAS  RE,SENDD                                                         
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*&&US                                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 09 HEADER - CHECK JOB STATUS FOR NEW ORDER                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV09H   DS    0H                                                               
*                                                                               
* VALIDATE 1P EXISTS                                                            
         MVC   ERROR,=Y(QMISS1P)                                                
         USING ACTRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(2),=C'1P'       1P ACCOUNT                               
         MVC   ACTKACT,=C'999999999999'                                         
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   SND09H                                                           
         DROP  R4                                                               
*                                                                               
* ASSUME ALL FIELDS ARE VALID UNTIL IT IS FOUND OTHERWISE                       
         MVI   SVVALEXP,OFFVAL+DEPTVAL+PERSVAL+CLTVAL+PRDVAL                    
*                                                                               
* IF OFFICE NOT REQUIRED THEN OFFICE, DEPT AND PERSON IS INVALID                
         TM    COMPSTA1,CPYSOROE   OFFICE REQUIRED?                             
         BO    *+8                 YES                                          
         NI    SVVALEXP,X'FF'-OFFVAL-DEPTVAL-PERSVAL                            
*                                                                               
* IF DOESN'T USE COST ACCT SYSTEMS, THEN CLIENT AND PRODUCT IS INVALID          
         TM    COMPSTA5,CPYSNCST   USES COST?                                   
         BO    RCV09H10            YES                                          
         NI    SVVALEXP,X'FF'-CLTVAL-PRDVAL                                     
         B     RCV09H30            DON'T NEED FURTHER TESTS ON CLT/PRD          
*                                                                               
         USING CATD,R1                                                          
RCV09H10 LA    R1,CATBLK           YES, GET COST SETTING FROM CATCALL           
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR                                                 
         MVC   CATSEAC(1),CUL                                                   
         MVC   CATSEAC+1(14),SVEXPACC                                           
         GOTO1 VCATCALL                                                         
         CLI   CATPST,C'Y'                                                      
         BE    RCV09H20                                                         
         NI    SVVALEXP,X'FF'-CLTVAL-PRDVAL                                     
         B     RCV09H30            CLIENT INV SO DON'T TEST PRODUCT             
         DROP  R1                                                               
*                                                                               
* IF PRODUCT NOT REQUIRED, THEN PRODUCT IS INVALID                              
RCV09H20 TM    COMPSTA5,CPYSEXPP   PRODUCT REQUIRED?                            
         BO    *+8                 YES                                          
         NI    SVVALEXP,X'FF'-PRDVAL                                            
*                                                                               
* READ EXPENSE ACCOUNT TO CHECK DEPT, PERSON, CLIENT                            
RCV09H30 MVC   KEY,SPACES                                                       
         MVC   KEY(1),CUL          COMPANY                                      
         MVC   KEY+1(14),SVEXPACC  EXPENSE ACCOUNT                              
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     COMPARE SIGNIFICANT DATA                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              GET RECORD                                   
*                                                                               
         USING RSTELD,R6                                                        
         GOTO1 GETELEM,DMCB,RSTELQ                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RSTSTAT1,RSTSEADD   DEPARTMENT REQUIRED?                         
         BO    *+8                                                              
         NI    SVVALEXP,X'FF'-DEPTVAL NO, DEPT INVALID                          
*                                                                               
         TM    RSTSTAT1,RSTSGPEI   PERSON REQUIRED?                             
         BO    RCV09H40            YES, GO READ 2P                              
         NI    SVVALEXP,X'FF'-PERSVAL NO, PERSON INVALID                        
         B     RCV09H50                                                         
*                                                                               
* IF PERSON REQUIRED, READ 2P LEDGER                                            
RCV09H40 BAS   RE,READ2P                                                        
         CLI   TWOPLVL,2           IF 2 LEVEL 2P, MUST REQUIRE DEPT             
         BL    RCV09H50                                                         
         OI    SVVALEXP,DEPTVAL                                                 
         CLI   TWOPLVL,3           IF 3 LEVEL 2P, MUST REQUIRE OFFICE           
         BL    RCV09H50                                                         
         OI    SVVALEXP,OFFVAL                                                  
*                                                                               
* IF PERSON IS REQUIRED, THEN CLI/PRD IS OPTIONAL IF USES COST ACCOUTNG         
         TM    COMPSTA5,CPYSNCST   USES COST?                                   
         BNO   RCV09H50            YES                                          
         TM    SVVALEXP,CLTVAL     IF NOT ALREADY VALID MAKE CLT OPTNL          
         BO    *+8                                                              
         OI    SVVALEXP,CLTOPT+CLTVAL                                           
         TM    SVVALEXP,PRDVAL     IF NOT ALREADY VALID MAKE PRD OPTNL          
         BO    *+8                                                              
         OI    SVVALEXP,PRDOPT+PRDVAL                                           
*                                                                               
RCV09H50 DS    0H                                                               
         XC    ERROR,ERROR                                                      
*                                                                               
SND09H   LA    R1,X'0009'          SEND RECORD HEADER                           
         BAS   RE,SENDH                                                         
*                                                                               
* IF THERE IS AN ERROR, SEND THAT INSTEAD OF FLAG                               
         SR    R1,R1                                                            
         ICM   R1,3,ERROR                                                       
         BZ    SND09H10            NO ERROR                                     
         CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(4,FULL),FILL=0                                         
         LA    R1,MCRTNCD          SEND RETURN CODE                             
         LA    R4,FULL                                                          
         LA    R5,4                                                             
         BRAS  RE,SENDD                                                         
         B     XIT                                                              
*                                                                               
* SEND FLAG WITH VALID EXPENSE ANALYSIS FIELDS                                  
SND09H10 ZIC   R1,SVVALEXP                                                      
         CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(3,EXPFLAG),FILL=0                                      
         LA    R1,MCVALFL          SEND RETURN CODE                             
         LA    R4,EXPFLAG                                                       
         LA    R5,L'EXPFLAG                                                     
         BRAS  RE,SENDD                                                         
*                                                                               
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 10 HEADER - VALIDATE EXPENSE ANALYSIS FIELDS                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV10H   DS    0H                                                               
*                                                                               
         MVC   COSTACCT,CLICOST                                                 
         CLC   PRDCOST,SPACES                                                   
         BNH   *+10                                                             
         MVC   COSTACCT,PRDCOST                                                 
*                                                                               
         BAS   RE,READ1C           VALIDATE 1C ACCOUNT                          
         BNE   SND10H              SEND ERROR                                   
*                                                                               
         MVC   ERROR,=Y(QINVDOF)   INVALID DOF                                  
         MVC   LENGTH,SVDOFLN                                                   
         MVC   OFFICE,SVDOF                                                     
         BAS   RE,VALOFF           VALIDATE DOF                                 
         BNE   SND10H              SEND ERROR                                   
*                                                                               
RCV10H10 CLI   SVAOFLN,0           USING AOF?                                   
         BE    RCV10H20            NO                                           
         MVC   ERROR,=Y(QINVAOF)   INVALID AOF                                  
         MVC   LENGTH,SVAOFLN                                                   
         MVC   OFFICE,SVAOF                                                     
         BAS   RE,VALOFF           VALIDATE AOF                                 
         BNE   SND10H              SEND ERROR                                   
*                                                                               
RCV10H20 CLI   SVCOFLN,0           USING COF?                                   
         BE    RCV10H30            NO                                           
         MVC   ERROR,=Y(QINVCOF)   INVALID COF                                  
         MVC   LENGTH,SVCOFLN                                                   
         MVC   OFFICE,SVCOF                                                     
         BAS   RE,VALOFF           VALIDATE COF                                 
         BNE   SND10H              SEND ERROR                                   
*                                                                               
RCV10H30 BAS   RE,VALDEPT          VALIDATE DEPT                                
         BNE   SND10H              SEND ERROR                                   
*                                                                               
         USING CATD,R1                                                          
         LA    R1,CATBLK           YES, GET COST SETTING FROM CATCALL           
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,VDATAMGR                                                 
         MVC   CATSEAC(1),CUL                                                   
         MVC   CATSEAC+1(14),SVEXPACC                                           
         MVC   CATOFF,SVAOF                                                     
         CLC   SVAOF,SPACES        IF NO ANALYSIS, USE DEBIT                    
         BH    *+10                                                             
         MVC   CATOFF,SVDOF                                                     
         MVC   CATDPT,SVDEPT                                                    
         GOTO1 VCATCALL                                                         
         MVC   ERROR,=Y(QMISS13)                                                
         CLI   CATERR,0            CHECK FOR A PROBLEM                          
         BNE   SND10H              SEND ERROR                                   
*                                                                               
         BAS   RE,VALPER           VALIDATE PERSON                              
         BNE   SND10H              SEND ERROR                                   
*                                                                               
         XC    ERROR,ERROR                                                      
*                                                                               
SND10H   LA    R1,X'0010'          SEND RECORD HEADER                           
         BAS   RE,SENDH                                                         
*                                                                               
* SEND ERROR, OR 0000 FOR NO ERROR                                              
         SR    R1,R1                                                            
         ICM   R1,3,ERROR                                                       
         CVD   R1,DUB                                                           
         EDIT  (P8,DUB),(4,FULL),FILL=0                                         
         LA    R1,MCRTNCD          SEND RETURN CODE                             
         LA    R4,FULL                                                          
         LA    R5,4                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
* IF THERE WASN'T AN ERROR, AND USER ENTER ++ OR DOF, SEND PRESTO DOF           
         OC    ERROR,ERROR                                                      
         BNZ   SND10HX                                                          
         TM    FLAG,SNDDOF         NEED TO SEND DOF BACK TO PRESTO?             
         BNO   SND10HX                                                          
         ZIC   R5,CMPOFFLN                                                      
         LA    R1,MCDOF                                                         
         LA    R4,SVDOF                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
SND10HX  B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* READ 2P LEDGER AND GET PERSON LENGTH                                          
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
READ2P   NTR1                                                                   
         USING LDGRECD,R6                                                       
         MVC   ERROR,=Y(QMISS2P)                                                
         LA    R6,KEY                                                           
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUL                                                      
         MVC   LDGKUNT(2),=C'2P'   GET 2P LEDGER VALUES                         
         GOTO1 HIGH                                                             
         CLC   LDGKEY,KEYSAVE                                                   
         BNE   NO                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETELEM,DMCB,ACLELQ GET HEIRARCHY ELEMENT                        
*                                                                               
         USING ACLELD,R6                                                        
         MVC   LDGTLVA,ACLVALS                                                  
         MVC   LDGTLVB,ACLVALS+(L'ACLVALS*1)                                    
         MVC   LDGTLVC,ACLVALS+(L'ACLVALS*2)                                    
         MVC   LDGTLVD,ACLVALS+(L'ACLVALS*3)                                    
*                                                                               
         MVI   TWOPLVL,3           SET 2P LEDGER LEVEL                          
         ZIC   R1,LDGTLVB          GET DISPLACEMENT TO PERSON CODE              
         CLI   LDGTLVC,L'ACTKACT   TEST FOR 3 LEVEL LEDGER                      
         BE    READ2P10            YES                                          
*                                                                               
         IC    R1,LDGTLVA                                                       
         MVI   TWOPLVL,2                                                        
         CLI   LDGTLVB,L'ACTKACT   TEST FOR 2 LEVEL LEDGER                      
         BE    READ2P10                                                         
*                                                                               
         SR    R1,R1                                                            
         MVI   TWOPLVL,1                                                        
         CLI   LDGTLVA,L'ACTKACT                                                
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
READ2P10 LA    RE,L'ACTKACT                                                     
         SR    RE,R1                                                            
         STC   RE,PERLN2P          SAVE L'PERSON CODE                           
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* READ FOR 1C ACCOUNT                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
READ1C   NTR1                                                                   
*                                                                               
* IF NOT USING COST (CLIENT ISN'T VALID), DON'T READ 1C ACCOUNT                 
         TM    SVVALEXP,CLTVAL                                                  
         BNO   YES                                                              
*                                                                               
         MVC   ERROR,=Y(QMISS1C)                                                
* READ EXPENSE ACCOUNT                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),CUL          COMPANY                                      
         MVC   KEY+1(14),SVEXPACC  EXPENSE ACCOUNT                              
         GOTO1 HIGH                                                             
         CLC   KEY(15),KEYSAVE     COMPARE SIGNIFICANT DATA                     
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC              GET RECORD                                   
*                                                                               
         USING RSTELD,R6                                                        
         GOTO1 GETELEM,DMCB,RSTELQ                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EXPCNTR,RSTCCTR     COST CENTER                                  
         MVC   EXPCPOS,RSTCCTRR    COST POSITION                                
*                                                                               
         OC    EXPCNTR,SPACES                                                   
         LA    R3,COSTACCT+7       R3=A(REPLACEMENT POSITION POINTER)           
         CLI   EXPCPOS,0           ANY POSITION OVERRIDDE?                      
         BE    READ1C10            NO                                           
                                                                                
         SR    R3,R3                                                            
         IC    R3,EXPCPOS          YES                                          
         LA    R3,COSTACCT+2(R3)   GET TO NEW LOCATION                          
                                                                                
READ1C10 LA    R1,EXPCNTR          R1=A(OVERRIDE COST CENTER)                   
         LA    RE,3                LOOP                                         
                                                                                
READ1C20 CLI   0(R1),C' '          LOOK FOR DATA                                
         BE    *+10                                                             
         MVC   0(1,R3),0(R1)                                                    
         AHI   R1,1                                                             
         AHI   R3,1                                                             
         BCT   RE,READ1C20                                                      
                                                                                
         USING ACTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,COSTACCT                                                
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* READ FOR 29 ACCOUNT                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
READ29   NTR1                                                                   
         MVC   ERROR,=Y(QMISS29)                                                
         USING ACTRECD,R4                                                       
         LA    R4,KEY                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(2),=C'29'   CHECK FOR 29 ACCOUNT                         
         MVC   ACTKACT,=12C'9'     SET UP DEFAULT                               
         CLC   COSTACCT,SPACES                                                  
         BNH   *+10                                                             
         MVC   ACTKACT,COSTACCT+3  USE ACTUAL ACCOUNT IF PRESENT                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   NO                                                               
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE OFFICE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VALOFF   NTR1                                                                   
*                                                                               
* IF DEBIT OFFICE IS "++", GET OFFICE FROM PRD OR CLIENT PROFILE                
         CLC   SVDOF,=C'++'                                                     
         BNE   VOFF45                                                           
         OI    FLAG,SNDDOF         NEED TO SEND DOF BACK TO PRESTO              
         CLI   CMPOFFLN,2          CHECK IF USING 1 OR 2 CHAR OFFICES           
         BE    VOFF20                                                           
         CLI   PRODOFF,C' '        CHECK IF HAS A PRODUCT OFFICE                
         BNH   VOFF10                                                           
         MVC   SVDOF(1),PRODOFF                                                 
         B     VOFF40                                                           
VOFF10   MVC   SVDOF(1),CLIOFF     OTHERWISE USE CLIENT OFFICE                  
         B     VOFF40                                                           
*                                                                               
* 2 CHAR OFFICES ARE IN A DIFFERENT PLACE                                       
VOFF20   CLC   PRODOFFC,SPACES     CHECK IF HAS A PRODUCT OFFICE                
         BNH   VOFF30                                                           
         MVC   SVDOF,PRODOFFC                                                   
         B     VOFF40                                                           
VOFF30   MVC   SVDOF,CLIOFFC       OTHERWISE USE CLIENT OFFICE                  
VOFF40   MVC   OFFICE,SVDOF                                                     
         B     VOFF47                                                           
*                                                                               
VOFF45   ZIC   R2,LENGTH           ACTUAL LENGTH OF OFFICE                      
         ZIC   R3,CMPOFFLN         OFFICE LENGTH IN COMPANY RECORD              
         CR    R2,R3                                                            
         BH    NO                  NOT RIGHT LENGTH, ERROR                      
*                                                                               
* CHECK IF OFFICE RECORD EXISTS                                                 
         USING OFFRECD,R4                                                       
VOFF47   LA    R4,KEY                                                           
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUL                                                      
         MVC   OFFKOFF,OFFICE                                                   
         GOTO1 HIGH                                                             
         CLC   OFFKEY(OFFKEND),KEYSAVE                                          
         BNE   NO                                                               
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         TM    OFFRSTA,OFFSLIST                                                 
         BO    NO                                                               
*                                                                               
         TM    CMPBSEC,CPYBSOFF    TEST OFFICE SECURITY INHIBITED               
         BZ    VOFF50                                                           
         LA    R1,OFFBLK                                                        
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,OFFICE                                                  
         MVI   OFFAACT,OFFAPST                                                  
         GOTO1 VOFFAL                                                           
         BNE   NO                                                               
         DROP  R1                                                               
*                                                                               
VOFF50   LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVI   ACTKUNT,C'2'                                                     
         MVI   ACTKLDG,C'D'                                                     
         MVC   ACTKACT(L'OFFICE),OFFICE                                         
*        MVC   XTRAMESS,ACTKULA                                                 
         GOTO1 HIGH                                                             
         CLC   ACTKEY(L'ACTKEY),KEYSAVE                                         
         BNE   NO                                                               
*                                                                               
VALANOX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE THE DEPT                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VALDEPT  NTR1                                                                   
         CLI   SVDEPTLN,0          CHECK IF THERE IS A DEPT                     
         BE    YES                                                              
*                                                                               
         MVC   ERROR,=Y(QINVDEPT)  INVALID DEPTARTMENT                          
         ZIC   R2,SVDEPTLN         ACTUAL LENGTH                                
         ZIC   R3,CMPDEPLN         DEPT LENGTH IN COMPANY RECORD                
         CR    R2,R3                                                            
         BH    NO                  NOT RIGHT LENGTH, ERROR                      
*                                                                               
         USING ACTRECD,R6                                                       
         LA    R6,KEY                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(2),=C'2D'   VALIDATE 2D ACCOUNT                          
         LA    R1,ACTKACT                                                       
         OC    SVDOF,SVDOF         USING OFFICE?                                
         BZ    VDEPT10             NO                                           
         LA    RE,SVAOF                                                         
         CLC   SVAOF,SPACES        IF NO ANALYSIS, USE DEBIT                    
         BH    *+8                                                              
         LA    RE,SVDOF                                                         
         SR    RF,RF                                                            
         IC    RF,CMPOFFLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
         LA    R1,1(RF,R1)                                                      
*                                                                               
VDEPT10  SR    RF,RF                                                            
         IC    RF,CMPDEPLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVDEPT                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   NO                                                               
         B     YES                                                              
         DROP  R6                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* VALIDATE THE PERSON                                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
VALPER   NTR1                                                                   
         CLI   SVPRSNLN,0          CHECK IF THERE IS A PERSON                   
         BE    YES                                                              
*                                                                               
         MVC   ERROR,=Y(QINVPERS)  INVALID PERSON                               
*                                                                               
         CLI   PERLN2P,0           DID WE ALREADY READ 2P LEDGER?               
         BNE   VPER20              YES                                          
         BAS   RE,READ2P                                                        
         BNE   NO                  ERROR                                        
*                                                                               
         ZIC   R2,SVPRSNLN         ACTUAL LENGTH                                
         ZIC   R3,PERLN2P          PERSON LENGTH IN 2P LEDGER                   
         CR    R2,R3                                                            
         BH    NO                  NOT RIGHT LENGTH, ERROR                      
*                                                                               
         USING ACTRECD,R4                                                       
VPER20   LA    R4,KEY                                                           
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUL                                                      
         MVC   ACTKUNT(2),=C'2P'   VALIDATE 2P ACCOUNT                          
         LA    R1,ACTKACT                                                       
*                                                                               
         CLI   TWOPLVL,3           ONLY MOVE OFFICE FOR 3 LEVEL ACCTS           
         BL    VPER30                                                           
         LA    RE,SVAOF                                                         
         CLC   SVAOF,SPACES        IF NO ANALYSIS OFFICE USE DEBIT              
         BH    *+8                                                              
         LA    RE,SVDOF                                                         
         SR    RF,RF                                                            
         IC    RF,CMPOFFLN         LENGTH OF OFFICE                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(RE)                                                    
         LA    R1,1(RF,R1)                                                      
*                                                                               
VPER30   CLI   TWOPLVL,2                                                        
         BL    VPER40                                                           
         SR    RF,RF                                                            
         IC    RF,CMPDEPLN         LENGTH OF DEPARTMENT                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVDEPT                                                   
         LA    R1,1(RF,R1)                                                      
*                                                                               
VPER40   SR    RF,RF                                                            
         IC    RF,PERLN2P                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),SVPRSN                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   ACTKEY,KEYSAVE                                                   
         BNE   NO                                                               
         BAS   RE,READ29           VALIDATE 29 ACCOUNT                          
         BNE   NO                  SEND ERROR                                   
         B     YES                                                              
         DROP  R4                                                               
*&&                                                                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* RECEIVE 11 HEADER - DELETE ORDER RESERVATION                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
RCV11H   DS    0H                                                               
         BAS   RE,BLDKEY           BUILD ORDER RESERVATION KEY                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING OBRRECD,R6                                                       
         CLC   OBRKEY(OBRKEND),KEYSAVE COMPARE SIGNIFICANT DATA                 
         BNE   XIT                                                              
         MVI   OBRKSTAT,OBRSDELT   SET DELETE BIT                               
         GOTO1 WRITE               AND WRITE BACK KEY                           
*                                                                               
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RESERVATION                          
         L     R6,AIO              POINT TO RECORD                              
         MVI   OBRRSTAT,OBRSDELT   SET DELETE BIT                               
         GOTO1 PUTREC              WRITE BACK RECORD                            
         B     XIT                                                              
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 03 HEADER - RETURN SUCCESS OR ERROR AND EXIT                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND03H   MVI   BLOCK,C'0'          NO ERROR                                     
         LA    R5,1                                                             
         B     S03H10                                                           
*                                                                               
ERROVER  MVI   BLOCK,C'1'          OVERLAPS ANOTHER RESERVATION                 
         LA    R5,1                                                             
         B     S03H10                                                           
*                                                                               
ERRORD   MVI   BLOCK,C'2'          FOUND ORDER WITHIN REQUEST RANGE             
         LA    R5,1                                                             
         B     S03H10                                                           
*                                                                               
ERRDUP   LA    R1,X'0003'          SEND RECORD HEADER                           
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,MCRTNCD          SEND RETURN CODE                             
         LA    R4,BLOCK                                                         
         MVI   BLOCK,C'3'          DUPLICATE ORDER RESERVATION REQUEST          
         LA    R5,1                                                             
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,MCPRSN           SEND PERSON WHO MADE REQUEST                 
         LA    R4,DUPPRS                                                        
         BRAS  RE,SENDD                                                         
*                                                                               
         LA    R1,MCORSRV          SEND SERVER WHERE THE REQUEST IS             
         LA    R4,DUPSRV                                                        
         BRAS  RE,SENDD                                                         
         B     XIT                                                              
*                                                                               
S03H10   LA    R1,X'0003'          SEND RECORD HEADER                           
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,MCRTNCD          SEND RETURN CODE                             
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         B     XIT                                                              
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* SEND 03 HEADER - RETURN SUCCESS OR ERROR AND EXIT                             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SND04H   MVI   BLOCK,C'0'          NO ERROR                                     
         LA    R5,1                                                             
         B     S04H10                                                           
*                                                                               
ERRNFO   MVI   BLOCK,C'1'          ORDER IS NOT FULLY OPEN                      
         LA    R5,1                                                             
*                                                                               
S04H10   LA    R1,X'0004'          SEND RECORD HEADER                           
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,MCRTNCD          SEND RETURN CODE                             
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
         B     XIT                                                              
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON ENTRY R1 CONTAINS HEADER CODE                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
SENDHX   LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN                              
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
SNDERMSG GOTO1 SENDMSG                                                          
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
* CONSTANTS                                                                     
*                                                                               
CONTROL  DC    C'000000'           ORDER CONTROL RECORD NUMBER                  
ACORDELQ EQU   X'67'                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACPRFWRK                                                       
WORKD    DSECT                                                                  
         ORG   MEDWORK             480 BYTES IN OVWORK                          
*                                                                               
FLAG     DS    X                                                                
RESDEL   EQU   X'80'                                                            
EXPORDER EQU   X'40'                                                            
SNDDOF   EQU   X'20'                                                            
OVLPDATA DS    0CL12                                                            
OVLPFRST DS    CL6                 OVERLAP FIRST NUMBER                         
OVLPLAST DS    CL6                 OVERLAP LAST NUMBER                          
DUPDATA  DS    0CL26                                                            
DUPPRS   DS    CL10                DUPLICATE RES PERSON                         
DUPSRV   DS    CL16                DUPLICATE SQL SERVER ID                      
VPTAORD  DS    A                   A(ACPTAORD)                                  
VCATCALL DS    A                   A(CATCALL)                                   
*                                                                               
OFFICE   DS    CL2                 OFFICE                                       
LENGTH   DS    X                   OFFICE CODE LENGTH                           
COSTACCT DS    CL15                COST ACCOUNT                                 
EXPCNTR  DS    CL3                 COST CENTER (RSTCCTR)                        
EXPCPOS  DS    CL1                 COST POSITION (RSTCCTRR)                     
*                                                                               
LDGTLVA  DS    XL1                 LEVEL A LENGTH                               
LDGTLVB  DS    XL1                 LEVEL B LENGTH                               
LDGTLVC  DS    XL1                 LEVEL C LENGTH                               
LDGTLVD  DS    XL1                 LEVEL D LENGTH                               
*                                                                               
EXPFLAG  DS    CL3                 EXPENSE FLAG                                 
*                                                                               
CATBLK   DS    CL(CATLNQ)          CATCALL AREA                                 
*                                                                               
* INCLUDED DSECTS                                                               
* FAFACTS                                                                       
* ACGENFILE                                                                     
* ACGENBOTH                                                                     
* FASELIST                                                                      
* FAUTL                                                                         
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAUTL                                                          
       ++INCLUDE ACDDEQUS                                                       
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACPRF03   04/18/06'                                      
         END                                                                    
