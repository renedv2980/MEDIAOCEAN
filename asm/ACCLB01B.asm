*          DATA SET ACCLB01B   AT LEVEL 070 AS OF 12/22/99                      
*PHASE T62101B                                                                  
CLB01    TITLE '- BILL PROGRAM - SETUP SCREEN'                                  
CLB01    CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL SETWORKX-SETWORKD,**CLB1**,R8,R7,R6,CLEAR=YES,RR=RE              
         USING SETWORKD,RC         RC=A(LOCAL WORKING STORAGE)                  
         USING WORKD,R9            R9=A(GLOBAL WORKING STORAGE)                 
         USING TWAD,RA             RA=A(TWA)                                    
*                                                                               
         SRL   RF,32-8                                                          
         LTR   RF,RF               TEST IF EXIT ROUTINE REQUIRED                
         BZ    SET02                                                            
         OI    SETFLAG,SETFEXIT    SET EXIT FLAG                                
         NI    CSINDSL1,FF-(CSIRDSPC)                                           
         BCTR  RF,0                                                             
         CLM   RF,1,=AL1(SETNTRYM)                                              
         BNH   *+6                                                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     SETNTRYS(RF)                                                     
*                                                                               
SETNTRYS DS    0XL4                                                             
SETSUB#1 EQU   1                                                                
         B     SET02                                                            
SETNTRYM EQU   (*-SETNTRYS)/L'SETNTRYS                                          
         SPACE 2                                                                
SET02    CLI   TWASCRN,S#BILSET    TEST SETUP SCREEN LOADED                     
         BNE   *+16                                                             
         TM    SETFLAG,SETFEXIT    TEST EXIT FLAG SET                           
         BNZ   SETDSP                                                           
         B     SETVAL                                                           
         GOTO1 AOVRSCR,BOPARM,('S#BILSET',BASOLAYH)                             
         BNE   EXIT                                                             
         GOTO1 VDICTAT,BODMCB,C'SL  ',SETDFA                                    
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BNZ   SET06                                                            
         XC    SETCURP,SETCURP     NO - CLEAR CURRENCY AND EXCHANGE             
         XC    SETEXRP,SETEXRP     FIELDS AND PROTECT FROM INPUT                
         OI    SETCURH+FHOID,FHOITR                                             
         OI    SETCURH+FHATD,FHATPR                                             
         OI    SETEXRH+FHOID,FHOITR                                             
         OI    SETEXRH+FHATD,FHATPR                                             
SET06    CLI   CUCTRY,CTRYGER      TEST GERMANY                                 
         BE    SET08                                                            
         XC    SETLNGP,SETLNGP     NO - CLEAR & PROTECT LANGUAGE FIELDS         
         OI    SETLNGPH+FHOID,FHOITR                                            
         OI    SETLNGH+FHOID,FHOITR                                             
         OI    SETLNGH+FHATD,FHATPR                                             
SET08    TM    CSINDSG1,CSINDSET   TEST SETUP VALUES SAVED                      
         BNZ   SETDSP                                                           
         LA    R1,SETCLIH                                                       
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$EREQF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
         SPACE 2                                                                
EXITCLR  BAS   RE,CLRSCRN          CLEAR SCREEN                                 
         B     EXIT                                                             
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITN    DS    0H                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITY    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY CURRENT KEY VALUES                                          *         
***********************************************************************         
         SPACE 1                                                                
SETDSP   MVC   SETCLI(L'BCCLICOD),BCCLICOD                                      
         OI    SETCLIH+FHIID,FHIIVA                                             
         SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    R1,BCPROCOD(RE)                                                  
         SR    RF,RF                                                            
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SETPRO(0),0(R1)                                                  
         OI    SETPROH+FHIID,FHIIVA                                             
         IC    RF,BCPROLEN                                                      
         LA    R1,BCJOBCOD(RF)                                                  
         IC    RE,BCJOBLEN                                                      
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   SETJOB(0),0(R1)                                                  
         OI    SETJOBH+FHIID,FHIIVA                                             
*                                                                               
         CLI   CSFORMAT,0                                                       
         BE    *+8                                                              
         BAS   RE,GETFMT           OUTPUT FORMAT CODE                           
*                                                                               
         TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BZ    SETDSPX                                                          
         CLC   CSBILCUR,BCSPACES   TEST CURRENCY SET                            
         BNH   SETDSPX                                                          
         BAS   RE,GETCUR           OUTPUT CURRENCY                              
         CLC   CSBILCUR,CSCPYCUR   TEST BILLING IN PRIMARY CURR                 
         BE    SETDSPX                                                          
         CLC   CSBILCUR,BCCPYSEC   TEST BILLING IN SECOND CURR                  
         BNE   *+14                                                             
         XC    CSEXCVAL,CSEXCVAL   THEN CLEAR RATE                              
         B     SETDSPX                                                          
         OC    CSEXCVAL,CSEXCVAL   TEST EXCHANGE RATE SET                       
         BZ    SETDSPX                                                          
         BAS   RE,GETRAT           EDIT OUT EXCHANGE RATE                       
*                                                                               
SETDSPX  DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY FIELDS                                                 *         
***********************************************************************         
         SPACE 1                                                                
SETVAL   NI    CSINDSG1,FF-CSINDSET                                             
         LA    R2,SETKEY                                                        
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
*                                                                               
         GOTO1 VACSRCHC,BOPARM,(4,SETCLIH),ATWA,BCCPYPRD,ACOM,         *        
               (X'11',0)                                                        
         MVC   FVMAXL,BCCLILEN                                                  
         GOTO1 AFVAL,SETCLIH                                                    
         BE    SETVAL02                                                         
         BH    EXITCLR                                                          
         XC    BCCLI(BCCLIL),BCCLI                                              
         MVC   FVMSGNO,=AL2(AI$EKDPF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITCLR                                                          
SETVAL02 MVC   ACTKACT,FVIFLD                                                   
         TM    SETCLIH+FHIID,FHIIVA                                             
         BNZ   SETVAL04                                                         
         NI    SETPROH+FHIID,FF-FHIIVA                                          
         OI    SETFLAG,SETFCHNG                                                 
*                                                                               
SETVAL04 GOTO1 ASETUP,BOPARM,(X'80',ACTKACT),0,0                                
         BNE   EXITCLR                                                          
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCCLICOD),BCCLICOD                    
         GOTO1 VACSRCHC,BOPARM,SETCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),ACCODE,(L'BCCLINAM,BCCLINAM)                          
         OI    SETCLIH+FHIID,FHIIVA                                             
*&&                                                                             
         GOTO1 VACSRCHC,BOPARM,(4,SETPROH),ATWA,BCCPYPRD,              *        
               (BCCLILEN,ACOM),(X'22',BCCLICOD)                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,BCCLILEN                                                      
         IC    RF,BCPROLEN                                                      
         SR    RF,RE                                                            
         STC   RF,FVMAXL                                                        
         GOTO1 AFVAL,SETPROH                                                    
         BE    SETVAL06                                                         
         BH    EXITCLR                                                          
         XC    BCPRO(BCPROL),BCPRO                                              
         MVC   FVMSGNO,=AL2(AI$EKDPF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITCLR                                                          
SETVAL06 SR    RE,RE                                                            
         IC    RE,BCCLILEN                                                      
         LA    RE,ACTKACT(RE)                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         TM    SETPROH+FHIID,FHIIVA                                             
         BNZ   SETVAL08                                                         
         NI    SETJOBH+FHIID,FF-FHIIVA                                          
         OI    SETFLAG,SETFCHNG                                                 
*                                                                               
SETVAL08 GOTO1 ASETUP,BOPARM,(X'40',ACTKACT),0,0                                
         BNE   EXITCLR                                                          
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCPROCOD),BCPROCOD                    
         GOTO1 VACSRCHC,BOPARM,SETPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),ACCODE,(L'BCPRONAM,BCPRONAM)                          
         OI    SETPROH+FHIID,FHIIVA                                             
*&&                                                                             
         GOTO1 VACSRCHC,BOPARM,(4,SETJOBH),ATWA,BCCPYPRD,              *        
               (BCPROLEN,ACOM),(X'33',BCPROCOD)                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RE,BCPROLEN                                                      
         IC    RF,BCJOBLEN                                                      
         SR    RF,RE                                                            
         STC   RF,FVMAXL                                                        
         GOTO1 AFVAL,SETJOBH                                                    
         BE    SETVAL10                                                         
         BH    EXITCLR                                                          
         XC    BCJOB(BCJOBL),BCJOB                                              
         MVC   FVMSGNO,=AL2(AI$EKDPF)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITCLR                                                          
SETVAL10 SR    RE,RE                                                            
         IC    RE,BCPROLEN                                                      
         LA    RE,ACTKACT(RE)                                                   
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   0(0,RE),FVIFLD                                                   
         TM    SETJOBH+FHIID,FHIIVA                                             
         BNZ   *+8                                                              
         OI    SETFLAG,SETFCHNG                                                 
         GOTO1 ASETUP,BOPARM,(X'20',ACTKACT),0,0                                
         BNE   EXITCLR                                                          
         DROP  R2                                                               
SETVAL12 DS   0H                                                                
*&&US                                                                           
         MVC   ACCODE+(ACTKACT-ACTRECD)(L'BCJOBCOD),BCJOBCOD                    
         GOTO1 VACSRCHC,BOPARM,SETJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),ACCODE,(L'BCJOBNAM,BCJOBNAM)                          
         OI    SETJOBH+FHIID,FHIIVA                                             
*&&                                                                             
*&&UK                                                                           
         GOTO1 VACSRCHC,BOPARM,SETCLIH,ATWA,(BCCLILEN,0),              *        
               (X'C0',19),ACCODE,(L'BCCLINAM,BCCLINAM)                          
         OI    SETCLIH+FHIID,FHIIVA                                             
         GOTO1 VACSRCHC,BOPARM,SETPROH,ATWA,(BCPROLEN,BCCLILEN),       *        
               (X'C0',19),ACCODE,(L'BCPRONAM,BCPRONAM)                          
         OI    SETPROH+FHIID,FHIIVA                                             
         GOTO1 VACSRCHC,BOPARM,SETJOBH,ATWA,(BCJOBLEN,BCPROLEN),       *        
               (X'C0',19),ACCODE,(L'BCJOBNAM,BCJOBNAM)                          
         OI    SETJOBH+FHIID,FHIIVA                                             
*&&                                                                             
         TM    SETFLAG,SETFCHNG    TEST CHANGE OF JOB                           
         BZ    SETVAL14                                                         
         BAS   RE,CLRSCRN                                                       
         MVI   TWASESNL,0          CHANGE OF KEY STARTS AGAIN                   
*                                                                               
SETVAL14 ZAP   SETCBAPN,BCPZERO    PENDING ALLOCATION (NET)                     
         ZAP   SETCBAPC,BCPZERO    PENDING ALLOCATION (COMMISSION)              
         ZAP   SETCBWPT,BCPZERO    PENDING WRITE-OFF (TIME/LABOR)               
         ZAP   SETCBWPC,BCPZERO    PENDING WRITE-OFF (COST/OOP)                 
         ZAP   SETCBRPT,BCPZERO    PENDING RECOVERY (TIME/LABOR)                
         ZAP   SETCBRPC,BCPZERO    PENDING RECOVERY (COST/OOP)                  
         ZAP   SETCBTP,BCPZERO     PENDING TRANSFER                             
         ZAP   SETCBIP,BCPZERO     PENDING INTERNAL INVOICE                     
         ZAP   SETBLBL,BCPZERO     TOTAL BILLABLE (JOB BALANCE)                 
*                                                                               
         L     R1,AIO1             PROCESS JOB RECORD                           
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         SR    R0,R0                                                            
*                                                                               
         USING ABLELD,R1                                                        
SETVAL20 CLI   ABLEL,ABLELQ        PROCESS BALANCE ELEMENT                      
         BNE   SETVAL22                                                         
         ZAP   SETBLBL,ABLDR                                                    
         SP    SETBLBL,ABLCR                                                    
         B     SETVAL30                                                         
*                                                                               
         USING SCIELD,R1                                                        
SETVAL22 CLI   SCIEL,SCIELQ        PROCESS SUBSIDIARY CASH ELEMENTS             
         BNE   SETVAL30                                                         
         LA    RE,SETCBAPN         ALLOCATION                                   
         LA    RF,SETCBAPC                                                      
         CLI   SCITYPE,SCITCBAP                                                 
         BE    SETVAL24                                                         
*                                                                               
         LA    RE,SETCBWPT         WRITE-OFFS                                   
         LA    RF,SETCBWPC                                                      
         CLI   SCITYPE,SCITCBWP                                                 
         BE    SETVAL24                                                         
*                                                                               
         LA    RE,SETCBRPT         RECOVERIES                                   
         LA    RF,SETCBRPC                                                      
         CLI   SCITYPE,SCITCBRP                                                 
         BE    SETVAL24                                                         
*                                                                               
         SR    RF,RF               TRANSFERS                                    
         LA    RE,SETCBTP                                                       
         CLI   SCITYPE,SCITCBTP                                                 
         BE    SETVAL24                                                         
*                                                                               
         LA    RE,SETCBIP          INTERNAL INVOICES                            
         CLI   SCITYPE,SCITCBIP                                                 
         BE    SETVAL24                                                         
*                                                                               
         CLI   SCITYPE,SCITT99S    BILLING NOT IN ABLCR                         
         BNE   SETVAL30                                                         
         SP    SETBLBL,SCIAMNT     TAKE OFF BILLABLE BALANCE                    
         B     SETVAL30                                                         
*                                                                               
SETVAL24 AP    0(L'SETCBAPN,RE),SCIAMNT                                         
         LTR   RF,RF                                                            
         BZ    SETVAL30                                                         
         AP    0(L'SETCBAPC,RF),SCIADMN                                         
         B     SETVAL30                                                         
*                                                                               
SETVAL30 IC    R0,1(R1)            BUMP TO NEXT ELEMENT                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   SETVAL20                                                         
         DROP  R1                                                               
*                                                                               
         TM    SETFRMH+FHIID,FHIITH                                             
         BZ    *+8                                                              
         NI    SETFRMH+FHIID,FF-FHIIVA                                          
         TM    SETLNGH+FHIID,FHIITH                                             
         BZ    *+8                                                              
         NI    SETLNGH+FHIID,FF-FHIIVA                                          
         TM    SETFLAG,SETFCHNG    TEST CHANGE OF JOB                           
         BZ    SETVAL34                                                         
         TM    SETFRMH+FHIID,FHIIVA                                             
         BZ    SETVAL32            TEST FORMAT CODE INPUT                       
         NI    SETFRMH+FHIID,FF-FHIIVA                                          
         MVC   SETFRM,BCSPACES     NO - CLEAR CURRENT FORMAT CODE               
         OI    SETFRMH+FHOID,FHOITR                                             
         MVC   SETFRMN,BCSPACES                                                 
         OI    SETFRMNH+FHOID,FHOITR                                            
SETVAL32 TM    SETLNGH+FHIID,FHIIVA                                             
         BZ    SETVAL34            TEST LANGUAGE INPUT                          
         NI    SETLNGH+FHIID,FF-FHIIVA                                          
         MVC   SETLNG,BCSPACES     NO - CLEAR CURRENT LANGUAGE                  
         OI    SETLNGH+FHOID,FHOITR                                             
*                                                                               
SETVAL34 TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BZ    SETVAL40                                                         
         TM    SETCURH+FHIID,FHIITH                                             
         BZ    *+8                                                              
         NI    SETCURH+FHIID,FF-FHIIVA                                          
         TM    SETFLAG,SETFCHNG    TEST CHANGE OF JOB                           
         BZ    SETVAL36                                                         
         TM    SETCURH+FHIID,FHIIVA                                             
         BZ    SETVAL36            YES - TEST CURRENCY CODE INPUT               
         NI    SETCURH+FHIID,FF-FHIIVA                                          
         MVC   SETCUR,BCSPACES     NO - CLEAR CURRENCY                          
         OI    SETCURH+FHOID,FHOITR                                             
         MVC   SETCURN,BCSPACES                                                 
         OI    SETCURNH+FHOID,FHOITR                                            
*                                                                               
SETVAL36 TM    SETEXRH+FHIID,FHIITH                                             
         BZ    *+8                                                              
         NI    SETEXRH+FHIID,FF-FHIIVA                                          
         TM    SETCURH+FHIID,FHIIVA                                             
         BO    SETVAL40            TEST CHANGE OF CURRENCY                      
         MVC   SETEXRD,BCSPACES                                                 
         OI    SETEXRDH+FHOID,FHOITR                                            
         TM    SETEXRH+FHIID,FHIIVA                                             
         BZ    SETVAL40            YES - TEST RATE INPUT                        
         NI    SETEXRH+FHIID,FF-FHIIVA                                          
         MVC   SETEXR,BCSPACES     NO - CLEAR RATE                              
         OI    SETEXRH+FHOID,FHOITR                                             
*                                                                               
SETVAL40 TM    SETLNGH+FHIID,FHIIVA                                             
         BZ    *+12                                                             
         TM    SETFRMH+FHIID,FHIIVA                                             
         BO    SETVAL60                                                         
         NI    CSINDSL1,FF-CSIRDSPC                                             
         MVI   CSFMLANG,0          DEFAULT LANGUAGE                             
         GOTO1 AFVAL,SETLNGH                                                    
         BL    SETVAL46                                                         
         L     R1,ALANG                                                         
         LH    RE,0(R1)            RE=ENTRY LENGTH                              
         L     RF,2(R1)            RF=A(END OF TABLE-1)                         
         LA    R1,6(R1)            R1=A(FIRST LANGUAGE ENTRY)                   
         USING LANGTABD,R1                                                      
         SR    R2,R2                                                            
         IC    R2,FVXLEN                                                        
SETVAL42 LA    R3,LANGFULN         NATIVE LANGUAGE FULL NAME                    
         CLM   R2,1,L'LANGSHRN-1                                                
         BH    *+8                                                              
         LA    R3,LANGSHRN         NATIVE LANGUAGE SHORT NAME                   
         EX    R2,*+8                                                           
         BE    SETVAL44                                                         
         CLC   FVIFLD(0),0(R3)                                                  
         LA    R3,LANGFUL          ENGLISH LANGUAGE FULL NAME                   
         CLM   R2,1,L'LANGSHR-1                                                 
         BH    *+8                                                              
         LA    R3,LANGSHR          ENGLISH LANGUAGE SHORT NAME                  
         EX    R2,*+8                                                           
         BE    SETVAL44                                                         
         CLC   FVIFLD(0),0(R3)                                                  
         BXLE  R1,RE,SETVAL42                                                   
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
SETVAL44 MVC   CSFMLANG,LANGCODE   SET INPUT LANGUAGE CODE                      
         CLC   CSFMLANG,CULANG     TEST AGENCY LANGUAGE                         
         BNE   *+12                                                             
         MVI   CSFMLANG,0          SET DEFAULT                                  
         B     SETVAL45                                                         
         CLI   CSFMLANG,LANGENG    TEST LANGUAGE ENGLISH                        
         BNE   SETVAL45                                                         
         MVI   CSFMLANG,LANGEUK    USE THE UK ENGLISH CODE                      
SETVAL45 OI    SETLNGH+FHIID,FHIIVA                                             
         DROP  R1                                                               
         TM    SETFRMH+FHIID,FHIIVA                                             
SETVAL46 GOTO1 AFVAL,SETFRMH                                                    
         BNL   SETVAL48            NO INPUT - GET DEFAULT VALUE                 
         GOTO1 ASETUP,BOPARM,(X'04',0),0,0                                      
         BE    SETVAL50                                                         
         B     EXIT                                                             
SETVAL48 TM    FVIIND,FVINUM       TEST INPUT NUMERIC                           
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NONIF)                                           
         B     EXIT                                                             
         MVC   CSFORMAT,BCFULL+3                                                
         CLI   CSFORMAT,0                                                       
         BNE   SETVAL50                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
SETVAL50 BAS   RE,GETFMT           VALIDATE CODE                                
         BNE   EXIT                                                             
         OI    SETFRMH+FHIID,FHIIVA                                             
*                                                                               
SETVAL60 TM    BCCPYST6,CPYSFBIL   TEST FOREIGN CURRENCY BILLING                
         BZ    SETVALX                                                          
         TM    SETCURH+FHIID,FHIIVA                                             
         BO    SETVAL70                                                         
         NI    CSINDSL1,FF-CSIRDSPC                                             
         MVC   SETCURN,BCSPACES                                                 
         OI    SETCURNH+FHOID,FHOITR                                            
         TM    BCJOBSTA,BCJOBPEN   TEST ANYTHING PENDING                        
         BNZ   SETVAL62                                                         
*                                                                               
         GOTO1 AFVAL,SETCURH                                                    
         GOTO1 ASETUP,BOPARM,(X'10',0),FVIFLD,0                                 
         BE    SETVAL62                                                         
         MVC   SETCUR,CSBILCUR     INVALID CURRENCY CODE                        
         OI    SETCURH+FHOID,FHOITR                                             
         B     EXIT                                                             
SETVAL62 BAS   RE,GETCUR                                                        
*                                                                               
SETVAL70 CLC   CSBILCUR,CSCPYCUR   TEST BILLINGIN PRIMARY CURR                  
         BE    *+14                                                             
         CLC   CSBILCUR,BCCPYSEC   TEST BILLING IN SECOND CURR                  
         BNE   SETVAL72                                                         
         MVC   SETEXR,BCSPACES                                                  
         OI    SETEXRH+FHOID,FHOITR                                             
         B     SETVALX                                                          
SETVAL72 TM    SETEXRH+FHIID,FHIIVA                                             
         BO    SETVALX                                                          
         NI    CSINDSL1,FF-CSIRDSPC                                             
         XC    SETXVAL,SETXVAL                                                  
         TM    BCJOBSTA,BCJOBPEN   TEST ANYTHING PENDING                        
         BNZ   SETVAL80                                                         
*                                                                               
*&&UK                                                                           
         GOTO1 ATSTEURO,BOPARM,CSCPYCUR                                         
         BNE   *+8                                                              
         OI    SETXIND,CSEXCIFE    FROM EURO CURRENCY                           
         GOTO1 (RF),(R1),CSBILCUR                                               
         BNE   *+8                                                              
         OI    SETXIND,CSEXCITE    TO EURO CURRENCY                             
*&&                                                                             
*                                                                               
         MVC   SETEXRD,BCSPACES                                                 
         OI    SETEXRDH+FHOID,FHOITR                                            
         TM    SETXIND,CSEXCIFE+CSEXCITE                                        
         BO    SETVAL80            DON'T VALIDATE IF TO & FROM EURO CUR         
         BZ    SETVAL74                                                         
         CLC   CSCPYCUR,=C'EUR'                                                 
         BE    SETVAL80                                                         
         CLC   CSBILCUR,=C'EUR'                                                 
         BE    SETVAL80                                                         
SETVAL74 GOTO1 AFVAL,SETEXRH                                                    
         BNE   SETVAL80                                                         
         TM    SETXIND,CSEXCIFE+CSEXCITE                                        
         BZ    SETVAL76                                                         
         CLI   FVIFLD,C'E'                                                      
         BNE   SETVAL76                                                         
         MVC   FVIFLD(L'FVIFLD-1),FVIFLD+1                                      
         MVC   FVILEN,FVXLEN                                                    
SETVAL76 GOTO1 AVALAMT,BOPARM,(X'85',FVIHDR),(L'BOPL61,BOPL61)                  
         BNE   EXIT                                                             
         CP    BOPL61,BCPZERO                                                   
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INAMT)                                           
         B     EXIT                                                             
         SRP   BOPL61,1,0                                                       
         MVC   SETXRAT,BOPL61                                                   
         IC    RE,CSCURCPY+(CURTDECP-CURTABD)                                   
         IC    RF,CSCURBIL+(CURTDECP-CURTABD)                                   
         SR    RF,RE                                                            
         STC   RF,SETXSHF                                                       
SETVAL80 GOTO1 ASETUP,BOPARM,(X'08',0),0,SETXVAL                                
         BNE   EXIT                                                             
         BAS   RE,GETRAT                                                        
*                                                                               
SETVALX  OI    CSINDSG1,CSINDSET                                                
         EJECT                                                                  
***********************************************************************         
* TEST FOR DRAFT BILL SELECTIONS                                      *         
***********************************************************************         
         SPACE 1                                                                
SETSEL   TM    CSINDSL1,CSIRDSPC   TEST RECORD DISPLAYED FOR CHANGE             
         BZ    SETLST                                                           
         TM    SETFLAG,SETFEXIT    TEST EXIT FLAG SET                           
         BNZ   *+12                                                             
         CLI   BCPFKEY,PFKSAFMQ    TEST AUTO FORM                               
         BE    AUTFRM                                                           
         NI    SETFLAG,FF-(SETFRES+SETFDEL)                                     
         LA    R4,BILLTAB                                                       
         USING BILLTABD,R4                                                      
         LA    R3,DRATAB                                                        
SETSEL02 SR    R1,R1                                                            
         ICM   R1,3,0(R3)          TEST END OF TABLE                            
         BZ    SETSELX                                                          
         LA    R1,TWAD(R1)                                                      
         USING SETDB1AH,R1                                                      
         TM    SETDB1AH+FHATD,FHATPR                                            
         BNZ   SETSELX                                                          
         MVC   SETBLNO,SETDB1B                                                  
         GOTO1 AFVAL,(R1)                                                       
         BNE   SETSEL18            NO INPUT - GO TO NEXT FIELD                  
         CLI   FVIFLD,C'*'                                                      
         BE    SETSEL18                                                         
         DROP  R1                                                               
*                                                                               
         LA    R5,SUBTAB                                                        
         USING SUBTABD,R5                                                       
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
SETSEL04 CLI   SUBTABD,EOT         TEST END OF SUB-ACTION TABLE                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         SR    RE,RE                                                            
         ICM   RE,3,SUBTDSPN       RE=DISPLACEMENT TO ACTION WORD               
         LA    RE,TWAD(RE)                                                      
         EX    RF,*+8                                                           
         BNE   SETSEL08                                                         
         CLC   FVIFLD(0),0(RE)                                                  
*                                                                               
         XR    RF,RF               TEST SUB-ACTION ROUTINE TO CALL              
         ICM   RF,3,SUBTROUT                                                    
         BZ    SETSEL20            NO - NTRSES                                  
         MVC   FVIFLD(L'SETDB1A),0(RE)                                          
         LA    RF,CLB01(RF)                                                     
         GOTO1 (RF),BILLTABD                                                    
         BNE   SETSEL08                                                         
         XR    RE,RE                                                            
         ICM   RE,3,0(R3)                                                       
         LA    RE,TWAD(RE)                                                      
         USING SETDB1AH,RE                                                      
         MVI   SETDB1A,C'*'                                                     
         MVC   SETDB1A+1(L'SETDB1A-1),FVIFLD                                    
         OI    SETDB1AH+FHOID,FHOITR                                            
         DROP  RE                                                               
         B     SETSEL18                                                         
*                                                                               
SETSEL08 LA    R5,SUBTABL(R5)      BUMP TO NEXT SUB-ACTION ENTRY                
         B     SETSEL04                                                         
*                                                                               
SETSEL18 LA    R4,BILLTABL(R4)     BUMP TO BILL TABLE ENTRY                     
         LA    R3,L'DRATAB(R3)     BUMP TO NEXT TWA INPUT FIELD                 
         B     SETSEL02                                                         
*                                                                               
SETSEL20 TM    BILLINDS,BILLIDEL   TEST BILL WAS DELETED                        
         BZ    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXIT                                                             
         TM    SUBTINDS,SUBTICUR   TEST CURRENCY MUST BE EQUAL                  
         BZ    SETSEL30                                                         
         CLC   BILLCUR,CSBILCUR                                                 
         BE    SETSEL22                                                         
         MVC   FVXTRA(L'BILLCUR),BILLCUR                                        
         MVC   FVMSGNO,=AL2(AE$BCDMA)                                           
         B     EXIT                                                             
SETSEL22 CLC   CSCPYCUR,CSBILCUR                                                
         BE    SETSEL24                                                         
         CLC   BCCPYSEC,CSBILCUR                                                
         BE    SETSEL24                                                         
         CLC   BILLRRAT,CSEXCRAT   COMPARE EXCHANGE RATES                       
         BE    SETSEL24                                                         
         GOTO1 AEDTRAT,BOPARM,(L'FVXTRA,FVXTRA),BILLRRAT,0                      
         MVC   FVMSGNO,=AL2(AE$BRDMA)                                           
         B     EXIT                                                             
*                                                                               
SETSEL24 TM    SUBTINDS,SUBTIALL   TEST ALLOCATION MUST BE EQUAL                
         BZ    SETSEL30                                                         
         CP    BILLNET,SETCBAPN                                                 
         BNE   *+14                                                             
         CP    BILLCOM,SETCBAPC                                                 
         BE    SETSEL30                                                         
         MVC   FVMSGNO,=AL2(AE$DBNEJ)                                           
         B     EXIT                                                             
*                                                                               
SETSEL30 MVC   CSBILNUM,SETBLNO    SET BILL NUMBER                              
         GOTO1 ANTRSES,SUBTPARM    CALL ACTION SUBROUTINE                       
         DROP  R5                                                               
         DROP  R4                                                               
*                                                                               
SETSELX  TM    SETFLAG,SETFDEL+SETFRES                                          
         BZ    SETLST              TEST ANY BILLS DELETED/RESTORED              
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$DBHBD)                                           
         TM    SETFLAG,SETFDEL                                                  
         BO    SETLSTX                                                          
         MVC   FVMSGNO,=AL2(AI$DBHBR)                                           
         B     SETLSTX                                                          
         EJECT                                                                  
***********************************************************************         
* BUILD A LIST OF DRAFT BILL SELECTIONS                               *         
***********************************************************************         
         SPACE 1                                                                
SETLST   LA    R3,DRATAB                                                        
         LA    R4,BILLTAB                                                       
         USING BILLTABD,R4                                                      
         LA    R2,IOKEY            READ DRAFT BILL RECORDS & DISPLAY            
         USING PBRRECD,R2                                                       
         XC    PBRKEY,PBRKEY                                                    
         MVI   PBRKTYP,PBRKTYPQ                                                 
         MVC   PBRKCPY,CUABIN                                                   
         MVI   PBRKSUB,PBRKACTQ                                                 
         MVC   PBRKJOB,BCJOBCOD                                                 
*                                                                               
SETLST02 ICM   R1,3,PBRKSEQ                                                     
         CLM   R1,3,BCEFFS                                                      
         BE    SETLST14                                                         
         LA    R1,1(R1)                                                         
         STCM  R1,3,PBRKSEQ                                                     
         MVI   PBRKPARA,0                                                       
         MVI   PBRKLINE,0                                                       
         GOTO1 AIO,IOHID+IOACCDIR+IO1                                           
         CLC   PBRKEY(PBRKSEQ-PBRKEY),IOKEYSAV                                  
         BNE   SETLST14                                                         
         TM    IOERR,IOEDEL        IGNORE DELETED BILL RECORDS                  
         BNZ   SETLST02                                                         
         OC    PBRKBILD,PBRKBILD   TEST LIVE BILL                               
         BNZ   SETLST02                                                         
         CLI   PBRKPARA,0          SHOULD NEVER HAPPEN BUT DOES                 
         BNE   SETLST02                                                         
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                DIE IF CAN'T READ BILL RECORD                
         MVC   BILLDA,PBRKDA       SAVE BILL D/A                                
         GOTO1 GETAMT                                                           
         DROP  R2                                                               
*                                                                               
         XC    SETBLNO,SETBLNO     SET BILL NUMBER TO BINARY ZEROES             
         L     R1,AIO1                                                          
         LA    R1,PBRRFST-PBRRECD(R1)                                           
         XR    RF,RF                                                            
*                                                                               
         USING BLHELD,R1                                                        
SETLST04 CLI   BLHEL,BLHELQ        TEST BILL HEADER ELEMENT                     
         BE    SETLST06                                                         
         CLI   BLHEL,0                                                          
         BE    SETLST02                                                         
         IC    RF,BLHLN                                                         
         BXH   R1,RF,SETLST04                                                   
*                                                                               
SETLST06 MVC   SETBLNO,BLHBLNO     GET BILL NUMBER                              
*                                                                               
         MVC   BILLCUR,BLHCUR      SET UP BILL TABLE ENTRY                      
         CLC   BILLCUR,BCSPACES                                                 
         BH    *+10                                                             
         MVC   BILLCUR,CSCPYCUR                                                 
         MVC   BILLRVAL,BLHRVAL                                                 
         ZAP   BILLNET,SETBHAPN                                                 
         ZAP   BILLCOM,SETBHAPC                                                 
         MVI   BILLINDS,0                                                       
         DROP  R1                                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         LA    RF,TWAD(RF)         RF=A(TWA DRAFT BILL SELECT FIELD)            
         USING SETDB1AH,RF                                                      
         XC    SETDB1A,SETDB1A                                                  
         NI    SETDB1AH+FHATD,FF-FHATPR                                         
         OI    SETDB1AH+FHOID,FHOITR                                            
         OI    SETDB1BH+FHOID,FHOITR                                            
         MVC   SETDB1B(L'SETBLNO),SETBLNO                                       
         MVI   SETDB1B+L'SETBLNO,C'?'                                           
         CLC   BILLCUR,CSBILCUR    TEST BILL CURRENCY = ALLOCATION              
         BNE   SETLST12                                                         
         CLC   BILLCUR,CSCPYCUR                                                 
         BE    SETLST08                                                         
         CLC   BILLCUR,BCCPYSEC                                                 
         BE    SETLST08                                                         
         CLC   BILLRRAT,CSEXCRAT   TEST BILL EXC. RATE = ALLOCATION             
         BNE   SETLST12                                                         
SETLST08 MVI   SETDB1B+L'SETBLNO,C' '                                           
         CP    SETBHAPN,SETCBAPN   TEST ALLOCATION SAME AS JOB RECORD           
         BNE   *+14                                                             
         CP    SETBHAPC,SETCBAPC                                                
         BE    SETLST12                                                         
         ZAP   BODUB1,SETBHAPN                                                  
         AP    BODUB1,SETBHAPC                                                  
         ZAP   BODUB2,SETCBAPN                                                  
         AP    BODUB2,SETCBAPC                                                  
         MVI   SETDB1B+L'SETBLNO,C'*'                                           
         CP    BODUB1,BODUB2                                                    
         BE    SETLST12                                                         
         MVI   SETDB1B+L'SETBLNO,C'-'                                           
         BH    *+8                                                              
         MVI   SETDB1B+L'SETBLNO,C'+'                                           
         DROP  RF                                                               
*                                                                               
SETLST12 LA    R4,BILLTABL(R4)     BUMP TO NEXT BILL TABLE ENTRY                
         LA    R3,L'DRATAB(R3)     BUMP TO NEXT TWA DISPLACEMENT                
         OC    0(L'DRATAB,R3),0(R3)                                             
         BNZ   SETLST02                                                         
         DROP  R4                                                               
*                                                                               
SETLST14 SR    RF,RF               PROTECT REMAINING SELECT FIELDS              
         ICM   RF,3,0(R3)                                                       
         BZ    SETLST16                                                         
         LA    RF,TWAD(RF)                                                      
         USING SETDB1AH,RF                                                      
         XC    SETDB1A,SETDB1A                                                  
         OI    SETDB1AH+FHATD,FHATPR                                            
         OI    SETDB1AH+FHOID,FHOITR                                            
         XC    SETDB1B,SETDB1B                                                  
         OI    SETDB1BH+FHATD,FHATPR                                            
         OI    SETDB1BH+FHOID,FHOITR                                            
         LA    R3,L'DRATAB(R3)                                                  
         B     SETLST14                                                         
         DROP  RF                                                               
*                                                                               
         USING TRNRECD,IOKEY                                                    
SETLST16 ZAP   SETCBOO,BCPZERO                                                  
         XC    TRNKEY,TRNKEY                                                    
         MVC   TRNKCPY,CUABIN                                                   
         MVC   TRNKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   TRNKACT,BCJOBCOD                                                 
         MVC   TRNKWORK,=C'**'                                                  
         GOTO1 AIO,IOHI+IOACCDIR+IO1                                            
         B     LAB2                                                             
LAB1     GOTO1 AIO,IOSEQ+IOACCDIR+IO1                                           
*                                                                               
LAB2     CLC   TRNKEY(TRNKCULC-TRNKEY),IOKEYSAV                                 
         BNE   SETLST17            CHANGE OF JOB/WORKCODE                       
         CLC   TRNKREF,BCSPACES    TEST IF TRANSACTION                          
         BNH   LAB1                NO - MUST BE CONTRA HEADER                   
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         ZAP   BODUB1,BCPZERO                                                   
         ZAP   BODUB2,BCPZERO                                                   
         L     RE,AIO1                                                          
         LA    RE,TRNRFST-TRNRECD(RE)                                           
         SR    R0,R0                                                            
LAB3     CLI   0(RE),0             TEST E-O-R                                   
         BE    LAB7                YES - GET NEXT                               
         CLI   0(RE),OAMELQ                                                     
         BE    LAB5                                                             
         CLI   0(RE),PTAELQ                                                     
         BE    LAB6                                                             
LAB4     IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     LAB3                                                             
*                                                                               
         USING OAMELD,RE                                                        
LAB5     AP    SETCBOO,OAMAMNT                                                  
         ZAP   BODUB1,OAMIVAL                                                   
         B     LAB4                                                             
         DROP  RE                                                               
*                                                                               
         USING PTAELD,RE                                                        
LAB6     CLI   PTATYPE,PTATRAL     TEST REGULAR ALLOCATION                      
         BNE   LAB4                                                             
         TM    PTASTAT1,PTASPEND   TEST UPDATED                                 
         BO    LAB4                NO - STILL PENDING                           
         AP    BODUB2,PTANET       KEEP AGENCY CURRENCY TOTAL                   
         B     LAB4                                                             
         DROP  RE                                                               
*                                                                               
LAB7     CP    BODUB1,BODUB2       GET GREATER OF INVOICED/BILLED               
*        BH    *+10                ** CHANGED **                                
*        ZAP   BODUB1,BODUB2       ALWAYS USE INVOICED AMOUNT                   
         SP    SETCBOO,BODUB1      REDUCE OUTSTANDING AMOUNT                    
         B     LAB1                                                             
*                                                                               
SETLST17 OI    SETDFAH+FHOID,FHOITR                                             
         CLC   CSCPYCUR,CSBILCUR   SHOW CURRENCY IF NOT AGYCURR                 
         BNE   SETLST18                                                         
         MVCDD SETDFA,AC#NDRAC,F                                                
         GOTO1 VDICTAT,BODMCB,C'SL  ',SETDFA                                    
         B     SETLST20                                                         
SETLST18 LA    RF,SETDFA+L'SETDFA-5                                             
         MVI   0(RF),C'('                                                       
         MVC   1(L'CSBILCUR,RF),CSBILCUR                                        
         MVI   L'CSBILCUR+1(RF),C')'                                            
SETLST20 CLI   CUCTRY,CTRYUSA      TEST USA                                     
         BE    SETLST24                                                         
         CURED SETCBAPN,(L'SETDA1,SETDA1),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA1H+FHOID,FHOITR                                             
         CURED SETCBAPC,(L'SETDA2,SETDA2),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA2H+FHOID,FHOITR                                             
         AP    SETCBWPT,SETCBRPT                                                
         CURED SETCBWPT,(L'SETDA3,SETDA3),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA3H+FHOID,FHOITR                                             
         AP    SETCBWPC,SETCBRPC                                                
         CURED SETCBWPC,(L'SETDA4,SETDA4),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA4H+FHOID,FHOITR                                             
         CURED SETCBTP,(L'SETDA5,SETDA5),CSCURBIL,MINUS=YES,           X        
               ZERO=NOBLANK                                                     
         OI    SETDA5H+FHOID,FHOITR                                             
         CURED SETCBIP,(L'SETDA6,SETDA6),CSCURBIL,MINUS=YES,           X        
               ZERO=NOBLANK                                                     
         OI    SETDA6H+FHOID,FHOITR                                             
*                                  ORDERS                                       
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    SETLST21                                                         
*&&UK                                                                           
         LA    R2,BOWORK1                                                       
         USING EURKBLKD,R2                                                      
         XC    0(EURKBLKL,R2),0(R2)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         ZAP   BODUB1,SETCBOO                                                   
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),BODUB1,SETCBOO                 
         DROP  R2                                                               
*&&                                                                             
SETLST21 CURED SETCBOO,(L'SETDA7,SETDA7),CSCURBIL,MINUS=YES,           X        
               ZERO=NOBLANK                                                     
         OI    SETDA7H+FHOID,FHOITR                                             
         CLC   CSBILCUR,CSCPYCUR                                                
         BE    SETLST22                                                         
*&&UK                                                                           
         LA    R2,BOWORK1                                                       
         USING EURKBLKD,R2                                                      
         XC    0(EURKBLKL,R2),0(R2)                                             
         MVC   EURKCUFR,CSCPYCUR                                                
         MVC   EURKCUTO,CSBILCUR                                                
         MVC   EURKRULE,CSEXCVAL                                                
         ZAP   BODUB1,SETBLBL                                                   
         GOTO1 VEUREKA,BODMCB,('APPLYQ',BOWORK1),BODUB1,SETBLBL                 
         DROP  R2                                                               
*&&                                                                             
SETLST22 CURED SETBLBL,(L'SETDA8,SETDA8),CSCURBIL,MINUS=YES,           X        
               ZERO=NOBLANK                                                     
         OI    SETDA8H+FHOID,FHOITR                                             
         B     SETLST26                                                         
*                                                                               
SETLST24 CURED SETCBAPN,(L'SETDA1,SETDA1),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA1H+FHOID,FHOITR                                             
         CURED SETCBWPT,(L'SETDA2,SETDA2),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA2H+FHOID,FHOITR                                             
         CURED SETCBWPC,(L'SETDA3,SETDA3),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA3H+FHOID,FHOITR                                             
         CURED SETCBTP,(L'SETDA4,SETDA4),CSCURBIL,MINUS=YES,           X        
               ZERO=NOBLANK                                                     
         OI    SETDA4H+FHOID,FHOITR                                             
         CURED SETCBIP,(L'SETDA5,SETDA5),CSCURBIL,MINUS=YES,           X        
               ZERO=NOBLANK                                                     
         OI    SETDA5H+FHOID,FHOITR                                             
         CURED SETBLBL,(L'SETDA8,SETDA8),CSCURBIL,MINUS=YES,           X        
               ZERO=NOBLANK                                                     
         OI    SETDA8H+FHOID,FHOITR                                             
         CURED SETCBRPT,(L'SETDA6,SETDA6),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA6H+FHOID,FHOITR                                             
         CURED SETCBRPC,(L'SETDA7,SETDA7),CSCURBIL,MINUS=YES,          X        
               ZERO=NOBLANK                                                     
         OI    SETDA7H+FHOID,FHOITR                                             
*                                                                               
SETLST26 TM    SETDB1AH+FHATD,FHATPR                                            
         BZ    SETLST28                                                         
         CLI   BCPFKEY,PFKSAFMQ    TEST AUTO FORM                               
         BE    AUTFRM                                                           
         LA    R1,SETCLIH          NO BILLS - CURSOR TO CLIENT                  
         ST    R1,FVADDR                                                        
         MVC   FVMSGNO,=AL2(AI$PFACT)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXIT                                                             
*                                                                               
SETLST28 MVC   FVMSGNO,=AL2(AI$SBOPF)                                           
         MVI   FVOMTYP,GTMINF                                                   
*                                                                               
SETLSTX  LA    R1,SETDB1AH         SET CURSOR TO FIRST BILL# FIELD              
         ST    R1,FVADDR                                                        
         OI    CSINDSL1,CSIRDSPC   SET RECORD DISPLAYED FOR CHANGE              
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DELETE A BILL                                                       *         
*                                                                     *         
* NTRY: R1 = A(BILL TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
DELBILL  NTR1  ,                                                                
         LR    R4,R1                                                            
         USING BILLTABD,R4         R4=A(BILL TABLE ENTRY)                       
         TM    BILLINDS,BILLIDEL   TEST BILL ALREADY DELETED                    
         BO    EXITN                                                            
*                                                                               
         MVC   IODAOVER,BILLDA     DELETE BILL FILE RECORD                      
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         TM    PBRRSTAT,PBRSDELT   TEST DELETED ELSEWHERE                       
         BO    DELBILLX                                                         
         OI    PBRRSTAT,PBRSDELT                                                
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3           R3=A(BILL HEADER ELEMENT)                    
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         MVC   IOKEY(L'PBRKEY),PBRKEY                                           
         LA    R2,IOKEY            DELETE BILL DIRECTORY RECORD                 
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PBRKSTAT,PBRSDELT                                                
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    PBRPAS,PBRPAS       DELETE PASSIVE                               
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,BLHBLNO                                                 
         MVI   PBRPIND,PBRPIDFT                                                 
         MVC   PBRPUSER,BLHUSER                                                 
         MVC   PBRPJOB,BLHJOB                                                   
         MVC   PBRPCRED,BLHCRED                                                 
         MVC   PBRPFORM,BLHFORM                                                 
         MVC   PBRPPERS,BLHPERS                                                 
         GOTO1 AIO,IORDUP+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PBRKSTAT,PBRSDELT                                                
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
DELBILLX OI    BILLINDS,BILLIDEL                                                
         OI    SETFLAG,SETFDEL                                                  
         B     EXITY                                                            
         DROP  R4                                                               
         SPACE 1                                                                
***********************************************************************         
* RESTORE A BILL                                                      *         
*                                                                     *         
* NTRY: R1 = A(BILL TABLE ENTRY)                                      *         
***********************************************************************         
         SPACE 1                                                                
RESBILL  NTR1  ,                                                                
         LR    R4,R1                                                            
         USING BILLTABD,R4         R4=A(BILL TABLE ENTRY)                       
         TM    BILLINDS,BILLIDEL   TEST BILL IS DELETED                         
         BZ    EXITN                                                            
*                                                                               
         MVC   IODAOVER,BILLDA     RESTORE BILL FILE RECORD                     
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         NI    PBRRSTAT,FF-PBRSDELT                                             
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R3,PBRRFST                                                       
         USING BLHELD,R3           R3=A(BILL HEADER ELEMENT)                    
         XR    RF,RF                                                            
         CLI   BLHEL,BLHELQ                                                     
         BE    *+12                                                             
         IC    RF,BLHLN                                                         
         BXH   R3,RF,*-12                                                       
*                                                                               
         MVC   IOKEY(L'PBRKEY),PBRKEY                                           
         LA    R2,IOKEY            RESTORE BILL DIRECTORY RECORD                
         GOTO1 AIO,IORDUPD+IOACCDIR                                             
         NI    PBRKSTAT,FF-PBRSDELT                                             
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    PBRPAS,PBRPAS       RESTORE PASSIVE                              
         MVI   PBRPTYP,PBRPTYPQ                                                 
         MVC   PBRPCPY,CUABIN                                                   
         MVI   PBRPSUB,PBRPPASQ                                                 
         MVC   PBRPBLNO,BLHBLNO                                                 
         MVI   PBRPIND,PBRPIDFT                                                 
         MVC   PBRPUSER,BLHUSER                                                 
         MVC   PBRPJOB,BLHJOB                                                   
         MVC   PBRPCRED,BLHCRED                                                 
         MVC   PBRPFORM,BLHFORM                                                 
         MVC   PBRPPERS,BLHPERS                                                 
         GOTO1 AIO,IORDUPD+IOACCDIR                                             
         NI    PBRKSTAT,FF-PBRSDELT                                             
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
RESBILLX NI    BILLINDS,FF-BILLIDEL                                             
         OI    SETFLAG,SETFRES                                                  
         B     EXITY                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* AUTOFORM REQUESTED                                                  *         
***********************************************************************         
         SPACE 1                                                                
AUTFRM   GOTO1 VCOLY,BODMCB,('O#BILFRM',0),0,0                                  
         L     RF,BODMCB                                                        
         BASR  RE,RF                                                            
         BNE   EXIT                                                             
         GOTO1 ANTRSES,SUBEDT                                                   
*                                                                               
SUBEDT   DC    AL1(RECBIL,ACTEDT)                                               
         DC    AL1(1,0,0,0)                                                     
         EJECT                                                                  
***********************************************************************         
* ESTABLISH CURRENCY TABLE ENTRY                                      *         
***********************************************************************         
         SPACE 1                                                                
GETCUR   NTR1  ,                                                                
         MVC   SETCUR,CSBILCUR                                                  
         OI    SETCURH+FHOID,FHOITR                                             
         MVC   SETCURN,BCSPACES                                                 
         NI    SETCURH+FHIID,FF-FHIIVA                                          
         GOTO1 AGETCUR,BOPARM,(X'40',CSBILCUR),,SETCURN                         
         BNE   EXITN                                                            
         OI    SETCURH+FHIID,FHIIVA                                             
         OI    SETCURNH+FHOID,FHOITR                                            
         B     EXITY                                                            
         SPACE 2                                                                
***********************************************************************         
* EDIT EXCHANGE RATE INTO TWA FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
GETRAT   NTR1  ,                                                                
         LA    RF,SETEXR                                                        
         TM    CSEXCIND,CSEXCIFE+CSEXCITE  FROM/TO EURO                         
         BZ    *+12                                                             
         MVI   0(RF),C'E'                                                       
         LA    RF,1(RF)                                                         
         GOTO1 AEDTRAT,BOPARM,(L'SETEXR,(RF)),CSEXCRAT,0                        
         OI    SETEXRH+FHOID,FHOITR                                             
         OI    SETEXRH+FHIID,FHIIVA                                             
         MVC   SETEXRD,BCSPACES                                                 
         OI    SETEXRDH+FHOID,FHOITR                                            
         LA    R0,RATDSCN          GET RATE DESCRIPTION                         
         LA    RF,RATDSC                                                        
         CLC   CSTYPEXC,0(RF)                                                   
         BE    GETRAT02                                                         
         LA    RF,L'RATDSC(RF)                                                  
         BCT   R0,*-14                                                          
         B     GETRATX                                                          
GETRAT02 MVC   SETEXRD(4),1(RF)                                                 
         GOTO1 VDICTAT,BOPARM,C'SL  ',SETEXRD                                   
         CLI   CSTYPEXC,JCBXEURQ                                                
         BE    EXIT                                                             
         LA    R2,SETEXRD+14                                                    
         CLI   0(R2),C' '                                                       
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         MVI   2(R2),C'-'                                                       
         GOTO1 VDATCON,BOPARM,(X'42',CSDATEXC),(17,4(R2))                       
GETRATX  B     EXIT                                                             
         SPACE 1                                                                
RATDSC   DS    0XL5                ** RATE DESCRIPTION TABLE **                 
         DC    AL1(JCBXINPQ)       INPUT RATE                                   
         DCDDL AC#INPRT,RATDSCLQ                                                
         DC    AL1(JCBXFTQ)        FT RATE                                      
         DCDDL AC#FTRAT,RATDSCLQ                                                
         DC    AL1(JCBXAGYQ)       AGENCY RATE                                  
         DCDDL AC#AGYRT,RATDSCLQ                                                
         DC    AL1(JCBXCLIQ)       CLIENT RATE                                  
         DCDDL AC#CLIRT,RATDSCLQ                                                
         DC    AL1(JCBXEURQ)       FIXED EURO RATE                              
         DCDDL AC#FEURR,L'SETEXRD                                               
RATDSCX  DS    0X                                                               
RATDSCN  EQU   (*-RATDSC)/L'RATDSC                                              
RATDSCLQ EQU   15                                                               
         DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE FORMAT CODE                                     *         
*                                                                     *         
* NTRY: CSFORMAT = FORMAT CODE                                        *         
*       CSFMLANG = LANGUAGE CODE OR 0                                 *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETFMT   NTR1  ,                                                                
         MVC   SETFRM,BCSPACES     OUTPUT NUMBER                                
         OI    SETFRMH+FHOID,FHOITR                                             
         EDIT  CSFORMAT,(3,SETFRM),0,ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1          
         NI    SETFRMH+FHIID,FF-FHIIVA                                          
         TM    SETLNGH+FHATD,FHATPR                                             
         BO    GETFMT10                                                         
         MVC   SETLNG,BCSPACES     OUTPUT LANGUAGE                              
         OI    SETLNGH+FHOID,FHOITR                                             
         NI    SETLNGH+FHIID,FF-FHIIVA                                          
         L     R1,ALANG                                                         
         LH    RE,0(R1)            RE=ENTRY LENGTH                              
         L     RF,2(R1)            RF=A(END OF TABLE-1)                         
         LA    R1,6(R1)            R1=A(FIRST LANGUAGE ENTRY)                   
         MVC   BOBYTE1,CSFMLANG                                                 
         CLI   BOBYTE1,0                                                        
         BNE   *+14                                                             
         MVC   BOBYTE1,CULANG                                                   
         B     GETFMT02                                                         
         CLI   BOBYTE1,LANGEUK                                                  
         BNE   GETFMT02                                                         
         MVI   BOBYTE1,LANGENG                                                  
         USING LANGTABD,R1                                                      
GETFMT02 CLC   LANGCODE,BOBYTE1                                                 
         BE    GETFMT04                                                         
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                                                             
GETFMT04 CLC   BOBYTE1,CULANG      TEST CONNECTED IN NATIVE LANGUAGE            
         BE    GETFMT06                                                         
         LA    RF,L'LANGFUL-1      TRY ENGLISH LANGUAGE FULL NAME               
         LA    RE,LANGFUL                                                       
         CLM   RF,1,=AL1(L'SETLNG-1)                                            
         BNH   *+12                                                             
         LA    RF,L'LANGSHR-1      TRY ENGLISH LANGUAGE SHORT NAME              
         LA    RE,LANGSHR                                                       
         CLM   RF,1,=AL1(L'SETLNG-1)                                            
         BNH   *+8                                                              
         LA    RF,L'SETLNG-1       TRUNCATE SHORT NAME TO FIT SCREEN            
         EX    RF,*+4                                                           
         MVC   SETLNG(0),0(RE)                                                  
         B     GETFMT10                                                         
GETFMT06 LA    RF,L'LANGFULN-1     TRY NATIVE LANGUAGE FULL NAME                
         LA    RE,LANGFULN                                                      
         CLM   RF,1,=AL1(L'SETLNG-1)                                            
         BNH   *+12                                                             
         LA    RF,L'LANGSHRN-1     TRY NATIVE LANGUAGE SHORT NAME               
         LA    RE,LANGSHRN                                                      
         CLM   RF,1,=AL1(L'SETLNG-1)                                            
         BNH   *+8                                                              
         LA    RF,L'SETLNG-1       TRUNCATE SHORT NAME TO FIT SCREEN            
         EX    RF,*+4                                                           
         MVC   SETLNG(0),0(RE)                                                  
         DROP  R1                                                               
*                                                                               
GETFMT10 MVC   SETFRMN,BCSPACES                                                 
         OI    SETFRMNH+FHOID,FHOITR                                            
         USING PBCRECD,IOKEY       READ FORMAT CONTROL RECORD                   
         XC    PBCKEY,PBCKEY                                                    
         MVI   PBCKTYP,PBCKTYPQ                                                 
         MVC   PBCKCPY,CUABIN                                                   
         MVI   PBCKSUB,PBCKCONQ                                                 
         MVC   PBCKFMT,CSFORMAT                                                 
         MVC   PBCKLANG,CSFMLANG                                                
         GOTO1 AIO,IO3+IOACCDIR+IORD                                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITN                                                            
         MVC   IODAOVER,PBCKDA     OUTPUT FORMAT NAME                           
         GOTO1 AIO,IO3+IOACCMST+IOGET                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO3                                                          
         LA    R1,PBCRFST-PBCRECD(R1)                                           
         USING NAMELD,R1                                                        
         XR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         CLI   NAMEL,NAMELQ                                                     
         BE    *+8                                                              
         BXH   R1,RF,*-12                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+4                                                           
         MVC   SETFRMN(0),NAMEREC                                               
*                                  ENSURE A SECTION DEFINITION EXISTS           
K        USING PBSRECD,IOKEY                                                    
         XC    K.PBSKEY,K.PBSKEY                                                
         MVI   K.PBSKTYP,PBSKTYPQ                                               
         MVC   K.PBSKCPY,CUABIN                                                 
         MVI   K.PBSKSUB,PBSKDEFQ                                               
         MVC   K.PBSKFMT,CSFORMAT                                               
         MVC   K.PBSKLANG,CSFMLANG                                              
         GOTO1 AIO,IOHIGH+IOACCDIR                                              
         CLC   K.PBSKEY(PBSKSEC-PBSKEY),IOKEYSAV                                
         BE    GETFMT20                                                         
         MVC   FVMSGNO,=AL2(AE$NOSEC)                                           
         XC    FVPARMS,FVPARMS                                                  
         MVI   FVPARMS,L'SETFRM+1                                               
         MVC   FVPARMS+1(L'SETFRM),SETFRM                                       
         MVI   FVPARMS+L'SETFRM+1,1                                             
         TM    SETLNGH+FHATD,FHATPR                                             
         BO    EXITN                                                            
         MVI   FVPARMS+L'SETFRM+1,L'SETLNG+1                                    
         MVC   FVPARMS+L'SETFRM+2(L'SETLNG),SETLNG                              
         B     EXITN                                                            
         DROP  K                                                                
*                                                                               
GETFMT20 OI    SETFRMH+FHIID,FHIIVA                                             
         OI    SETLNGH+FHIID,FHIIVA                                             
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* CLEAR SCREEN IF CLI/PRO/JOB NOT SPECIFIED                           *         
***********************************************************************         
         SPACE 1                                                                
CLRSCRN  NTR1                                                                   
         NI    CSINDSL1,FF-(CSIRDSPC)                                           
         LA    R3,DRATAB                                                        
CLRSCR10 SR    RF,RF               PROTECT SELECT FIELDS                        
         ICM   RF,3,0(R3)                                                       
         BZ    CLRSCR20                                                         
         LA    RF,TWAD(RF)                                                      
         USING SETDB1AH,RF                                                      
         XC    SETDB1A,SETDB1A                                                  
         OI    SETDB1AH+FHATD,FHATPR                                            
         OI    SETDB1AH+FHOID,FHOITR                                            
         XC    SETDB1B,SETDB1B                                                  
         OI    SETDB1BH+FHATD,FHATPR                                            
         OI    SETDB1BH+FHOID,FHOITR                                            
         LA    R3,L'DRATAB(R3)                                                  
         B     CLRSCR10                                                         
CLRSCR20 LA    R3,DATTAB                                                        
*                                                                               
CLRSCR30 SR    RF,RF               CLEAR DRAFT ACTIVITY FIELDS                  
         ICM   RF,3,0(R3)                                                       
         BZ    CLRSCR40                                                         
         LA    RF,TWAD(RF)                                                      
         XC    L'SETDA1H(L'SETDA1,RF),L'SETDA1H(RF)                             
         OI    (FVOIND-FVIHDR)(RF),FVOXMT                                       
         LA    R3,L'DATTAB(R3)                                                  
         B     CLRSCR30                                                         
*                                                                               
CLRSCR40 B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALCULATE NET/COMMISSION AMOUNTS                         *         
*                                                                     *         
* NTRY - IO1=BILL HEADER RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
GETAMT   NTR1  ,                                                                
         L     R2,AIO1                                                          
         USING PBRRECD,R2                                                       
         ZAP   SETBHAPN,BCPZERO    SET ALLOCATION (NET) TO ZERO                 
         ZAP   SETBHAPC,BCPZERO    SET ALLOCATION (COMMISSION) TO ZERO          
*                                                                               
         LA    R1,PBRRFST                                                       
         USING NDXELD,R1           R1=A(INDEX ELEMENT)                          
         XR    RF,RF                                                            
         CLI   NDXEL,NDXELQ                                                     
         BE    *+12                                                             
         IC    RF,NDXLN                                                         
         BXH   R1,RF,*-12                                                       
         XR    R0,R0                                                            
         ICM   R0,1,NDXACTV        R0=NO. OF ACTIVE ENTRIES                     
         BZ    EXIT                                                             
         LA    R5,NDXINDX          R5=A(LIST OF ACTIVE ENTRIES)                 
         DROP  R1                                                               
*                                                                               
GETAMT02 CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   IOKEY,PBRKEY                                                     
         MVC   IOKEY+(PBRKPARA-PBRRECD)(L'PBRKPARA),0(R5)                       
         GOTO1 AIO,IOREAD+IOACCMST+IO2                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2                                                          
         LA    R1,PBRRFST-PBRRECD(R1)                                           
         USING PGHELD,R1                                                        
         SR    RF,RF                                                            
         CLI   PGHEL,PGHELQ                                                     
         BE    *+12                                                             
         IC    RF,PGHLN                                                         
         BXH   R1,RF,*-12                                                       
         AP    SETBHAPN,PGHNET     UPDATE AMOUNTS                               
         AP    SETBHAPC,PGHCOM                                                  
         LA    R5,1(R5)                                                         
         BCT   R0,GETAMT02                                                      
         DROP  R1                                                               
*                                                                               
GETAMTX  B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
         LTORG                                                                  
         SPACE 1                                                                
ACCMST   DC    C'ACCMST '                                                       
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
DMCB     EQU   BOPARM                                                           
         SPACE 1                                                                
DRATAB   DS    0AL2                ** DISPS. TO TWA DRAFT BILL FLDS **          
         DC    AL2(SETDB1AH-TWAD)                                               
         DC    AL2(SETDB2AH-TWAD)                                               
         DC    AL2(SETDB3AH-TWAD)                                               
         DC    AL2(SETDB4AH-TWAD)                                               
         DC    AL2(SETDB5AH-TWAD)                                               
         DC    AL2(SETDB6AH-TWAD)                                               
         DC    AL2(SETDB7AH-TWAD)                                               
         DC    AL2(SETDB8AH-TWAD)                                               
         DC    AL2(SETDB9AH-TWAD)                                               
         DC    AL2(SETDBAAH-TWAD)                                               
         DC    AL2(SETDBBAH-TWAD)                                               
         DC    AL2(SETDBCAH-TWAD)                                               
DRATABX  DC    AL2(0)                                                           
*                                                                               
DATTAB   DS    0AL2                ** DISPS. TO DRAFT ACTIVITY FLDS **          
         DC    AL2(SETDA1H-TWAD)                                                
         DC    AL2(SETDA2H-TWAD)                                                
         DC    AL2(SETDA3H-TWAD)                                                
         DC    AL2(SETDA4H-TWAD)                                                
         DC    AL2(SETDA5H-TWAD)                                                
         DC    AL2(SETDA6H-TWAD)                                                
         DC    AL2(SETDA7H-TWAD)                                                
         DC    AL2(SETDA8H-TWAD)                                                
DATTABX  DC    AL2(0)                                                           
         SPACE 2                                                                
SUBTAB   DS    0X                  ** SUB-ACTION SELECTION TABLE **             
*                                                                               
         DC    AL2(UC@EDIT-TWAD)   EDIT A DRAFT BILL                            
         DC    AL1(SUBTICUR)                                                    
         DC    AL2(0)                                                           
         DC    AL1(RECBIL,ACTEDT)                                               
         DC    AL1(SETSUB#1,0,0,0)                                              
*                                                                               
         DC    AL2(UC@LSPAR-TWAD)  DISPLAY/CHANGE A DRAFT BILL                  
         DC    AL1(SUBTICUR)                                                    
         DC    AL2(0)                                                           
         DC    AL1(RECBIL,ACTLPAR)                                              
         DC    AL1(SETSUB#1,0,0,0)                                              
*                                                                               
         DC    AL2(UC@UPDT-TWAD)   UPDATE A BILL                                
         DC    AL1(SUBTIALL+SUBTICUR)                                           
         DC    AL2(0)                                                           
         DC    AL1(RECBIL,ACTUPD)                                               
         DC    AL1(SETSUB#1,0,0,0)                                              
*                                                                               
         DC    AL2(UC@DRAFT-TWAD)  DRAFT A BILL                                 
         DC    AL1(SUBTIALL+SUBTICUR)                                           
         DC    AL2(0)                                                           
         DC    AL1(RECBIL,ACTDRA)                                               
         DC    AL1(SETSUB#1,0,0,0)                                              
*                                                                               
         DC    AL2(UC@DEL-TWAD)    DELETE A BILL                                
         DC    AL1(0)                                                           
         DC    AL2(DELBILL-CLB01)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
*                                                                               
         DC    AL2(UC@RSR-TWAD)    RESTORE A BILL                               
         DC    AL1(0)                                                           
         DC    AL2(RESBILL-CLB01)                                               
         DC    AL1(0,0)                                                         
         DC    AL1(0,0,0,0)                                                     
*                                                                               
SUBTABX  DC    AL1(EOT)                                                         
         EJECT                                                                  
SETWORKD DSECT                     ** LOCAL WORKING STORAGE **                  
SETCAAFC DS    A                   A(AFC ELEMENT ON JOB RECORD)                 
SETFLAG  DS    XL1                 FLAG BYTE                                    
SETFEXIT EQU   X'80'               EXIT ROUTINE CALLED                          
SETFCHNG EQU   X'40'               CHANGE OF CLIENT/PRODUCT/JOB KEY             
SETFDEL  EQU   X'20'               BILL HAS BEEN DELETED                        
SETFRES  EQU   X'10'               BILL HAS BEEN RESTORED                       
SETCBAPN DS    PL8                 PENDING ALLOCATION (NET)                     
SETCBAPC DS    PL8                 PENDING ALLOCATION (COMMISSION)              
SETCBWPT DS    PL8                 PENDING WRITE-OFF (TIME/LABOR)               
SETCBWPC DS    PL8                 PENDING WRITE-OFF (COST/OOP)                 
SETCBRPT DS    PL8                 PENDING RECOVERY (TIME/LABOR)                
SETCBRPC DS    PL8                 PENDING RECOVERY (COST/OOP)                  
SETCBTP  DS    PL8                 PENDING TRANSFER                             
SETCBIP  DS    PL8                 PENDING INTERNAL INVOICE                     
SETCBOO  DS    PL8                 OUTSTANDING ORDERS                           
SETBLBL  DS    PL8                 TOTAL BILLABLE ON JOB                        
SETBHAPN DS    PL8                 BILL HEADER ALLOCATION (NET)                 
SETBHAPC DS    PL8                 BILL HEADER ALLOCATION (COMMISSION)          
SETXVAL  DS    0XL7                EXCHANGE RATE VALUES                         
SETXIND  DS    XL1                 EXCHANGE STATUS                              
SETXRAT  DS    PL5                 EXCHANGE RATE                                
SETXSHF  DS    XL1                 EXCHANGE SHIFT                               
SETCURR  DS    CL3                 GETOPT CURRENCY CODE                         
SETBLNO  DS    CL(L'BLHBLNO)       DRAFT BILL NUMBER                            
SETKEY   DS    CL42                                                             
SETWORKX EQU   *                                                                
         SPACE 2                                                                
SUBTABD  DSECT                     ** BILL SUB-ACTION TABLE **                  
SUBTDSPN DS    AL2                 DISPLACEMENT TO ACTION WORD                  
SUBTINDS DS    XL1                 INDICATOR BYTE                               
SUBTIALL EQU   X'80'               BILL ALLOCATION MUST = PENDING               
SUBTICUR EQU   X'40'               BILL CURRENCY/RATE MUST = PENDING            
SUBTROUT DS    AL2                 ROUTINE DISPLACEMENT IF NOT NTRSES           
SUBTPARM DS    0XL6                                                             
SUBTRECA DS    0XL2                RECORD/ACTION COMBO                          
SUBTREC  DS    XL1                 RECORD NUMBER (OR ZERO)                      
SUBTACT  DS    XL1                 ACTION NUMBER (OR ZERO)                      
SUBTRTN  DS    XL1                 RETURN ROUTINE NUMBER                        
SUBTNXPF DS    XL1                 NEXT TIME AUTO PFKEY VALUE                   
SUBTNSI1 DS    XL1                 NEXT SESSION INDICATORS - 1                  
SUBTNSI2 DS    XL1                 NEXT SESSION INDICATORS - 2                  
SUBTABL  EQU   *-SUBTABD                                                        
         SPACE 2                                                                
BILLTABD DSECT                     ** BILL DATA TABLE **                        
BILLCUR  DS    CL3                 BILL CURRENCY                                
BILLRVAL DS    0XL7                BILL EXCHANGE RATE VALUE                     
BILLRIND DS    XL1                 BILL EXCHANGE STATUS                         
BILLRRAT DS    PL5                 BILL EXCHANGE RATE                           
BILLRSHF DS    XL1                 BILL EXCHANGE SHIFT                          
BILLNET  DS    PL8                 BILL NET AMOUNT                              
BILLCOM  DS    PL8                 BILL COMMISSION AMOUNT                       
BILLINDS DS    XL1                 BILL INDICATOR BYTE                          
BILLIDEL EQU   X'80'               USER HAS DELETED BILL                        
BILLDA   DS    AL4                 BILL DISK ADDRESS                            
BILLTABL EQU   *-BILLTABD                                                       
BILLTABN EQU   (DRATABX-DRATAB)/L'DRATAB                                        
         EJECT                                                                  
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCLBWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACCLBWORKB                                                     
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   BASOLAYH                                                         
       ++INCLUDE ACCLBFED                                                       
         SPACE 1                                                                
         ORG   OSVALS                                                           
BILLTAB  DS    (BILLTABN)XL(BILLTABL)                                           
         ORG   OSVALS+OSVALSL                                                   
         SPACE 1                                                                
* GEGENEXC                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENEXC                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* DDFH                                                                          
         PRINT OFF                                                              
       ++INCLUDE DDFH                                                           
         PRINT ON                                                               
         SPACE 1                                                                
* FALANG                                                                        
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'070ACCLB01B  12/22/99'                                      
         END                                                                    
