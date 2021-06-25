*          DATA SET RERMP0B    AT LEVEL 073 AS OF 06/21/11                      
*PHASE T8100BA                                                                  
*        TITLE 'T8100B - OVERNIGHT DELETIONS'                                   
         TITLE 'T8100B - OVERNIGHT DELETIONS'                                   
***********************************************************************         
*  HISTORY OF CHANGES:                                                *         
***********************************************************************         
*  JUL01/91 (BU ) ---  FIX INSTANCE WHERE NO SERVICE IS REQUESTED, AND*         
*                      NO DELETIONS ARE MADE.  SEE NOTE ADDED IN      *         
*                      ROUTINE 'DE145'.   BILL UHR                    *         
*                                                                     *         
*  JAN27/04 (BU ) --- EXPAND 'STACK' SPACE                            *         
*                                                                     *         
*  APR15/09 (KUI) --- SUPPORT NEW INVENTORY KEY                       *         
*                                                                     *         
*  SEP30/10 (SMY) --- FIX CODE IN "DELETES" AT DE145 AND DE170        *         
*                                                                     *         
*  MAR24/11 (SMY) --- MORE FIXES IN "DELETES"                         *         
*                                                                     *         
*  APR08/11 (SMY) --- ANOTHER TRY TO FIX "DELETES"                    *         
*                                                                     *         
*  JUN21/11 (SMY) --- EXPAND 'STACK' SPACE (10K TO 12K)               *         
*                                                                     *         
***********************************************************************         
T8100B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**OVDE**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T810FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         ST    R2,RELO0B                                                        
*                                                                               
         CLI   MODE,PRINTREP       ONLY DO PRINT REPORT HERE                    
         BNE   EXIT                                                             
*                                                                               
*        INIT PRINT LINES                                                       
*                                                                               
         LA    R0,9                MAX 9 PRINT LINES                            
         LA    R3,PL1              FIRST PRINT LINE                             
*                                                                               
         MVC   0(132,R3),SPACES    INIT PRINT LINES                             
         LA    R3,132(R3)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
*                                                                               
         MVC   REP,AGENCY                                                       
*                                                                               
         LA    R4,KEY              GET PARENT REP FROM REP RECORD               
         USING RREPKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REP                                                     
*                                                                               
         DROP  R4                                                               
*                                                                               
         GOTO1 READ                                                             
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RREPELEM,R6                                                      
*                                                                               
         MVC   REP,RREPPAR                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,BTODAY)                                
*                                                                               
         L     R2,=A(STACK)                                                     
         A     R2,RELO0B                                                        
         ST    R2,ASTACK                                                        
*                                                                               
         MVI   DUMPOPT,C'N'        *** ZAP THIS TO GET RECORD DUMPS             
*                                                                               
         CLI   TITREV,C'T'         CHECK FOR TRACE TO BE ON                     
         BNE   *+8                                                              
         MVI   DUMPOPT,C'Y'                                                     
*                                                                               
         CLI   TITREV,C'Y'         IF DOING REVERSE DELETE                      
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETES                                 
*                                                                               
         TITLE 'T8100B - OVERNIGHT DELETIONS - MSSTA'                           
***********************************************************************         
*                                                                     *         
*        DELETE RECORDS FOR A STATION                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MASTA    DS    0H                                                               
*                                                                               
         LA    R3,STLIST           POINT TO LIST OF STATIONS                    
*                                                                               
MASTALP  DS    0H                                                               
*                                                                               
         USING STLISTD,R3          ESTABLISH STATION LIST ENTRY                 
*                                                                               
         OC    STLISTD(STLISTL),STLISTD   DONE AT END OF LIST                   
         BZ    MASTADN                                                          
*                                                                               
         ST    R3,ASTA             SAVE STATION LIST POINTER                    
*                                                                               
         MVC   CSTAT,STLSSTAC      USE THESE CALL LETTERS                       
*                                                                               
         CLI   CSTAT+4,C' '        BLANK DEFAULTS TO TV                         
         BH    *+8                                                              
         MVI   CSTAT+4,C'T'                                                     
*                                                                               
         TITLE 'T8100B - OVERNIGHT DELETIONS - MAMSTXT'                         
***********************************************************************         
*                                                                     *         
*        DELETE STATION & MARKET FACT TEXT RECORDS                    *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
MAMSTXT  DS    0H                                                               
*                                                                               
         TM    DELBYTE,DELTXTQ     TEST FOR DELETE TEXT                         
         BNO   MAMSTXTX                                                         
*                                                                               
         CLI   TITTYP,C'I'         SKIP IF ONLY DELETE INV TEXT                 
         BE    MAMSTXTX                                                         
*                                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         ZAP   TXMAX,=P'18'                                                     
*                                                                               
         LA    R5,PL1+14                                                        
         ST    R5,PSTART                                                        
*                                                                               
         LA    R4,KEY              BUILD KEY FOR STM/MKT FACT TEXT              
         USING RINVKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,REP                                                     
         MVC   RINVKSTA,CSTAT                                                   
*                                                                               
         MVI   RINVKRTP,C'M'       ASSUME MARKET TEXT WANTED                    
*                                                                               
         CLI   TITTYP,C'S'         ADJUST IF STATION TEXT WANTED                
         BNE   *+8                                                              
MA003    MVI   RINVKRTP,C'S'                                                    
*                                                                               
MA005    BAS   RE,MSHEAD                                                        
*                                                                               
         TM    DELBYTE,DELRNGQ     IF RANGE DELETE                              
         BZ    *+10                                                             
         MVC   RINVKTXT,TXRANGE       START AT BEGINNING OF RANGE               
*                                                                               
MA010    GOTO1 HIGH                                                             
         B     MA030                                                            
*                                                                               
MA020    GOTO1 SEQ                                                              
*                                                                               
MA030    DS    0H                  SKIP IF NO RECORD TYPE BREAK                 
         CLC   KEYSAVE(RINVKBK-RINVKEY),KEY                                     
         BE    MA040                                                            
*                                                                               
*                                  DONE ON STATION BREAK                        
         CLC   KEYSAVE(RINVKRTP-RINVKEY),KEY                                    
         BNE   MA090                                                            
*                                                                               
         CLI   TITTYP,C'S'                                                      
         BE    MA090               STATION FACT ONLY                            
         CLI   TITTYP,C'M'                                                      
         BE    MA090               MKT FACT ONLY                                
*                                                                               
         CLI   RINVKRTP,C'S'                                                    
         BH    MA090               DONE                                         
*                                                                               
         B     MA003               SKIP TO STATION FACT                         
*                                                                               
MA040    TM    DELBYTE,DELRNGQ     IF RANGE DELETE                              
         BZ    MA050                                                            
*                                                                               
         CLC   RINVKTXT,TXRANGE      TEXT NUMBER MUST LIE IN RANGE              
         BNL   *+14                                                             
         MVC   RINVKTXT,TXRANGE                                                 
         B     MA010                                                            
*                                                                               
         CLC   RINVKTXT,TXRANGE+2                                               
         BNH   MA050                                                            
*                                                                               
         CLI   RINVKRTP,C'S'                                                    
         BE    MA090               DONE                                         
*                                                                               
         CLI   TITTYP,C'M'                                                      
         BE    MA090                                                            
*                                                                               
         B     MA003               SKIP TO STATION FACT                         
*                                                                               
MA050    GOTO1 GETREC              GET TEXT RECORD                              
*                                                                               
         TM    DELBYTE,DELRNGQ     TEST FOR BOOK DELETE                         
         BZ    MA060               YES- FILTER                                  
*                                                                               
         CLI   TITSVC,0            NO - TEST FOR SERVICE FILTER                 
         BE    MA070                                                            
*                                                                               
MA060    BAS   RE,TXFILT           FILTER AGAINST TEXT FILTER ELE               
         BNE   MA020                                                            
*                                                                               
MA070    BAS   RE,DELIT            GO DELETE                                    
*                                                                               
         TM    DELBYTE,DELRNGQ                                                  
         BZ    MA080                                                            
*                                                                               
         BAS   RE,TXRGDET          FORMAT TEXT RANGE DETAILS                    
*                                                                               
         B     MA020                                                            
*                                                                               
MA080    BAS   RE,TXBKDET          FORMAT TEXT BOOK DETAILS                     
         BNE   MA020                                                            
*                                                                               
         L     R6,RINVFELA                                                      
         USING RINVFEL,R6                                                       
*                                                                               
         L     R5,PSTART                                                        
*                                                                               
         MVC   WORK(2),RINVFBK                                                  
         MVI   WORK+2,1                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(6,(R5))                                    
*                                                                               
         MVC   7(3,R5),DUB                                                      
         BAS   RE,PRINT                                                         
*                                                                               
         B     MA020                                                            
*                                                                               
MA090    BAS   RE,PRINT                                                         
         EJECT                                                                  
*                                                                               
*        DELETE RECORDS FOR EACH DAYPART                                        
*                                                                               
         SPACE                                                                  
MAMSTXTX TM    DELBYTE,DELALLQ     SKIP IF ONLY DELETING                        
         BO    MA110                MARKET OR STATION TEXT                      
*                                                                               
         TM    DELBYTE,DELINVQ                                                  
         BO    MA110                                                            
*                                                                               
         CLI   TITTYP,C'M'                                                      
         BE    *+8                                                              
         CLI   TITTYP,C'S'                                                      
         BE    MADPTDN                                                          
*                                                                               
MA110    MVI   RCSUBPRG,2                                                       
*                                                                               
         ZAP   TXMAX,=P'6'                                                      
*                                                                               
         LA    R5,PL1+74                                                        
         ST    R5,PSTART                                                        
*                                                                               
         LA    R2,DPLIST                                                        
         LA    R3,L'DPLIST                                                      
*                                                                               
MADPTLP  DS    0H                                                               
*                                                                               
         CLI   0(R2),0             CONTROL FOR EACH DAYPART                     
         BE    MADPTDN                                                          
*                                                                               
         MVC   DPBYTE,0(R2)                                                     
         MVI   FORCEHED,C'Y'       NEW DAYPART                                  
         MVC   PAGE,=H'1'                                                       
*                                                                               
         BAS   RE,DAYPART          PROCESS THIS DAYPART                         
*                                                                               
MADPTCN  DS    0H                                                               
*                                                                               
         LA    R2,1(R2)                                                         
         BCT   R3,MADPTLP                                                       
*                                                                               
MADPTDN  DS    0H                                                               
*                                                                               
*                                                                               
*        CHECK IF A LOCK NEEDS TO BE RELEASED                                   
*                                                                               
         TM    WHEN,X'20'          SKIP IF NOT SOON                             
         BNO   PRLCKX                                                           
*                                                                               
         GOTO1 CHKLOCK,DMCB,('LKUNLKQ',0)  UNLOCK STATION                       
*                                                                               
         BE    *+6                 TEST FOR ERRORS                              
         DC    H'0'                                                             
*                                                                               
PRLCKX   DS    0H                                                               
*                                                                               
MASTACN  DS    0H                                                               
*                                                                               
         L     R3,ASTA             POINT TO CURRENT STATION LIST ENTRY          
         LA    R3,STLISTL(R3)      BUMP TO NEXT ENTRY                           
         B     MASTALP                                                          
*                                                                               
MASTADN  DS    0H                                                               
*                                                                               
MA200    CLI   TITDET,C'Y'         SKIP IF PRINTING DETAILS                     
         BE    MA999                                                            
*                                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P,SPACES                                                         
*                                                                               
         MVC   P(53),=C'***** INVENTORY DELETION SUCCESSFULLY COMPLETEDC        
                *****'                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
MA999    DS    0H                                                               
         B     EXIT                                                             
*                                                                               
EQEXIT   CR    RB,RB                                                            
         B     EXIT                                                             
*                                                                               
NEQEXIT  LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        PRINT ROUTINE                                                          
*                                                                               
PRINT    CLI   TITDET,C'Y'         PRINT OPTION                                 
         BNER  RE                                                               
*                                                                               
         L     R5,PSTART                                                        
*                                                                               
         CLC   0(10,R5),SPACES          ANYTHING TO PRINT                       
         BNHR  RE                                                               
*                                                                               
         ST    RE,SAVERE                                                        
*                                                                               
         CLI   PL1,C' '                                                         
         BE    PR010                                                            
*                                                                               
         MVC   PSAVE(132),PL1                                                   
         MVC   PSAVE+132(132),PL2                                               
         MVC   PSAVE+264(132),PL3                                               
         MVC   PL1,SPACES                                                       
         MVC   PL2,SPACES                                                       
         MVC   PL3,SPACES                                                       
*                                                                               
         BAS   RE,SPLAT            PRINT BLANK LINE SEPARATOR                   
*                                                                               
         MVC   PL1,PSAVE                                                        
         MVC   PL2,PSAVE+132                                                    
         MVC   PL3,PSAVE+264                                                    
*                                                                               
PR010    BAS   RE,SPLAT                                                         
*                                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
TXFILT   NTR1  LABEL=*                                                          
*                                                                               
*        ROUTINE TO FILTER AGAINST TEXT FILTER ELEMENT                          
*        OUTPUT: CC EQ PASSED                                                   
*                CC NE REJECTED                                                 
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL            GET TEXT FILTER ELE                          
         BNE   NEQEXIT                                                          
*                                                                               
         ST    R6,RINVFELA                                                      
*                                                                               
         USING RINVFEL,R6                                                       
*                                                                               
         CLI   TITSVCH+5,0         SKIP IF SERVICE NOT ENTERED                  
         BE    TF010                                                            
*                                                                               
         CLC   RINVFSRC,TITSVC     COMPARE SERVICE                              
         BNE   NEQEXIT                                                          
*                                                                               
TF010    TM    DELBYTE,DELRNGQ     IF BOOK DELETE (NOT A RANGE DELETE)          
         BO    EQEXIT                                                           
*                                                                               
         OC    RINVFBK,RINVFBK     AND IF THERE IS A FILTER BOOK                
         BZ    NEQEXIT                                                          
*                                                                               
         LA    RF,TXBKDEL                                                       
         LA    R1,DELBOOKS                                                      
*                                                                               
TF020    CLI   0(R1),X'FF'         COMPARE AGAINST EACH DELETE BOOK             
         BE    NEQEXIT                                                          
*                                                                               
         MVC   HALF(1),0(R1)       GET BOOK TYPE                                
         NI    HALF,X'FF'-X'41'    KILL SERVICE ASSUMED IN BOOKVAL              
*                                                                               
         CLI   TITSVCH+5,0         IF THERE IS NO SERVICE ASKED FOR             
         BE    TF025                                                            
*                                                                               
         CLI   TITSVC,C'N'                                                      
         BNE   *+12                                                             
         OI    HALF,X'40'          NSI                                          
         B     TF025                                                            
*                                                                               
         CLI   TITSVC,C'M'         MFX                                          
         BE    TF025                                                            
*                                                                               
         CLI   TITSVC,C'S'                                                      
         BNE   *+12                                                             
         OI    HALF,X'41'          SRC                                          
         B     TF025                                                            
*                                                                               
         CLI   TITSVC,C'A'         ARB                                          
         BE    TF025                                                            
*                                                                               
TF025    MVC   HALF+1(1),RINVFBKT  COPY BOOKVAL BITS                            
*                                                                               
         CLI   TITSVCH+5,0         IF THERE IS NO SERVICE ASKED FOR             
         BNE   *+8                                                              
         NI    HALF+1,X'FF'-X'41'     KILL SERVICE IN CODE                      
*                                                                               
         CLC   HALF(1),HALF+1                                                   
         BNE   TF030                                                            
*                                                                               
         CLC   RINVFBK,1(R1)                                                    
         BNE   TF030                                                            
*                                                                               
         CLC   RINVFBTP,3(R1)      MATCH ON BOOK TYPE                           
         BNE   TF030                                                            
*                                                                               
         ST    RF,SAVERF                                                        
         B     EQEXIT                                                           
*                                                                               
TF030    LA    R1,4(R1)                                                         
         LA    RF,1(RF)                                                         
         B     TF020                                                            
         EJECT                                                                  
DAYPART  NTR1  LABEL=*                                                          
*                                                                               
*        DAYPART PROCESS                                                        
*        R2 = A(DAYPART CODE)                                                   
*                                                                               
         L     R5,ASTACK           BUILD A STACK OF DISK ADDRESSES              
         SR    R6,R6                                                            
*                                                                               
         LA    R4,KEY                                                           
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,REP                                                     
         MVC   RIDPKSTA,CSTAT                                                   
         MVC   RIDPKDPT,0(R2)                                                   
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         B     DP020                                                            
*                                                                               
DP010    GOTO1 SEQ                                                              
*                                                                               
DP020    CLC   KEYSAVE(11),KEY     STATION D/P CONTROL BREAK                    
         BNE   DP030                                                            
*                                                                               
         CLI   RIDPKINV,0          ONLY WANT NEW FORMAT RECS                    
         BE    DP010                                                            
*                                                                               
         MVC   0(4,R5),KEY+28      ..PLACE D/A IN STACK                         
*                                                                               
         LA    R5,4(R5)            ..INCREMENT A(IN STACK)                      
         LA    R6,1(R6)            ..INCREMENT COUNTER                          
*                                                                               
         B     DP010                                                            
*                                                                               
DP030    LTR   R6,R6               TEST FOR ANY RECORDS                         
         BZ    DP999               ..NO ENTRIES - FINISHED                      
*                                                                               
         L     R5,ASTACK           ..SET A(1ST ENTRY)                           
*                                                                               
         B     DP050                                                            
*                                                                               
DP040    LM    R5,R6,SAVESTAK                                                   
         LA    R5,4(R5)            GET NEXT KEY FROM STACK                      
         BCT   R6,DP050            ..CYCLE ON STACK COUNT                       
*                                                                               
         B     DP999               DONE FOR THIS DAYPART                        
*                                                                               
DP050    MVC   KEY+28(4),0(R5)     ..SET D/A OF INV RECORD                      
*                                                                               
         STM   R5,R6,SAVESTAK      ..SAVE A(IN STACK) + COUNT                   
*                                                                               
         GOTO1 GETREC              GET HEADER RECORD                            
*                                                                               
         MVC   KEY(27),IO          SAVE THE KEY                                 
*                                                                               
         USING RINVKEY,R4          ESTABLISH KEY                                
*                                                                               
         MVI   ELCODE,1                                                         
         LA    R6,IO                                                            
         BAS   RE,GETEL            GET 01 ELEMENT                               
         USING RINVPEL,R6                                                       
*                                                                               
         TM    RINVGPRO,X'20'      SKIP IF PROTECTED FROM DELETION              
         BO    DP040                                                            
*                                                                               
*        SUPPRESS PRINT IN MULTIPLE DPTS                                        
*                                                                               
         CLI   DPLIST+1,0         NO CHANCE OF DUPLICATE IF ONLY 1 DPT          
         BE    DP065                 REQUESTED                                  
*                                                                               
         GOTO1 =A(CHKDPT),RR=RELO09  CHECK DAYPART LISTS FOR DUPES              
         BNE   DP040                 DROP INVENTORY RECORD                      
*                                                                               
DP065    DS    0H                                                               
*                                                                               
         LA    R1,RINVPFLT         FILTER FILTER                                
         LA    R5,TITFILT                                                       
         LA    R0,6                                                             
*                                                                               
DP070    CLI   0(R5),C'A'          TEST FOR ALPHANUMERIC FILTER                 
         BL    DP080                                                            
*                                                                               
         CLC   0(1,R5),0(R1)       IT MUST MATCH                                
         BNE   DP040                                                            
*                                                                               
DP080    LA    R1,1(R1)            ..BUMP A(FILT IN RECORD)                     
         LA    R5,1(R5)            ..BUMP A(FILT FROM SCREEN)                   
*                                                                               
*   MUST THESE BE POSITIONALLY LOCATED?  THEY ARE SCANNED IN                    
*     SYNCH.  THIS TEST DOES NOT REALLY MAKE MUCH SENSE.  BU.                   
*                                                                               
         BCT   R0,DP070            TEST NEXT FILTER CHARACTER                   
*                                                                               
         BAS   RE,DPHEAD           FORMAT INVENTORY DETAILS                     
*                                                                               
         BAS   RE,DELETES          DELETE THE RECORDS                           
*                                                                               
         BAS   RE,SPLAT                                                         
*                                                                               
         B     DP040                                                            
*                                                                               
DP999    B     EXIT                                                             
         EJECT                                                                  
DPHEAD   NTR1                                                                   
*                                                                               
*        ROUTINE TO FORMAT DAYPART HEADER DETAILS                               
*                                                                               
         MVC   PL1,SPACES                                                       
         MVC   PL2,SPACES                                                       
         MVC   PL3,SPACES                                                       
         MVC   PL4,SPACES                                                       
         ZAP   TXCNT,=P'0'                                                      
*                                                                               
         LA    R3,PL1              RESET LINE POINTER TO FIRST                  
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        SET TO FIND A DAY/TIME ELEMENTS              
*                                                                               
         BAS   RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   PRHWDTDN            NONE FOUND - SKIP PRINTING                   
*                                                                               
PRHWDTLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         GOTO1 UNDAY,PARAS,RIDTDAY,0(R3)      DAY                               
*                                                                               
         GOTO1 UNTIME,PARAS,RIDTTIME,9(R3)    TIME                              
*                                                                               
PRHWDTCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP TO NEXT LINE                            
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
         BE    PRHWDTLP                                                         
*                                                                               
PRHWDTDN DS    0H                  END OF DAY-TIME ELEMENTS                     
*                                                                               
*        PRINT PROGRAM NAMES                                                    
*                                                                               
         LA    R3,PL1              RESET LINE POINTER TO FIRST                  
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   PRHWPGX                                                          
*                                                                               
PRHWPGLP DS    0H                                                               
*                                                                               
         USING RIPGELEM,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN          ELEMENT LENGTH                               
         SH    RF,=Y(RIPGNAME-RIPGELEM)  PROGRAM NAME LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   22(0,R3),RIPGNAME   PRINT PROGRAM NAME                           
*                                                                               
PRHWPGCN DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP POINTER                                 
         BAS   RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    PRHWPGLP            ONE FOUND                                    
*                                                                               
PRHWPGDN DS    0H                                                               
*                                                                               
PRHWPGX  DS    0H                                                               
*                                                                               
         LA    R4,IO                                                            
         USING RINVKEY,R4                                                       
         MVC   PL1+50(4),RINVKINV                                               
*                                                                               
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,PL1+56)                              
         CLI   RINVPEFF+2,0                                                     
         BE    EXIT                                                             
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(8,PL1+65)                            
         B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
MSHEAD   NTR1  LABEL=*                                                          
*                                                                               
*        ROUTINE TO FORMAT MKT/STN TEXT HEADER DETAILS                          
*                                                                               
         USING RINVKEY,R4                                                       
*                                                                               
         BAS   RE,PRINT            ANYTHING TO PRINT                            
*                                                                               
         ZAP   TXCNT,=P'0'                                                      
         XC    TXBKDEL,TXBKDEL                                                  
         MVC   PL1,SPACES                                                       
*                                                                               
         CLI   RINVKRTP,C'M'                                                    
         BNE   *+14                                                             
         MVC   PL1(11),=C'MARKET TEXT'                                          
         B     EXIT                                                             
*                                                                               
         CLI   RINVKRTP,C'S'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PL1(12),=C'STATION TEXT'                                         
*                                                                               
MSHEADX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
TXRGDET  NTR1  LABEL=*                                                          
*                                                                               
*        ROUTINE TO FORMAT TEXT NUMBERS TO PRINT LINE                           
*                                                                               
         LA    R4,IO                                                            
         USING RINVREC,R4                                                       
*                                                                               
         CP    TXCNT,TXMAX                                                      
         BNE   TD010                                                            
*                                                                               
         BAS   RE,PRINT                                                         
         ZAP   TXCNT,=P'0'                                                      
*                                                                               
TD010    CP    TXCNT,=P'0'                                                      
         BNE   TD020                                                            
*                                                                               
         L     R5,PSTART                                                        
         MVC   0(4,R5),=C'TEXT'                                                 
         LA    R5,5(R5)                                                         
         ST    R5,TXNXT                                                         
*                                                                               
TD020    L     R5,TXNXT                                                         
         EDIT  (2,RINVKTXT),(4,0(R5)),FILL=0                                    
*                                                                               
         LA    R5,5(R5)                                                         
         ST    R5,TXNXT                                                         
         AP    TXCNT,=P'1'                                                      
*                                                                               
TXRGDETX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
DELETES  NTR1  LABEL=*                                                          
*                                                                               
*        ROUTINE TO HANDLE THE DELETES                                          
*                                                                               
         LA    R4,KEY              ESTABLISH KEY                                
         USING RINVKEY,R4                                                       
*                                                                               
         MVI   ELCODE,1                                                         
         LA    R6,IO                                                            
         BAS   RE,GETEL            GET 01 ELEMENT                               
         USING RINVPEL,R6                                                       
*                                                                               
         TM    RINVGPRO,X'20'      IF PROTECTED FROM DELETION                   
         BNO   DE004                                                            
*                                                                               
         MVC   PL1+74(09),=C'PROTECTED'   MARK LINE                             
*                                                                               
         B     DE999                  SKIP                                      
*                                                                               
DE004    DS    0H                                                               
*                                                                               
         TM    DELBYTE,DELALLQ     OPTION TO DELETE EVERYTHING                  
         BZ    DE020                THAT FITS WITHIN SELECTED DATES             
*                                                                               
         OC    STDATE,STDATE                                                    
         BZ    *+14                NO START DATE                                
         CLC   RINVPEFF(2),STDATE                                               
         BL    DE999               BEFORE START                                 
*                                                                               
         OC    ENDATE,ENDATE                                                    
         BZ    DE005               NO END DATE                                  
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    DE999                                                            
*                                                                               
         CLC   RINVPEFF+2(2),ENDATE                                             
         BH    DE999               AFTER END                                    
*                                                                               
DE005    MVC   PL1+74(29),=C'INVENTORY, ALL BOOKS AND TEXT'                     
*                                                                               
         BAS   RE,DELIT            DELETE THE HEADER                            
*                                                                               
         OI    DMINBTS,X'08'       RE-READ HEADER WE JUST DELETED               
         GOTO1 HIGH                KEY HAS HEADER KEY                           
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TITREV,C'Y'         IF REVERSING, ALWAYS PASS DELETES            
         BE    DE010                                                            
         NI    DMINBTS,X'F7'       ELSE, TURN OFF PASS DELETES                  
*                                                                               
DE010    GOTO1 SEQ                 NEXT                                         
*                                                                               
*                                  DONE ON INVENTORY BREAK                      
         CLC   KEY(RINVKRTP-RINVKEY),KEYSAVE                                    
         BNE   DE999                                                            
*                                                                               
         GOTO1 GETREC              READ ALL INVENTORY AND TEXT                  
*                                                                               
         BAS   RE,DELIT            DELETE THEM                                  
*                                                                               
         B     DE010                                                            
*                                                                               
DE020    TM    DELBYTE,DELINVQ+DELRNGQ  TEST FOR DELETE ONLY INV RANGE          
         BNO   DE030                                                            
*                                                                               
         CLC   RINVKINV,INRANGE                                                 
         BL    DE999               INV NO TOO LOW                               
*                                                                               
         CLC   RINVKINV,INRANGE+4                                               
         BH    DE999               INV NO TOO HIGH                              
*                                                                               
DE030    XC    TXBKDEL,TXBKDEL                                                  
*                                                                               
*                                  ..TRACKS:  THRU EFF. START DATE              
         LA    RF,RINVKSPR-RINVKEY-1                                            
*                                                                               
         MVI   RINVKRTP,X'01'       IN ORDER TO BYPASS MASTER                   
*                                                                               
         TM    DELBYTE,DELINVQ     OKAY IF DELETING INVENTORY                   
         BO    DE035                                                            
*                                                                               
*                                  ..TEXT: THRU X'FF' INDICATOR                 
         LA    RF,RINVKDSR-RINVKEY-1                                            
         MVI   RINVKRTP,X'FF'                                                   
*                                  DELETING TEXT                                
         TM    DELBYTE,DELRNGQ     DONE IF NO RANGE GIVEN                       
         BZ    DE035                                                            
*                                                                               
         MVC   RINVKTXT,TXRANGE    START AT TEXT RANGE START                    
*                                                                               
DE035    DS    0H                                                               
*                                                                               
         ST    RF,EXLEN            SAVE EXECUTE LENGTH                          
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         B     DE050                                                            
*                                                                               
DE040    GOTO1 SEQ                 NEXT                                         
*                                                                               
DE050    DS    0H                                                               
*                                                                               
         L     RF,EXLEN            RESTORE EXECUTE LENGTH                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      DONE ON CHANGE OF PARTIAL KEY                
         BNE   DE999               DONE                                         
*                                                                               
         CLI   RINVKRTP,X'FF'      SKIP IF TEXT RECORD                          
         BE    DE150                                                            
*                                                                               
         CLI   RINVKRTP,C'Z'       SKIP IF RATE RECORD                          
         BE    DE150                                                            
*                                                                               
*        TRANSLATE KSRC                                                         
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN      INIT GETKSRC PARAMETERS              
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,RINVKRSR    RATING                                       
         MVC   GSIQLF,RINVKQLF     QUALIFIER                                    
         MVC   GSIBKTYP,RINVKBTP   BOOKTYPE                                     
*                                                                               
***************************************************************                 
*SMY*      NOTE:A/O 03/25/11 GETKSRC CANNOT HANDLE BOOKTYPE "S"                 
*SMY*    CLI   RINVKBTP,C'S'                                                    
*SMY*    BE    DE040           SO SKIP "S" RECORDS                              
***************************************************************                 
         GOTO1 VGETKSRC,DMCB,(C'K',GSRCIN),GSRCOUT,ACOMFACS                     
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DE070    GOTO1 GETREC                                                           
*                                                                               
         MVC   DUB(3),=C'NSI'      NSI INV DATA                                 
*                                                                               
DE120    TM    DELBYTE,DELINVQ     TEST INV DELETE                              
         BZ    DE300                                                            
*                                                                               
         CLI   TITSVC,0            ..ANY SERVICE ENTERED?                       
         BE    DE130               ..NO                                         
*                                                                               
         CLC   TITSVC(1),DUB       MATCH ON FILTER SERVICE                      
         BNE   DE300                                                            
*                                                                               
DE130    TM    DELBYTE,DELRNGQ     DELETE RANGE                                 
         BO    DE160                                                            
*                                                                               
         LA    RF,DELBOOKS         NO - FILTER INV BOOK                         
*                                                                               
DE140    CLI   0(RF),X'FF'         DROP IF END OF TABLE REACHED                 
         BE    DE300                                                            
*                                                                               
         CLC   RINVKBK,1(RF)       MATCH ON YEAR/MONTH OF BOOK                  
         BE    DE145                                                            
*                                                                               
DE143    LA    RF,4(RF)                                                         
         B     DE140                                                            
*                                                                               
DE145    DS    0H                                                               
*                                                                               
*SMY*    CLC   GSOBKTYP,3(RF)      MATCH ON BOOK TYPE                           
         CLC   RINVKBTP,3(RF)      MATCH ON BOOK TYPE                           
         BNE   DE146                                                            
*                                                                               
*SMY*    MVC   HALF(1),GSOBITS     COPY BOOKVAL BITS FOR THIS INV REC           
         MVC   HALF(1),RINVKQLF    COPY BOOKVAL BITS (QUALIFIER)                
         NI    HALF,X'FF'-X'41'    KILL SERVICE BITS                            
*                                                                               
         MVC   HALF+1(1),0(RF)     COPY BOOKVAL BITS FOR FILTER                 
         NI    HALF+1,X'FF'-X'41'  KILL SERVICE BITS                            
*                                                                               
         CLC   HALF(1),HALF+1      MATCH ON BOOKVAL BITS                        
         BE    DE170                                                            
*                                                                               
DE146    DS    0H                                                               
*                                                                               
         LA    RF,4(RF)                                                         
         B     DE140                                                            
*                                                                               
*        DELETE INVENTORY TEXT RECORDS                                          
*                                                                               
DE150    TM    DELBYTE,DELTXTQ     SKIP IF NOT DOING INVENTORY TEXT             
         BZ    DE300                                                            
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         TM    DELBYTE,DELRNGQ     FILTERING BY BOOK OR RATING SERVICE          
         BZ    *+12                                                             
         CLI   TITSVC,0                                                         
         BE    DE155                                                            
*                                                                               
         BAS   RE,TXFILT           FILTER SERVICE AND/OR BOOK                   
         BNE   DE300                                                            
*                                                                               
         TM    DELBYTE,DELRNGQ     SKIP IF DELETING A BOOK                      
         BZ    DE180                                                            
*                                                                               
DE155    CLC   RINVKTXT,TXRANGE    FILTER TEXT RANGE                            
         BL    DE300                                                            
*                                                                               
         CLC   RINVKTXT,TXRANGE+2                                               
         BH    DE300                                                            
*                                                                               
         BAS   RE,TXRGDET          FORMAT TEXT RANGE                            
*                                                                               
         B     DE200                                                            
*                                                                               
DE160    CLC   PL1+78(3),DUB       FORMAT INV RANGE                             
         BE    DE200                                                            
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
         MVC   PL1+74(3),=C'ALL'                                                
         MVC   PL1+78(3),DUB                                                    
         MVC   PL1+82(9),=C'INVENTORY'                                          
*                                                                               
         B     DE200                                                            
*                                                                               
*        FORMAT INVENTORY BOOK                                                  
*                                                                               
DE170    DS    0H                                                               
         MVC   PL1+74(9),=C'INVENTORY' FORMAT INV BOOK                          
         MVC   PL1+84(1),GSOQLF    BOOK QUALIFIER                               
*                                                                               
         CLI   GSOQLF,C'E'         IF ESTIMATED BOOK?                           
         BNE   *+8                                                              
         CLI   RINVKBK+1,0         AND NO MONTH SPECIFIED                       
         BNE   DE171                                                            
*                                                                               
         MVC   PL1+84(5),=C' EST/'  ESTIMATED BOOK                              
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,RINVKBK        GET YEAR                                     
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         FORCE SIGN                                   
         UNPK  PL1+89(2),DUB       PRINT YEAR                                   
*                                                                               
         CLI   GSOBKTYP,C' '       PRINT BOOK TYPE IF PRESENT                   
         BE    DE195                                                            
         CLI   GSOBKTYP,0           IF THERE IS A BOOKTYPE,                     
         BE    DE195                                                            
*SMY     GOTO1 GETBTYPE,DMCB,(GSOBKTYP,0)                                       
         GOTO1 GETBTYPE,DMCB,(RINVKBTP,0)                                       
         CLI   DMCB,0                                                           
         BE    DE195                                                            
*                                                                               
         LA    RF,PL1+91                                                        
         MVI   0(RF),C'('                                                       
*                                                                               
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),DMCB+2                                                   
*                                                                               
         LA    RF,2(R1,RF)                                                      
         MVI   0(RF),C')'                                                       
*                                                                               
         B     DE195                                                            
*                                                                               
DE171    DS    0H                                                               
*                                                                               
         MVC   PL1+95(3),DUB       PRINT RATING SERVICE                         
         MVC   WORK(2),RINVKBK     SAVE BOOK                                    
*                                                                               
         B     DE190                                                            
*                                                                               
DE180    BAS   RE,TXBKDET          FORMAT TEXT BOOK                             
         BNE   DE200                                                            
*                                                                               
         MVC   PL1+95(3),DUB       RATING SERVICE                               
         MVC   PL1+74(4),=C'TEXT'                                               
*                                                                               
         L     R6,AIO                                                           
         LA    R6,RINVKTXT-RINVKEY(R6)    TEXT NUMBER                           
         EDIT  (2,0(R6)),(4,PL1+79),FILL=0                                      
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         L     R6,RINVFELA         POINT TO FILTER ELEMENT                      
         USING RINVFEL,R6          ESTABLISH FILTER ELEMENT                     
*                                                                               
         MVC   GSIRSVC,TITSVC      RATING SERVICE                               
         CLI   GSIRSVC,C' '        IF RATING SERVICE MISSING                    
         BH    *+8                                                              
         MVI   GSIRSVC,C'N'        DEFAULT TO NSI                               
*                                                                               
         MVC   GSIBITS,RINVFBKT    BOOKVAL BITS                                 
         MVC   GSIBKTYP,RINVFBTP   BOOK TYPE                                    
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'B',GSRCIN),GSRCOUT,ACOMFACS                     
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PL1+84(1),GSOQLF    BOOK QUALIFIER                               
*                                                                               
         MVC   WORK(2),RINVFBK                                                  
*                                                                               
DE190    MVI   WORK+2,1                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,WORK),(6,PL1+85)                                  
*                                                                               
         CLI   GSOBKTYP,C' '       PRINT BOOK TYPE IF PRESENT                   
         BE    DE195                                                            
         CLI   GSOBKTYP,0           IF THERE IS A BOOKTYPE,                     
         BE    DE195                                                            
*SMY*    GOTO1 GETBTYPE,DMCB,(GSOBKTYP,0)                                       
         GOTO1 GETBTYPE,DMCB,(RINVKBTP,0)                                       
         CLI   DMCB,0                                                           
         BE    DE195                                                            
                                                                                
         LA    RF,PL1+91                                                        
         MVI   0(RF),C'('                                                       
                                                                                
         ZIC   R1,DMCB                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),DMCB+2                                                   
                                                                                
         LA    RF,2(R1,RF)                                                      
         MVI   0(RF),C')'                                                       
*                                                                               
DE195    DS    0H                                                               
*                                                                               
*                                                                               
*SMYTEST  DC    H'0'                                                            
*                                                                               
         BAS   RE,PRINT                                                         
*                                                                               
DE200    BAS   RE,DELIT                                                         
*                                                                               
DE300    B     DE040                                                            
*                                                                               
DE999    B     EXIT                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
TXBKDET  NTR1                                                                   
*                                                                               
*        ROUTINE TO FIND OUT IF FORMAT BOOKS TO PRINT LINE NEEDED               
*                                                                               
         L     RF,SAVERF                                                        
         L     R6,RINVFELA                                                      
*                                                                               
         USING RINVFEL,R6                                                       
*                                                                               
         CLI   RINVFSRC,C'M'                                                    
         BNE   TB010                                                            
         TM    0(RF),X'02'                                                      
         BO    NEQEXIT                                                          
         OI    0(RF),X'02'                                                      
         MVC   DUB(3),=C'MFX'                                                   
         B     EQEXIT                                                           
*                                                                               
TB010    CLI   RINVFSRC,C'N'                                                    
         BNE   TB020                                                            
         TM    0(RF),X'04'                                                      
         BO    NEQEXIT                                                          
         OI    0(RF),X'04'                                                      
         MVC   DUB(3),=C'NSI'                                                   
         B     EQEXIT                                                           
*                                                                               
TB020    CLI   RINVFSRC,C'S'                                                    
         BNE   TB030                                                            
         TM    0(RF),X'08'                                                      
         BO    NEQEXIT                                                          
         OI    0(RF),X'08'                                                      
         MVC   DUB(3),=C'SRC'                                                   
         B     EQEXIT                                                           
*                                                                               
TB030    TM    0(RF),X'01'                                                      
         BO    NEQEXIT                                                          
         OI    0(RF),X'01'                                                      
         MVC   DUB(3),SPACES                                                    
         B     EQEXIT                                                           
         EJECT                                                                  
DELIT    NTR1                                                                   
*                                                                               
*        ROUTINE TO DELETE OR UNDELETE A RECORD                                 
*                                                                               
         LA    R6,IO                                                            
         USING RINVREC,R6                                                       
*                                                                               
         CLI   TITREV,C'Y'         TEST FOR REVERSE DELETE                      
         BE    DT010                                                            
*                                                                               
         TM    RINVCNTL,X'80'      TEST FOR ALREADY DELETED                     
         BO    DT999                                                            
*                                                                               
         OI    RINVCNTL,X'80'                                                   
         GOTO1 PUTREC              MARK RECORD FOR DELETION                     
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE               MARK KEY FOR DELETION                        
*                                                                               
         CLI   RINVKRTP,X'00'      IF DELETING THE HEADER,                      
         BNE   DT005                                                            
*                                                                               
         GOTO1 INVPTR,DMCB,(R6),PPAREA   BUILD PASSIVE POINTER                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R6,6                BCT THROUGH 6 DAYPART POINTERS               
         LA    R4,PPAREA                                                        
*                                                                               
DT003    MVC   KEY,0(R4)                                                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DT003A                                                           
*                                                                               
         OI    KEY+27,X'80'        MARK PASSIVE POINTER FOR DELETION            
         GOTO1 WRITE                                                            
*                                                                               
DT003A   DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           NEXT POINTER                                 
         OC    0(32,R4),0(R4)                                                   
         BZ    DT004               NO MORE                                      
         BCT   R6,DT003                                                         
*                                                                               
DT004    MVC   KEY,SAVEKEY         RE-READ HEADER WE JUST DELETED               
*                                                                               
         OI    DMINBTS,X'08'       SO BETTER PASS DELETES                       
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    DMINBTS,X'F7'                                                    
*                                                                               
DT005    CLI   DUMPOPT,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
*                                                                               
         B     DT999                                                            
*                                                                               
DT010    TM    RINVCNTL,X'80'      TEST FOR NOT ALREADY DELETED                 
         BNO   DT999                                                            
*                                                                               
         MVC   PL1+105(08),=C'RESTORED'    FLAG ENTRY                           
*                                                                               
         NI    RINVCNTL,X'7F'                                                   
         GOTO1 PUTREC              UNDELETE RECORD                              
         NI    KEY+27,X'7F'                                                     
         GOTO1 WRITE               UNDELETE KEY                                 
*                                                                               
         CLI   RINVKRTP,X'00'      IF UNDELETING THE HEADER,                    
         BNE   DT020                                                            
*                                                                               
         GOTO1 INVPTR,DMCB,(R6),PPAREA   BUILD PASSIVE POINTER                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R6,6                BCT THROUGH 6 DAYPART POINTERS               
         LA    R4,PPAREA                                                        
*                                                                               
DT013    MVC   KEY,0(R4)                                                        
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   DT013A                                                           
*****    BE    *+6                                                              
*****    DC    H'0'                                                             
         NI    KEY+27,X'7F'        UNDELETE PASSIVE POINTER                     
         GOTO1 WRITE                                                            
*                                                                               
DT013A   DS    0H                                                               
         LA    R4,32(R4)           NEXT POINTER                                 
         OC    0(32,R4),0(R4)                                                   
         BZ    DT014               NO MORE                                      
*                                                                               
         BCT   R6,DT013                                                         
*                                                                               
DT014    MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DT020    CLI   DUMPOPT,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,DUMP                                                          
*                                                                               
DT999    B     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'RERMP23 - T8100B - OVERNIGHT DELETES - SPLAT'                   
*******************************************************************             
*                                                                 *             
*        RERMP0B --- REP INVENTORY OVERNIGHT DELETE   FUNCTION    *             
*              ROUTINE TO OUTPUT CHUNKS                           *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
SPLAT    NTR1                                                                   
*                                                                               
*        PRINT DETAIL LINES - THERE COULD BE 8                                  
*                                                                               
         LA    R3,PL1              START OF PRINT LINES                         
*                                                                               
SPLTPR1L DS    0H                                                               
*                                                                               
         LA    R2,P                START OF PRINT AREA (4 LINES WORTH)          
         LA    R0,4                MAX 4 LINES AT A TIME                        
*                                                                               
SPLTPR2L DS    0H                                                               
*                                                                               
         MVC   0(132,R2),0(R3)                                                  
         MVC   0(132,R3),SPACES                                                 
*                                                                               
SPLTPR2C DS    0H                                                               
*                                                                               
         LA    R2,132(R2)                                                       
         LA    R3,132(R3)                                                       
         BCT   R0,SPLTPR2L                                                      
*                                                                               
SPLTPR2D DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    PRINT THE DATA                                
*                                                                               
SPLTPR1C DS    0H                                                               
*                                                                               
         CLC   0(132,R3),SPACES    DONE IF ONLY SPACES TO PRINT                 
         BH    SPLTPR1L            THERE IS EXTRA LINE TO STOP LOOP             
*                                                                               
*        GOTO1 SPOOL,DMCB,(R8)    SPACING LINE                                  
*                                                                               
SPLATX   DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T8100B - OVERNIGHT DELETES - DUMP'                    
*                                                                               
DUMP     NTR1                                                                   
         MVC   PSAVE(132),P                                                     
         MVC   PSAVE+132(132),P2                                                
         MVC   PSAVE+264(132),P3                                                
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P(34),IO            DUMP KEY                                     
         GOTO1 HEXOUT,PARAS,P,P2,34,=C'SEP'                                     
         MVC   P3(34),P2+34                                                     
         MVI   P2+34,C' '                                                       
         MVC   P+35(45),IO+34      DUMP 1ST ELEMENT                             
         GOTO1 HEXOUT,PARAS,P+35,P2+35,45,=C'SEP'                               
         MVC   P3+35(45),P2+80                                                  
         MVC   P2+80(45),SPACES                                                 
         CLI   IO+24,X'FF'                                                      
         BE    DUMP1                                                            
         CLI   IO+24,C'M'                                                       
         BE    DUMP1                                                            
         CLI   IO+24,C'S'                                                       
         BNE   DUMP2                                                            
*                                                                               
DUMP1    LA    R6,IO               DUMP TEXT 02 ELEMENT                         
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL                                                         
         BNE   DUMP2                                                            
         MVC   P+81(5),0(R6)                                                    
         GOTO1 HEXOUT,PARAS,P+81,P2+81,5,=C'SEP'                                
         MVC   P3+81(5),P2+86                                                   
         MVC   P2+86(5),SPACES                                                  
*                                                                               
DUMP2    GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,PSAVE                                                          
         MVC   P2,PSAVE+132                                                     
         MVC   P3,PSAVE+264                                                     
         B     EXIT                                                             
*                                                                               
PSAVE    DS    3CL132                                                           
         TITLE 'RERMP23 - T8100B - OVERNIGHT DELETES - HOOK'                    
*                                                                               
*              HEADLINE ROUTINES                                                
*                                                                               
         DS    0F                                                               
         USING *,RF                                                             
HOOK     NTR1                                                                   
         L     RE,4(RD)                                                         
         CLC   0(4,RE),=C'SPUL'                                                 
         BE    *+12                                                             
         L     RE,4(RE)                                                         
         B     *-14                                                             
         LM    RE,RC,12(RE)                                                     
         DROP  RF                                                               
         MVC   H4+10(4),CSTAT                                                   
*                                                                               
         CLI   CSTAT+4,C'T'        PRINT BAND IF NOT TV                         
         BE    HOOKBNDX                                                         
*                                                                               
         LA    RF,H4+10+3          POINT TO LAST CALL LETTER                    
*                                                                               
         CLI   0(RF),C' '          BACK UP IF BLANK                             
         BH    *+6                                                              
         BCTR  RF,0                                                             
*                                                                               
         MVI   1(RF),C'-'          PRINT BAND                                   
*                                                                               
         MVC   2(1,RF),CSTAT+4                                                  
*                                                                               
         CLI   2(RF),C'A'          IF AM/FM PRINT MODULATION                    
         BE    *+8                                                              
         CLI   2(RF),C'F'                                                       
         BNE   *+8                                                              
         MVI   3(RF),C'M'                                                       
*                                                                               
HOOKBNDX DS    0H                                                               
*                                                                               
         MVC   H4+19(24),CMKTNAM                                                
         CLI   37(RA),C'N'                                                      
         BNE   *+10                                                             
         MVC   H4+46(27),=C'*** RECORDS NOT DELETED ***'                        
         CLI   RCSUBPRG,1                                                       
         BE    HK999                                                            
*                                                                               
         LA    R3,DPTBL            LOOK UP DAYPART                              
         USING DPTBLD,R3                                                        
*                                                                               
HOOK2    CLC   DPBYTE,DPTBCODE     MATCH TO DAYPART CODE                        
         BE    HOOK4                                                            
*                                                                               
         LA    R3,DPTBLL(R3)                                                    
*                                                                               
         CLI   0(R3),X'00'         TEST EOT                                     
         BNE   HOOK2                                                            
*                                                                               
HOOK4    MVC   H4+84(15),DPTBLNAM  DAYPART                                      
*                                                                               
HK999    B     EXIT                                                             
*                                                                               
         EJECT                                                                  
         GETEL (R6),34,ELCODE                                                   
*                                                                               
RELOC    DC    A(*)                                                             
         EJECT                                                                  
*              CREATE NEW PASSIVE POINTER                                       
         SPACE 1                                                                
*              PARAM 1   BYTES 1-3 A(INVENTORY RECORD)                          
*              PARAM 2   BYTES 1-3 A(200 BYTE OUTPUT AREA)                      
         SPACE 1                                                                
         USING RINVREC,R2                                                       
         USING RIDPKEY,R4                                                       
INVPTR   NTR1                                                                   
         L     R2,0(R1)                                                         
         L     R4,4(R1)                                                         
         XC    0(200,R4),0(R4)                                                  
         LA    R6,6                                                             
         LA    R3,RINVDP                                                        
         SPACE 1                                                                
INVPTR1  MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,RINVKREP                                                
         MVC   RIDPKSTA,RINVKSTA                                                
         MVC   RIDPKDPT,0(R3)                                                   
         MVC   RIDPKINV,RINVKINV                                                
         MVC   RIDPKSTD,RINVKSTD                                                
         SPACE 1                                                                
*                                                                               
*  IF SELF ASSIGNED GET NEXT DAYPART                                            
*  ONLY COMPUTER GENERATED NUMBERS GET THE DAY,QTR HOUR                         
*  AND THE LENGTH FILLED IN.                                                    
*                                                                               
         TM    RINVSTAT,X'80'                                                   
         BO    INVPTR20            BIT ON SELF ASSIGNED                         
*                                                                               
         MVC   RIDPKDAY,RINVOINV+1   MOVE DAY CODE,                             
         MVC   RIDPKQTR,RINVOINV     QUARTER HOUR,                              
         MVC   RIDPKLEN,RINVOINV+2   AND PROGRAM LENGTH TO KEY                  
         SPACE                                                                  
INVPTR20 LA    R3,1(R3)            NEXT DAYPART CODE                            
         CLI   0(R3),X'40'                                                      
         BNH   INVPTX                                                           
         LA    R4,32(R4)                                                        
         BCT   R6,INVPTR1          DO NEXT POINTER                              
         SPACE 1                                                                
INVPTX   XIT1                                                                   
         SPACE 1                                                                
         DROP  R2,R4                                                            
         SPACE 1                                                                
*  THESE DAYPARTS GET A DAY CODE, QUARTER HOUR, AND PROGRAM LENGTH              
DAYCOD   DC    C'MDKNPOUXYWZ',X'FF'                                             
         SPACE 1                                                                
*  THESE DAYPARTS GET EFFECTIVE DATE, QUARTER HOUR, AND PROGRAM LENGTH          
EFFDAT   DC    C'VSJ',X'FF'                                                     
         SPACE 1                                                                
*  THESE DAYPARTS ONLY GET INVENTORY NUMBER AND START DATE                      
*       ERATLF - THEY ARE THE FRINGE "SUB-DAYPARTS"                             
* (W-WEEKEND IS NOT TREATED AS FRINGE FOR PASSIVE POINTERS, BUT                 
*    IS GROUPED WITH FRINGE EVERYWHERE ELSE)                                    
         EJECT                                                                  
*              END OLD INVENTORY THAT HAS NO END DATE                           
*    AIO1-OLD REORD TO BE CHANGED                                               
*    AIO2-NEW RECORD JUST ADDED                                                 
         SPACE 1                                                                
*****  **INCLUDE REINVPTRN                                                      
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP0B - T8100B - OVERNIGHT DELETES - CHKDPT'                  
*******************************************************************             
*                                                                 *             
*              COMPARE REQUESTED DAYPARTS TO INVENTORY RECORDS    *             
*              DROP INVENTORY IF PRIMARY DPT IS ONE OF THOSE      *             
*                REQUESTED BUT NOT THIS ONE                       *             
*                (IT WILL BE PROCESSED WHEN ITS PRIMARY DPT COMES *             
*                UP                                               *             
*              PROCESS INVENTORY THE FIRST TIME A NON-PRIMARY DPT *             
*                IS ENCOUNTERED                                   *             
*                                                                 *             
*                                                                 *             
*NTRY    R6==> RINVPEL                                            *             
*        R2==> DAYPART                                            *             
*                                                                 *             
*******************************************************************             
         SPACE 2                                                                
         DS    0H                                                               
CHKDPT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RINVPEL,R6          ESTABLISH PROGRAM ELEMENT                    
*                                                                               
*****    L     R2,ADPT             POINT TO CURRENT DAYPART                     
         CLC   0(1,R2),RINVDP      OKAY IF CURRENT MATCHES PRIMARY              
         BE    CHKDPTOK                                                         
*                                                                               
*        DROP RECORD IF PRIMARY DAYPART IS AMONG THOSE REQUESTED                
*                                                                               
         LA    RE,DPLIST           REQUESTED DAYPARTS                           
         LA    RF,L'DPLIST                                                      
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   *+22                                                             
         CLC   0(1,RE),RINVDP      MATCH LIST TO PRIMARY DPT                    
         BE    CHKDPTEX            DROP IF MATCH FOUND                          
         LA    RE,1(RE)            BUMP LIST POINTER                            
         BCT   RF,*-22                                                          
*                                                                               
*        OKAY IF REQUESTED DPTS MATCH ONLY ONE IN RECORD                        
*                                                                               
         USING RINVPEL,R6          ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         LA    RE,DPLIST           REQUESTED DAYPARTS                           
         LA    RF,L'DPLIST                                                      
*                                                                               
CHKDPT1L DS    0H                                                               
*                                                                               
         CLI   0(RE),C' '          DONE AT END OF LIST                          
         BNH   CHKDPT1D                                                         
*                                                                               
         LA    R1,RINVDP+1         INVENTORY DAYPARTS (FIRST ALREADY            
         LA    R0,L'RINVDP-1          CHECKED)                                  
*                                                                               
CHKDPT2L DS    0H                                                               
*                                                                               
         CLI   0(R1),C' '          CHECK FOR END OF LIST                        
         BNH   CHKDPT2D                                                         
*                                                                               
         CLC   0(1,RE),0(R1)       SKIP IF DAYPARTS DON'T MATCH                 
         BNE   CHKDPT2C                                                         
*                                                                               
         CR    RE,R2               IF MATCH BEFORE CURRENT DPT                  
         BL    CHKDPTEX               DROP INVENTORY AS DUPLICATE               
*                                                                               
         B     CHKDPTOK            ELSE PROCESS INVENTORY RECORD                
*                                                                               
CHKDPT2C DS    0H                                                               
*                                                                               
         LA    R1,1(R1)            BUMP LIST POINTER                            
         BCT   R0,CHKDPT2L                                                      
*                                                                               
CHKDPT2D DS    0H                                                               
*                                                                               
CHKDPT1C DS    0H                                                               
*                                                                               
         LA    RE,1(RE)            BUMP LIST POINTER                            
         BCT   RF,CHKDPT1L                                                      
*                                                                               
CHKDPT1D DS    0H                                                               
*                                                                               
CHKDPTOK DS    0H                                                               
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         B     CHKDPTX                                                          
*                                                                               
CHKDPTEX DS    0H                                                               
         LTR   RB,RB               SET NE CC                                    
*                                                                               
CHKDPTX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'RERMP23 - T81023 - OVERNIGHT TRANSFERS - TRACE'                 
         EJECT                                                                  
*                                                                               
PL1      DS    CL132               PRINT WORKAREA                               
PL2      DS    CL132               PRINT WORKAREA                               
PL3      DS    CL132               PRINT WORKAREA                               
PL4      DS    CL132               PRINT WORKAREA                               
PL5      DS    CL132               PRINT WORKAREA                               
PL6      DS    CL132               PRINT WORKAREA                               
PL7      DS    CL132               PRINT WORKAREA                               
PL8      DS    CL132               PRINT WORKAREA                               
PL9      DS    CL132               PRINT WORKAREA                               
*                                                                               
* FALOCKETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FALOCKETD                                                      
         PRINT ON                                                               
DUMMY    DSECT                                                                  
       ++INCLUDE REGENREP                                                       
       ++INCLUDE REGENINVA                                                      
       ++INCLUDE REGENSET                                                       
       ++INCLUDE REGENRDP                                                       
       ++INCLUDE RERMPWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RERMPFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RERMPD6D                                                       
         EJECT                                                                  
*                                                                               
*        WORKAREAS                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
DPLIST   DS    CL20                DAYPART LIST                                 
*                                                                               
DELBYTE  DS    X                   OPTION SWITCHES                              
DELALLQ  EQU   X'01'                 X'01' ALL DELETE                           
DELTXTQ  EQU   X'02'                 X'02' TEXT DELETE                          
DELINVQ  EQU   X'04'                 X'04' INV DELETE                           
DELRNGQ  EQU   X'08'                 X'08' DELETE RANGE                         
*                                                                               
TXRANGE  DS    XL4                 TEXT RANGE                                   
INRANGE  DS    XL8                 INV RANGE                                    
DELBOOKS DS    CL20                TABLE OF BOOKS                               
STDATE   DS    XL2                 START DATE                                   
ENDATE   DS    XL2                 END DATE                                     
DPMENU   DS    CL4                 DAYPART MENU                                 
DPTBL    DS    XL(24*DPTBLL)       DAYPART TABLE                                
*                                                                               
         DS    0F                                                               
RELO09   DS    A                                                                
RELO0B   DS    A                                                                
ACLPACK  DS    A                                                                
FADDR    DS    A                                                                
*                                                                               
STMENU   DS    CL4                 STATION MENU CODE                            
STMENUNM DS    CL60                STATION MENU NAME                            
*                                                                               
STLIST   DS    XL(24*STLISTL)      STATIONS LIST                                
*                                                                               
SAVESTAK DS    2F                                                               
ASTACK   DS    A                                                                
ASTA     DS    A                   A(ENTRY IN STATION LIST)                     
RELO     DS    A                                                                
TXNXT    DS    A                                                                
PSTART   DS    A                                                                
RINVFELA DS    F                                                                
SAVERE   DS    F                                                                
SAVERF   DS    F                                                                
EXLEN    DS    F                   EXECUTE LENGTH SAVEAREA                      
TXCNT    DS    PL2                                                              
TXMAX    DS    PL2                                                              
DPBYTE   DS    C                                                                
REP      DS    H                                                                
TXBKDEL  DS    CL4         1 BYTE PER DELETE BOOK                               
*                          BIT ON MEANS DETAIL HAS BEEN FORMATTED               
*                          X'01' NO SVC FILTER                                  
*                          X'02' MFX                                            
*                          X'04' NSI                                            
*                          X'08' SRC                                            
DUMPOPT  DS    C                                                                
SAVEKEY  DS    CL32                                                             
PPAREA   DS    CL200                                                            
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC                                        
*                                                                               
GSRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GSIRSVC  DS    CL1                 RATING SERVICE                               
GSIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSIBITS  DS    XL1                 BOOKVAL BITS                                 
GSIBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                 SPARE                                        
GSRCINL  EQU   *-GSRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GSRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GSORSVC  DS    CL1                 RATING SERVICE                               
GSOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSOBITS  DS    XL1                 BOOKVAL BITS                                 
GSOBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                 SPARE                                        
GSRCOUTL EQU   *-GSRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
         TITLE 'RERMP0B - T8100B - OVERNIGHT DELETES - DPTBLD'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T81021 --- RERMP21 --- OVERNIGHT TRANSFERS - STLISTD'           
***********************************************************************         
*                                                                     *         
*        DSECT FOR LIST OF STATIONS                                   *         
*                                                                     *         
***********************************************************************         
STLISTD  DSECT                                                                  
STLSSTCD DS    CL1                 STATION SORT CODE                            
STLSSTAC DS    CL5                 STATION CALL LETTERS                         
STLISTL  EQU   *-STLISTD           LENGTH OF TABLE ENTRY                        
*                                                                               
         EJECT                                                                  
STACK    CSECT                                                                  
*SMY*    DC    10000X'00'          2500 ENTRIES                                 
         DC    12000X'00'          3000 ENTRIES                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073RERMP0B   06/21/11'                                      
         END                                                                    
