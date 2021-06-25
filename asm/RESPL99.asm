*          DATA SET RESPL99    AT LEVEL 032 AS OF 05/01/02                      
*PHASE T80899A,*                                                                
         TITLE 'T80899 - OVERNIGHT DELETIONS'                                   
***********************************************************************         
*  HISTORY OF CHANGES:                                                *         
***********************************************************************         
*  JUL01/91 (BU ) ---  FIX INSTANCE WHERE NO SERVICE IS REQUESTED, AND*         
*                      NO DELETIONS ARE MADE.  SEE NOTE ADDED IN      *         
*                      ROUTINE 'DE145'.   BILL UHR                    *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
T80899   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**OVDE**                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING T80899+4096,R9                                                   
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T808FFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         STM   R8,RC,OVERR8                                                     
         LA    R2,HOOK                                                          
         ST    R2,HEADHOOK                                                      
         LA    R4,KEY              GET PARENT REP FROM REP RECORD               
         USING RREPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,REP                                                     
         DROP  R4                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RREPELEM,R6                                                      
         MVC   REP,RREPPAR                                                      
         GOTO1 DATCON,DMCB,(4,RCDATE),(2,BTODAY)                                
         LA    R1,RELOC                                                         
         S     R1,RELOC                                                         
         ST    R1,RELO                                                          
         L     R2,=A(STACK)                                                     
         A     R2,RELO                                                          
         ST    R2,ASTACK                                                        
*                                                                               
         MVI   DUMPOPT,C'N'        *** ZAP THIS TO GET RECORD DUMPS             
         CLI   TITREV,C'Y'         IF DOING REVERSE DELETE                      
         BNE   *+8                                                              
         OI    DMINBTS,X'08'       PASS DELETES                                 
         EJECT                                                                  
*                                                                               
*        DELETE STATION & MARKET FACT TEXT RECORDS                              
*                                                                               
         SPACE                                                                  
         TM    DELBYTE,X'02'       TEST FOR DELETE TEXT                         
         BNO   MA100                                                            
         CLI   TITTYP,C'I'         TEST FOR ONLY DELETE INV TEXT                
         BE    MA100                                                            
         MVI   RCSUBPRG,1                                                       
         ZAP   TXMAX,=P'18'                                                     
         LA    R5,P+14                                                          
         ST    R5,PSTART                                                        
         LA    R4,KEY              BUILD KEY FOR STM/MKT FACT TEXT              
         USING RINVKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,X'12'                                                   
         MVC   RINVKREP,REP                                                     
         MVC   RINVKSTA,ACTSTAT                                                 
         MVI   RINVKSRC,C'M'                                                    
         CLI   TITTYP,C'S'                                                      
         BNE   MA005                                                            
*                                                                               
MA003    MVI   RINVKSRC,C'S'                                                    
*                                                                               
MA005    BAS   RE,MSHEAD                                                        
         TM    DELBYTE,X'08'       TEST FOR RANGE DELETE                        
         BZ    MA010                                                            
         MVC   RINVKTXT,TXRANGE                                                 
*                                                                               
MA010    GOTO1 HIGH                                                             
         B     MA030                                                            
*                                                                               
MA020    GOTO1 SEQ                                                              
*                                                                               
MA030    CLC   KEYSAVE(25),KEY     TEST FOR RECORD TYPE BREAK                   
         BE    MA040                                                            
         CLC   KEYSAVE(24),KEY     TEST FOR STATION BREAK                       
         BNE   MA090                                                            
         CLI   TITTYP,C'S'                                                      
         BE    MA090               STATION FACT ONLY                            
         CLI   TITTYP,C'M'                                                      
         BE    MA090               MKT FACT ONLY                                
         CLI   RINVKSRC,C'S'                                                    
         BH    MA090               DONE                                         
         B     MA003               SKIP TO STATION FACT                         
*                                                                               
MA040    TM    DELBYTE,X'08'       TEST RANGE DELETE                            
         BZ    MA050                                                            
         CLC   RINVKTXT,TXRANGE                                                 
         BNL   *+14                                                             
         MVC   RINVKTXT,TXRANGE                                                 
         B     MA010                                                            
         CLC   RINVKTXT,TXRANGE+2                                               
         BNH   MA050                                                            
         CLI   RINVKSRC,C'S'                                                    
         BE    MA090               DONE                                         
         CLI   TITTYP,C'M'                                                      
         BE    MA090                                                            
         B     MA003               SKIP TO STATION FACT                         
*                                                                               
MA050    GOTO1 GETREC              GET TEXT RECORD                              
         TM    DELBYTE,X'08'       TEST FOR BOOK DELETE                         
         BZ    MA060               YES- FILTER                                  
         CLI   TITSVC,0            NO - TEST FOR SERVICE FILTER                 
         BE    MA070                                                            
*                                                                               
MA060    BAS   RE,TXFILT           FILTER AGAINST TEXT FILTER ELE               
         BNE   MA020                                                            
*                                                                               
MA070    BAS   RE,DELIT            GO DELETE                                    
         TM    DELBYTE,X'08'                                                    
         BZ    MA080                                                            
         BAS   RE,TXRGDET          FORMAT TEXT RANGE DETAILS                    
         B     MA020                                                            
*                                                                               
MA080    BAS   RE,TXBKDET          FORMAT TEXT BOOK DETAILS                     
         BNE   MA020                                                            
         L     R6,SAVER6                                                        
         USING RINVFEL,R6                                                       
         L     R5,PSTART                                                        
         MVC   WORK(2),RINVFBK                                                  
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,(R5))                                    
         MVC   7(3,R5),DUB                                                      
         BAS   RE,PRINT                                                         
         B     MA020                                                            
*                                                                               
MA090    BAS   RE,PRINT                                                         
         EJECT                                                                  
*                                                                               
*        DELETE RECORDS FOR EACH DAYPART                                        
*                                                                               
         SPACE                                                                  
MA100    TM    DELBYTE,X'01'       SKIP IF ONLY DELETING                        
         BO    MA110                MARKET OR STATION TEXT                      
         TM    DELBYTE,X'04'                                                    
         BO    MA110                                                            
         CLI   TITTYP,C'M'                                                      
         BE    MA200                                                            
         CLI   TITTYP,C'S'                                                      
         BE    MA200                                                            
*                                                                               
MA110    MVI   RCSUBPRG,2                                                       
         ZAP   TXMAX,=P'6'                                                      
         LA    R5,P+74                                                          
         ST    R5,PSTART                                                        
         LA    R2,DPLIST                                                        
         LA    R3,NDPT                                                          
*                                                                               
MA120    CLI   0(R2),0             CONTROL FOR EACH DAYPART                     
         BE    MA999                                                            
         MVC   DPBYTE,0(R2)                                                     
         MVI   FORCEHED,C'Y'       NEW DAYPART                                  
         MVC   PAGE,=H'1'                                                       
*                                                                               
MA130    BAS   RE,DAYPART          PROCESS THIS DAYPART                         
         LA    R2,1(R2)                                                         
         BCT   R3,MA120                                                         
*                                                                               
MA200    CLI   TITDET,C'Y'                                                      
         BE    MA999                                                            
         MVI   SKIPSPEC,C'Y'                                                    
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P,SPACES                                                         
         MVC   P(53),=C'***** INVENTORY DELETION SUCCESSFULLY COMPLETEDC        
                *****'                                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
MA999    B     EXIT                                                             
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
         L     R5,PSTART                                                        
         CLI   0(R5),C' '          ANYTHING TO PRINT                            
         BER   RE                                                               
         ST    RE,SAVERE                                                        
         CLI   P,C' '                                                           
         BE    PR010                                                            
         MVC   PSAVE(132),P                                                     
         MVC   PSAVE+132(132),P2                                                
         MVC   PSAVE+264(132),P3                                                
         MVC   P,SPACES                                                         
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE SEPARATOR                   
         MVC   P,PSAVE                                                          
         MVC   P2,PSAVE+132                                                     
         MVC   P3,PSAVE+264                                                     
*                                                                               
PR010    GOTO1 SPOOL,DMCB,(R8)                                                  
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
TXFILT   NTR1                                                                   
*                                                                               
*        ROUTINE TO FILTER AGAINST TEXT FILTER ELEMENT                          
*        OUTPUT: CC EQ PASSED                                                   
*                CC NE REJECTED                                                 
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,2                                                         
         BAS   RE,GETEL            GET TEXT FILTER ELE                          
         BNE   NEQEXIT                                                          
         ST    R6,SAVER6                                                        
         USING RINVFEL,R6                                                       
         CLI   TITSVC,0                                                         
         BE    TF010                                                            
         CLC   RINVFSRC,TITSVC     COMPARE SERVICE                              
         BNE   NEQEXIT                                                          
*                                                                               
TF010    TM    DELBYTE,X'08'       TEST FOR BOOK DELETE                         
         BO    EQEXIT                                                           
         OC    RINVFBK,RINVFBK     YES                                          
         BZ    NEQEXIT                                                          
         LA    RF,TXBKDEL                                                       
         LA    R1,DELBOOKS                                                      
*                                                                               
TF020    CLI   0(R1),X'FF'         COMPARE AGAINST EACH DELETE BOOK             
         BE    NEQEXIT                                                          
         ZIC   R2,0(R1)                                                         
         CLI   TITSVC,C'A'                                                      
         BE    TF025                                                            
         O     R2,=X'00000040'     NSI                                          
         CLI   TITSVC,C'N'                                                      
         BE    TF025                                                            
         O     R2,=F'01'           SRC                                          
*                                                                               
TF025    ZIC   R3,RINVFBKT                                                      
         CR    R2,R3                                                            
         BNE   TF030                                                            
         CLC   RINVFBK,1(R1)                                                    
         BNE   TF030                                                            
         ST    RF,SAVERF                                                        
         B     EQEXIT                                                           
TF030    LA    R1,3(R1)                                                         
         LA    RF,1(RF)                                                         
         B     TF020                                                            
         EJECT                                                                  
DAYPART  NTR1                                                                   
*                                                                               
*        DAYPART PROCESS                                                        
*        R2 = A(DAYPART CODE)                                                   
*                                                                               
         L     R5,ASTACK           BUILD A STACK OF DISK ADDRESSES              
         SR    R6,R6                                                            
         LA    R4,KEY                                                           
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RIDPKTYP,X'92'                                                   
         MVC   RIDPKREP,REP                                                     
         MVC   RIDPKSTA,ACTSTAT                                                 
         MVC   RIDPKDPT,0(R2)                                                   
         GOTO1 HIGH                                                             
         B     DP020                                                            
*                                                                               
DP010    GOTO1 SEQ                                                              
*                                                                               
DP020    CLC   KEYSAVE(11),KEY     STATION D/P CONTROL BREAK                    
         BNE   DP030                                                            
         MVC   0(4,R5),KEY+28      ..PLACE D/A IN STACK                         
         LA    R5,4(R5)            ..INCREMENT A(IN STACK)                      
         LA    R6,1(R6)            ..INCREMENT COUNTER                          
         B     DP010                                                            
*                                                                               
DP030    LTR   R6,R6               TEST FOR ANY RECORDS                         
         BZ    DP999 ..NO ENTRIES - FINISHED                                    
         L     R5,ASTACK ..SET A(1ST ENTRY)                                     
         B     DP050                                                            
*                                                                               
DP040    LM    R5,R6,SAVESTAK                                                   
         LA    R5,4(R5)            GET NEXT KEY FROM STACK                      
         BCT   R6,DP050            ..CYCLE ON STACK COUNT                       
         B     DP999               DONE FOR THIS DAYPART                        
*                                                                               
DP050    MVC   KEY+28(4),0(R5)     ..SET D/A OF INV RECORD                      
         STM   R5,R6,SAVESTAK      ..SAVE A(IN STACK) + COUNT                   
         GOTO1 GETREC              GET HEADER RECORD                            
         MVC   KEY(27),IO          SAVE THE KEY                                 
         USING RINVKEY,R4                                                       
         MVI   ELCODE,1                                                         
         LA    R6,IO                                                            
         BAS   RE,GETEL            GET 01 ELEMENT                               
         USING RINVPEL,R6                                                       
         LA    R1,RINVPFLT         FILTER FILTER                                
         LA    R5,TITFILT                                                       
         LA    R0,6                                                             
*                                                                               
DP070    CLI   0(R5),C'A'          TEST FOR ALPHANUMERIC FILTER                 
         BL    DP080                                                            
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
         BAS   RE,DELETES          DELETE THE RECORDS                           
         BAS   RE,PRINT                                                         
         B     DP040                                                            
*                                                                               
DP999    B     EXIT                                                             
         EJECT                                                                  
DPHEAD   NTR1                                                                   
*                                                                               
*        ROUTINE TO FORMAT DAYPART HEADER DETAILS                               
*                                                                               
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         ZAP   TXCNT,=P'0'                                                      
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         GOTO1 UNDAY,DMCB,RINVPDAY,P                                            
         GOTO1 UNTIME,DMCB,RINVPTIM,P+9                                         
         ZIC   R5,RINVPLEN                                                      
         SH    R5,=H'40'                                                        
         GOTO1 CHOPPER,DMCB,((R5),RINVPROG),(27,P+22),(C'P',3)                  
         EDIT  (1,RINVKQTR),(2,P+50),FILL=0                                     
         MVC   P+52(2),RINVKDAY                                                 
         CLI   P+53,C'0'                                                        
         BNE   *+8                                                              
         MVI   P+53,C' '                                                        
         GOTO1 DATCON,DMCB,(2,RINVPEFF),(8,P+56)                                
         CLI   RINVPEFF+2,0                                                     
         BE    EXIT                                                             
         GOTO1 DATCON,DMCB,(2,RINVPEFF+2),(8,P+65)                              
         B     EXIT                                                             
         EJECT                                                                  
MSHEAD   NTR1                                                                   
*                                                                               
*        ROUTINE TO FORMAT MKT/STN TEXT HEADER DETAILS                          
*                                                                               
         BAS   RE,PRINT            ANYTHING TO PRINT                            
         ZAP   TXCNT,=P'0'                                                      
         XC    TXBKDEL,TXBKDEL                                                  
         MVC   P,SPACES                                                         
         CLI   RINVKSRC,C'M'                                                    
         BNE   *+14                                                             
         MVC   P(11),=C'MARKET TEXT'                                            
         B     EXIT                                                             
         CLI   RINVKSRC,C'S'                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   P(12),=C'STATION TEXT'                                           
         B     EXIT                                                             
         EJECT                                                                  
TXRGDET  NTR1                                                                   
*                                                                               
*        ROUTINE TO FORMAT TEXT NUMBERS TO PRINT LINE                           
*                                                                               
         CP    TXCNT,TXMAX                                                      
         BNE   TD010                                                            
         BAS   RE,PRINT                                                         
         ZAP   TXCNT,=P'0'                                                      
*                                                                               
TD010    CP    TXCNT,=P'0'                                                      
         BNE   TD020                                                            
         L     R5,PSTART                                                        
         MVC   0(4,R5),=C'TEXT'                                                 
         LA    R5,5(R5)                                                         
         ST    R5,TXNXT                                                         
*                                                                               
TD020    L     R5,TXNXT                                                         
         EDIT  (2,RINVKTXT),(4,0(R5)),FILL=0                                    
         LA    R5,5(R5)                                                         
         ST    R5,TXNXT                                                         
         AP    TXCNT,=P'1'                                                      
         B     EXIT                                                             
         EJECT                                                                  
DELETES  NTR1                                                                   
*                                                                               
*        ROUTINE TO HANDLE THE DELETES                                          
*                                                                               
         TM    DELBYTE,X'01'       OPTION TO DELETE EVERYTHING                  
         BZ    DE020                THAT FITS WITHIN SELECTED DATES             
         OC    STDATE,STDATE                                                    
         BZ    *+14                NO START DATE                                
         CLC   RINVPEFF(2),STDATE                                               
         BL    DE999               BEFORE START                                 
         OC    ENDATE,ENDATE                                                    
         BZ    DE005               NO END DATE                                  
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    DE999                                                            
         CLC   RINVPEFF+2(2),ENDATE                                             
         BH    DE999               AFTER END                                    
*                                                                               
DE005    MVC   P+74(29),=C'INVENTORY, ALL BOOKS AND TEXT'                       
         BAS   RE,DELIT            DELETE THE HEADER                            
         OI    DMINBTS,X'08'       RE-READ HEADER WE JUST DELETED               
         GOTO1 HIGH                KEY HAS HEADER KEY                           
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   TITREV,C'Y'         IF REVERSING, ALWAYS PASS DELETES            
         BE    DE010                                                            
         NI    DMINBTS,X'F7'       ELSE, TURN OFF PASS DELETES                  
*                                                                               
DE010    GOTO1 SEQ                 NEXT                                         
         CLC   KEY(24),KEYSAVE                                                  
         BNE   DE999                                                            
         GOTO1 GETREC              READ ALL INVENTORY AND TEXT                  
         BAS   RE,DELIT            DELETE THEM                                  
         B     DE010                                                            
*                                                                               
DE020    CLI   DELBYTE,X'0C'       TEST FOR DELETE ONLY INV RANGE               
         BNE   DE030                                                            
         CLC   RINVKINV,INRANGE                                                 
         BL    DE999               INV NO TOO LOW                               
         CLC   RINVKINV,INRANGE+3                                               
         BH    DE999               INV NO TOO HIGH                              
*                                                                               
DE030    XC    TXBKDEL,TXBKDEL                                                  
         LA    R7,23               ..TRACKS:  THRU EFF. START DATE              
         MVI   RINVKSRC,C'A'      MOVE 'A' INORDER TO BYPASS MASTER             
         TM    DELBYTE,X'04'                                                    
         BO    DE035                                                            
         LA    R7,24               ..TEXT: THRU X'FF' INDICATOR                 
         MVI   RINVKSRC,X'FF'                                                   
         TM    DELBYTE,X'08'                                                    
         BZ    DE035                                                            
         MVC   RINVKTXT,TXRANGE    START AT TEXT RANGE START                    
*                                                                               
DE035    GOTO1 HIGH                                                             
         B     DE050                                                            
*                                                                               
DE040    GOTO1 SEQ                 NEXT                                         
*                                                                               
DE050    EX    R7,KEYCOMP                                                       
         BNE   DE999               DONE                                         
         LA    R5,SRCTAB                                                        
*                                                                               
DE060    CLI   0(R5),0             FIND RECORD TYPE CODE IN TABLE               
         BNE   *+6                                                              
         DC    H'0'                INVALID                                      
         CLC   RINVKSRC,0(R5)      COMPARE INVENTORY TO TABLE                   
         BE    DE070                                                            
         LA    R5,6(R5)                                                         
         B     DE060                                                            
*                                                                               
DE070    GOTO1 GETREC                                                           
         SR    RF,RF                                                            
         ICM   RF,7,1(R5)          ..A(ROUTINE) FROM SRCTAB                     
         A     RF,RELO                                                          
         BR    RF                  BRANCH APPROPRIATELY                         
*                                                                               
DE100    MVC   DUB(3),=C'ARB'      ARB INV DATA                                 
         B     DE120                                                            
*                                                                               
DE110    MVC   DUB(3),=C'NSI'      NSI INV DATA                                 
         B     DE120                                                            
*                                                                               
DE115    MVC   DUB(3),=C'SRC'      SRC INV DATA                                 
*                                                                               
DE120    TM    DELBYTE,X'04'       TEST INV DELETE                              
         BZ    DE300                                                            
         CLI   TITSVC,0            ..ANY SERVICE ENTERED?                       
         BE    DE130               ..NO                                         
         CLC   TITSVC(1),DUB       FILTER SERVICE                               
         BNE   DE300                                                            
*                                                                               
DE130    TM    DELBYTE,X'08'       DELETE RANGE                                 
         BO    DE160                                                            
         LA    RF,DELBOOKS         FILTER INV BOOK                              
*                                                                               
DE140    CLI   0(RF),X'FF'                                                      
         BE    DE300                                                            
         CLC   RINVKBK,1(RF)                                                    
         BE    DE145                                                            
DE143    LA    RF,3(RF)                                                         
         B     DE140                                                            
*                                  FILTER BOOK TYPE                             
DE145    CLI   0(R5),C'A'          ARB                                          
         BE    DE147                                                            
         CLI   0(R5),C'N'          NSI                                          
         BE    DE147                                                            
         CLI   0(R5),C'T'          SRC                                          
         BE    DE147                                                            
*                                                                               
         ZIC   RE,4(R5)            BITS CORRESPOND TO BOOKVAL                   
         ZIC   R0,0(RF)                                                         
*                                                                               
*   ONCE BOOK IS FOUND, THE CORRECT PREFIX MUST BE DETERMINED FOR               
*     THE COMPARISON.  PREVIOUSLY, THIS ROUTINE WAS USING THE SCREEN            
*     FIELD 'TITSVC' TO DETERMINE WHETHER ARB, NSI, OR SRC WAS BEING            
*     REQUESTED.  WHEN THIS FIELD WAS EMPTY, THE ROUTINE ALWAYS FORCED          
*     TEST TO SRC, RESULTING IN A NON-MATCH FOR DELETION.  CHANGE MADE          
*     TO PICK UP SERVICE FROM CURRENT POSITION IN 'SRCTAB'.                     
*                                                                               
         CLI   0(R5),C'F'          ..A-E: ARBRITRON PREFIXES                    
         BL    DE146                                                            
         O     R0,=X'00000040'     ..PRESET NIELSEN BIT RANGE                   
         CLI   0(R5),C'S'          ..N-R: NIELSEN PREFIXES                      
         BL    DE146                                                            
         O     R0,=F'01'           ..TUX: SRC PREFIXES                          
DE146    CR    R0,RE                                                            
         BE    DE170                                                            
         LA    RF,3(RF)                                                         
         B     DE140                                                            
*                                                                               
DE147    TM    0(RF),X'2E'                                                      
         BZ    DE170                                                            
         LA    RF,3(RF)                                                         
         B     DE140                                                            
*                                                                               
DE150    TM    DELBYTE,X'02'       INVENTORY TEXT                               
         BZ    DE300                                                            
         TM    DELBYTE,X'08'                                                    
         BZ    *+12                                                             
         CLI   TITSVC,0                                                         
         BE    DE155                                                            
         BAS   RE,TXFILT           FILTER SERVICE AND/OR BOOK                   
         BNE   DE300                                                            
         TM    DELBYTE,X'08'                                                    
         BZ    DE180                                                            
*                                                                               
DE155    CLC   RINVKTXT,TXRANGE    FILTER TEXT RANGE                            
         BL    DE300                                                            
         CLC   RINVKTXT,TXRANGE+2                                               
         BH    DE300                                                            
         BAS   RE,TXRGDET          FORMAT TEXT RANGE                            
         B     DE200                                                            
*                                                                               
DE160    CLC   P+78(3),DUB         FORMAT INV RANGE                             
         BE    DE200                                                            
         BAS   RE,PRINT                                                         
         MVC   P+74(3),=C'ALL'                                                  
         MVC   P+78(3),DUB                                                      
         MVC   P+82(9),=C'INVENTORY'                                            
         B     DE200                                                            
*                                                                               
DE170    MVC   P+74(9),=C'INVENTORY'   FORMAT INV BOOK                          
         MVC   P+84(1),5(R5)       BOOK TYPE                                    
         TM    0(RF),X'20'         ESTIMATED BOOK?                              
         BZ    *+8                                                              
         MVI   P+84,C'E'                                                        
*                                                                               
         MVC   WORK(2),RINVKBK                                                  
         B     DE190                                                            
*                                                                               
DE180    BAS   RE,TXBKDET          FORMAT TEXT BOOK                             
         BNE   DE200                                                            
         MVC   P+74(4),=C'TEXT'                                                 
         L     R6,SAVER6                                                        
         MVC   WORK(2),RINVFBK-RINVFEL(R6)                                      
*                                                                               
DE190    MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,P+85)                                    
         MVC   P+92(3),DUB                                                      
         BAS   RE,PRINT                                                         
*                                                                               
DE200    BAS   RE,DELIT                                                         
*                                                                               
DE300    B     DE040                                                            
*                                                                               
DE999    B     EXIT                                                             
         SPACE 2                                                                
KEYCOMP  CLC   KEY(0),KEYSAVE      ** EXECUTED                                  
         SPACE                                                                  
******SRCCOMP  TM    0(RF),0             ** EXECUTED                            
         SPACE 2                                                                
*  SOURCE TABLE                                                                 
*    BYTE 1      RINVKSRC VALUE                                                 
*    BYTE 2-4    BRANCH ADDRESS                                                 
*    BYTE 5      BOOKVAL BOOK-TYPE BITS                                         
*    BYTE 6      BOOK-TYPE CHARACTER FOR PRINTING                               
         SPACE 2                                                                
SRCTAB   DC    CL1'A',AL3(DE100),XL1'00',CL1' '   ARB                           
         DC    CL1'B',AL3(DE100),XL1'04',CL1'P'      PROJECTED                  
         DC    CL1'C',AL3(DE100),XL1'08',CL1'T'      TIME PERIOD                
         DC    CL1'D',AL3(DE100),XL1'02',CL1'S'      SPECIAL SURVEY             
         DC    CL1'E',AL3(DE100),XL1'20',CL1'E'      ESTIMATED                  
*                                                                               
         DC    CL1'N',AL3(DE110),XL1'40',CL1' '   NSI                           
         DC    CL1'O',AL3(DE110),XL1'44',CL1'P'      PROJECTED                  
         DC    CL1'P',AL3(DE110),XL1'48',CL1'T'      TIME PERIOD                
         DC    CL1'Q',AL3(DE110),XL1'42',CL1'S'      SPECIAL SURVEY             
         DC    CL1'R',AL3(DE110),XL1'60',CL1'E'      ESTIMATED                  
*                                                                               
         DC    CL1'T',AL3(DE115),XL1'41',CL1' '   SRC                           
         DC    CL1'U',AL3(DE115),XL1'45',CL1'P'      PROJECTED                  
         DC    CL1'X',AL3(DE115),XL1'61',CL1'E'      ESTIMATED                  
*                                                                               
         DC    XL1'FF',AL3(DE150),XL1'00',CL1' '                                
         DC    AL1(0)                                                           
         EJECT                                                                  
TXBKDET  NTR1                                                                   
*                                                                               
*        ROUTINE TO FIND OUT IF FORMAT BOOKS TO PRINT LINE NEEDED               
*                                                                               
         L     RF,SAVERF                                                        
         L     R6,SAVER6                                                        
         USING RINVFEL,R6                                                       
         CLI   RINVFSRC,C'A'                                                    
         BNE   TB010                                                            
         TM    0(RF),X'02'                                                      
         BO    NEQEXIT                                                          
         OI    0(RF),X'02'                                                      
         MVC   DUB(3),=C'ARB'                                                   
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
         CLI   TITREV,C'Y'         TEST FOR REVERSE DELETE                      
         BE    DT010                                                            
         TM    RINVCNTL,X'80'      TEST FOR ALREADY DELETED                     
         BO    DT999                                                            
         OI    RINVCNTL,X'80'                                                   
         GOTO1 PUTREC              MARK RECORD FOR DELETION                     
         OI    KEY+27,X'80'                                                     
         GOTO1 WRITE               MARK KEY FOR DELETION                        
*                                                                               
         CLI   RINVKSRC,X'00'      IF DELETING THE HEADER,                      
         BNE   DT005                                                            
         GOTO1 INVPTR,DMCB,(R6),PPAREA   BUILD PASSIVE POINTER                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R6,6                BCT THROUGH 6 DAYPART POINTERS               
         LA    R4,PPAREA                                                        
DT003    MVC   KEY,0(R4)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    KEY+27,X'80'        MARK PASSIVE POINTER FOR DELETION            
         GOTO1 WRITE                                                            
         LA    R4,32(R4)           NEXT POINTER                                 
         OC    0(32,R4),0(R4)                                                   
         BZ    DT004               NO MORE                                      
         BCT   R6,DT003                                                         
*                                                                               
DT004    MVC   KEY,SAVEKEY         RE-READ HEADER WE JUST DELETED               
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
         B     DT999                                                            
*                                                                               
DT010    TM    RINVCNTL,X'80'      TEST FOR NOT ALREADY DELETED                 
         BNO   DT999                                                            
         NI    RINVCNTL,X'7F'                                                   
         GOTO1 PUTREC              UNDELETE RECORD                              
         NI    KEY+27,X'7F'                                                     
         GOTO1 WRITE               UNDELETE KEY                                 
*                                                                               
         CLI   RINVKSRC,X'00'      IF UNDELETING THE HEADER,                    
         BNE   DT020                                                            
         GOTO1 INVPTR,DMCB,(R6),PPAREA   BUILD PASSIVE POINTER                  
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         LA    R6,6                BCT THROUGH 6 DAYPART POINTERS               
         LA    R4,PPAREA                                                        
DT013    MVC   KEY,0(R4)                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    KEY+27,X'7F'        UNDELETE PASSIVE POINTER                     
         GOTO1 WRITE                                                            
         LA    R4,32(R4)           NEXT POINTER                                 
         OC    0(32,R4),0(R4)                                                   
         BZ    DT014               NO MORE                                      
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
         DROP  R6                                                               
         EJECT                                                                  
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
         EJECT                                                                  
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
         MVC   H4+10(4),TITSTAT                                                 
         MVC   H4+16(24),TITMKT                                                 
         CLI   37(RA),C'N'                                                      
         BNE   *+10                                                             
         MVC   H4+43(27),=C'*** RECORDS NOT DELETED ***'                        
         CLI   RCSUBPRG,1                                                       
         BE    HK999                                                            
         LA    R3,DPTBL            LOOK UP DAYPART                              
*                                                                               
HK010    CLC   DPBYTE,0(R3)                                                     
         BE    HK020                                                            
         LA    R3,L'DPTBL(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         BNE   HK010                                                            
*                                                                               
HK020    MVC   H4+84(20),1(R3)     DAYPART                                      
*                                                                               
HK999    B     EXIT                                                             
         EJECT                                                                  
         GETEL (R6),34,ELCODE                                                   
         SPACE 2                                                                
         DS    0H                                                               
DPTBL    DS    0CL21                                                            
         DC    CL21'MMORNING'                                                   
         DC    CL21'DDAYTIME'                                                   
         DC    CL21'EEARLY FRINGE'                                              
         DC    CL21'REARLY NEWS'                                                
         DC    CL21'APRIME ACCESS'                                              
         DC    CL21'TLATE NEWS'                                                 
         DC    CL21'LLATE FRINGE'                                               
         DC    CL21'WWEEKEND'                                                   
         DC    CL21'KKIDS'                                                      
         DC    CL21'FFRINGE'                                                    
         DC    CL21'NNEWS'                                                      
         DC    CL21'PPRIME'                                                     
         DC    CL21'VMOVIES'                                                    
         DC    CL21'SSPECIAL'                                                   
         DC    CL21'JSPORTS'                                                    
         DC    CL21'OSOAPS'                                                     
         DC    CL21'UCOMPETITIVE'                                               
         DC    CL21'XLOCAL'                                                     
         DC    CL21'YOTHER'                                                     
         DC    X'FF'                                                            
         DC    CL20'GENERAL'                                                    
NDPT     EQU   (*-DPTBL)/L'DPTBL                                                
         SPACE                                                                  
RELOC    DC    A(*)                                                             
         EJECT                                                                  
       ++INCLUDE REINVPTR                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
DUMMY    DSECT                                                                  
       ++INCLUDE REGENREP                                                       
       ++INCLUDE REGENINV                                                       
       ++INCLUDE RESPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE RESPLFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE RESPLD9D                                                       
DPLIST   DS    CL20                DAYPART LIST                                 
DELBYTE  DS    X                   X'01' ALL DELETE                             
*                                  X'02' TEXT DELETE                            
*                                  X'04' INV DELETE                             
*                                  X'08' DELETE RANGE                           
TXRANGE  DS    XL4                 TEXT RANGE                                   
INRANGE  DS    XL6                 INV RANGE                                    
DELBOOKS DS    CL15                TABLE OF BOOKS                               
STDATE   DS    XL2                 START DATE                                   
ENDATE   DS    XL2                 END DATE                                     
*                                                                               
SAVESTAK DS    2F                                                               
ASTACK   DS    A                                                                
RELO     DS    A                                                                
TXNXT    DS    A                                                                
PSTART   DS    A                                                                
SAVER6   DS    F                                                                
SAVERE   DS    F                                                                
SAVERF   DS    F                                                                
TXCNT    DS    PL2                                                              
TXMAX    DS    PL2                                                              
DPBYTE   DS    C                                                                
BTODAY   DS    CL2                                                              
TXBKDEL  DS    CL4         1 BYTE PER DELETE BOOK                               
*                          BIT ON MEANS DETAIL HAS BEEN FORMATTED               
*                          X'01' NO SVC FILTER                                  
*                          X'02' ARB                                            
*                          X'04' NSI                                            
*                          X'08' SRC                                            
DUMPOPT  DS    C                                                                
SAVEKEY  DS    CL32                                                             
PPAREA   DS    CL200                                                            
         PRINT OFF                                                              
         SPACE 2                                                                
STACK    CSECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032RESPL99   05/01/02'                                      
         END                                                                    
