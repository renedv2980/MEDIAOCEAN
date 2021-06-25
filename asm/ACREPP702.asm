*          DATA SET ACREPP702  AT LEVEL 014 AS OF 05/01/02                      
*PHASE ACP702A                                                                  
*INCLUDE ACCDIV                                                                 
*INCLUDE SQUASHER                                                               
*INCLUDE ACLIST                                                                 
         TITLE 'NEW AGEING'                                                     
ACP702   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACP7**,R9,RR=RE                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA          RA=GENERAL W/S                               
         LA    RC,SPACEND                                                       
         USING ACP7D,RC            RC=PROGRAM W/S                               
         ST    RE,PRELOC                                                        
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BNE   UJ10                                                             
         GOTO1 PROLLER,DMCB,0,TABLE,4,7                                         
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
UJ10     CLI   MODE,REQFRST        BUILD DATE TABLES AND HEADLINES              
         BNE   UJ40                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         MVC   TYPE,QOPT1                                                       
         MVC   HEADTAB,SPACES                                                   
         GOTO1 DATCON,DMCB,(0,QEND),(2,END2)                                    
         ZAP   DUB,=P'30'          SET UP DEFAULT INCR                          
         CLC   QOPT3(2),SPACES                                                  
         BNE   UJ10A                                                            
         CLI   TYPE,C'D'                                                        
         BE    UJ10C                                                            
         ZAP   DUB,=P'1'                                                        
         B     UJ10C                                                            
UJ10A    PACK  DUB,QOPT3(2)        INCREMENT                                    
UJ10C    CVB   RF,DUB                                                           
         LNR   RF,RF                                                            
         ST    RF,INCR                                                          
*                                                                               
         CLI   QOPT1,C'M'          MONTH OF SERVICE AGEING                      
         BNE   UJ20                                                             
*                                                                               
         MVC   WORK(4),QEND                                                     
         MVC   WORK+4(2),=C'28'    BUILD END3 FOR THIS TYPE OF REQUEST          
         BAS   RE,FENDM            FORCE TO END OF MONTH                        
         GOTO1 DATCON,DMCB,(0,WORK),(1,END3)                                    
         MVC   WORK(6),QEND        BUILD Y/M/D TABLE                            
         MVC   WORK+4(2),=C'15'                                                 
         L     RF,INCR                                                          
         AH    RF,=H'1'            INCREMENT (NEGATIVE) LESS 1                  
         MH    RF,=H'30'           NO OF MONTHS WIDTH FOR COLS                  
         ST    RF,FULL                                                          
         LA    R2,DATAB                                                         
         LA    R3,4                                                             
UJ12     MVC   0(6,R2),WORK        FIRST HALF OF ENTRY                          
         L     RF,FULL                                                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         MVC   6(6,R2),WORK+6      SECOND HALF OF ENTRY                         
         LA    R2,12(R2)                                                        
         L     R5,=F'-30'                                                       
         GOTO1 ADDAY,DMCB,WORK+6,WORK,(R5)                                      
         BCT   R3,UJ12                                                          
*                                                                               
         LA    R4,ENDMOS                                                        
         LA    R2,QEND                                                          
         BAS   RE,DTOMOS                                                        
         LA    R2,DATAB            CONVERT DATES TO MONTHS OF SERVICE           
         LA    R4,MOSTAB                                                        
         LA    R5,HEADTAB                                                       
         LA    R3,8                                                             
         MVI   BYTE,0                                                           
UJ14     BAS   RE,DTOMOS                                                        
         CLC   QOPT3(2),=C'01'                                                  
         BNE   UJ15                                                             
         CLI   BYTE,1              SPECIAL FOR ONE MONTH WIDE COLS              
         BNE   UJ15                                                             
         B     UJ15A                                                            
UJ15     GOTO1 DATCON,DMCB,(0,(R2)),(9,1(R5))                                   
UJ15A    CLI   BYTE,0                                                           
         BNE   UJ16                                                             
         CLC   QOPT3(2),=C'01'                                                  
         BE    UJ15C                                                            
         MVC   7(2,R5),=C'TO'                                                   
UJ15C    MVI   BYTE,1                                                           
         B     *+8                                                              
UJ16     MVI   BYTE,0                                                           
         LA    R5,10(R5)                                                        
         LA    R2,6(R2)                                                         
         LA    R4,2(R4)                                                         
         BCT   R3,UJ14                                                          
         SH    R4,=H'2'            CLEAR LAST MOS ENTRY                         
         XC    0(2,R4),0(R4)       FOR 'AND PRIOR'                              
         SH    R5,=H'20'                                                        
         MVC   7(12,R5),=C'   AND PRIOR'                                        
         B     EXIT                                                             
         EJECT                                                                  
UJ20     LA    R4,DATAB3           DATE AGEING                                  
         MVC   WORK(6),QEND                                                     
         CLC   QOPT3(2),SPACES                                                  
         BE    UJ21                                                             
         CLC   QOPT3(2),=C'15'     FOR INCR LESS THAN 15                        
         BL    UJ26                ASSUME VALUE IS WHOLE MONTHS                 
UJ21     GOTO1 DATCON,DMCB,(0,QEND),(1,END3)                                    
         LA    R5,HEADTAB                                                       
         LA    R3,4                                                             
UJ22     GOTO1 DATCON,DMCB,(0,WORK),(1,(R4))                                    
         GOTO1 (RF),(R1),,(8,0(R5))                                             
         L     RF,INCR                                                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         MVC   WORK(6),WORK+6                                                   
         GOTO1 DATCON,DMCB,(0,WORK),(1,3(R4))                                   
         GOTO1 (RF),(R1),,(8,10(R5))                                            
         L     RF,=F'-1'                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         MVC   WORK(6),WORK+6                                                   
         MVI   8(R5),C'-'                                                       
         LA    R4,6(R4)                                                         
         LA    R5,20(R5)                                                        
         BCT   R3,UJ22                                                          
         SH    R5,=H'20'                                                        
         MVC   8(12,R5),=C'  AND PRIOR '                                        
         B     EXIT                                                             
         EJECT                                                                  
UJ26     MVC   WORK+4(2),=C'28'    DATE AGEING BY WHOLE MONTHS                  
         BAS   RE,FENDM            FORCE TO END OF MONTH                        
         GOTO1 DATCON,DMCB,(0,WORK),(1,END3)                                    
         LA    R5,HEADTAB                                                       
         LA    R3,4                                                             
         L     RF,INCR                                                          
         AH    RF,=H'1'                                                         
         MH    RF,=H'32'                                                        
         ST    RF,INCR             GO BACK 32*N-1 DAYS                          
UJ28     GOTO1 DATCON,DMCB,(0,WORK),(1,(R4))                                    
         GOTO1 (RF),(R1),,(8,0(R5))                                             
         L     RF,INCR                                                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         MVC   WORK(6),WORK+6                                                   
         MVC   WORK+4(2),=C'01'    FORCE TO START OF MONTH                      
         GOTO1 DATCON,DMCB,(0,WORK),(1,3(R4))                                   
         GOTO1 (RF),(R1),,(8,10(R5))                                            
         L     RF,=F'-1'                                                        
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         MVC   WORK(6),WORK+6                                                   
         MVI   8(R5),C'-'                                                       
         LA    R4,6(R4)                                                         
         LA    R5,20(R5)                                                        
         BCT   R3,UJ28                                                          
         SH    R5,=H'20'                                                        
         MVC   8(12,R5),=C'  AND PRIOR '                                        
         B     EXIT                                                             
         SPACE 2                                                                
FENDM    NTR1                      FIND END OF MONTH                            
FENDM2   LA    RF,1                                                             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(RF)                                      
         CLC   WORK+2(2),WORK+8    SAME MONTH                                   
         BNE   EXIT                                                             
         MVC   WORK(6),WORK+6                                                   
         B     FENDM2                                                           
         EJECT                                                                  
UJ40     CLI   MODE,LEVAFRST                                                    
         BNE   UJ42                                                             
         CLI   QOPT7,C'Y'                                                       
         BE    EXIT                                                             
         MVI   FORCEHED,C'Y'                                                    
         B     EXIT                                                             
         SPACE 2                                                                
UJ42     CLI   MODE,LEVBFRST                                                    
         BNE   UJ50                                                             
         MVI   PRODPEND,0                                                       
         B     EXIT                                                             
         SPACE 2                                                                
UJ50     CLI   MODE,PROCACC                                                     
         BNE   UJ100                                                            
         MVI   JACTIV,C'N'                                                      
         MVI   FILTER,C'N'                                                      
         CLI   QOPT5,C' '          CLOSED OPTION                                
         BE    UJ60                                                             
         SR    RE,RE                                                            
         L     RF,ADACC                                                         
         AH    RF,DATADISP                                                      
UJ52     CLI   0(RF),0             LOOK FOR DATE CLOSED                         
         BE    EXIT                                                             
         CLI   0(RF),X'26'         HELD IN JOB ELEMENT                          
         BE    UJ54                                                             
         IC    RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     UJ54                                                             
         USING ACJOBD,RF                                                        
UJ54     MVC   WORK(3),ACJBCLOS                                                 
*                                                                               
         L     R2,ADACCSTA                                                      
         USING ACSTATD,R2                                                       
         CLI   QOPT5,C'S'          SUPPRESS CLOSED                              
         BE    UJ56                                                             
         TM    ACSTSTAT,X'40'      CLOSED ONLY                                  
         BZ    EXIT                                                             
         CLC   WORK(3),END3        CLOSED AFTER END DATE                        
         BH    EXIT                SO FORGET IT                                 
         B     UJ60                                                             
*                                                                               
UJ56     TM    ACSTSTAT,X'40'                                                   
         BZ    UJ60                CARRY ON IF NOT CLOSED                       
         CLC   WORK(3),END3        CLOSED BEFORE END DATE                       
         BL    EXIT                SO FORGET IT                                 
         B     UJ60                OTHERWISE PRETEND NOT CLOSED                 
*                                                                               
UJ60     CLI   QOPT6,C' '          ZERO BALANCE OPTION                          
         BE    UJ70                                                             
         L     R2,ADACCBAL                                                      
         USING ACBALD,R2                                                        
         CLI   QOPT6,C'Z'                                                       
         BE    UJ62                                                             
         CP    ACBLDR,ACBLCR       SUPPRESS JOBS WITH ZERO BAL                  
         BE    EXIT                                                             
         B     UJ70                                                             
UJ62     CP    ACBLDR,ACBLCR                                                    
         BNE   EXIT                ZERO BALANCE ONLY                            
UJ70     MVI   FILTER,C'Y'                                                      
         B     EXIT                                                             
         EJECT                                                                  
UJ100    CLI   MODE,PROCTRNS                                                    
         BNE   UJ150                                                            
         L     R2,ADTRANS                                                       
         USING TRANSD,R2                                                        
         CLI   0(R2),X'44'                                                      
         BNE   EXIT                                                             
         CLC   TRNSANAL,=C'**'                                                  
         BE    EXIT                                                             
         CLI   FILTER,C'Y'         DON'T WANT THIS JOB                          
         BNE   EXIT                                                             
         L     RF,VEXTRAS          LOOK FOR LIST RECORD                         
         USING RUNXTRAD,RF                                                      
         MVI   LISTSW,C'N'                                                      
         OC    VLISTREC,VLISTREC                                                
         BZ    UJ101                                                            
         GOTO1 =V(ACLIST),DMCB,VLISTREC,TRNSANAL,RR=RB                          
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                BAD LIST RECORD                              
         MVI   LISTSW,C'Y'                                                      
         CLI   DMCB,C'E'           EXCLUDE THIS WORK CODE                       
         BE    EXIT                                                             
*                                                                               
UJ101    GOTO1 PROLLER,DMCB,1,TABLE,1                                           
         L     R3,DMCB             ADDRESS LINE 1                               
         CLI   TYPE,C'M'                                                        
         BE    UJ102                                                            
         CLC   TRNSDATE,END3       IF DATE AGEING - DONT WANT ANY               
         BH    EXIT                TRANSACTION BEYOND END DATE                  
         B     UJ110                                                            
*                                                                               
UJ102    GOTO1 ACDATE,DMCB,(6,TRNSBTCH),ENDMOS,DATCON                           
         CLI   DMCB,0                                                           
         BNE   *+6                                                              
         DC    H'0'                INVALID INPUT                                
         CLI   DMCB,X'0C'          IF MOS - USE ACDATE TO DETERMINE             
         BE    EXIT                IF WE WANT TRANSACTION                       
*                                                                               
UJ110    DS    0H                                                               
         TM    TRNSSTAT,X'80'                                                   
         BO    UJ120               BRANCH IF DEBIT                              
         MVI   JACTIV,C'Y'                                                      
         AP    24(6,R3),TRNSAMNT   ADD BILL TO BILL COL IN TABLE                
         B     EXIT                                                             
*                                                                               
UJ120    DS    0H                                                               
         CLI   QOPT2,C' '          AUTH/UNAUTH OPTION                           
         BE    UJ121                                                            
         CLI   QOPT2,C'U'                                                       
         BNE   UJ120A                                                           
         TM    TRNSSTAT,X'08'                                                   
         BO    EXIT                                                             
         B     UJ121                                                            
*                                                                               
UJ120A   TM    TRNSSTAT,X'08'      A=AUTH ONLY                                  
         BZ    EXIT                                                             
*                                                                               
UJ121    MVI   JACTIV,C'Y'                                                      
         ZAP   BILLAMNT,=P'0'                                                   
*                                                                               
         USING ACMD,R4                                                          
         L     R4,AMONACC                                                       
         L     R4,ACMAPRO2                                                      
*                                                                               
         USING PTAELD,R4                                                        
UJ123    CLI   0(R4),PTAELQ                                                     
         BNE   UJ130                                                            
         TM    PTASTAT1,PTASPEND   SKIP PENDING ITEMS                           
         BO    UJ124                                                            
         TM    PTASTAT1,PTASREVS   SKIP REVERSED                                
         BO    UJ124                                                            
         TM    PTASTAT1,PTASREVU   SKIP REVERSALS                               
         BO    UJ124                                                            
         CLI   PTATYPE,PTATWOF     TAKE WRITE-OFFS                              
         BE    UJ125                                                            
         CLI   PTATYPE,PTATWOFR    RECOVERIES                                   
         BE    UJ125                                                            
         CLI   PTATYPE,PTATRAL     AND BILLING                                  
         BE    UJ126                                                            
*                                                                               
UJ124    SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         AR    R4,RF                                                            
         B     UJ123               GET NEXT                                     
*                                                                               
UJ125    GOTO1 DATCON,DMCB,(1,PTAWDAT),(2,WORK)                                 
         CLC   WORK(2),END2                                                     
         BH    UJ124               SKIP IF BILLED AFTER END DATE                
         B     UJ128                                                            
*                                                                               
UJ126    CLC   PTARBLDT,END2                                                    
         BH    UJ124               SKIP IF BILLED AFTER END DATE                
*                                                                               
UJ128    AP    BILLAMNT,PTANET                                                  
         B     UJ124                                                            
*                                                                               
UJ130    SP    24(6,R3),BILLAMNT   REDUCE BILLING BY ALLOC AMNT                 
         ZAP   DUB,TRNSAMNT                                                     
         SP    DUB,BILLAMNT                                                     
         CP    DUB,=P'0'           AGE DEBIT BALANCE IF ANY                     
         BE    EXIT                                                             
         LR    R6,R3                                                            
         LA    R5,3                                                             
         CLI   TYPE,C'M'                                                        
         BE    UJ140                                                            
         LA    R4,DATAB3                                                        
UJ132    CLC   TRNSDATE,3(R4)                                                   
         BNL   UJ144                                                            
         LA    R6,6(R6)                                                         
         LA    R4,6(R4)                                                         
         BCT   R5,UJ132                                                         
         B     UJ144                                                            
*                                                                               
UJ140    LA    R4,MOSTAB                                                        
UJ142    GOTO1 ACDATE,DMCB,(6,TRNSBTCH),2(R4),DATCON                            
         CLI   DMCB,X'0C'                                                       
         BE    UJ144                                                            
         CLI   DMCB,X'08'                                                       
         BE    UJ144                                                            
         LA    R6,6(R6)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,UJ142                                                         
UJ144    AP    0(6,R6),DUB                                                      
         AP    30(6,R3),DUB        TOTAL                                        
         B     EXIT                                                             
         EJECT                                                                  
UJ150    CLI   MODE,ACCLAST                                                     
         BNE   UJ300                                                            
         CLI   JACTIV,C'N'                                                      
         BE    EXIT                                                             
         CLI   QOPT7,C'Y'          CLIENT LINES ONLY                            
         BE    UJ152                                                            
         CLI   PRODPEND,0                                                       
         BNE   UJ152                                                            
         MVI   PRODPEND,1                                                       
         BAS   RE,PRODPRT                                                       
UJ152    GOTO1 PROLLER,DMCB,1,TABLE,1                                           
         L     R3,DMCB             ADDRESS LINE 1                               
         CLI   LISTSW,C'Y'                                                      
         BNE   *+10                NO PRE-BILLING ON LIST REQUESTS              
         ZAP   24(6,R3),=P'0'                                                   
         SP    30(6,R3),24(6,R3)   COMPLETE LINE                                
         GOTO1 PROLLER,DMCB,6,TABLE                                             
         LA    R3,1                                                             
         BAS   RE,FORMAT                                                        
         CLI   QOPT7,C'Y'                                                       
         BE    UJ154                                                            
         BAS   RE,JOBPRT                                                        
         GOTO1 MYREPORT                                                         
         B     EXIT                                                             
UJ154    MVC   P,SPACES                                                         
         B     EXIT                                                             
         EJECT                                                                  
UJ300    CLI   MODE,LEVBLAST       LAST FOR PRODUCT                             
         BNE   UJ350                                                            
         MVC   P+10(17),=C'TOTAL FOR PRODUCT'                                   
         LA    R3,2                                                             
         BAS   RE,FORMAT                                                        
         CLC   P+45(70),SPACES                                                  
         BE    UJ302                                                            
         CLI   QOPT7,C'Y'          OPTION FOR CLIENT LINES ONLY                 
         BE    UJ302                                                            
         GOTO1 MYREPORT                                                         
         GOTO1 MYREPORT                                                         
UJ302    MVC   P,SPACES                                                         
         B     EXIT                                                             
         EJECT                                                                  
UJ350    CLI   MODE,LEVALAST       LAST FOR CLIENT                              
         BNE   UJ400                                                            
         MVC   P+10(16),=C'TOTAL FOR CLIENT'                                    
         LA    R3,3                                                             
         BAS   RE,FORMAT                                                        
         CLC   P+45(70),SPACES                                                  
         BE    UJ356                                                            
         CLI   QOPT7,C'Y'                                                       
         BNE   UJ354                                                            
         MVC   WORK(50),SPACES                                                  
         L     RF,ADHEIRA                                                       
         MVC   WORK(6),3(RF)                                                    
         L     R2,ADLVANAM                                                      
         LA    R3,WORK+7                                                        
         BAS   RE,GETNAME                                                       
         GOTO1 =V(SQUASHER),DMCB,WORK,50,RR=PRELOC                              
         L     RF,DMCB+4                                                        
         GOTO1 CHOPPER,DMCB,((RF),WORK),(40,P+1),(C'P',2)                       
UJ354    GOTO1 MYREPORT                                                         
         GOTO1 MYREPORT                                                         
UJ356    MVC   P,SPACES                                                         
         B     EXIT                                                             
         EJECT                                                                  
UJ400    CLI   MODE,REQLAST                                                     
         BNE   EXIT                                                             
         CLI   QOPT7,C'Y'                                                       
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+10(17),=C'TOTAL FOR REQUEST'                                   
         LA    R3,4                                                             
         BAS   RE,FORMAT                                                        
         GOTO1 MYREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              FORMAT A LINE OF PROLLER TABLE                                   
         SPACE 1                                                                
FORMAT   NTR1                                                                   
         GOTO1 PROLLER,DMCB,1,TABLE,(R3)                                        
         L     R3,DMCB                                                          
         LA    R4,P+44                                                          
         LA    R5,6                                                             
FMT2     CP    0(6,R3),=P'0'                                                    
         BE    FMT4                                                             
         EDIT  (P6,0(R3)),(11,0(R4)),2,MINUS=YES                                
         ZAP   0(6,R3),=P'0'                                                    
FMT4     LA    R3,6(R3)                                                         
         LA    R4,11(R4)                                                        
         BCT   R5,FMT2                                                          
         B     EXIT                                                             
         SPACE 2                                                                
PRODPRT  NTR1                                                                   
         GOTO1 =V(ACCDIV),DMCB,ADLDGHIR,ADACC,ACDIVWK,RR=PRELOC                 
         LA    RF,ACDIVWK                                                       
         MVC   P+1(6),14(RF)       PRODUCT CODE                                 
         L     RF,ADLVBNAM                                                      
         ZIC   R2,1(RF)                                                         
         SH    R2,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R2),2(RF)),(30,P+8),(C'P',2)                      
         GOTO1 MYREPORT                                                         
         GOTO1 MYREPORT                                                         
         B     EXIT                                                             
         SPACE 2                                                                
JOBPRT   NTR1                                                                   
         GOTO1 =V(ACCDIV),DMCB,ADLDGHIR,ADACC,ACDIVWK,RR=PRELOC                 
         LA    RF,ACDIVWK                                                       
         MVC   P+1(7),27(RF)       JOB CODE                                     
         L     RF,ADACCNAM                                                      
         ZIC   R2,1(RF)                                                         
         SH    R2,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R2),2(RF)),(30,P+8),(C'P',2)                      
         GOTO1 MYREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*              FILL HEADLINES AND PRINT                                         
         SPACE 2                                                                
MYREPORT NTR1                                                                   
         CLI   MODE,REQLAST                                                     
         BE    MYRPT4                                                           
         CLI   QOPT7,C'Y'                                                       
         BNE   MYRPT2                                                           
         MVC   HEAD5+12(7),=C'SUMMARY'                                          
         MVC   HEAD8+1(6),=C'CLIENT'                                            
         MVC   HEAD9+1(6),=C'------'                                            
         B     MYRPT4                                                           
*                                                                               
MYRPT2   L     RF,ADHEIRA                                                       
         MVC   HEAD5+12(6),3(RF)                                                
         MVC   HEAD8+1(15),=C'PRODUCT AND JOB'                                  
         MVC   HEAD9+1(15),=C'---------------'                                  
         L     R2,ADLVANAM                                                      
         LA    R3,HEAD5+17                                                      
         BAS   RE,GETNAME                                                       
*                                                                               
MYRPT4   LA    R2,HEAD8+45                                                      
         LA    R3,HEADTAB                                                       
         LA    R4,4                                                             
MYRPT6   MVC   0(10,R2),0(R3)                                                   
         MVC   132(10,R2),10(R3)                                                
         LA    R2,11(R2)                                                        
         LA    R3,20(R3)                                                        
         BCT   R4,MYRPT6                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         SPACE 2                                                                
GETNAME  ZIC   RF,1(R2)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R3),2(R2)                                                    
         EJECT                                                                  
* CONVERT YYMMDD (R2) TO MOS (R4)                                               
*                                                                               
DTOMOS   NTR1                                                                   
         MVC   0(1,R4),1(R2)       YEAR                                         
         MVC   1(1,R4),3(R2)       MONTH                                        
         CLI   2(R2),C'1'                                                       
         BNE   EXIT                                                             
         MVI   1(R4),C'A'                                                       
         CLI   3(R2),C'0'                                                       
         BE    EXIT                                                             
         MVI   1(R4),C'B'                                                       
         CLI   3(R2),C'1'                                                       
         BE    EXIT                                                             
         MVI   1(R4),C'C'                                                       
         B     EXIT                                                             
         EJECT                                                                  
* CONVERTS DIFFERENT FORMAT DATES TO A 2 BYTE MONTH OF SERVICE FORMAT.          
* COMPARES TWO MONTH OF SERVICE DATES AND RETURNS A CODE INDICATING             
* WHICH IS GREATER.                                                             
*                                                                               
* TO CONVERT A DATE.                                                            
*                                                                               
* PARAMETER 1  BYTE  0        INPUT TYPE (SAME AS DATCON)                       
*              BYTES 1-3      A(INPUT DATE)                                     
*                                                                               
* PARAMETER 2  BYTES 1-3      A(OUTPUT DATE)                                    
*                                                                               
* PARAMETER 3  BYTE  1-3      A(DATCON)                                         
*                                                                               
*                                                                               
* TO COMPARE TWO MONTH OF SERVICE DATES.                                        
*                                                                               
* PARAMETER 1  BYTE  0        INPUT TYPE = 6                                    
*              BYTES 1-3      A(FIRST DATE)                                     
*                                                                               
* PARAMETER 2  BYTES 1-3      A(SECOND DATE)                                    
*                                                                               
* AFTER COMPARE PARAMETER 1  BYTE 0  X'00'   INVALID INPUT                      
*                                    X'04'   A IS LOWER THAN B                  
*                                    X'08'   A IS EQUAL B                       
*                                    X'0C'   A IS GREATER THAN B                
         SPACE 2                                                                
ACDATE   NTR1                                                                   
         CLI   0(R1),6                                                          
         BNE   DATCON2                                                          
         L     R2,0(R1)            A(FIRST DATE)                                
         L     R3,4(R1)            A(SECOND DATE)                               
         MVI   0(R1),0             SET INVALID                                  
         SPACE 1                                                                
         XR    R4,R4                                                            
         XR    R5,R5                                                            
         IC    R4,0(R2)            YEAR 1                                       
         IC    R5,0(R3)            YEAR 2                                       
         SH    R4,=H'240'                                                       
         SH    R5,=H'240'                                                       
         MH    R4,=H'10'                                                        
         LA    R4,YRTAB(R4)                                                     
         AR    R4,R5                                                            
         IC    R5,0(R4)            SAVE CODE FOR YEAR                           
         SPACE 1                                                                
         XR    R6,R6                                                            
         XR    R4,R4               VALIDATE MONTH                               
         IC    R4,1(R2)            MONTH 1                                      
         IC    R6,1(R3)            MONTH 2                                      
         CH    R4,=H'240'                                                       
         BH    *+8                                                              
         AH    R4,=H'57'           MAKE A C1 AN FA                              
         SPACE 1                                                                
         CH    R6,=H'240'                                                       
         BH    *+8                                                              
         AH    R6,=H'57'                                                        
         SPACE 1                                                                
         STC   R5,0(R1)       SET CODE FROM YEAR COMPARE                        
         CLI   0(R1),8        IF EQUAL                                          
         BE    *+8            MUST COMPARE MONTH                                
         B     ACDXIT         ELSE, OK TO RETURN                                
         SPACE 1                                                                
         CR    R4,R6                                                            
         BE    ACDXIT                                                           
         MVI   0(R1),LT                                                         
         BL    ACDXIT                                                           
         MVI   0(R1),GT                                                         
         B     ACDXIT                                                           
         SPACE 1                                                                
DATCON2  L     R2,0(R1)                                                         
         MVC   LORK+8(6),0(R2)     INPUT DATE TO WORK                           
         MVC   LORK(4),0(R1)       INPUT PARAMETER                              
         LA    R2,LORK+8           RETURN DATE HERE                             
         ST    R2,LORK+4                                                        
         L     R3,4(R1)            OUTPUT ADDRESS                               
         L     RF,8(R1)            DATCON                                       
         LA    R1,LORK                                                          
         CLI   LORK,0                                                           
         BE    *+6                                                              
         BASR  RE,RF                                                            
         SPACE 1                                                                
         MVC   0(1,R3),LORK+9      YEAR                                         
         MVC   1(1,R3),LORK+11     MONTH                                        
         CLC   LORK+10(2),=C'10'                                                
         BL    ACDXIT                                                           
         MVI   1(R3),C'A'                                                       
         CLC   LORK+10(2),=C'11'                                                
         BL    ACDXIT                                                           
         MVI   1(R3),C'B'                                                       
         CLC   LORK+10(2),=C'12'                                                
         BL    ACDXIT                                                           
         MVI   1(R3),C'C'                                                       
         SPACE 1                                                                
ACDXIT   XMOD1 1                                                                
         SPACE 2                                                                
FSTSW    DC    C'Y'                                                             
LORK     DS    4F                                                               
         SPACE 1                                                                
EQ       EQU   8                                                                
LT       EQU   4                                                                
GT       EQU   12                                                               
         SPACE 1                                                                
*              SECOND    YEAR                                                   
*                   0  1  2  3  4  5  6  7  8  9                                
YRTAB    DC    AL1(EQ,LT,LT,LT,LT,LT,GT,GT,GT,GT)          0   F                
         DC    AL1(GT,EQ,LT,LT,LT,LT,LT,GT,GT,GT)          1   I                
         DC    AL1(GT,GT,EQ,LT,LT,LT,LT,LT,GT,GT)          2   R                
         DC    AL1(GT,GT,GT,EQ,LT,LT,LT,LT,LT,GT)          3   S                
         DC    AL1(GT,GT,GT,GT,EQ,LT,LT,LT,LT,LT)          4   T                
         DC    AL1(LT,GT,GT,GT,GT,EQ,LT,LT,LT,LT)          5                    
         DC    AL1(LT,LT,GT,GT,GT,GT,EQ,LT,LT,LT)          6   Y                
         DC    AL1(LT,LT,LT,GT,GT,GT,GT,EQ,LT,LT)          7   E                
         DC    AL1(LT,LT,LT,LT,GT,GT,GT,GT,EQ,LT)          8   A                
         DC    AL1(LT,LT,LT,LT,LT,GT,GT,GT,GT,EQ)          9   R                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR PROGRAM                                                
         SPACE 2                                                                
ACP7D    DSECT                                                                  
INCR     DS    F                                                                
PRELOC   DS    F                                                                
ADBUFF   DS    F                                                                
END3     DS    CL3                                                              
END2     DS    XL2                                                              
ENDMOS   DS    CL3                                                              
BILLAMNT DS    PL6                                                              
ACDIVWK  DS    CL52                FOR ACCDIV                                   
JACTIV   DS    CL1                                                              
FILTER   DS    CL1                                                              
LISTSW   DS    CL1                                                              
DATAB    DS    8CL6                                                             
MOSTAB   DS    8CL4                                                             
DATAB3   DS    8CL3                                                             
HEADTAB  DS    CL80                                                             
TYPE     DS    CL1                                                              
PRODPEND DS    CL1                                                              
TABLE    DS    CL170               L=4,C=7                                      
         EJECT                                                                  
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
*ACMASTD                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
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
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACREPP702 05/01/02'                                      
         END                                                                    
