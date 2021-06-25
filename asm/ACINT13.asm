*          DATA SET ACINT13    AT LEVEL 012 AS OF 05/01/02                      
*PHASE T61913A,*                                                                
         TITLE 'T61913 - CHECK ESTIMATE'                                        
T61913   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T61913**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         ST    R2,RELO                                                          
         SPACE 1                                                                
         CLI   MODE,VALKEY                                                      
         BE    EST2                                                             
         CLI   MODE,VALREC                                                      
         BE    EST4                                                             
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALKEY LOGIC                                                       *          
**********************************************************************          
         SPACE 1                                                                
EST2     CLI   CALLER,X'15'        TEST CALLED BY LIST                          
         BNE   *+8                                                              
         BAS   RE,SETHED           SET PROTECTED HEADLINE FIELDS                
*                                                                               
         BAS   RE,VALHED                                                        
         MVI   INTMODE,EDTLIST     INITIALIZE INTERNAL MODE                     
         CLI   RACHANGE,C'Y'       TEST FOR RECORD/ACTION CHANGE                
         BNE   *+8                                                              
         MVI   INTMODE,DISLIST     YES-SET FIRST TIME LIST                      
         CLI   KEYCHG,C'Y'         TEST FOR CHANGE IN KEY FIELDS                
         BNE   *+8                                                              
         MVI   INTMODE,DISLIST     YES                                          
         CLI   RETURNED,X'14'      TEST RETURNED FROM EST ADJUST                
         BNE   *+8                 NO                                           
         MVI   INTMODE,DISLIST     YES-REDISPLAY ESTIMATE                       
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALREC LOGIC-DISPLAY OR CHANGE                                     *          
**********************************************************************          
         SPACE 1                                                                
EST4     BAS   RE,SETSCR                                                        
         BAS   RE,BLDEST                                                        
         CLI   INTMODE,DISLIST     TEST DISPLAYING                              
         BE    EST6                                                             
*                                                                               
         BAS   RE,PROCPF           PROCESS ANY PF KEYS                          
         CLI   PFKEY,PF10          TEST PF10=ERASE ALLOCATIONS                  
         BE    EST10               YES                                          
         BAS   RE,TSTEDT           TEST ANYTHING TO EDIT                        
         BE    EST20               YES                                          
*                                                                               
         MVI   INTMODE,DISLIST     CONTINUE LIST                                
*                                                                               
* DISPLAY LOGIC                                                                 
*                                                                               
EST6     GOTO1 VCLEARF,DMCB,AFSTMON,AENDSCR                                     
         GOTO1 (RF),(R1),(1,AFSTMON),APFFLD                                     
         BAS   RE,SCREEN                                                        
*                                                                               
         BAS   RE,DISEST                                                        
         BAS   RE,DISTOT                                                        
*                                                                               
         MVC   CONHEAD(L'DISMSG),DISMSG                                         
         BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
         B     EST14               ALL DONE                                     
*                                                                               
* LOGIC FOR PF10=ERASE ALLOCATIONS                                              
*                                                                               
EST10    BAS   RE,BLDEST           RE-BUILD ESTIMATE TABLE                      
         BAS   RE,DISEST           RE-DISPLAY ESTIMATE AND TOTALS               
         BAS   RE,DISTOT                                                        
         MVC   CONHEAD(L'ERASEMSG),ERASEMSG                                     
*                                                                               
EST14    L     R2,AFSTMON                                                       
         BAS   RE,BUMPTOUN         FIND FIRST UNP OPEN FIELD                    
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
*                                                                               
* EDIT LOGIC                                                                    
*                                                                               
EST20    BAS   RE,EDT                                                           
         BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
         BAS   RE,BLDEST           RE-BUILD THE ESTIMATE TABLE                  
         BAS   RE,DISEST           RE-DISPLAY THE SCREEN                        
         BAS   RE,DISTOT                                                        
         LA    R2,ESTPROH                                                       
         ST    R2,ACURFORC                                                      
         MVC   CONHEAD(L'EDTMSG),EDTMSG                                         
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO VALIDATE THE HEADLINE FIELDS                        *          
**********************************************************************          
         SPACE 1                                                                
VALHED   NTR1                                                                   
         MVI   KEYCHG,C'N'         INITIALIZE KEY FIELD CHANGE SWITCH           
         MVI   REVERSE,C'N'                                                     
         MVI   OPTION,0                                                         
         MVC   UNIT(2),PRODLEDG                                                 
         GOTO1 SETHEIR                                                          
*                                                                               
         LA    R2,ESTCLIH                                                       
         MVC   CLICODE,SPACES                                                   
         SR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   CLICODE(0),ESTCLI   SET CLIENT CODE                              
         MVC   TSARCLT,CLICODE     CLIENT CODE                                  
*                                                                               
VALHED2  LA    R2,ESTPROH          EDIT PRODUCT                                 
         BAS   RE,TSTKEY                                                        
         GOTO1 VALPROD                                                          
         MVC   TSARPROD,PRODCODE                                                
*                                                                               
VALHED4  LA    R2,ESTMEDH                                                       
         BAS   RE,TSTKEY                                                        
         GOTO1 VALMED                                                           
         MVC   TSARMED,MEDIA       SET MEDIA CODE                               
*                                                                               
VALHED6  LA    R2,ESTESTH                                                       
         BAS   RE,TSTKEY                                                        
         XC    ESTESTP,ESTESTP     CLEAR ESTIMATE PERIOD FIELD                  
         OI    ESTESTPH+6,X'80'    AND SEND IT BACK                             
         GOTO1 VALEST                                                           
         MVC   TSAREST,ESTIMATE                                                 
*                                                                               
VALHED8  L     R1,ATSARBLK         GET TSAR RECORD FOR ESTIMATE                 
         USING TSARD,R1                                                         
         MVI   TSACTN,TSARDH       READ BY KEY                                  
         GOTO1 TSAR                                                             
         BE    VALHED9             FOUND THE RECORD                             
         MVI   ERROR,NOTFOUND                                                   
         B     ERREND                                                           
         DROP  R1                                                               
*                                                                               
VALHED9  LA    R4,KEY              NOW GET THE ESTIMATE ITSELF                  
         USING ACINKEY,R4                                                       
         MVC   ACINKEY,SPACES                                                   
         MVI   ACINCOD,ACINEQU                                                  
         MVI   ACINSREC,ACINSEQU                                                
         MVC   ACINCUL(L'RECEIVE),RECEIVE                                       
         MVC   ACINCLT,CLICODE                                                  
         MVC   ACINPRD,PRODCODE                                                 
         MVC   ACINEST,ESTIMATE                                                 
         MVC   ACINMED,MEDIA                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         LA    R2,ESTESTPH                                                      
         MVC   LISTAR,SPACES                                                    
         MVI   ELCODE,ACIPFEQU                                                  
         BAS   RE,GETELIO                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACINPRFD,R6                                                      
         MVC   WORK(2),ACIPFPRS                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(18,LISTAR)                                 
         CLC   ACIPFPRS,ACIPFPRE   TEST ONE MONTH PERIOD                        
         BE    VALHED10            YES                                          
         MVI   LISTAR+6,C'-'                                                    
         MVC   WORK(2),ACIPFPRE                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(18,LISTAR+7)                               
         DROP  R6                                                               
*                                                                               
VALHED10 BAS   RE,MOVEFLD          MOVE OUT THE ESTIMATE PERIOD                 
*                                                                               
VALHED12 LA    R2,ESTOPTH                                                       
         BAS   RE,TSTKEY                                                        
         CLI   5(R2),0                                                          
         BE    VALHED15                                                         
*                                                                               
         MVI   ERROR,INVALID                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),=C'REVERSE'                                              
         BNE   ERREND                                                           
         MVI   REVERSE,C'Y'                                                     
*                                                                               
VALHED15 OI    ESTPROH+4,X'20'     SET ON PREV VALID BITS                       
         OI    ESTMEDH+4,X'20'                                                  
         OI    ESTESTH+4,X'20'                                                  
         OI    ESTOPTH+4,X'20'                                                  
*                                                                               
VALHEDX  B     XIT                                                              
         SPACE 2                                                                
TSTKEY   TM    4(R2),X'20'                                                      
         BOR   RE                                                               
         MVI   KEYCHG,C'Y'                                                      
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO SET HEADLINE FIELDS WHEN CALLED FROM                *          
* HEADER TO BEGIN A LIST                                             *          
**********************************************************************          
         SPACE 1                                                                
SETHED   NTR1  ,                                                                
*                                                                               
         MVC   ESTRCV,RECEIVE+3    DISPLAY RECEIVABLE A/C                       
         OI    ESTRCVH+6,X'80'                                                  
*                                                                               
         LA    R2,ESTCLIH                                                       
         OI    1(R2),X'20'         PROTECT CLIENT FIELD                         
*                                                                               
SETHED1  LA    R2,ESTPERH          DISPLAY ADVERTISING PERIOD                   
         MVC   LISTAR,SPACES                                                    
         MVC   FULL(2),ADVST                                                    
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(9,LISTAR)                                  
         CLC   ADVST,ADVEND        TEST SINGLE MONTH                            
         BE    SETHED2                                                          
*                                                                               
         MVI   LISTAR+6,C'-'                                                    
         MVC   FULL(2),ADVEND      DISPLAY PERIOD END                           
         GOTO1 DATCON,DMCB,(1,FULL),(9,LISTAR+7)                                
*                                                                               
SETHED2  BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO PROCESS PF KEYS                                     *          
**********************************************************************          
         SPACE 1                                                                
PROCPF   NTR1                                                                   
         CLI   PFKEY,PF3           TEST FOR PF3=ESTIMATE ADJUST                 
         BE    *+12                YES                                          
         CLI   PFKEY,PF10          TEST FOR PF10=ERASE                          
         BNE   PROCPFX                                                          
*                                                                               
PROCPF2  L     RE,ATWA                                                          
         AH    RE,MODLAST                                                       
         LA    R1,CONACTH                                                       
         CR    RE,R1               TEST FOR ANY FIELD MODIFIED                  
         BNH   PROCPF4             AFTER ACTION                                 
         MVI   PFKEY,0             YES-CLEAR PFKEY                              
         B     PROCPFX             AND EXIT RIGHT AWAY                          
*                                                                               
PROCPF4  CLI   PFKEY,PF10          TEST PF10=ERASE                              
         BE    PROCPF6             YES                                          
*                                                                               
         MVI   PFKEY,0                                                          
         L     R4,AIO                                                           
         USING ACKEYD,R4                                                        
         MVI   DUB,2               SET UP MEDIA PARAMETER                       
         MVC   DUB+1(2),ACINMED                                                 
         TM    ACSTATUS,X'02'      TEST MI USED                                 
         BZ    PROCPF5                                                          
*                                                                               
         MVI   DUB,5                                                            
         MVC   DUB+1(3),=C'MI='                                                 
         MVC   DUB+4(2),ACINMED                                                 
*                                                                               
PROCPF5  GOTO1 VCALL,WORK,=C'EST',=C'ADJUST',(L'ACINACC,ACINACC),(L'ACIX        
               NCLT,ACINCLT),(L'ACINPRD,ACINPRD),(DUB,DUB+1),(L'ACINESTX        
               ,ACINEST),0                                                      
*                                                                               
PROCPF6  ZIC   R3,ADVNUM           R3=LOOP COUNTER                              
         LA    R2,ADVTAB           R2=A(ADVERTISING MONTHS)                     
         LA    R6,TSARPAID                                                      
         ZAP   THISPAY,=P'0'                                                    
*                                                                               
PROCPF7  ZIC   R0,NMONTHS          R0=N'MONTHS IN ESTIMATE                      
         LA    R5,ESTTAB                                                        
         USING ESTTABD,R5                                                       
         CLC   ESTMTH,0(R2)        TEST IF MONTH IN W/IN ESTIMATE               
         BE    PROCPF8             YES-OK TO CLEAR IT                           
         LA    R5,ESTTABL(R5)                                                   
         BCT   R0,*-14                                                          
         B     PROCPF9             ADV MONTHS IS NOT W/IN ESTIMATE              
*                                                                               
PROCPF8  ZAP   DUB,0(L'TSARPAID,R6)                                             
         MP    DUB,=P'-1'          COMPLEMENT AMOUNT ALLOCATED                  
         AP    THISPAY,DUB                                                      
         ZAP   0(L'TSARPAID,R6),=P'0' CLEAR THE BUCKET                          
*                                                                               
PROCPF9  LA    R6,L'TSARPAID(R6)                                                
         LA    R2,L'ADVTAB(R2)     NEXT MONTH                                   
         BCT   R3,PROCPF7                                                       
*                                                                               
PROCPF10 L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAWRT       WRITE BACK THE TSAR RECORD                   
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    TOTPAID,THISPAY     UPDATE TOTAL PAID                            
         BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
         DROP  R1                                                               
*                                                                               
PROCPFX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO TEST FOR ANYTHING TO EDIT ON SCREEN                 *          
* ON EXIT, CC=EQ TO EDIT, CC=NEQ TO CONTINUE DISPLAY                 *          
**********************************************************************          
         SPACE 1                                                                
TSTEDT   NTR1  ,                                                                
         L     R2,AFSTMON          R2=A(FIRST SELECT FIELD)                     
         LA    R3,12                                                            
*                                                                               
TSTEDT2  LA    R4,NUMFLDS          R4=FIELD COUNTER                             
*                                                                               
TSTEDT3  TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    *+12                YES                                          
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BZ    TSTEDTY             YES                                          
*                                                                               
         BAS   RE,BUMP                                                          
         BCT   R4,TSTEDT3                                                       
*                                                                               
         BCT   R3,TSTEDT2          ANOTHER LINE                                 
*                                                                               
TSTEDTN  LTR   RB,RB               SET CC=NEQ                                   
         B     TSTEDTX                                                          
*                                                                               
TSTEDTY  CR    RB,RB               SET CC=EQ                                    
*                                                                               
TSTEDTX  B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD A TABLE OF THE ESTIMATE BY MONTH              *          
**********************************************************************          
         SPACE 1                                                                
BLDEST   NTR1  ,                                                                
         ZAP   GROSS,=P'0'                                                      
         ZAP   RECV,=P'0'                                                       
         ZAP   POSTED,=P'0'                                                     
         ZAP   PAID,=P'0'                                                       
         ZAP   OPEN,=P'0'                                                       
         ZAP   ALLOC,=P'0'                                                      
         LA    R5,ESTTAB                                                        
         USING ESTTABD,R5                                                       
         SR    R3,R3               R3=MONTH COUNTER                             
*                                                                               
         MVI   ELCODE,ACIESEQU                                                  
         BAS   RE,GETELIO                                                       
*                                                                               
         USING ACINESTD,R6                                                      
BLDEST2  BNE   BLDEST6                                                          
*                                                                               
         MVC   ESTMTH,ACIESMTH     MONTH                                        
         ZAP   ESTGROSS,ACIESGRS   GROSS                                        
         ZAP   ESTRECV,ACIESREC    RECEIVABLE                                   
         ZAP   ESTPOST,=P'0'                                                    
         MVC   ESTDAT,ACIESDAT                                                  
         TM    ACIESTAT,X'80'      TEST IF MONTH POSTED                         
         BZ    *+10                NO                                           
         AP    ESTPOST,ACIESREC    YES-POSTED=RECEIVABLE                        
         ZAP   ESTPAID,ACIESPD                                                  
         ZAP   ESTALLOC,=P'0'                                                   
         LA    R3,1(R3)            INCREMENT COUNT                              
         LA    R5,ESTTABL(R5)                                                   
*                                                                               
BLDEST4  BAS   RE,NEXTEL                                                        
         B     BLDEST2                                                          
*                                                                               
BLDEST6  STC   R3,NMONTHS                                                       
         LA    R5,ESTTAB           R5=A(START OF TABLE)                         
*                                                                               
BLDEST7  ZIC   R0,ADVNUM           R0=N'ADV PERIOD MONTHS                       
         LA    RE,ADVTAB           RE=A(ADV PERIOD MONTH TABLE)                 
         LA    R1,TSARPAID         R1=A(ALLOCATED BUCKETS)                      
*                                                                               
BLDEST8  CLC   ESTMTH,0(RE)        TEST IF MONTH IN ADV PERIOD                  
         BE    BLDEST9             YES                                          
         LA    R1,L'TSARPAID(R1)                                                
         LA    RE,L'ADVTAB(RE)                                                  
         BCT   R0,BLDEST8                                                       
         B     BLDEST10            NOT IN ADV PERIOD                            
*                                                                               
BLDEST9  AP    ESTPAID,0(L'TSARPAID,R1)                                         
         AP    ESTALLOC,0(L'TSARPAID,R1)                                        
*                                                                               
BLDEST10 ZAP   ESTOPEN,ESTRECV                                                  
         SP    ESTOPEN,ESTPAID     COMPUTE OPEN AMOUNT                          
         AP    OPEN,ESTOPEN        UPDATE TOTAL BUCKETS                         
         AP    GROSS,ESTGROSS                                                   
         AP    RECV,ESTRECV                                                     
         AP    POSTED,ESTPOST                                                   
         AP    PAID,ESTPAID                                                     
         AP    ALLOC,ESTALLOC                                                   
         LA    R5,ESTTABL(R5)                                                   
         BCT   R3,BLDEST7                                                       
*                                                                               
BLDESTX  B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO ADJUST THE SCREEN FOR MONTHS WITHIN PAY PERIOD      *          
**********************************************************************          
         SPACE 1                                                                
SCREEN   NTR1  ,                                                                
         L     R2,AFSTMON                                                       
         ST    R2,ATHISLIN                                                      
         ZIC   R3,NMONTHS          R3=LOOP COUNTER                              
         LA    R5,ESTTAB                                                        
         USING ESTTABD,R5                                                       
*                                                                               
SCREEN2  L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         L     R2,AOPEN                                                         
*                                                                               
         OI    1(R2),X'20'         PROTECT THE FIELD                            
         ZIC   R0,ADVNUM                                                        
         LA    R1,ADVTAB                                                        
         CLC   ESTMTH,0(R1)        TEST IF MONTH W/IN PAY PERIOD                
         BE    SCREEN4             YES                                          
         LA    R1,L'ADVTAB(R1)                                                  
         BCT   R0,*-14                                                          
         B     SCREEN6                                                          
*                                                                               
SCREEN4  NI    1(R2),X'FF'-X'20'                                                
         NI    4(R2),X'FF'-X'20'                                                
*                                                                               
SCREEN6  MVC   ATHISLIN,ANEXTMON                                                
         LA    R5,ESTTABL(R5)                                                   
         BCT   R3,SCREEN2                                                       
*                                                                               
SCREEN8  LA    R3,12               COMPUTE N'LINE ON REST OF SCREEN             
         ZIC   R0,NMONTHS                                                       
         SR    R3,R0                                                            
         BZ    SCREENX             ALL DONE                                     
*                                                                               
SCREEN9  L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         L     R2,AOPEN                                                         
         OI    1(R2),X'20'         PROTECT IT                                   
         MVC   ATHISLIN,ANEXTMON                                                
         BCT   R3,SCREEN9                                                       
*                                                                               
SCREENX  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO DISPLAY LIST LINE DATA FOR AN ESTIMATE              *          
* ROUTINE USES ESTTAB TO GENERATE SCREEN DISPLAY                     *          
**********************************************************************          
         SPACE 1                                                                
DISEST   NTR1  ,                                                                
         L     R2,AFSTMON                                                       
         ST    R2,ATHISLIN                                                      
         LA    R5,ESTTAB                                                        
         USING ESTTABD,R5                                                       
         ZIC   R6,NMONTHS                                                       
*                                                                               
DISEST2  L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         MVC   LISTAR,SPACES                                                    
         LA    R3,LISTAR                                                        
         USING MONTHD,R3                                                        
         L     R2,ADATA            DISPLAY MONTH DATA                           
         MVC   FULL(2),ESTMTH                                                   
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(18,MONMTH)                                 
         OC    ESTDAT,ESTDAT       TEST IF POSTING DATE                         
         BZ    DISEST3             NO                                           
         GOTO1 DATCON,DMCB,(2,ESTDAT),(17,MONDAT)                               
*                                                                               
DISEST3  CURED ESTGROSS,(L'MONGROSS,MONGROSS),2,ZERO=NOBLANK,FLOAT=-            
         CURED ESTRECV,(L'MONRECV,MONRECV),2,ZERO=NOBLANK,FLOAT=-               
         CURED ESTPOST,(L'MONPOST,MONPOST),2,ZERO=NOBLANK,FLOAT=-               
         CURED ESTPAID,(L'MONPAID,MONPAID),2,ZERO=NOBLANK,FLOAT=-               
         BAS   RE,MOVEFLD                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
DISEST4  L     R2,AOPEN                                                         
         OI    6(R2),X'80'         XMIT BACK                                    
         MVC   LISTAR,SPACES                                                    
         MVI   LISTAR,C'*'                                                      
         CURED ESTOPEN,(10,LISTAR+1),2,ZERO=NOBLANK,ALIGN=LEFT,FLOAT=-          
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    DISEST8             YES-ALL DONE                                 
*                                                                               
         OI    4(R2),X'20'         NOTE OPEN AS PREV VALID                      
         CLI   REVERSE,C'Y'        TEST REVERSE OPTION                          
         BNE   DISEST8                                                          
*                                                                               
DISEST6  CLI   INTMODE,EDTLIST     TEST RE-DISPLAYING ITEM                      
         BE    *+8                 YES                                          
         NI    4(R2),X'FF'-X'20'   TURN OFF PREV VALID                          
         CP    ESTALLOC,=P'0'      TEST IF ANYTHING PAID                        
         BNE   *+14                YES-DEFAULT IS NO DOUBLE PAY                 
         CP    ESTOPEN,=P'0'       TEST FOR AN OPEN AMOUNT                      
         BNE   DISEST8             YES-WE CAN PAY IT AGAIN                      
         MVI   LISTAR,C'N'                                                      
         OI    4(R2),X'20'         SET VALID ON                                 
*                                                                               
DISEST8  BAS   RE,MOVEFLD                                                       
*                                                                               
DISEST10 MVC   ATHISLIN,ANEXTMON   RESET LINE POINTER                           
         LA    R5,ESTTABL(R5)                                                   
         BCT   R6,DISEST2                                                       
*                                                                               
DISESTX  B     XIT                                                              
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO DISPLAY TOTALS LINE                                 *          
**********************************************************************          
         SPACE 1                                                                
DISTOT   ST    RE,SAVERE                                                        
         MVC   ESTTOT,SPACES                                                    
         OI    ESTTOTH+6,X'80'                                                  
         LA    R4,ESTTOT                                                        
         USING MONTHD,R4                                                        
         MVC   MONTOT,=C'Totals'                                                
         CURED GROSS,(L'MONGROSS,MONGROSS),2,ZERO=NOBLANK,FLOAT=-               
         CURED RECV,(L'MONRECV,MONRECV),2,ZERO=NOBLANK,FLOAT=-                  
         CURED POSTED,(L'MONPOST,MONPOST),2,ZERO=NOBLANK,FLOAT=-                
         CURED PAID,(L'MONPAID,MONPAID),2,ZERO=NOBLANK,FLOAT=-                  
         CURED OPEN,(L'MONOPEN,MONOPEN),2,ZERO=NOBLANK,ALIGN=LEFT,FLOATX        
               =-                                                               
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO MOVE OUT DISPLAY DATA                               *          
**********************************************************************          
         SPACE 1                                                                
MOVEFLD  ST    RE,SAVERE                                                        
         ZIC   R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),LISTAR                                                   
*                                                                               
MOVEFLDX L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO EDIT THE SCREEN AND UPDATE THE TSAR RECORD          *          
**********************************************************************          
         SPACE 1                                                                
EDT      NTR1  ,                                                                
         L     R2,AFSTMON                                                       
         ST    R2,ATHISLIN         SET LINE POINTER TO FIRST ONE                
         ZIC   R3,NMONTHS          R3=N'MONTHS IN EST=COUNTER                   
         LA    R5,ESTTAB                                                        
         USING ESTTABD,R5                                                       
         ZAP   THISPAY,=P'0'       INITIALIZE PAID THIS TIME BUCKET             
*                                                                               
EDT2     L     R2,ATHISLIN                                                      
         BAS   RE,SETLIN                                                        
         L     R2,AOPEN                                                         
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         BO    EDT10               YES-SKIP EDIT                                
*                                                                               
         TM    4(R2),X'20'         TEST IF FIELD CHANGED                        
         BO    EDT10               NO                                           
         MVI   BYTE,C'*'           SET SKIP EDIT CHARACTER                      
         CLI   REVERSE,C'Y'                                                     
         BNE   *+8                                                              
         MVI   BYTE,C'N'           FOR REVERSE, 'N'=NO PAYMENT                  
         CLC   BYTE,8(R2)                                                       
         BE    EDT10               SKIP EDIT                                    
*                                                                               
EDT4     GOTO1 ANY                                                              
         CLI   8(R2),C'U'          TEST 'U'=UNMARK                              
         BNE   EDT6                                                             
*                                                                               
         LA    R6,TSARPAID         R6=A(ADV MONTH BUCKET)                       
         ZIC   R0,ADVNUM           R0=LOOP COUNTER                              
         LA    RE,ADVTAB           RE=A(ADV MONTH TABLE)                        
*                                                                               
EDT5     CLC   ESTMTH,0(RE)        FIND ESTIMATE MONTHS' POSITION               
         BE    EDT5A               IN THE ADV TABLE AND TSAR RECORD             
         LA    R6,L'TSARPAID(R6)                                                
         LA    RE,L'ADVTAB(RE)                                                  
         BCT   R0,EDT5                                                          
         DC    H'0'                                                             
*                                                                               
EDT5A    ZAP   DUB,0(L'TSARPAID,R6) GET PAID AMOUNT                             
         MP    DUB,=P'-1'          COMPLEMENT IT                                
         AP    THISPAY,DUB                                                      
         ZAP   0(L'TSARPAID,R6),=P'0' SET PAID AMOUNT TO ZERO                   
         B     EDT10               DONE WITH FIELD                              
*                                                                               
EDT6     MVI   ERROR,INVALID                                                    
         ZIC   R0,5(R2)                                                         
         CLI   8(R2),C'Y'          TEST FIELD STARTS WITH 'Y'                   
         BE    EDT7                YES                                          
         CLI   REVERSE,C'Y'        TEST FOR REVERSE OPTION                      
         BNE   EDT8                NO                                           
         CLI   8(R2),C'*'          TEST FIELD STARTS WITH ASTER                 
         BNE   EDT8                NO                                           
*                                                                               
EDT7     SH    R0,=H'1'            STRIP OUT FIRST CHARACTER                    
         BZ    ERREND                                                           
         MVC   WORK(L'WORK-1),WORK+1                                            
*                                                                               
EDT8     GOTO1 CASHVAL,DMCB,(X'82',WORK),(R0)                                   
         CLI   0(R1),0                                                          
         BNE   ERREND                                                           
         ZAP   DUB,4(8,R1)                                                      
         CP    DUB,=P'0'           TEST IF ZERO WAS INPUT                       
         BE    ERREND                                                           
*                                                                               
         LA    R6,TSARPAID         R6=A(ADV MONTH BUCKET)                       
         ZIC   R0,ADVNUM           R0=LOOP COUNTER                              
         LA    RE,ADVTAB           RE=A(ADV MONTH TABLE)                        
*                                                                               
EDT9     CLC   ESTMTH,0(RE)        FIND ESTIMATE MONTHS' POSITION               
         BE    EDT9A               IN THE ADV TABLE AND TSAR RECORD             
         LA    R6,L'TSARPAID(R6)                                                
         LA    RE,L'ADVTAB(RE)                                                  
         BCT   R0,EDT9                                                          
         DC    H'0'                                                             
*                                                                               
EDT9A    AP    THISPAY,DUB                                                      
         AP    0(L'TSARPAID,R6),DUB ADD IN PAID AMOUNT                          
*                                                                               
EDT10    MVC   ATHISLIN,ANEXTMON                                                
         LA    R5,ESTTABL(R5)                                                   
         LA    R6,L'TSARPAID(R6)                                                
         BCT   R3,EDT2                                                          
*                                                                               
EDT20    L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSAWRT       WRITE BACK TSAR RECORD                       
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         AP    TOTPAID,THISPAY     UPDATE TOTAL PAID BUCKET                     
*                                                                               
EDTX     B     XIT                                                              
         DROP  R1,R5                                                            
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINES TO SET SCREEN ADDRESSES                               *          
**********************************************************************          
         SPACE 1                                                                
SETSCR   ST    RE,SAVERE                                                        
         LA    R2,ESTMON1H                                                      
         ST    R2,AFSTMON                                                       
         LA    R2,ESTTOTH                                                       
         ST    R2,ATOTAL                                                        
         LA    R2,ESTPFH                                                        
         ST    R2,APFFLD                                                        
         BAS   RE,BUMP                                                          
         CLI   0(R2),0             TEST FOR EOS                                 
         BNE   *-8                                                              
         ST    R2,AENDSCR          NOTE END OF SCREEN                           
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* AT ENTRY, R2=A(SELECT FIELD HEADER)                                *          
**********************************************************************          
         SPACE 1                                                                
SETLIN   ST    RE,SAVERE                                                        
         LA    R0,NUMFLDS                                                       
         LA    R1,ADATA                                                         
         ST    R2,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BAS   RE,BUMP                                                          
         BCT   R0,*-12                                                          
         ST    R2,ANEXTMON                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SUPPORTING SUBROUTINES                                             *          
**********************************************************************          
         SPACE 1                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
BUMPTOUN ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED                     
         AR    R2,RF                                                            
         CLI   0(R2),0                                                          
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         BO    BUMPTOUN                                                         
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
         EJECT                                                                  
**********************************************************************          
* SAVE TSAR RECORD                                                   *          
**********************************************************************          
         SPACE 1                                                                
SAVTSAR  ST    RE,SAVERE                                                        
         L     R1,ATSARBLK                                                      
         USING TSARD,R1                                                         
         MVI   TSACTN,TSASAV                                                    
         GOTO1 TSAR                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RE,SAVERE                                                        
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* EXIT WITH ERRORS                                                   *          
**********************************************************************          
         SPACE 1                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
*                                                                               
ERREND   BAS   RE,SAVTSAR          SAVE THE TSAR BLOCK                          
         GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
EDTMSG   DC    C'Changes completed'                                             
DISMSG   DC    C'Monthly data for estimate displayed - enter any changex        
               s'                                                               
ERASEMSG DC    C'Allocations for advertising period erased'                     
         SPACE 2                                                                
         EJECT                                                                  
**********************************************************************          
* PATCH AREA                                                         *          
**********************************************************************          
         SPACE 1                                                                
PATCH    DS    0H                                                               
         DC    XL32'00'                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* ++INCLUDES  -  DSECTS ARE HIDDEN IN HERE                           *          
**********************************************************************          
         SPACE 1                                                                
*ACINTWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACINTWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACINT12COM                                                     
         ORG   OVERWRK                                                          
INTMODE  DS    X                   INTERNAL MODE                                
KEYCHG   DS    C                                                                
REVERSE  DS    CL1                 Y=REVERSE PAYMENT OPTION                     
*                                                                               
GROSS    DS    PL6                                                              
RECV     DS    PL6                                                              
POSTED   DS    PL6                                                              
PAID     DS    PL6                                                              
ALLOC    DS    PL6                                                              
OPEN     DS    PL6                                                              
THISPAY  DS    PL6                                                              
*                                                                               
ACURSOR  DS    A                                                                
*                                                                               
AFSTMON  DS    A                                                                
ATOTAL   DS    A                                                                
APFFLD   DS    A                                                                
AENDSCR  DS    A                                                                
*                                                                               
ATHISLIN DS    A                                                                
ADATA    DS    A                                                                
AOPEN    DS    A                                                                
ANEXTMON DS    A                                                                
*                                                                               
NMONTHS  DS    X                                                                
ESTTAB   DS    12CL(ESTTABL)       ESTIMATE TABLE AND OPEN AMOUNTS              
*                                                                               
         DS    CL(L'OVERWRK-(*-OVERWRK)) SPARE                                  
         EJECT                                                                  
T619FFD  DSECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE ACINTF3D                                                       
         SPACE 2                                                                
LSAVES   DS    0D                                                               
         DS    CL((SAVAREA-LSAVES)-(*-LSAVES))  SPARE                           
         SPACE 2                                                                
         EJECT                                                                  
**********************************************************************          
* EQUATES                                                            *          
**********************************************************************          
         SPACE 1                                                                
NUMFLDS  EQU   2                   N'FIELDS ON LIST SCREEN LINE                 
DISLIST  EQU   2                                                                
EDTLIST  EQU   3                                                                
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER MONTH LINE PROTECTED DATA FIELD                     *          
**********************************************************************          
         SPACE 1                                                                
MONTHD   DSECT                                                                  
MONMTH   DS    CL6                 MONTH (MMM/YY)                               
         DS    CL2                                                              
MONDAT   DS    CL8                 POSTING DATE (MMMDD/YY)                      
         DS    CL2                                                              
MONGROSS DS    CL11                                                             
         DS    CL1                                                              
MONRECV  DS    CL11                                                             
         DS    CL1                                                              
MONPOST  DS    CL11                                                             
         DS    CL1                                                              
MONPAID  DS    CL11                                                             
         DS    CL1                                                              
MONOPEN  DS    CL11                                                             
         ORG   MONMTH                                                           
MONTOT   DS    CL6                 TOTALS LABEL                                 
         ORG                                                                    
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER ESTIMATE MONTH TABLE                                *          
**********************************************************************          
         SPACE 1                                                                
ESTTABD  DSECT                                                                  
ESTMTH   DS    PL2                 MONTH                                        
ESTDAT   DS    XL2                 POSTING DATE                                 
ESTGROSS DS    PL6                                                              
ESTRECV  DS    PL6                                                              
ESTPOST  DS    PL6                                                              
ESTPAID  DS    PL6                                                              
ESTALLOC DS    PL6                                                              
ESTOPEN  DS    PL6                                                              
ESTTABL  EQU   *-ESTTABD                                                        
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACINT13   05/01/02'                                      
         END                                                                    
