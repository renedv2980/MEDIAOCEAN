*          DATA SET ACREPC302  AT LEVEL 030 AS OF 12/11/09                      
*PHASE ACC302A,+0                                                               
*INCLUDE SQUASHER                                                               
         TITLE 'ACC3 - TIME SHEETS'                                             
ACC302   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACC3**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACC3D,RC                                                         
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ACC302+4096,R9                                                   
         USING PROFD,R8                                                         
         LA    R8,PROGPROF                                                      
         EJECT                                                                  
*                                                                               
TS10     CLI   MODE,RUNFRST                                                     
         BNE   TS20                                                             
         RELOC RELO                                                             
         LA    RE,RELOTAB                                                       
         LA    R1,ATYPES           RELOCATE A-TYPES                             
TS11     L     RF,0(RE)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   TS11                                                             
*                                                                               
         L     RF,=A(SAVERC)                                                    
         A     RF,RELO                                                          
         ST    RC,0(RF)                                                         
         MVC   HEADHOOK,AHOOK      A(HOOK)                                      
         L     R4,VEXTRAS                                                       
         USING RUNXTRAD,R4                                                      
         L     R4,ADMASTD                                                       
         USING MASTD,R4                                                         
         MVC   ABOXC,MCBXAREA      A(BOXC)                                      
*                                                                               
         SR    R0,R0               GET MAIN STORAGE                             
         LA    R4,MAINTAB                                                       
*                                                                               
RNF03    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    RNF05                                                            
         A     R0,0(R4)            ADD THE LENGTH OF EACH TABLE                 
         LA    R4,L'MAINTAB(R4)                                                 
         B     RNF03                                                            
*                                                                               
RNF05    ST    R0,MAINLEN          SAVE LENGTH OF TABLE                         
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,MAINBGN          START OF AREA                                
         LA    R4,MAINTAB                                                       
*                                                                               
RNF07    L     R3,4(R4)                                                         
         ST    R1,0(R3)            A(START OF THIS TABLE)                       
         L     R0,0(R4)            LENGTH OF THIS TABLE                         
         AR    R1,R0               R1 TO NEXT AREA                              
         LA    R4,L'MAINTAB(R4)                                                 
         DROP  R4                                                               
         CLI   0(R4),X'FF'                                                      
         BNE   RNF07                                                            
         L     RE,ADMASTC          PRINT BUFFER IN DUMP                         
         USING MASTD,RE                                                         
         STCM  R1,15,MCUSRDMP+4    R1 POINTS TO END OF TABLES                   
         L     R1,MAINBGN          R1 POINTS TO BEGINNING OF TABLES             
         STCM  R1,15,MCUSRDMP      R1 POINTS TO END OF TABLES                   
         DROP  RE                                                               
*                                                                               
         BAS   RE,RDSJ             BUILD TABLE OF SJ ACCOUNTS                   
*                                                                               
         B     TSXIT                                                            
         EJECT                                                                  
*                                                                               
TS20     CLI   MODE,REQFRST                                                     
         BNE   TS30                                                             
         BAS   RE,BLDCAL           GO BUILD DAY/DATES CORP LEVEL                
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         MVC   OLDATE,=3X'FF'                                                   
         ZIC   R3,PROFDAYS                                                      
         LTR   R3,R3                                                            
         BZ    TS25                                                             
         LNR   R3,R3                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         GOTO1 DATCON,DMCB,WORK+6,(1,OLDATE)                                    
*                                                                               
TS25     MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         LH    R3,=H'14'           DEFAULT IS 14 LINES PER PAGE                 
         CLI   PROFROWS,0          PROFILE CAN OVERRIDE                         
         BE    *+8                                                              
         IC    R3,PROFROWS                                                      
         STH   R3,MAXROW                                                        
*                                                                               
         CLI   PROFOFNM,C'Y'                                                    
         BNE   TS28                                                             
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         MVC   COMPNAM(L'ORIGINAM),ORIGINAM                                     
         B     TSXIT                                                            
*                                                                               
TS28     L     R4,ADCMPNAM                                                      
         LA    R5,COMPNAM                                                       
         BAS   RE,NAMOUT                                                        
         B     TSXIT                                                            
         EJECT                                                                  
*                                                                               
TS30     CLI   MODE,LEDGFRST                                                    
         BNE   TS32                                                             
         MVC   SALHKEY,SPACES      KEY FOR SAL HST LOOKUP                       
         USING PHIRECD,RF                                                       
         LA    RF,SALHKEY                                                       
         MVC   PHIKEY,SPACES                                                    
         MVI   PHIKTYP,PHIKTYPQ    X'3E'                                        
         MVI   PHIKSUB,PHIKSUBQ    X'05'                                        
         MVC   PHIKCPY,RCCOMPFL                                                 
         XC    PHIKMOA,PHIKMOA                                                  
         XC    PHIKSEQ,PHIKSEQ                                                  
         DROP  RF                                                               
         CLI   PROFOFNM,C'Y'                                                    
         BNE   TS30A                                                            
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         MVC   COMPNAM(L'ORIGINAM),ORIGINAM                                     
*                                                                               
TS30A    L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         MVC   LEVA,SPACES                                                      
         MVC   LEVB,SPACES                                                      
         MVC   LEVC,SPACES                                                      
         MVC   LEVD,SPACES                                                      
         MVC   LEVA(15),ACHRDESA                                                
         MVC   LEVB(15),ACHRDESB                                                
         MVC   LEVC(15),ACHRDESC                                                
         MVC   LEVD(15),ACHRDESD                                                
         MVC   LEVALN,ACHRLEVA                                                  
         MVC   LEVBLN,ACHRLEVB                                                  
         MVC   LEVCLN,ACHRLEVC                                                  
         MVC   LEVDLN,ACHRLEVD                                                  
         B     TSXIT                                                            
         EJECT                                                                  
*                                                                               
TS32     CLI   MODE,PROCLEVA                                                    
         BNE   TS34                                                             
         CLI   PROFOFNM,C'Y'                                                    
         BNE   TS32A                                                            
         L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         MVC   COMPNAM(L'ORIGINAM),ORIGINAM                                     
*                                                                               
TS32A    L     R2,ADHEIRA                                                       
         MVC   LEVA+13(6),3(R2)                                                 
         USING PHIRECD,RF                                                       
         LA    RF,SALHKEY                                                       
         MVC   PHIKOFC,3(R2)                                                    
         MVC   WKOFFICE,PHIKOFC                                                 
         BAS   RE,BLDCAL           GO BUILD DAY/DATES OFFICE LEVEL              
         DROP  RF                                                               
         L     R4,ADLVANAM                                                      
         LA    R5,LEVA+20                                                       
         BAS   RE,NAMOUT                                                        
         B     TSXIT                                                            
*                                                                               
TS34     CLI   MODE,PROCLEVB                                                    
         BNE   TS36                                                             
         L     R2,ADHEIRB                                                       
         ZIC   R5,LEVALN                                                        
         LA    R5,3(R5,R2)                                                      
         MVC   LEVB+13(6),0(R5)                                                 
         USING PHIRECD,RF                                                       
         LA    RF,SALHKEY                                                       
         MVC   PHIKDPT,0(R5)                                                    
         DROP  RF                                                               
         L     R4,ADLVBNAM                                                      
         LA    R5,LEVB+20                                                       
         BAS   RE,NAMOUT                                                        
         B     TSXIT                                                            
*                                                                               
TS36     CLI   MODE,PROCLEVC                                                    
         BNE   TS38                                                             
         L     R2,ADHEIRC                                                       
         ZIC   R5,LEVBLN                                                        
         LA    R5,3(R5,R2)                                                      
         MVC   LEVC+13(6),0(R5)                                                 
         USING PHIRECD,RF                                                       
         LA    RF,SALHKEY                                                       
         MVC   PHIKSBD,0(R5)                                                    
         DROP  RF                                                               
         L     R4,ADLVCNAM                                                      
         LA    R5,LEVC+20                                                       
         BAS   RE,NAMOUT                                                        
         B     TSXIT                                                            
*                                                                               
TS38     CLI   MODE,PROCLEVD                                                    
         BNE   TS39                                                             
         L     R2,ADHEIRD                                                       
         ZIC   R5,LEVCLN                                                        
         LA    R5,3(R5,R2)                                                      
         MVC   LEVD+13(6),0(R5)                                                 
         USING PHIRECD,RF                                                       
         LA    RF,SALHKEY                                                       
         MVC   PHIKPER,0(R5)                                                    
         DROP  RF                                                               
         L     R4,ADLVDNAM                                                      
         LA    R5,LEVD+20                                                       
         BAS   RE,NAMOUT                                                        
         B     TSXIT                                                            
*                                                                               
         USING ACNAMED,R4                                                       
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R5),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),ACNMNAME                                                 
         EJECT                                                                  
TS39     CLI   MODE,PROCACC                                                     
         BNE   TS40                                                             
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDTIME,C'N'                                                    
         L     R4,ADACC                                                         
         CLI   PROFSAL,C'Y'                                                     
         BE    TS39A                                                            
         MVI   ELCODE,X'52'                                                     
         BAS   RE,GETEL                                                         
         BE    TS39A               IGNORE IF NO RATE ELEMENTS(OLD)              
*                                                                               
         USING PHIRECD,R4                                                       
         L     R4,ACREC                                                         
         MVC   PHIKEY,SPACES       IGNORE IF NO SALHST RECS(NEW)                
         MVC   PHIKEY,SALHKEY                                                   
         BAS   RE,HIGH                                                          
         CLC   PHIKEY(PHIKMOA-PHIKEY),SALHKEY                                   
         BNE   TSXIT                                                            
         DROP  R4                                                               
*                                                                               
TS39A    L     R4,ADACC                                                         
         CLI   3(R4),C'9'          OR IF AN OVERHEAD ACCOUNT                    
         BE    TSXIT                                                            
         CLC   4(2,R4),=C'999'                                                  
         BE    TSXIT                                                            
         CLC   8(3,R4),=C'999'                                                  
         BE    TSXIT                                                            
*                                                                               
         L     R4,ADACC                                                         
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   TSXIT                                                            
         USING ACSTATD,R4                                                       
         TM    ACSTSTAT,X'20'                                                   
         BO    TSXIT               SKIP LOCKED                                  
         MVI   FCRDTRNS,C'Y'                                                    
         MVI   FCRDTIME,C'Y'                                                    
         B     TSXIT                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
TS40     CLI   MODE,PROCTRNS                                                    
         BNE   TSPRTM                                                           
*                                                                               
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'        TRANSACTIONS HOLD THE                        
         BNE   TSXIT                                                            
         CLC   TRNSDATE,OLDATE                                                  
         BL    TSXIT               IGNORE OLD ONES                              
         MVI   ELCODE,X'51'        PROJECT CONTROL ELEMENT                      
         BAS   RE,NEXTEL                                                        
         BNE   TSXIT                                                            
*                                                                               
         USING ACPCD,R4                                                         
         USING TSCDE,R5                                                         
         LA    R5,WORK                                                          
         MVC   WORK,SPACES                                                      
         CLC   ACPCCLI+1(2),=C'SJ'                                              
         BNE   TSXIT               SKIP NON CLIENT                              
         MVC   TSKEY,ACPCCLI       CLIENT PRODUCT                               
         MVC   TSKEY(1),RCCOMPFL                                                
         CLI   ACPCLEN,X'22'                                                    
         BNL   TS41                PROJECT/TASK ON THIS ACCOUNT                 
*                                  NO PROJECT/TASK ON THIS ACCOUNT              
         CLI   PROFPRD,C'Y'        DOES PROD OVERRIDE CLIENT?                   
         BE    TS42                YES-PUT TO TABLE AS IS                       
         MVC   TSKEY+6(L'TSKEY-6),SPACES NO - CLEAR OUT PRODUCT                 
         B     TS42                NO PROJECT/TASK ON THIS ACCOUNT              
TS41     MVC   TSPJT,ACPCPRJT+9                                                 
         MVC   TSKEY+9(6),ACPCPRJT+9                                            
         MVC   TSTSK,ACPCTSK                                                    
*                                                                               
TS42     GOTO1 BINADD,DMCB,(R5),ATSCODE                                         
         B     TSXIT                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
TSPRTM   CLI   MODE,PROCTIME                                                    
         BNE   TS50                                                             
*                                                                               
         L     R4,ADTRANS                                                       
         USING TIMELD,R4                                                        
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   TSXIT                                                            
*                                                                               
         CLI   TIMETYP,TIMEINP     MAKE SURE IT'S OF TYPE INPUT DETAIL          
         BNE   TSXIT                                                            
*                                                                               
         L     R7,AMONACC                                                       
         USING ACMD,R7                                                          
         L     R7,ACMALTN          GET ADDRESS OF TIME RECORD                   
         USING TIMRECD,R7                                                       
         CLC   TIMKPEDT,OLDATE                                                  
         BL    TSXIT               IGNORE OLD ONES                              
*                                                                               
         USING TSCDE,R5                                                         
         LA    R5,WORK                                                          
         MVC   WORK,SPACES                                                      
         CLC   TIMACC(2),=C'SJ'                                                 
         BNE   TSXIT               SKIP NON CLIENT                              
         MVC   TSKEY+1(L'TIMACC),TIMACC   CLIENT PRODUCT                        
         MVC   TSKEY(1),RCCOMPFL                                                
*                                                                               
         MVC   TSPJT,TIMACC+8                                                   
         MVC   TSKEY+9(6),TIMACC+8                                              
         MVC   TSTSK,TIMTSK                                                     
*                                                                               
TSPTM02  GOTO1 BINADD,DMCB,(R5),ATSCODE                                         
         B     TSXIT                                                            
         DROP  R7                                                               
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
TS50     CLI   MODE,ACCLAST                                                     
         BNE   TS70                                                             
         XC    PRTLINE,PRTLINE                                                  
         L     R5,ANONCLI          ADD NON-CLIENT TO TABLE                      
         ZIC   R3,NUMNON                                                        
         LTR   R3,R3                                                            
         BZ    TS54                                                             
         MVC   WORK,SPACES                                                      
TS52     MVC   WORK(15),0(R5)                                                   
         GOTO1 BINADD,DMCB,WORK,ATSCODE                                         
         LA    R5,15(R5)                                                        
         BCT   R3,TS52                                                          
*                                                                               
TS54     BAS   RE,PGEPRNT          PRINT THE PAGE                               
         B     TSXIT                                                            
         EJECT                                                                  
TS70     CLI   MODE,REQLAST                                                     
         BNE   RL10                                                             
         CLI   PROFLSTP,C'Y'                                                    
         BNE   TS78                NO BLANK PAGE AT END                         
         XC    PRTLINE,PRTLINE                                                  
         L     R5,ANONCLI          ADD NON-CLIENT TO TABLE                      
         ZIC   R3,NUMNON                                                        
         LTR   R3,R3                                                            
         BZ    TS74                                                             
         MVC   WORK,SPACES                                                      
TS72     MVC   WORK(15),0(R5)                                                   
         GOTO1 BINADD,DMCB,WORK,ATSCODE                                         
         LA    R5,15(R5)                                                        
         BCT   R3,TS72                                                          
*                                                                               
TS74     BAS   RE,PGEPRNT          PRINT THE PAGE                               
TS78     L     R4,ABOXC                                                         
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'N'                                                     
         B     TSXIT                                                            
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -           
RL10     CLI   MODE,RUNLAST                                                     
         BNE   TSXIT                                                            
         LM    R0,R1,MAINLEN                                                    
         FREEMAIN R,LV=(0),A=(1)                                                
         B     TSXIT                                                            
*                                                                               
*              PRINT A PAGE                                                     
PGEPRNT  NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         CLI   PROFPAGE,C'Y'       CONTINUOUS PAGE NUMBERING                    
         BE    *+10                YES - DON'T RESET PAGE NUMBER                
         MVC   PAGE,=H'1'          NO  - RESET PAGE FOR EACH EMPLOYEE           
         SR    R2,R2                                                            
         L     R5,ATSCODE                                                       
         USING BIND,R5                                                          
         L     R3,BININ            NUMBER IN TABLE                              
         XC    BININ,BININ                                                      
         LTR   R3,R3                                                            
         BZ    TS66                NOTHING IN TABLE                             
         L     R5,BINTABLE                                                      
*                                                                               
         USING TSCDE,R5                                                         
TS56     CLC   TSKEY+1(2),=C'1M'   SPECIAL BLANKS AFTER CLIENT                  
         BE    TS58                                                             
         MVC   P+27(7),TSKEY+3                                                  
         CLC   TSKEY+1(2),=C'SJ'                                                
         BNE   TS58                                                             
         MVC   P+27(7),SPACES                                                   
         MVC   P+27(3),TSKEY+3          CLIENT CODE                             
         LA    R1,P+29                                                          
         CLI   0(R1),C' '                                                       
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
         MVI   0(R1),C','                                                       
         MVC   1(3,R1),TSKEY+6          PRODUCT CODE                            
         CLI   1(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         MVC   P+35(6),TSPJT            PROJECT CODE                            
         LA    R1,P+35                                                          
         CLI   0(R1),C' '                                                       
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-12                                                             
         MVI   0(R1),C','                                                       
         MVC   1(2,R1),TSTSK           TASK CODE                                
         CLI   1(R1),C' '                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
*                                                                               
TS58     LR    R6,R5               GET NAME FROM CODELIST                       
         CLC   TSKEY+1(2),=C'1M'                                                
         BE    TS64                BLANKS AFTER CLIENT                          
*                                                                               
         L     R5,ACODLST                                                       
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ                                                 
         L     R7,BINTABLE                                                      
         MVC   WRKEY,SPACES                                                     
         CLC   1(2,R6),=C'SJ'            IS THIS A JOB?                         
         BNE   TS59                      NO                                     
         CLI   PROFJOBN,C'Y'             JOB NAME IF AVAILABLE?                 
         BNE   TS59                                                             
         CLC   TSPJT-TSCDE(L'TSPJT,R6),SPACES          JOB CODE                 
         BNH   TS59                                                             
         MVC   WRKEY(9),0(R6)            CUL/CLT(3)PRD(3)                       
         MVC   WRKEY+9(L'TSPJT),TSPJT-TSCDE(R6)          JOB CODE               
         GOTO1 BINSRCH,DMCB,WRKEY,(R7)   FIND NAME IN TABLE                     
         CLI   DMCB,0                    WAS JOB FOUND IN TABLE?                
         BE    TS62                      YES USE JOB NAME                       
*                                                                               
TS59     MVC   WRKEY,SPACES                                                     
         MVC   WRKEY(9),0(R6)            CUL/CLT(3)PRD(3)                       
         CLC   1(2,R6),=C'1N'            IS THIS A 1N ACCOUNT                   
         BNE   *+10                                                             
         MVC   WRKEY,0(R6)                                                      
         GOTO1 BINSRCH,DMCB,WRKEY,(R7)   FIND NAME IN TABLE                     
         CLI   DMCB,0                    WAS ITEM FOUND IN TABLE?               
         BNE   TS60                      NO - TRY CLIENT ACCOUNT                
*                                                                               
         CLC   1(2,R6),=C'SJ'            IS THIS A JOB?                         
         BNE   TS62                      NO                                     
*                                                                               
         CLI   PROFPRD,C'Y'              DOES PROD OVERRIDE CLIENT?             
         BE    TS62                      YES                                    
*                                                                               
TS60     MVC   P+1(25),SPACES            CLEAR OUT CLIENT PROD NAME             
         MVC   P+30(4),SPACES            CLEAR OUT PRODUCT CODE                 
*                                                                               
         L     R5,ACODLST                                                       
         USING BIND,R5                                                          
         L     R7,BINTABLE                                                      
         MVC   WRKEY+6(3),SPACES         COMPANY/U/L/CLIENT                     
         GOTO1 BINSRCH,DMCB,WRKEY,(R7)   FIND NAME IN TABLE                     
*                                                                               
TS62     DS    0H                                                               
         L     R5,DMCB                                                          
         USING CLICDE,R5                                                        
         MVC   P+1(25),CLINAME           CLIENT/PROD NAME                       
*                                                                               
TS64     BAS   RE,PRNTIT                                                        
         LR    R5,R6                                                            
         LA    R5,TSLEN(R5)                                                     
         BCT   R3,TS56                                                          
*                                                                               
TS66     ZIC   R3,PROFBLKS         NUMBER OF BLANKS                             
         LTR   R3,R3                                                            
         BZ    TS66B                                                            
TS66A    BAS   RE,PRNTIT           BLANK LINES                                  
         BCT   R3,TS66A                                                         
*                                                                               
TS66B    LH    R3,MAXROW                                                        
         SH    R3,PRTLINE          PRINT BLANKS TO END OF PAGE                  
         CH    R3,=H'1'                                                         
         BH    *+12                                                             
         BE    TS68                1 LINE LEFT PRINT TOTAL                      
         LH    R3,MAXROW           NO MORE FOR TOTAL SO START NEW PAGE          
         BCTR  R3,0                                                             
TS67     BAS   RE,PRNTIT           BLANK LINES                                  
         BCT   R3,TS67                                                          
*                                                                               
TS68     MVC   P+35(5),=C'TOTAL'                                                
         BAS   RE,PRNTIT                                                        
         CLI   PROFSIGN,C'B'       DO YOU WANT A SIGNATURE LINE                 
         BE    TS68A               AND APPROVAL LINE                            
         CLI   PROFSIGN,C'S'       DO YOU WANT A SIGNATURE LINE                 
         BE    TS68A               YES - PRINT SIGNATURE LINE                   
         CLI   PROFSIGN,C'A'       DO YOU WANT  APPROVAL LINE                   
         BE    TS68D               YES - PRINT APPROVAL LINE                    
TS68A    GOTO1 ACREPORT                                                         
         MVC   P+83(9),=C'SIGNATURE'                                            
         MVC   P+95(31),=34X'6D'                                                
         GOTO1 ACREPORT                                                         
TS68C    CLI   PROFSIGN,C'B'       DO YOU WANT BOTH                             
         BNE   TSXIT               NO -                                         
TS68D    GOTO1 ACREPORT                                                         
         MVC   P+83(11),=C'APPROVED BY'                                         
         MVC   P+95(31),=34X'6D'                                                
         GOTO1 ACREPORT                                                         
         B     TSXIT                                                            
         EJECT                                                                  
*************************************************************                   
*              BUILD CALENDAR DATE TABLE                                        
*************************************************************                   
*                                                                               
BLDCAL   NTR1                                                                   
         MVC   DAYLNE,SPACES       SET-UP HEADLINES                             
         MVC   DTELNE,SPACES                                                    
         MVC   DAYLNE(6),=C' TOTAL '                                            
         MVI   COLUMNS,0                                                        
         CLI   MODE,REQFRST                                                     
         BNE   BLDC00                                                           
         MVI   CALENDAR,C'N'                                                    
         MVC   PERIODCP,SPACES                                                  
         MVC   DAYLNECP,SPACES                                                  
         MVC   DTELNECP,SPACES                                                  
         MVC   WKOFFICE,SPACES     OFFICE TO SPACES                             
         MVI   COLSCORP,0          NUMBER OF CORP COLS                          
         MVC   STDATE,QSTART                                                    
         MVC   EDDATE,QEND                                                      
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         CLC   QEND,SPACES         IF START AND END SPECIFIED USE IT            
         BNE   BLDC12                                                           
         B     *+12                                                             
BLDC00   CLI   CALENDAR,C'Y'       IF NO CORP CALENDAR                          
         BNE   BLDC10              DONT BOTHER LOOKING FOR OFFICE LEVEL         
         L     R7,ACREC            LOOK FOR CALENDAR PASSIVE POINTER            
         USING CASRECD,R7                                                       
         XC    CASPAS,CASPAS                                                    
         MVI   CASPTYP,CASPTYPQ    X'3E0C'                                      
         MVI   CASPSUB,CASPSUBQ                                                 
         MVC   CASPCPY,RCCOMPFL                                                 
         MVC   CASPEDTE,START                                                   
         MVC   CASPOFC,WKOFFICE                                                 
         MVC   SAVEKEY,CASPAS                                                   
         MVC   COMMAND,DMRDHI                                                   
         SR    R6,R6               CLEAR COUNT NUMBER OF PERIODS                
BLDC02   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,ACCDIR,ACREC,ACREC                          
         CLC   CASPAS(CASPEDTE-CASRECD),SAVEKEY                                 
         BNE   BLDC10                                                           
         CLC   CASPSDTE,START      SEE IF START FALLS WITHIN REC DATES          
         BH    BLDC10                                                           
         CLC   CASPEDTE,START                                                   
         BL    BLDC10                                                           
*                                                                               
BLDC03   CLC   CASPOFC,WKOFFICE                                                 
         BE    BLDC04                                                           
         MVC   COMMAND,DMRSEQ                                                   
         B     BLDC02                                                           
*                                                                               
BLDC04   MVC   DA,CASPDA           DISK ADDRESS                                 
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,DA,ACREC,DMWORK                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,CASRFST          FIRST ELEMENT                                
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         USING TMPELD,R4                                                        
BLDC06   CLI   TMPEL,0                                                          
         BE    BLDC10                                                           
         CLI   TMPEL,TMPELQ        TIME SHEET PERIOD ELEMENT?                   
         BE    BLDC08                                                           
BLDC06A  SR    R1,R1                                                            
         IC    R1,TMPLN                                                         
         AR    R4,R1                                                            
         B     BLDC06                                                           
*                                                                               
BLDC08   CLC   TMPSTART,START      SEE IF START FALLS WITHIN  DATES             
         BH    BLDC06A                                                          
         CLC   TMPEND,START                                                     
         BL    BLDC06A                                                          
         GOTO1 DATCON,DMCB,(1,TMPSTART),(0,STDATE)                              
         GOTO1 DATCON,DMCB,(1,TMPEND),(0,EDDATE)                                
         MVI   CALENDAR,C'Y'                                                    
         B     BLDC12                                                           
*                                                                               
*                                  ****NO CALENDAR FOUND******                  
BLDC10   CLI   MODE,REQFRST        IF CORP LEVEL JUST USE QSTART QEND           
         BE    BLDC12                                                           
         MVC   DAYLNE,DAYLNECP     IF OFFICE LEVEL                              
         MVC   DTELNE,DTELNECP     USE CORP DAYS AND DATES                      
         MVC   COLUMNS,COLSCORP                                                 
         MVC   PERIOD,PERIODCP                                                  
         B     BLDC20                                                           
*                                                                               
*                                  CALC DAYS AND DATES                          
BLDC12   LA    R6,DAYLNE+7                                                      
         LA    R7,DTELNE+7                                                      
         LA    R3,1                DAYS IN EACH BOX                             
         MVC   WORK(6),STDATE                                                   
         SR    R2,R2                                                            
*                                                                               
BLDC14   MVC   1(2,R7),WORK+4      DATE                                         
         GOTO1 GETDAY,DMCB,WORK,(R6)                                            
         AH    R2,=H'1'                                                         
         CH    R2,=H'16'                                                        
         BE    BLDC16              MAX. IS 16 COLUMNS                           
         LA    R6,5(R6)                                                         
         LA    R7,5(R7)                                                         
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         CLC   EDDATE,SPACES                                                    
         BE    BLDC15                                                           
         CLC   WORK+6(6),EDDATE                                                 
         BH    BLDC16                                                           
BLDC15   MVC   WORK(6),WORK+6                                                   
         B     BLDC14                                                           
*                                                                               
BLDC16   STC   R2,COLUMNS                                                       
         MVC   PERIOD,SPACES                                                    
         GOTO1 DATCON,DMCB,(0,STDATE),(8,PERIOD)                                
         MVI   PERIOD+9,C'-'                                                    
         MVC   WORK(6),EDDATE                                                   
         CLC   WORK(6),SPACES                                                   
         BNE   BLDC18                                                           
         MVC   WORK(6),WORK+6                                                   
*        L     R3,=F'-1'                                                        
*        GOTO1 ADDAY,DMCB,WORK+6,WORK,(R3)                                      
BLDC18   GOTO1 DATCON,DMCB,(0,WORK),(8,PERIOD+11)                               
         CLI   MODE,REQFRST                                                     
         BNE   BLDC20                                                           
         MVC   DAYLNECP,DAYLNE     SAVE CORP LEVEL DAY AND DATES                
         MVC   DTELNECP,DTELNE                                                  
         MVC   COLSCORP,COLUMNS    NUMBER OF CORP COLS                          
         MVC   PERIODCP,PERIOD                                                  
*                                                                               
BLDC20   DS    0H                                                               
         B     TSXIT                                                            
         DROP  R4,R7                                                            
         EJECT                                                                  
PRNTIT   NTR1                                                                   
         MVI   SPACING,3                                                        
         CLI   PROFROWS,21                                                      
         BNE   *+8                                                              
         MVI   SPACING,2                                                        
         MVC   HEAD3+14(36),COMPNAM                                             
         MVC   HEAD4+1(L'LEVA),LEVA                                             
         MVC   HEAD5+1(L'LEVB),LEVB                                             
         MVC   HEAD6+1(L'LEVC),LEVC                                             
         MVC   HEAD7+1(L'LEVD),LEVD                                             
         CLI   PROFLSTP,C'Y'                                                    
         BNE   PRNTIT1             NO BLANK PAGE AT END                         
         CLI   MODE,REQLAST                                                     
         BNE   PRNTIT1                                                          
         MVC   HEAD4+1(L'LEVA),SPACES                                           
         MVC   HEAD4+1(6),=C'OFFICE'                                            
         MVC   HEAD5+1(L'LEVB),SPACES                                           
         MVC   HEAD5+1(10),=C'DEPARTMENT'                                       
         MVC   HEAD6+1(L'LEVC),SPACES                                           
         MVC   HEAD6+1(15),=C'SUB-DEPARTMENT'                                   
         MVC   HEAD7+1(L'LEVD),SPACES                                           
         MVC   HEAD7+1(8),=C'EMPLOYEE'                                          
*                                                                               
PRNTIT1  MVC   HEAD5+107(L'PERIOD),PERIOD                                       
         MVC   HEAD10+45(L'DAYLNE),DAYLNE                                       
         MVC   HEAD11+45(L'DTELNE),DTELNE                                       
         CLI   PROFPRJT,C'Y'                                                    
         BNE   *+16                NO PROJECT CONTROL                           
         MVC   HEAD10+35(8),=C'PROJECT/'                                        
         MVC   HEAD11+35(5),=C' TASK'                                           
         LH    R1,PRTLINE                                                       
         AH    R1,=H'1'                                                         
         STH   R1,PRTLINE                                                       
         CLC   PRTLINE,MAXROW                                                   
         BNH   PRNTIT2                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   PRTLINE,=H'1'                                                    
PRNTIT2  DS    0H                                                               
PRNTIT4  GOTO1 ACREPORT                                                         
*                                                                               
TSXIT    XMOD1 1                                                                
         EJECT                                                                  
*              ROUTINE TO PUT SJ AND 1N ACCOUNTS INTO CODELIST                  
*                                                                               
         USING ACKEYD,R4                                                        
RDSJ     NTR1                                                                   
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         L     R3,=F'-365'                                                      
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         GOTO1 DATCON,DMCB,WORK+6,(1,ACTDATE)                                   
*                                                                               
         MVI   NUMNON,0                                                         
         L     R4,ACREC                                                         
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'SJ'                                             
         MVI   ACKEYACC+3,X'41'                                                 
RDSJ0    L     R4,ACREC                                                         
RDSJ1    MVI   ACKEYACC+L'ACKEYACC,X'FF'                                        
         BAS   RE,HIGH                                                          
         CLC   ACKEYACC(3),SAVEKEY                                              
         BNE   RD1N1                                                            
         CLC   ACKEYACC+L'ACKEYACC(42-L'ACKEYACC),SPACES                        
         BNE   RDSJ1                                                            
         L     R4,ACREC                                                         
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDSJ0                                                            
         USING ACSTATD,R4                                                       
*        TM    ACSTSTAT,X'40'      CLOSED?                                      
*        BNO   *+14                NO - CONTINUE                                
         L     RE,ACREC                                                         
         CLI   9(RE),C' '          DON'T PUT INACTIVE JOBS                      
         BNH   *+14                                                             
         CLC   ACTDATE,ACSTLAST    LAST ACTIVE MORE THAN 1 YR AGO?              
         BH    RDSJ0               YES - SKIP IT                                
         CLI   ACSTFILT+1,C'T'     ACCOUNT FILTER 2 = T                         
         BNE   RDSJ2               MEANS PRINT ON ALL TIMESHEETS                
         TM    ACSTSTAT,X'20'+X'40' CLOSED OR LOCKED?                           
         BNZ   RDSJ2                YES - DON'T PUT ON EVERY TS                 
         L     R4,ACREC                                                         
         CLI   9(R4),C' '          DON'T PUT JOBS WITH F2=T                     
         BH    RDSJ2                                                            
         ZIC   R1,NUMNON                                                        
         MH    R1,=H'15'                                                        
         L     R5,ANONCLI                                                       
         AR    R5,R1                                                            
         MVC   0(15,R5),0(R4)                                                   
         ZIC   R0,NUMNON                                                        
         AH    R0,=H'1'                                                         
         STC   R0,NUMNON                                                        
RDSJ2    BAS   RE,ADDSJ            ADD TO CODE LIST                             
         B     RDSJ0                                                            
*                                                                               
         USING ACKEYD,R4                                                        
RD1N1    L     R4,ACREC                                                         
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES                               
         MVC   ACKEYACC(1),RCCOMPFL                                             
         MVC   ACKEYACC+1(2),=C'1N'                                             
         MVI   ACKEYACC+3,X'41'                                                 
         BAS   RE,HIGH                                                          
RD1N3    CLC   ACKEYACC(3),SAVEKEY                                              
         BNE   RDIN10              ADD IN ACCOUNT AND NAMES                     
         BAS   RE,ADDSJ                                                         
         L     R4,ACREC                                                         
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   RD1N4                                                            
         USING ACSTATD,R4                                                       
         CLI   ACSTFILT+1,C'T'     ACCOUNT FILTER 2 = T                         
         BNE   RD1N4               MEANS PRINT ON ALL TIMESHEETS                
         L     R4,ACREC                                                         
         ZIC   R1,NUMNON                                                        
         MH    R1,=H'15'                                                        
         L     R5,ANONCLI                                                       
         AR    R5,R1                                                            
         MVC   0(15,R5),0(R4)                                                   
         ZIC   R0,NUMNON                                                        
         AH    R0,=H'1'                                                         
         STC   R0,NUMNON                                                        
*                                                                               
         USING ACKEYD,R4                                                        
RD1N4    L     R4,ACREC                                                         
         LA    R1,ACKEYACC+14                                                   
RD1N5    CLI   0(R1),C' '                                                       
         BNE   RD1N6                                                            
         BCTR  R1,0                                                             
         B     RD1N5                                                            
RD1N6    MVI   1(R1),X'41'                                                      
         BAS   RE,HIGH                                                          
         B     RD1N3                                                            
*                                                                               
RDIN10   CLI   PROFXCLI,0                                                       
         BE    TSXIT               NO BLANKS AFTER CLIENTS                      
         ZIC   R3,PROFXCLI                                                      
RDIN12   ZIC   R1,NUMNON                                                        
         MH    R1,=H'15'                                                        
         L     R5,ANONCLI                                                       
         AR    R5,R1                                                            
         MVC   0(15,R5),SPACES                                                  
         MVC   0(1,R5),RCCOMPFL    KEY IS COMPANY                               
         MVC   1(2,R5),=C'1M'      1M TO SORT BEFORE 1N                         
         MVC   3(1,R5),NUMNON      TO KEEP IT UNIQUE                            
         ZIC   R0,NUMNON                                                        
         AH    R0,=H'1'                                                         
         STC   R0,NUMNON                                                        
         BCT   R3,RDIN12                                                        
         B     TSXIT                                                            
         EJECT                                                                  
*              ROUTINE TO ADD CLIENT/NON-CLIENT TO CODE LIST TABLE              
*                                                                               
         USING ACKEYD,R4                                                        
         USING CLICDE,R5                                                        
ADDSJ    NTR1                                                                   
         L     R4,ACREC                                                         
         LA    R5,WORK                                                          
         MVC   CLIKEY(CLILEN),SPACES                                            
         MVC   CLIKEY,ACKEYACC                                                  
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDSJ2                                                           
         USING ACNAMED,R4                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CLINAME(0),ACNMNAME                                              
*                                                                               
ADDSJ2   DS    0H                                                               
         GOTO1 BINADD,DMCB,WORK,ACODLST                                         
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         B     TSXIT                                                            
         EJECT                                                                  
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
*                                                                               
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         L     R6,BINTABLE         A(TABLE)                                     
         L     R4,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         B     TSXIT                                                            
         EJECT                                                                  
*                                                                               
*              DATAMGR INTERFACE                                                
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   SAVEKEY,0(R4)                                                    
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
GTREC    NTR1                                                                   
         L     R4,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R4),(R4)                       
         B     TSXIT                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS                                                        
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    A(CODLIST)                                                       
         DC    A(TSCODE)                                                        
         DC    V(SQUASHER)                                                      
         DC    A(NONCLI)                                                        
         DC    A(HOOK)                                                          
         DC    X'FF'                                                            
*                                                                               
CODNUM   EQU   60000                                                            
TSNUM    EQU   300                                                              
*                                                                               
MAINTAB  DS    0D                                                               
         DC    AL4((((CODNUM*CLILEN)+7)/8)*8),A(CODTAB)                         
         DC    AL4((((TSNUM*TSLEN)+7)/8)*8),A(TSTAB)                            
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              BOX ROUTINE                                                      
HOOK     DS    0D                                                               
         NMOD1 0,*HOOK*                                                         
         L     RC,SAVERC                                                        
         L     R4,ABOXC                                                         
         USING BOXD,R4                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
         MVI   BOXWT,1                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
*        MVI   BOXCOLS+45,C'L'                                                  
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+45,C'C'                                                  
         LA    R1,BOXCOLS+51                                                    
         ZIC   R2,COLUMNS                                                       
         MVI   0(R1),C'C'                                                       
         LA    R1,5(R1)                                                         
         BCT   R2,*-8                                                           
         MVI   0(R1),C'R'                                                       
*                                                                               
         MVC   BOXROWS(60),ROW14                                                
         CLC   MAXROW,=H'14'                                                    
         BE    *+10                                                             
         MVC   BOXROWS(60),ROW21                                                
HOOKX    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
SAVERC   DC    A(0)                                                             
ROW14    DC    C'        T  M  M  M  M  M  M  M  M  M  M  M  M  M  M'           
         DC    C'  B      '                                                     
ROW21    DC    C'        T  M M M M M M M M M M M M M M M M M M M M '           
         DC    C'M B      '                                                     
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
*                                                                               
*                                                                               
CODLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(CLILEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'CLIKEY)       KEY LENGTH                                   
         DC    AL4(CODNUM)         MAX IN TABLE                                 
CODTAB   DS    AL4(0)              ADDRESS OF THE TABLE                         
*                                                                               
TSCODE   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(TSLEN)          RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'TSKEY)        KEY LENGTH                                   
         DC    AL4(TSNUM)          MAX IN TABLE                                 
TSTAB    DS    AL4(0)              ADDRESS OF THE TABLE                         
*                                                                               
RECORD   DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
*                                                                               
NONCLI   DS    0D                                                               
         DS    100CL15                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*              DSECT FOR WORKING STORAGE                                        
ACC3D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ACREC    DS    A                                                                
ACODLST  DS    A                                                                
ATSCODE  DS    A                                                                
SQUASHER DS    V                                                                
ANONCLI  DS    A                                                                
AHOOK    DS    A                                                                
*                                                                               
ABOXC    DS    A                                                                
*                                                                               
MAINLEN  DS    F                                                                
MAINBGN  DS    F                                                                
*                                                                               
NUMNON   DS    CL1                                                              
CALENDAR DS    CL1                 CALENDAR RECORD SWITCH                       
COMMAND  DS    CL6                                                              
DA       DS    XL4                                                              
ELCODE   DS    CL1                                                              
STDATE   DS    CL6                 START DATE                                   
EDDATE   DS    CL6                 END DATE                                     
START    DS    XL3                 QSTART YMD                                   
END      DS    XL3                 QEND YMD                                     
SIGNSW   DS    CL1                 SIGNATURE LINE SWITCH                        
WKOFFICE DS    CL(L'CASPOFC)       WORK OFFICE SAVE AREA                        
DAYLNE   DS    CL86                                                             
DTELNE   DS    CL86                                                             
DAYLNECP DS    CL86                CORP LEVEL  DAY LINE                         
DTELNECP DS    CL86                CORP LEVEL DATE LINE                         
PERIOD   DS    CL19                                                             
PERIODCP DS    CL19                CORP PERIOD                                  
COLUMNS  DS    CL1                                                              
COLSCORP DS    CL1                 NUMBER OF CORP COLS                          
MAXROW   DS    H                                                                
PRTLINE  DS    H                                                                
OLDATE   DS    CL3                                                              
ACTDATE  DS    CL3                 TODAY MINUS 365 DAYS                         
WRKEY    DS    CL15                                                             
SAVEKEY  DS    CL42                                                             
SALHKEY  DS    CL42                                                             
*                                                                               
COMPNAM  DS    CL36                                                             
LEVA     DS    CL56                                                             
LEVB     DS    CL56                                                             
LEVC     DS    CL56                                                             
LEVD     DS    CL56                                                             
LEVALN   DS    CL1                                                              
LEVBLN   DS    CL1                                                              
LEVCLN   DS    CL1                                                              
LEVDLN   DS    CL1                                                              
         EJECT                                                                  
*                                                                               
*              DSECT FOR PROFILES                                               
PROFD    DSECT                                                                  
PROFROWS DS    CL1             ROWS PER PAGE 14 OR 21                           
PROFDAYS DS    CL1             DAYS FOR PREVIOUS CODES 0-90                     
PROFPRJT DS    CL1             PROJECT CONTROL Y OR N                           
PROFPRD  DS    CL1             PRODUCT OVERRIDES CLIENT Y OR N                  
PROFBLKS DS    CL1             MINIMUN BLANK ROWS 0-20                          
PROFXCLI DS    CL1             BLANK LINES AFTER CLIENTS                        
PROFLSTP DS    CL1             BLANK PAGE AT END OF RUN                         
PROFSAL  DS    CL1             NO SALARY REQUIRED Y,N                           
PROFSIGN DS    CL1             SIGNATURE, APPROVAL, BOTH, NONE S,A,B,N          
PROFPAGE DS    CL1             CONTINUOUS PAGE NUMBERING, Y OR N                
PROFOFNM DS    CL1             USE ORIGIN ID NAME INSTEAD OF COMP NAME          
PROFJOBN DS    CL1             JOB NAME IF AVAILABLE? Y OR N                    
*                                                                               
*              DSECT FOR BINSRCH PARAMETERS                                     
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
*                                                                               
*              DSECT FOR CODE LIST TABLE                                        
CLICDE   DSECT                                                                  
CLIKEY   DS    CL15                                                             
CLINAME  DS    CL36                                                             
CLILEN   EQU   *-CLIKEY                                                         
*                                                                               
*              DSECT FOR TIME SHEET CODES                                       
TSCDE    DSECT                                                                  
TSKEY    DS    CL15                                                             
TSPJT    DS    CL6                                                              
TSTSK    DS    CL2                                                              
TSLEN    EQU   *-TSKEY                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*ACGENBOTH                                                                      
*ACGENFILE                                                                      
*ACGENMODES                                                                     
*ACREPWORKD                                                                     
*ACMASTD                                                                        
*DDREPXTRAD                                                                     
*DDREPMASTD                                                                     
*DDBIGBOX                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030ACREPC302 12/11/09'                                      
         END                                                                    
