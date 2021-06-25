*          DATA SET ACREPR502  AT LEVEL 003 AS OF 12/11/09                      
*PHASE ACR502A,+0                                                               
*INCLUDE CENTER                                                                 
*INCLUDE ACGETSTD                                                               
*INCLUDE PERVERT                                                                
*INCLUDE UNDERLIN                                                               
*INCLUDE ACLIST                                                                 
         TITLE 'WEEKLY REVENUE SUMMARY'                                         
********************************************************************            
*        REQUESTED OFF THE 1R LEDGER                               *            
*                                                                  *            
*        JOB READS 1R AND                                          *            
*                                                                  *            
*        POSTS B TIME INTO WEEKLY BUCKETS                          *            
*        CALCULATES RATES BASED ON REVENUE/DAYS                    *            
*                                                                  *            
*        MOS START AND END ARE REQUIRED                            *            
********************************************************************            
         EJECT                                                                  
ACR502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACR5**,R9,R8                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         USING ACR5D,RC                                                         
         LA    RC,SPACEND                                                       
*                                                                               
         MVC   MYMODE,MODE         SAVE MODE FROM MONACC                        
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LEDGF                                                            
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,PROCTRNS                                                    
         BE    PTRN                                                             
         CLI   MODE,PROCTIME                                                    
         BE    PTIME                                                            
         CLI   MODE,ACCLAST                                                     
         BE    ACCL                                                             
         CLI   MODE,LEVCLAST                                                    
         BE    LEVCLST                                                          
         CLI   MODE,LEVBLAST                                                    
         BE    LEVBLST                                                          
         CLI   MODE,LEVALAST                                                    
         BE    LEVALST                                                          
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR RUN FIRST                                              *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         LA    RE,RELOTAB          ADDRESSES OF RELOS                           
         LA    R1,ATYPES           LABELS OF ATYPES                             
         MVC   0(ATYPLNQ,R1),0(RE) RELOCATE MY ATYPES                           
*                                                                               
         L     R2,=A(BOXRC)                                                     
         ST    RC,0(R2)            SAVE RC                                      
*                                                                               
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         L     R2,MCBXAREA                                                      
         ST    R2,ADBOX            STORE ADDR OF BOX ROUTINE                    
         L     R3,=A(HDHOOK)                                                    
         ST    R3,HEADHOOK                                                      
*                                                                               
         L     R0,=A(BUFFSIZE)     ACQUIRE STORAGE                              
         AR    R0,RF               AND STD HRS KEYS                             
         GETMAIN R,LV=(0)                                                       
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ACALKEYS         SAVE A(CALENDAR KEY)                         
         AH    R1,=Y(GPBUFSZQ)                                                  
         ST    R1,ASTDKEYS                                                      
         AH    R1,=Y(GSBUFSZQ)                                                  
         ST    R1,ACALREC                                                       
         AH    R1,=Y(IOLNQ)                                                     
         ST    R1,ASTDHRS                                                       
*                                                                               
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* ROUTINE FOR REQUEST FIRST                                          *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         USING CPYRECD,RF                                                       
         XC    FLGCST,FLGCST       CLEAR COST SYSTEM FLAG                       
         L     RF,ADCOMP           ADDRESS OF COMPANY RECORD                    
         LA    RE,ACCORFST(RF)     BUMP TO ELEMENT                              
         USING CPYELD,RE                                                        
REQF10   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),CPYELQ        X'10' COMPANY ELEMNT                         
         BE    REQF20                                                           
         SR    R1,R1                                                            
         IC    R1,CPYLN                                                         
         AR    RF,R1                                                            
         B     REQF10                                                           
*                                                                               
REQF20   TM    CPYSTAT5,CPYSNCST   TEST FOR NEW COST                            
         BZ    *+8                                                              
         OI    FLGCST,FLGNUCST     SET FLAG FOR RUNNING NEW COST                
         DROP  RE,RF                                                            
*                                                                               
         GOTO1 =A(CLTNP),DMCB,(RC)     BUILD LIST OF NEW BUS/PRO BONO           
         MVI   RCSUBPRG,0                                                       
         MVC   PAGE,=H'1'                                                       
*                                                                               
         USING ACMD,R2                                                          
         GOTO1 DATCON,DMCB,(5,RCDATE),(1,RUNDTE)                                
         L     R2,AMONACC                                                       
         CLI   ACMMEND,X'FF'          MEND SPECIFIED IN REQUEST                 
         BE    REQF30                 NO, USE THIS MONTH                        
         MVC   MOSEND,ACMMEND                                                   
         MVC   MOSSTR,ACMMEND                                                   
         B     REQF40                                                           
*                                                                               
REQF30   MVC   MOSEND,RUNDTE       YYMM OF RCDATE                               
         MVC   MOSSTR,RUNDTE                                                    
*                                                                               
REQF40   CLC   QEND,SPACES         SPECIAL AS OF DATE REQUESTED?                
         BE    REQF50              NO                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,RUNDTE)                                  
         MVC   MOSEND,RUNDTE       YYMM OF REQUESTED AS OF DATE                 
         MVC   MOSSTR,RUNDTE                                                    
*                                                                               
REQF50   L     R2,AOFFNAME                                                      
         XC    0(2,R2),0(R2)                                                    
*                                                                               
* BUILD OFFICE NAME TABLE                                                       
*                                                                               
         USING LISTD,R3                                                         
         LA    R3,WORK                                                          
         MVC   LISTKEY,SPACES                                                   
         USING OGRRECD,R5                                                       
         LA    R5,SVKEY                                                         
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ    X'2C' RECORD TYPE                            
         MVI   OGRKSUB,OGRKOFFQ    X'04' - OFFICE                               
         MVC   OGRKCPY,RCCOMPFL                                                 
         MVC   OGRKUNT(2),=C'SJ'                                                
         MVC   OGRKOFC,SPACES                                                   
         LA    R2,SVKEY                                                         
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
REQF60   LA    R5,IO                                                            
         CLC   0(OGRKCODE-OGRKEY,R2),OGRKEY                                     
         BNE   REQF80                                                           
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         CLC   OGRKOFC,SPACES      MAKE SURE SOMETHING IS THERE                 
         BNH   REQF70                                                           
         LA    R3,WORK                                                          
         MVC   LISTKEY(2),OGRKOFC                                               
         MVI   ELCODE,NAMELQ       X'20' - NAME ELEMENT                         
         BAS   RE,GETEL2                                                        
         LA    R3,LISTNAME                                                      
         BAS   RE,GETNAME                                                       
         L     R3,AOFFNAME                                                      
         BAS   RE,BUILDLST                                                      
REQF70   GOTO1 =A(DMSEQDR),DMCB,(RC)                                            
         B     REQF60                                                           
*                                                                               
REQF80   DS    0H                                                               
         BAS   RE,ZAPTOTS                                                       
         XC    TRANSTAT,TRANSTAT                                                
         XC    PREVSVG,PREVSVG                                                  
         MVC   WORK(2),MOSEND                                                   
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(9,MOSPRNT)                                 
         GOTO1 DATCON,DMCB,(1,RUNDTE),(5,ASOFPRNT)                              
         B     EXIT                                                             
         DROP  R2,R3,R5                                                         
         EJECT                                                                  
**********************************************************************          
* GET LEDGER PROFILES, SET REPORT OPTIONS, SET REPORT BREAK          *          
* TITLES, GET BUDGET LEVELS                                          *          
**********************************************************************          
         SPACE 1                                                                
LEDGF    DS    0H                                                               
         SR    R3,R3                                                            
         GOTO1 ABLDPRO,DMCB,(RC),(R3)    GET PROFILE                            
         CLI   PRNEWPRO,C'Y'             PRINT PRO BONO, NEW BUSINESS?          
         BNE   *+8                       NO                                     
         MVI   RCSUBPRG,1                                                       
*                                                                               
* FIND OUT AT WHAT LEVEL I SHOULD USE SJ OFFICE NAMES                           
*                                                                               
         MVI   SJOFFLEV,C'N'                                                    
         USING LDGELD,R2                                                        
         L     R2,ADLDGEL          ADDRESS LEDGERR ELEMENT                      
         CLI   LDGOPOS,LDGONKHI                                                 
         BNL   LEDGF10             OFFICE NOT IN KEY                            
*                                                                               
* SAVE OFFSET FOR NOW, NEED HEIR TO                                             
* DETERMINE THE LEVEL                                                           
* CONVERT POSITION INTO OFFSET                                                  
*                                                                               
         MVC   BYTE,LDGOPOS                                                     
         NI    BYTE,X'FF'-LDGOKEY2 DON'T CARE IF ITS 2 BYTES (YET)              
         XR    RE,RE                                                            
         IC    RE,BYTE                                                          
         BCTR  RE,0                                                             
         STC   RE,SJOFFLEV                                                      
*                                                                               
LEDGF10  BAS   RE,GETLEVS                                                       
         USING TOTTBLD,R3                                                       
         LA    R3,TOTTBL                                                        
         CLC   LEVCDSC,SPACES      IS THIS DEFINED?                             
         BNH   LEDGF20             NO, USE DEFAULT IN TABLE                     
         MVC   TOTTYPE,LEVCDSC                                                  
         BAS   RE,SQUASH           SQUASH IT                                    
         MVC   PRLVCNAM,LEVCDSC                                                 
         CLC   LEVC,SJOFFLEV       IS L'THIS LEVEL = OFF POS                    
         BNE   *+8                 NO                                           
         MVI   SJOFFLEV,C'D'                                                    
*                                                                               
LEDGF20  LA    R3,TOTTBLLN(R3)                                                  
         CLC   LEVBDSC,SPACES      IS THIS DEFINED?                             
         BNH   LEDGF30             YES USE DEFAULT IN TABLE                     
         MVC   TOTTYPE,LEVBDSC                                                  
         BAS   RE,SQUASH                                                        
*                                                                               
         CLC   LEVB,SJOFFLEV       IS OFFSET TO OFFICE SAME AS L'LEV B          
         BNE   *+8                 NO                                           
         MVI   SJOFFLEV,C'C'       Y, OFFPOS IS LEVEL C                         
*                                                                               
LEDGF30  LA    R3,2*TOTTBLLN(R3)   SKIP PAST SERVICE GROUP                      
         CLC   LEVADSC,SPACES      IS THIS DEFINED?                             
         BNH   LEDGF40             YES USE DEFAULT IN TABLE                     
         MVC   TOTTYPE,LEVADSC                                                  
         BAS   RE,SQUASH                                                        
*                                                                               
         CLC   LEVA,SJOFFLEV       IS OFFSET TO OFFICE SAME AS L'LEV A          
         BNE   *+8                 NO                                           
         MVI   SJOFFLEV,C'B'                                                    
         CLI   SJOFFLEV,0          IS OFFSET 0                                  
         BNE   *+8                 NO                                           
         MVI   SJOFFLEV,C'A'       THEN ITS THE FIRST LEVEL                     
*                                                                               
LEDGF40  L     R2,AOFFNAME                                                      
         OC    0(2,R2),0(R2)       ARE THERE ANY SJ OFFICE RECORDS              
         BNZ   *+8                 YES                                          
         MVI   SJOFFLEV,C'N'       NO, FORGET ABOUT THIS                        
*                                                                               
         L     R2,ADLDGHIR                                                      
         CLI   QOPT1,C' '                                                       
         BNE   *+14                                                             
         MVC   THISLEV,LEVBDSC                                                  
         B     LEDGF50                                                          
*                                                                               
         CLI   QOPT1,C'A'                                                       
         BNE   *+14                                                             
         MVC   THISLEV,LEVCDSC                                                  
         B     LEDGF50                                                          
*                                                                               
         CLI   QOPT1,C'B'                                                       
         BNE   *+14                                                             
         MVC   THISLEV,LEVDDSC                                                  
*                                                                               
* FIND OUT WHAT LEVEL THE USER HAS DEFINED BUDS AT                              
*                                                                               
         USING BUDD,R2                                                          
LEDGF50  CLI   QOPT2,C' '          STD HRS REQUEST OVERRIDE                     
         BE    *+10                                                             
         MVC   PRSTDHR,QOPT2                                                    
         LA    R2,BUSTDHRS         READ FOR STANDARD HOURS                      
         MVI   BUTYPE,BUSTDHR                                                   
         MVC   BUDNUM,PRSTDHR                                                   
         BAS   RE,SETBUD                                                        
*                                                                               
         LA    R2,BUSTDRAT         STANDARD RATE                                
         MVI   BUTYPE,BUSTDRT                                                   
         MVC   BUDNUM,PRSTDRAT                                                  
         BAS   RE,SETBUD                                                        
*                                                                               
         LA    R2,BUTRGPCT         TARGET PCT                                   
         MVI   BUTYPE,BUTRGPC                                                   
         MVC   BUDNUM,PRTRGPCT                                                  
         BAS   RE,SETBUD                                                        
*                                                                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* LEVEL A FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,RF                                                       
LEVAF    DS    0H                                                               
         L     RF,ADHEIRA                                                       
         MVC   SVOFF,SPACES                                                     
         SR    R1,R1                                                            
         IC    R1,LEVLNQA               LEVEL A INDIVIDUAL LENGTH               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SVOFF(0),ACTKACT         MOVE IN OFFICE CODE                     
         GOTO1 ANEWCAL,DMCB,(RC),SVOFF                                          
*                                                                               
LEVAF10  NI    TRANSTAT,X'FF'-GOTLEVA                                           
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* LEVEL B FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         NI    TRANSTAT,X'FF'-GOTLEVB                                           
         BAS   RE,SETCDE                                                        
         CLC   PREVSVG,LEVACDE     CHECK FOR NEW SERVICE GROUP                  
*                                  PREVSVG IS L'EMPLVA+1                        
         BE    EXIT                                                             
         NI    TRANSTAT,X'FF'-GOTSVGP                                           
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL C FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         NI    TRANSTAT,X'FF'-GOTLEVC                                           
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         NI    TRANSTAT,X'FF'-GOTACC                                            
         L     R2,ADACC                                                         
         USING ACTRECD,R2                                                       
         MVC   P+8(12),ACTKACT                                                  
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R2                                                               
**********************************************************************          
* PROCESS A TRANSACTION                                              *          
**********************************************************************          
         SPACE 1                                                                
         USING TRNRECD,R6                                                       
         USING TRNELD,R7                                                        
PTRN     DS    0H                                                               
         L     R7,ADTRANS                                                       
         LR    R6,R7                                                            
         SH    R6,DATADISP                                                      
*                                                                               
         CLI   TRNEL,TRNELQ        X'44' - THIS IS A TRANSACTION EL             
         BNE   PTRNX               NO, BAD TRANS                                
         CLI   TRNTYPE,49            IS TRANS TYPE 49?                          
         BNE   PTRNX                 NO                                         
*                                                                               
         CLC   TRNDATE,RUNDTE        TRNS WITHIN DATE RANGE                     
         BH    PTRNX                                                            
*                                                                               
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         CLC   ACMMDTE,MOSSTR                                                   
         BL    PTRNX                                                            
         CLC   ACMMDTE,MOSEND                                                   
         BH    PTRNX                                                            
*                                                                               
         LR    R5,R7                                                            
         MVI   ELCODE,X'40'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PTRNX                                                            
*                                                                               
         TM    TRANSTAT,GOTLEVA+GOTSVGP+GOTLEVB+GOTLEVC+GOTACC                  
         BO    PTRN10              I'M ALL SET                                  
*                                                                               
         BAS   RE,SETCDE           BREAK 1R ACCOUNT INTO COMPONENTS             
*                                                                               
         TM    TRANSTAT,GOTLEVA    DO ANY FIRSTS YOU MIGHT NEED                 
         BO    *+8                                                              
         BAS   RE,NEWLEVA                                                       
         TM    TRANSTAT,GOTSVGP                                                 
         BO    *+8                                                              
         BAS   RE,NEWSVGP                                                       
         TM    TRANSTAT,GOTLEVB                                                 
         BO    *+8                                                              
         BAS   RE,NEWLEVB                                                       
         TM    TRANSTAT,GOTLEVC                                                 
         BO    *+8                                                              
         BAS   RE,NEWLEVC                                                       
         TM    TRANSTAT,GOTACC                                                  
         BO    *+8                                                              
         BAS   RE,NEWACC                                                        
*                                                                               
         USING PRTELD,R5                                                        
PTRN10   TM    PRTSTAT,X'80'      'B' TIME                                      
         BO    PTRN40              Y, PUT REVENUE IN BUCKETS BY DATE            
         TM    PRTSTAT,X'20'      'R' TIME                                      
         BNO   PTRNX              NO, IGNORE                                    
*                                                                               
         ZAP   PL16,PRTRATE       CALCULATE REVENUE                             
         MP    PL16,PRTHOUR                                                     
         SRP   PL16,64-2,5                                                      
         ZAP   DUB,PL16                                                         
*                                                                               
         CLI   RCSUBPRG,1          DO THEY WANT PROBONO                         
         BNE   PTRN30              NO                                           
*                                                                               
         L     R5,ADTRANS                                                       
         MVI   ELCODE,PCIELQ       X'51' - PROJECT CONTROL INFO ELEMENT         
         BAS   RE,NEXTEL                                                        
         BNE   PTRN30              MUST BE PLAIN R TIME                         
         USING PCIELD,R5                                                        
         MVC   WORK,SPACES         CLEAR WORK AREA                              
         MVC   WORK(CLIKLNQ),PCICLI+3  MOVE IN CLI/PROD FROM ELEMENT            
         BAS   RE,CHKCLI           CHECK AND SEE IF CLIENT IS NB/PB             
*                                                                               
         TM    CLISTAT,CLINB       IS THIS NEW BUSINESS?                        
         BNO   PTRN20              NO                                           
         AP    ACCNEWB,DUB                                                      
         B     PTRN30              ADD TO RTIME                                 
PTRN20   TM    CLISTAT,CLIPB       IS THIS PRO BONO                             
         BNO   PTRN30              NO, ITS RTIME                                
         AP    ACCPROB,DUB                                                      
*                                                                               
PTRN30   AP    ACCRTIME,DUB        ACCUMULATE TOTAL R TIME REVENUE              
         B     PTRNX                                                            
*                                                                               
         USING PRTELD,R5                                                        
PTRN40   ZAP   PL16,PRTRATE       CALCULATE REVENUE                             
         MP    PL16,PRTHOUR                                                     
         SRP   PL16,64-2,5                                                      
         LA    R3,ACCBUCKS                                                      
         LA    R4,DATETAB                                                       
         LA    R1,NDATES           NUMBER OF DATES IN DATE TABLE                
PTRN50   CLC   TRNDATE,0(R4)                                                    
         BNH   PTRN60                                                           
         LA    R4,L'DATETAB(R4)    NEXT DATE                                    
         LA    R3,L'ACCBUCKS(R3)   NEXT BUCKET                                  
         BCT   R1,PTRN50                                                        
*                                                                               
PTRN60   AP    0(L'ACCBUCKS,R3),PL16                                            
*                                                                               
PTRNX    B     EXIT                                                             
         DROP  R2,R5,R6,R7                                                      
         EJECT                                                                  
**********************************************************************          
* PROCESS TMS RECORDS                                                *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMELD,R5                                                        
PTIME    DS    0H                                                               
         L     R5,ADTRANS                                                       
         USING ACMD,RF                                                          
         L     RF,AMONACC                                                       
         USING TIMRECD,R2                                                       
         L     R2,ACMALTN          R2 = A(TIME RECORD)                          
         DROP  RF                                                               
*                                                                               
         CLI   TIMEL,TIMELQ                                                     
         BNE   PTIMEX                                                           
         CLI   TIMETYP,TIMEINP     IS THIS INPUT DETAIL                         
         BNE   PTIMEX                                                           
         CLI   TIMLN,TIMILN2Q      AT LEAST BILLABLE INPUT LENGTH               
         BL    PTIMEX                                                           
*                                                                               
         CLC   TIMKPEDT,RUNDTE     ACTIVITY DATE WITHIN DATE RANGE              
         BH    PTIMEX                                                           
*                                                                               
         CLC   TIMMOA,MOSSTR       MOS DATE                                     
         BL    PTIMEX                                                           
         CLC   TIMMOA,MOSEND       MOS DATE                                     
         BH    PTIMEX                                                           
*                                                                               
         TM    TRANSTAT,GOTLEVA+GOTSVGP+GOTLEVB+GOTLEVC+GOTACC                  
         BO    PTIME10             I'M ALL SET                                  
*                                                                               
         BAS   RE,SETCDE           BREAK 1R ACCOUNT INTO COMPONENTS             
*                                                                               
         TM    TRANSTAT,GOTLEVA    DO ANY FIRSTS YOU MIGHT NEED                 
         BO    *+8                                                              
         BAS   RE,NEWLEVA                                                       
         TM    TRANSTAT,GOTSVGP                                                 
         BO    *+8                                                              
         BAS   RE,NEWSVGP                                                       
         TM    TRANSTAT,GOTLEVB                                                 
         BO    *+8                                                              
         BAS   RE,NEWLEVB                                                       
         TM    TRANSTAT,GOTLEVC                                                 
         BO    *+8                                                              
         BAS   RE,NEWLEVC                                                       
         TM    TRANSTAT,GOTACC                                                  
         BO    *+8                                                              
         BAS   RE,NEWACC                                                        
*                                                                               
PTIME10  TM    TIMTTYP,TIMTCB      'B' TIME - BILLABLE                          
         BO    PTIME40             Y, PUT REVENUE IN BUCKETS BY DATE            
         TM    TIMTTYP,TIMTCR      'R' TIME - REALIZATION                       
         BNO   PTIMEX              NO, IGNORE                                   
*                                                                               
         ZAP   PL16,TIMAMNT       CALCULATE REVENUE                             
         ZAP   DUB,PL16                                                         
*                                                                               
         CLI   RCSUBPRG,1          DO THEY WANT PROBONO                         
         BNE   PTIME30             NO                                           
*                                                                               
         MVC   WORK,SPACES         CLEAR WORK AREA                              
         MVC   WORK(CLIKLNQ),TIMACC+2  MOVE IN CLI/PROD FROM ELEMENT            
         BAS   RE,CHKCLI           CHECK AND SEE IF CLIENT IS NB/PB             
*                                                                               
         TM    CLISTAT,CLINB       IS THIS NEW BUSINESS?                        
         BNO   PTIME20             NO                                           
         AP    ACCNEWB,DUB                                                      
         B     PTIME30             ADD TO RTIME                                 
PTIME20  TM    CLISTAT,CLIPB       IS THIS PRO BONO                             
         BNO   PTIME30             NO, ITS RTIME                                
         AP    ACCPROB,DUB                                                      
*                                                                               
PTIME30  AP    ACCRTIME,DUB        ACCUMULATE TOTAL R TIME REVENUE              
         B     PTIMEX                                                           
*                                                                               
PTIME40  ZAP   PL16,TIMAMNT       CALCULATE REVENUE                             
         LA    R3,ACCBUCKS                                                      
         LA    R4,DATETAB                                                       
         LA    R1,NDATES           NUMBER OF DATES IN DATE TABLE                
PTIME50  CLC   TIMKPEDT,0(R4)                                                   
         BNH   PTIME60                                                          
         LA    R4,L'DATETAB(R4)    NEXT DATE                                    
         LA    R3,L'ACCBUCKS(R3)   NEXT BUCKET                                  
         BCT   R1,PTIME50                                                       
*                                                                               
PTIME60  AP    0(L'ACCBUCKS,R3),PL16                                            
*                                                                               
PTIMEX   B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* ACCOUNT LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
ACCL     DS    0H                                                               
         TM    TRANSTAT,GOTACC     DID I USE THIS ACCOUNT                       
         BNO   EXIT                NO                                           
         MVI   MYMODE,ACCLAST                                                   
*                                                                               
* CALCULATE BUDGET AMOUNTS                                                      
*                                                                               
         USING BUDD,R2                                                          
         LA    R2,BUSTDHRS                                                      
         ZAP   PL16,BUDAMNT                                                     
         LA    R2,BUSTDRAT                                                      
         MP    PL16,BUDAMNT                                                     
         SRP   PL16,64-2,5                                                      
         LA    R2,BUTRGPCT                                                      
         ZAP   DOUBLE,BUDAMNT                                                   
         SRP   DOUBLE,64-2,5                                                    
         MP    PL16,DOUBLE                                                      
         SRP   PL16,64-2,5                                                      
         ZAP   ACCBUDG,PL16                                                     
*                                                                               
         BAS   RE,BUMPTOTS                                                      
         CLI   QOPT1,C'B'          PRINT EMPLOYEE DATA                          
         BNE   EXIT                                                             
         LA    R2,ACCBUCKS                                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRTBUCKS                                                      
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LAST FOR LEVEL C                                                   *          
**********************************************************************          
         SPACE 1                                                                
LEVCLST  DS    0H                                                               
         TM    TRANSTAT,GOTLEVC    DID I USE THIS ACCOUNT                       
         BNO   EXIT                NO                                           
         MVI   MYMODE,LEVCLAST                                                  
         BAS   RE,BUMPTOTS                                                      
         CLI   QOPT1,C'A'          WANT TOTS HERE                               
         BL    LEVCLX              NO                                           
         BE    LEVCL10                                                          
         MVC   P+1(9),=C'TOTAL FOR'                                             
         BAS   RE,GETTYPE                                                       
         MVC   P+11(10),WORK                                                    
LEVCL10  MVI   SPACING,2                                                        
         BAS   RE,PRTBUCKS                                                      
*                                                                               
LEVCLX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LAST FOR LEVEL B                                                   *          
**********************************************************************          
         SPACE 1                                                                
LEVBLST  DS    0H                                                               
         TM    TRANSTAT,GOTLEVB    DID I USE THIS ACCOUNT                       
         BNO   LEVBLX              NO                                           
         MVI   MYMODE,LEVBLAST                                                  
         BAS   RE,BUMPTOTS                                                      
         CLI   QOPT1,C' '          WANT TOTS HERE                               
         BE    LEVBL10             NO                                           
         MVC   P+1(9),=C'TOTAL FOR'                                             
         BAS   RE,GETTYPE                                                       
         MVC   P+11(10),WORK                                                    
LEVBL10  MVI   SPACING,2                                                        
         BAS   RE,PRTBUCKS                                                      
*                                                                               
LEVBLX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LAST FOR LEVEL A                                                   *          
**********************************************************************          
         SPACE 1                                                                
LEVALST  DS    0H                                                               
         TM    TRANSTAT,GOTLEVA    DID I USE THIS ACCOUNT                       
         BNO   LEVALX              NO                                           
         BAS   RE,LASTSVGP                                                      
         MVI   MYMODE,LEVALAST                                                  
         BAS   RE,BUMPTOTS                                                      
         MVI   SPACING,2                                                        
         GOTO1 ACREPORT                                                         
         MVC   P+1(9),=C'TOTAL FOR'                                             
         BAS   RE,GETTYPE                                                       
         MVC   P+11(10),WORK                                                    
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,BOXMID                                                        
         MVC   P+1(16),=C'OFFICE RUN RATES'                                     
         BAS   RE,PUTRATE          PRINT THE RATES                              
*                                                                               
LEVALX   B     EXIT                                                             
         EJECT                                                                  
*********************************************************************           
* LAST REQUEST - PRODUCE REPORT                                     *           
*********************************************************************           
         SPACE 1                                                                
REQL     DS    0H                                                               
         TM    TRANSTAT,GOTREQ     ANY DATA FOR THIS REQUEST                    
         BNO   REQLX               NO                                           
         BAS   RE,BOXMID                                                        
         MVC   P+1(9),=C'TOTAL FOR'                                             
         MVC   P+11(7),=C'REQUEST'                                              
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,BOXMID                                                        
         MVC   P+1(16),=C'REPORT RUN RATES'                                     
         BAS   RE,PUTRATE          PRINT THE RATES                              
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
GETLEVS  NTR1                                                                   
         L     R5,ADLDGHIR         GET HEIRARCHY LEVELS                         
         MVC   LEVELS(LEVLNQ),SPACES                                            
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+4                                                           
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R4,R4                                                            
GLEV10   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
SETCDE   NTR1                                                                   
         L     R5,ADACC                                                         
         MVC   LEVSCDE(LVCDLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,ACTKACT          FULL ACCOUNT CODE                            
         LA    R2,LEVSCDE          FIRST LEVEL CODE                             
         LA    R3,LEVLNQS          FIRST LEVEL LENGTHS                          
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R3)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R2),0(R1)                                                    
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R2,L'LEVSCDE(R2)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R3,1(R3)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* CHECK AND SEE IF CLIENT CODE IS IN IN NEW BUSINESS/PRO BONO TABLE  *          
**********************************************************************          
         SPACE 1                                                                
CHKCLI   NTR1                                                                   
         XC    CLISTAT,CLISTAT     CLEAR CLIENT STATUS FIELD                    
         USING BIND,R2                                                          
         L     R2,ACLITAB          R2 = CLIENT TABLE                            
         SR    R0,R0                                                            
         ICM   R0,15,BININ                                                      
         BZ    CHKCX               NOTHING IN TABLE EXIT                        
         USING CLITBLD,R3                                                       
         LA    R3,BINTAB                                                        
*                                                                               
CHKC10   CLC   CLICDE(CLIKLNQ),WORK    COMPARE ON CLIENT CODE                   
         BNE   CHKC20                                                           
*                                                                               
         CLI   CLIID,C'B'                                                       
         BNE   *+12                                                             
         OI    CLISTAT,CLINB       SET NEW BUSINESS SWITCH                      
         B     CHKCX                                                            
         CLI   CLIID,C'P'                                                       
         BNE   *+12                                                             
         OI    CLISTAT,CLIPB       SET PRO BONO SWITCH                          
         B     CHKCX                                                            
*                                                                               
CHKC20   LA    R3,CLILNQ(R3)       BUMP TO NEXT RECORD                          
         BCT   R0,CHKC10                                                        
*                                                                               
CHKCX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LAST FOR SERVICE GROUP, CALLED FROM LEVBFRST OF LEVALAST           *          
* BUMP TOTALS AND PRINT IF NEEDED                                    *          
* R5 IS SET TO 1) A(ACCOUNT YOU ARE PROCESSING)                      *          
*              2) A(SPACES), TO FORCE LASTSVGP                       *          
**********************************************************************          
         SPACE 1                                                                
LASTSVGP NTR1                                                                   
         OC    PREVSVG,PREVSVG     DO I HAVE A PREV SVGP                        
         BZ    EXIT                NO ,                                         
LSV10    MVI   MYMODE,SVGPMODE                                                  
         BAS   RE,BUMPTOTS         PRINT SERVICE GROUP TOTS HERE                
*                                                                               
         CLI   PRBRSVGP,C'Y'       REPORT SV GROUP TOTALS?                      
         BE    LSV20               YES                                          
         BAS   RE,ZAPTOTS          ZAP THESE NASTY BUCKETS                      
         B     LSVX                                                             
*                                                                               
LSV20    MVC   P+1(9),=C'TOTAL FOR'                                             
         MVC   P+11(10),=C'SERV GROUP'                                          
         MVI   SPACING,2                                                        
         BAS   RE,PRTBUCKS                                                      
         BAS   RE,ZAPTOTS          ZAP THESE NASTY BUCKETS                      
LSVX     B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PRINT THE 6 BUCKETS AT 0(R2)                                       *          
**********************************************************************          
         SPACE 1                                                                
PRTBUCKS NTR1                                                                   
         USING PRTD,R4                                                          
         LA    R4,P                R2 POINTS TO TIME BUCKET AFTER LOOP          
         CLI   MODE,ACCLAST                                                     
         BE    PRTB30                                                           
         USING TOTTBLD,R2                                                       
         LA    R1,TOTNUM                                                        
         LA    R2,TOTTBL           GET THE BUCKET YOU ARE PRINTING              
PRTB10   CLC   MYMODE(1),TOTLMODE                                               
         BE    PRTB20                                                           
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R1,PRTB10                                                        
PRTB20   LA    R2,TOTBUCKS                                                      
PRTB30   LA    R1,5                PRINT THE WEEKLY BUCKETS                     
         LA    R5,PRWK1                                                         
         ZAP   MYDUB,=P'0'                                                      
PRTB40   AP    MYDUB,0(8,R2)                                                    
         ZAP   DOUBLE,0(8,R2)                                                   
         BAS   RE,SRPEDIT                                                       
         LA    R2,8(R2)            BUMP BUCKS                                   
         LA    R5,11(R5)           BUMP PRINT FIELD                             
         BCT   R1,PRTB40                                                        
*                                                                               
         LA    R5,PRRTIME                                                       
         ZAP   DOUBLE,0(8,R2)                                                   
         BAS   RE,SRPEDIT                                                       
*                                                                               
         LA    R5,PRTOT            PRINT TOTAL                                  
         ZAP   DOUBLE,MYDUB                                                     
         BAS   RE,SRPEDIT                                                       
*                                                                               
         LA    R5,PRBUD            PRINT BUDGET AMOUNT                          
         LA    R2,8(R2)                                                         
         ZAP   DOUBLE,0(8,R2)                                                   
         ZAP   BUDSAVE,0(8,R2)     SAVE BUD AMOUNT TO CALC DAY TI BUD           
         BAS   RE,SRPEDIT                                                       
*                                                                               
         CLI   RCSUBPRG,1          PRINT PRO BONO COL?                          
         BNE   PRTB50                                                           
         LA    R5,PRPROBON         PRINT NEW BUSINESS                           
         LA    R2,8(R2)                                                         
         ZAP   DOUBLE,0(8,R2)                                                   
         BAS   RE,SRPEDIT                                                       
         LA    R5,132(R5)          PRINT PRO BONO                               
         LA    R2,8(R2)                                                         
         ZAP   DOUBLE,0(8,R2)                                                   
         BAS   RE,SRPEDIT                                                       
*                                                                               
PRTB50   ZAP   DOUBLE,MYDUB                                                     
         SR    R6,R6                                                            
         IC    R6,NWKDYS           NUM OF DAYS IN THIS MNTH                     
         BAS   RE,CALCRATE         RETURN DOUBLE AS A RATE                      
         ZAP   MYDUB,DOUBLE        SAVE RATE IN MYDUB                           
         LA    R5,PRRATE                                                        
         BAS   RE,SRPEDIT                                                       
         GOTO1 ACREPORT                                                         
*                                                                               
         LA    R5,PRBUD            PRINT DAYS TO BUD                            
         LA    R5,132(R5)                                                       
         ZAP   PL16,BUDSAVE        BUDEGET AMOUNT DEVIDED BY RATE               
         CP    MYDUB,=P'0'         ANY RATE HERE?                               
         BE    PRTBX               NO, DAY TO BUD IS UNDEFINED                  
         DP    PL16,MYDUB                                                       
         ZAP   DOUBLE,PL16(8)                                                   
         BAS   RE,SRPEDIT                                                       
PRTBX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ADD THE ACCUMS FOR THIS LEVEL                                       *         
* TO THE NEXT LEVEL                                                   *         
***********************************************************************         
         SPACE 1                                                                
BUMPTOTS NTR1                                                                   
         USING TOTTBLD,R2                                                       
         LA    R2,TOTTBL                                                        
         CLI   MYMODE,ACCLAST                                                   
         BNE   BMPT20                                                           
         LA    R3,ACCBUCKS                                                      
         LA    R2,TOTBUCKS                                                      
         B     BMPT50                                                           
*                                                                               
BMPT20   LA    R1,TOTNUM                                                        
BMPT30   CLC   MYMODE(1),TOTLMODE                                               
         BE    BMPT40                                                           
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R1,BMPT30                                                        
         DC    H'0'                                                             
BMPT40   LA    R3,TOTBUCKS                                                      
         LA    R2,TOTTBLLN(R2)                                                  
         LA    R2,TOTBUCKS                                                      
BMPT50   LA    R1,NUMBUCKS                                                      
         BAS   RE,ADDEM                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* ZAP THE ACCUMS FOR THIS LEVEL                                       *         
***********************************************************************         
ZAPTOTS  NTR1                                                                   
         USING TOTTBLD,R2                                                       
         LA    R2,TOTTBL                                                        
         CLI   MYMODE,PROCACC      CALLED TO DO ACCOUNT BUCKETS                 
         BNE   ZAPT20                                                           
         LA    R2,ACCBUCKS                                                      
         B     ZAPT50                                                           
*                                                                               
ZAPT20   LA    R1,TOTNUM                                                        
ZAPT30   CLC   MYMODE,TOTFMODE                                                  
         BE    ZAPT40                                                           
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R1,ZAPT30                                                        
ZAPT40   LA    R2,TOTBUCKS                                                      
ZAPT50   LA    R1,NUMBUCKS                                                      
         BAS   RE,ZAPEM                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CALCULATE AND PRINT THE RUN RATES                                  *          
* CALLED @ LASTLEVA OF REQ LAST                                      *          
**********************************************************************          
         SPACE 1                                                                
PUTRATE  NTR1                                                                   
         USING TOTTBLD,R2                                                       
         LA    R2,TOTTBL                                                        
*                                                                               
         LA    R1,TOTNUM                                                        
PUTR30   CLC   MODE(1),TOTLMODE                                                 
         BE    PUTR40                                                           
         LA    R2,TOTTBLLN(R2)                                                  
         BCT   R1,PUTR30                                                        
         DC    H'0'                                                             
PUTR40   LA    R2,TOTBUCKS                                                      
         LA    R1,5                5 WEEKLY BUCKETS                             
         LA    R3,BINUMS                                                        
         USING PRTD,R4                                                          
         LA    R4,P                                                             
         LA    R5,PRWK1                                                         
         ZAP   MYDUB,=P'0'         FOR TOTAL RATE CALC                          
*                                                                               
PUTR50   SR    R6,R6                                                            
         ZIC   R6,0(R3)                                                         
         LTR   R6,R6               ANY DAYS IN THIS WEEK?                       
         BZ    PUTR60              NO, DON'T CALC RATE                          
         ZAP   DOUBLE,0(8,R2)                                                   
         AP    MYDUB,DOUBLE                                                     
         BAS   RE,CALCRATE                                                      
         BAS   RE,SRPEDIT                                                       
*                                                                               
PUTR60   LA    R3,1(R3)                                                         
         LA    R2,8(R2)                                                         
         LA    R5,11(R5)                                                        
         BCT   R1,PUTR50                                                        
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NWKDYS           NUM OF DAYS IN THIS MNTH                     
         ZAP   DOUBLE,MYDUB                                                     
         BAS   RE,CALCRATE         RETURN DOUBLE AS A RATE                      
         LA    R5,PRTOT                                                         
         BAS   RE,SRPEDIT                                                       
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CALCULATE A RATE, R6 IS NUMBER OF DAYS                              *         
*           DOUBLE HAS AMOUNT                                         *         
*           RATE IS RETURNED IN DOUBLE                                *         
***********************************************************************         
         SPACE 1                                                                
CALCRATE NTR1                                                                   
         LTR   R6,R6                                                            
         BZ    CALCRERR                                                         
*                                                                               
         CVD   R6,DUB              PACK IT INTO DUB                             
         DP    DOUBLE,DUB+6(2)     ONLY NEED THE LAST HALF OF DUB               
         ZAP   DUB,DOUBLE(6)                                                    
         ZAP   DOUBLE,DUB                                                       
         B     EXIT                                                             
*                                                                               
CALCRERR ZAP   DOUBLE,=P'0'        RETURN A ZERO RATE                           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* NEW LEVEL A NEEDED, GET NAME, ETC                                   *         
***********************************************************************         
         SPACE 1                                                                
NEWLEVA  NTR1                                                                   
         MVI   MYMODE,LEVAFRST                                                  
         BAS   RE,ZAPTOTS                                                       
         OI    TRANSTAT,GOTLEVA                                                 
         OI    TRANSTAT,GOTREQ     TURN ON BIT FOR REQUEST ALSO                 
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 GETBUD,DMCB,ADHEIRA    READ BUDGETS, IF NEEDED                   
*                                                                               
         L     R5,ADLVANAM                                                      
         LA    R3,LEVANME                                                       
         BAS   RE,GETNAME                                                       
*                                                                               
         CLI   SJOFFLEV,C'A'       PRINT OFFICE NAMES HERE                      
         BNE   NLA50                                                            
*                                                                               
         L     R3,AOFFNAME                                                      
         MVC   WORK,SPACES                                                      
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         MVC   LISTKEY(L'LEVACDE),LEVACDE                                       
         BAS   RE,GETLIST                                                       
         MVC   LEVANME,LISTNAME                                                 
*                                                                               
*                                                                               
NLA50    MVC   FULL,SPACES                                                      
         MVC   FULL,LEVACDE                                                     
         LA    R3,FULL                                                          
         GOTO1 ABLDPRO,DMCB,(RC),(R3)    GET PROFILE FOR THIS LVA               
*                                                                               
         XC    PREVSVG,PREVSVG                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* NEW SERVICE GROUP, GET NAME, ETC                                    *         
***********************************************************************         
         SPACE 1                                                                
NEWSVGP  NTR1                                                                   
         BAS   RE,LASTSVGP         SEE IF I NEED TO CLOSE A SERVICE GP          
         OI    TRANSTAT,GOTSVGP                                                 
*                                                                               
*        PRINT NEW SERVICE GROUP DATA, WHEN I FIND OUT WHERE IT IS              
*                                                                               
         MVI   MYMODE,SVGPMODE                                                  
         BAS   RE,ZAPTOTS                                                       
*                                                                               
         MVC   PREVSVG,LEVACDE     SAVE LVA/SVGP                                
         CLI   PRBRSVGP,C'Y'       PROFILE TO BREAK ON SERVICE GROUP            
         BNE   EXIT                                                             
         MVC   P+1(13),=C'SERVICE GROUP'                                        
         MVC   P+15(1),LEVSVGP                                                  
         GOTO1 UNDERLIN,DMCB,(20,P+1),PSECOND+1                                 
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* NEW LEVEL B NEEDED, GET NAME, ETC                                   *         
* NOTE LEVEL B NAME IS ACTUALLY CORRISPONDING SJ OFFICE               *         
***********************************************************************         
         SPACE 1                                                                
NEWLEVB  NTR1                                                                   
         MVI   MYMODE,LEVBFRST                                                  
         BAS   RE,ZAPTOTS                                                       
         OI    TRANSTAT,GOTLEVB                                                 
*                                                                               
         GOTO1 GETBUD,DMCB,ADHEIRB    READ BUDGETS, IF NEEDED                   
*                                                                               
         LA    R4,P+3                                                           
         CLI   QOPT1,C' '          OFFICE LEVEL REUQEST                         
         BE    NEWLB10                                                          
         BAS   RE,GETTYPE          GET LEVEL B TITLE                            
         MVC   P+1(15),WORK                                                     
         LA    R4,P+2                                                           
         ZIC   R1,BYTE             LENGTH RETURNED IN BYTE                      
         AR    R4,R1               R1 IS LENGTH OF TOTTYPE FIELD                
NEWLB10  MVC   0(L'LEVBCDE,R4),LEVBCDE                                          
*                                                                               
         CLI   SJOFFLEV,C'B'       USE SJ OFFICE NAMES AT THIS LEVEL            
         BE    NEWLB20             YES                                          
*                                                                               
         USING LISTD,R2                                                         
         MVC   WORK,SPACES                                                      
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         LA    R3,LISTNAME                                                      
         L     R5,ADLVBNAM                                                      
         BAS   RE,GETNAME                                                       
         B     NEWLB30                                                          
*                                                                               
NEWLB20  L     R3,AOFFNAME                                                      
         MVC   WORK,SPACES                                                      
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         MVC   LISTKEY(L'LEVBCDE),LEVBCDE                                       
         BAS   RE,GETLIST                                                       
*                                                                               
NEWLB30  BAS   RE,SQUAPPER                                                      
         CLI   QOPT1,C' '          OFFICE LEVEL REQEST                          
         BE    NEWLBX              DONT PRINT UNTIL YOU GET THE BUCKETS         
         MVC   PTHIRD+1(20),=20C'-'                                             
         GOTO1 ACREPORT                                                         
*                                                                               
NEWLBX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* NEW LEVEL C NEEDED, GET NAME, ETC                                   *         
***********************************************************************         
         SPACE 1                                                                
NEWLEVC  NTR1                                                                   
         OI    TRANSTAT,GOTLEVC                                                 
         MVI   MYMODE,LEVCFRST                                                  
         BAS   RE,ZAPTOTS                                                       
         LA    R4,P+3                                                           
         L     R5,ADHEIRC                                                       
*                                                                               
         GOTO1 GETBUD,DMCB,ADHEIRC    READ BUDGETS, IF NEEDED                   
*                                                                               
         CLI   QOPT1,C'A'          DO THEY WANT TYPE DATA                       
         BL    EXIT                NO                                           
         BE    NEWLC10             TYPE IS LOWEST LEVEL                         
         BAS   RE,GETTYPE                                                       
         MVC   P+1(15),WORK                                                     
         LA    R4,P+2                                                           
         ZIC   R1,BYTE             LENGTH RETURNED IN BYTE                      
         AR    R4,R1                                                            
NEWLC10  MVC   0(L'LEVCCDE,R4),LEVCCDE                                          
         MVC   WORK,SPACES                                                      
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         LA    R3,LISTNAME                                                      
         L     R5,ADLVCNAM                                                      
         BAS   RE,GETNAME                                                       
         BAS   RE,SQUAPPER         PRINT THE NAME AT PSECOND+4                  
         CLI   QOPT1,C'A'          IS THIS THE LOW LEVEL                        
         BE    NEWLAX              YES, DONT PRINT TILL YOU HAVE TOTALS         
         MVC   PTHIRD+1(20),=20C'-'                                             
         GOTO1 ACREPORT                                                         
NEWLAX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET ACCOUNT LEVEL DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
NEWACC   NTR1                                                                   
         OI    TRANSTAT,GOTACC                                                  
         MVI   MYMODE,PROCACC                                                   
         BAS   RE,ZAPTOTS                                                       
         CLI   QOPT1,C'B'          DO THEY WANT EMPLOYEE DATA                   
         BNE   NEWAX                                                            
         MVC   P+3(L'LEVDCDE),LEVDCDE                                           
*                                                                               
         GOTO1 GETBUD,DMCB,ADACC      READ BUDGETS, IF NEEDED                   
*                                                                               
         L     R5,ADACCNAM                                                      
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         LA    R3,LISTNAME                                                      
         BAS   RE,GETNAME                                                       
         BAS   RE,SQUAPPER         SQUASH AND CHOP INTO PSECOND                 
*                                                                               
NEWAX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* FOR A PARTICULAR RUN MODE, RETURN THE                               *         
* LEDGER DEFINED LEVEL NAME IN WORK(15)                               *         
* AND THE LENGTH IN BYTE                                              *         
***********************************************************************         
         SPACE 1                                                                
GETTYPE  NTR1                                                                   
         USING TOTTBLD,R2                                                       
         LA    R1,TOTNUM                                                        
         LA    R2,TOTTBL                                                        
GETT10   CLC   MYMODE,TOTFMODE                                                  
         BE    GETT20                                                           
         CLC   MYMODE,TOTLMODE                                                  
         BNE   GETT30                                                           
GETT20   MVC   WORK(15),TOTTYPE                                                 
         MVC   BYTE,TOTTLEN                                                     
         B     GETTX                                                            
GETT30   LA    R2,TOTTBLLN(R2)                                                  
         BCT   R1,GETT10                                                        
GETTX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* STORE 1R ACCOUNT NAMES                                              *         
* ADD THE LISTD RECORD IN WORK TO THE TABLE AT 0(R3)                  *         
***********************************************************************         
         SPACE 1                                                                
         USING LISTD,R2                                                         
BUILDLST NTR1                                                                   
         LA    R2,WORK                                                          
         SR    R4,R4               PREPARE E/O PAIR                             
         LH    R5,0(R3)            NUMBER IN TABLE                              
         LH    R0,2(R3)            MAX                                          
         CR    R5,R0               IS THERE ANY ROOM?                           
         BL    BLST10                                                           
         DC    H'0'                NO                                           
         DC    C'TBLISFUL'                                                      
         DS    0H                                                               
BLST10   LA    R0,6(R3)            START OF DATA                                
         LA    R1,LISTDLN          LENGTH OF ONE TABLE RECORD                   
         MR    R4,R1               TIMES THE NUMBER IN THE TABLE (E/O)          
         AR    R5,R0               ADDED TO START ADDRESS IS OFFSET             
         MVC   0(LISTDLN,R5),LISTREC                                            
         LH    R0,0(R3)            BUMP # IN TABLE                              
         AH    R0,=H'1'                                                         
         STH   R0,0(R3)                                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET 1R ACCOUNT NAMES KEY IS FIRST 12 OF WORK                        *         
* TABLE IS AT 0(R3)                                                   *         
* RETURN THE NAME IN WORK+12(36)                                      *         
* RETURN ONLY THE SIGNIFICANT PORTIONS OF THE KEY                     *         
***********************************************************************         
         SPACE 1                                                                
         USING LISTD,R3                                                         
GETLIST  NTR1                                                                   
         LH    R0,0(R3)            NUMBER IN TABLE                              
         LTR   R0,R0               ANYTHING THERE                               
         BZ    EXIT                NO                                           
         LH    R2,4(R3)            OFFSET INTO KEY TO RETURN                    
         LA    R3,6(R3)            START OF DATA                                
*                                                                               
GETL10   CLC   LISTKEY,WORK                                                     
         BE    GETL20                                                           
         LA    R3,LISTDLN(R3)      NEXT TABLE ENTRY                             
         BCT   R0,GETL10                                                        
         B     EXIT                                                             
GETL20   LA    R1,WORK                                                          
         MVC   LISTNAME-LISTD(L'LISTNAME,R1),LISTNAME                           
         MVC   WORK+50(L'LISTKEY),SPACES                                        
         LA    R5,L'LISTKEY                                                     
         SR    R5,R2               L'TO MOVE IS TOTAL LENGTH - OFFSET           
         BCTR  R5,0                                                             
         AR    R2,R1               OFFSET+ A(KEY) IS DATA TO MOVE               
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK+50(0),0(R2)                                                 
         MVC   0(L'LISTKEY,R1),WORK+50                                          
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* GET NAME ROUTINE                                                   *          
*     R5 IS ADDRESS OF THE 20 ELEMENT                                *          
*     R3 IS ADDRESS OF 36 BYTE AREA                                  *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R5                                                        
GETNAME  NTR1                                                                   
         MVC   0(36,R3),SPACES                                                  
         CLI   NAMEL,NAMELQ        X'20' - IS THIS THE NAME ELEMENT             
         BE    *+6                 IT BETTER BE                                 
         DC    H'0'                                                             
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BNP   EXIT                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),NAMEREC                                                  
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SETBUD - FIND OUT WHAT LEVELS THE USER HAS DEFINED HIS              *         
*          BUDGETS AT SO YOU CAN READ FOR THEM WITH THE CORRECT       *         
*          ACCOUNT (WHEN YOU HAVE TO)                                 *         
* BUDGET NUMBER IS AT 0(R2), RETURS MODE TO READ IT AT IN 1(R2)       *         
***********************************************************************         
         SPACE 1                                                                
SETBUD   NTR1                                                                   
         USING BUDD,R2                                                          
         MVI   BUDMODE,X'FF'       INIT MODE TO INVALID                         
         ZAP   BUDAMNT,=P'0'                                                    
*                                                                               
         USING BUDRECD,R4                                                       
         LA    R4,SVKEY                                                         
         XC    BUDKEY(ACCORFST),BUDKEY  CLEAR KEY                               
         MVI   BUDKTYP,BUDKTYPQ         BUDGET RECORD                           
         MVC   BUDKCPY,QCOMPANY                                                 
         MVC   BUDKNO1+1(1),BUDNUM      BUD NUMBER                              
         GOTO1 =A(DMHIGHDR),DMCB,(RC)   READ HIGH                               
         CLC   IOKEY((BUDKNO1-BUDKEY)+L'BUDKNO1),SVKEY                          
         BNE   SETBX                  CAN'T FIND THIS BUD                       
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         MVI   ELCODE,BIVELQ       X'1C' - GET BUDGET VALID'TN ELEMENT          
         LA    R6,IO                                                            
         AH    R6,DISP2            ADD DISPLACEMENT TO FIRST ELEMENT            
*                                                                               
SETB30   BAS   RE,NEXTEL2                                                       
         BNE   SETBX                                                            
*                                                                               
         USING BIVELD,R6                                                        
         CLC   BIVAUNT(2),=C'1R'                                                
         BNE   SETB30                                                           
         CLC   =C'1C',BIVVCUNT                                                  
         BNE   SETB30                                                           
         CLI   BIVACLV,0                                                        
         BNE   SETB40                                                           
         MVI   BUDMODE,LEDGFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB40   CLI   BIVACLV,1          SET MODE AT WHICH TO READ THIS BUD            
         BNE   SETB50                                                           
         MVI   BUDMODE,LEVAFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB50   CLI   BIVACLV,2                                                        
         BNE   SETB60                                                           
         MVI   BUDMODE,LEVBFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB60   CLI   BIVACLV,3                                                        
         BNE   SETB70                                                           
         MVI   BUDMODE,LEVCFRST                                                 
         B     SETBX                                                            
*                                                                               
SETB70   CLI   BIVACLV,4                                                        
         BNE   SETBX                                                            
         MVI   BUDMODE,PROCACC                                                  
*                                                                               
SETBX    B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
**********************************************************************          
* GETBUD - READS BUDGET RECORD  AT EMPLOYEE LEVEL OF 1R              *          
*          P1=A(ACCOUNT RECORD)                                      *          
**********************************************************************          
GETBUD   NTR1                                                                   
         USING BUDD,R2                                                          
         L     R5,0(R1)            GET A(ACCOUNT RECORD)                        
         LA    R0,3                                                             
         LA    R2,BUDAREA                                                       
GETB10   CLC   BUDMODE,MYMODE      DO THEY WANT THIS BUD AT THIS LEVEL          
         BNE   *+8                 NO                                           
         BAS   RE,READBUD                                                       
         LA    R2,BUDDLN(R2)                                                    
         BCT   R0,GETB10                                                        
         B     EXIT                                                             
*                                                                               
* READ BUDGET RECORDS                                                           
*                                                                               
READBUD  NTR1                                                                   
         USING BUDRECD,R4                                                       
         LA    R4,SVKEY                                                         
*                                                                               
         XC    BUDKEY(ACCORFST),BUDKEY   CLEAR KEY                              
         MVI   BUDKTYP,BUDKTYPQ          BUDGET RECORD                          
         MVC   BUDKCPY,QCOMPANY          COMPANY                                
         MVC   BUDKCULA,0(R5)            1R ACCOUNT                             
         MVC   BUDKWORK,SPACES                                                  
         MVC   BUDKCCPY(15),SPACES       SPACE OUT CONTRA                       
         MVC   BUDKCCPY,QCOMPANY         COMPANY                                
         MVC   BUDKCUNT(2),=C'1C'        1C CONTRA                              
         MVC   BUDKBUDN+1(1),BUDNUM      TYPE OF BUDGET                         
         NI    BUDKBUDN+1,X'0F'          MAKE IT BINARY                         
         GOTO1 =A(DMHIGHDR),DMCB,(RC)    READ HIGH                              
         CLC   SVKEY((BUDKBUDN-BUDKEY)+L'BUDKBUDN),IOKEY                        
         BNE   READBX                  NO MATCH, BUDGET IS INCOMPLETE           
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)    GET RECORD                             
         USING BAMELD,R5           RETURN THE AMOUNT FOR THIS MONTH             
         LA    R5,IO                                                            
         AH    R5,DISP2                                                         
         ZAP   BUDAMNT,=P'0'               INCASE MONTH NOT FOUND               
*                                                                               
READB10  CLI   0(R5),0                      END OF RECORD?                      
         BE    READBX                       YES                                 
         CLI   BAMEL,BAMELQ                 X'1D' - BUDGET AMT ELEMENT          
         BNE   READB30                      NO, GET NEXT EL                     
         CLI   BUTYPE,BUTRGPC               TARGET PCT BUDGET                   
         BE    READB20                                                          
         CLC   BAMMNTH,MOSEND              BUDGET FOR THIS MONTH ?              
         BNE   READB30                      NO, TRY NEXT EL                     
         ZAP   BUDAMNT,BAMBUDG             SAVE THIS BUDGET AMOUNT              
         B     READBX                       AND LEAVE                           
*                                                                               
READB20  DS    0H                                                               
         CLC   BAMMNTH(1),MOSEND           SAME 'YY' ?                          
         BNE   READB30                      NO, TRY NEXT EL                     
         AP    BUDAMNT,BAMBUDG             SUM BUDGET AMOUNTS                   
*                                           AND GET NEXT EL                     
READB30  SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                        IN SEACH OF X'1D'                   
         B     READB10                                                          
*                                                                               
READBX   BAS   RE,RESET            RESET DM SEQUENCE                            
         B     EXIT                                                             
         DROP  R2,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* RESET DATA MANAGER SEQUENCE                                        *          
**********************************************************************          
         SPACE 1                                                                
RESET    NTR1                                                                   
         L     RE,LASTIO                                                        
         MVC   SVKEY,0(RE)         RE-READ THE LAST ACCOUNT                     
         GOTO1 =A(DMREADDR),DMCB,(RC)                                           
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SQUASH THE NAME IN LISTNAME (WORK)                                 *          
* THEN CHOP IT INTO PSECOND+4,  LENGTH OF 15                         *          
**********************************************************************          
         SPACE 1                                                                
SQUAPPER NTR1                                                                   
         USING LISTD,R2                                                         
         LA    R2,WORK                                                          
         LA    R3,36                                                            
         GOTO1 ADSQUASH,DMCB,LISTNAME,(R3)                                      
         LA    R3,3                                                             
         GOTO1 CHOPPER,DMCB,(36,LISTNAME),(17,PSECOND+4),(C'P',(R3))            
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* PRINT A MIDLINE, WHEREVER YOU ARE                                  *          
**********************************************************************          
         SPACE 1                                                                
BOXMID   NTR1                                                                   
         L     R5,ADBOX                                                         
         USING BOXD,R5                                                          
         SR    R2,R2                                                            
         IC    R2,LINE                                                          
         CH    R2,=H'52'                                                        
         BL    BOXM10                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     BOXM20                                                           
BOXM10   SR    R2,R2                                                            
         IC    R2,LINE                                                          
         LA    R3,BOXROWS(R2)                                                   
         BCTR  R3,0                                                             
         MVI   0(R3),C'M'                                                       
         MVI   BOXINIT,0                                                        
BOXM20   GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* ADD THE NUMBUCKS BUCKETS AT R3 TO THOSE AT R2                      *          
* ZAPEM ZEROS OUT THE BUCKETS AT R2                                  *          
**********************************************************************          
         SPACE 1                                                                
ADDEM    NTR1                                                                   
         AP    0(L'TOTBUCKS,R2),0(L'TOTBUCKS,R3)                                
         LA    R2,L'TOTBUCKS(R2)                                                
         LA    R3,L'TOTBUCKS(R3)                                                
         BCT   R1,*-14                                                          
         B     EXIT                                                             
*                                                                               
ZAPEM    NTR1                                                                   
         LA    R1,NUMBUCKS                                                      
         ZAP   0(L'TOTBUCKS,R2),=P'0'                                           
         LA    R2,L'TOTBUCKS(R2)                                                
         BCT   R1,*-10                                                          
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R5,DATADISP,ELCODE                                               
         EJECT                                                                  
**********************************************************************          
* GETEL # 2                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R5,DISP2,ELCODE,2                                               
         EJECT                                                                  
**********************************************************************          
* SHIFT ROUND AND PACK/EDIT DOUBLE FIELD INTO FIELD AT R5            *          
**********************************************************************          
         SPACE 1                                                                
SRPEDIT  NTR1                                                                   
         SRP   DOUBLE,64-2,5      ROUND OUT THE PENNIES                         
         EDIT  (P8,DOUBLE),(10,(R5)),MINUS=YES                                  
         B     EXIT                                                             
**********************************************************************          
* SQUASH 0(R6) FOR A LENGTH OF R1                                    *          
*        R3 = A TOTTBLE (TOTAL TABLE)                                *          
**********************************************************************          
         SPACE 1                                                                
         USING TOTTBLD,R3                                                       
SQUASH   NTR1                                                                   
         LA    R5,L'TOTTYPE                                                     
         GOTO1 ADSQUASH,DMCB,TOTTYPE,(R5)                                       
         L     R5,DMCB+4                                                        
         STC   R5,TOTTLEN                                                       
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    A(CLITAB)                                                        
         DC    A(LVBLIST)                                                       
         DC    A(LVALIST)                                                       
         DC    A(CAL)                                                           
         DC    A(NEWCAL)                                                        
         DC    A(BUILDPRO)                                                      
         DC    V(UNDERLIN)                                                      
         DC    V(ACLIST)                                                        
         DC    V(CENTER)                                                        
         DC    V(GETPER)                                                        
         DC    V(GETSTD)                                                        
         EJECT                                                                  
**********************************************************************          
* TABLE OF SUBTOTAL VALUES                                           *          
* LEVEL, LEVEL LENGTH, TITLE,NAME, 9 (L'TOTBUCKS)P ACCUMS            *          
* TABLE IS COVERED BY TOTTBLD                                        *          
**********************************************************************          
         SPACE 1                                                                
TOTTBL   DS    0H                                                               
         DC    AL1(TYPLEV)                                                      
         DC    AL1(LEVCFRST)                                                    
         DC    AL1(LEVCLAST)                                                    
         DC    CL1' '                                                           
         DC    CL15'TYPE'                                                       
         DS    CL(TOTBKLNQ)        PACKED AREA                                  
*                                                                               
         DC    AL1(DEPLEV)                                                      
         DC    AL1(LEVBFRST)                                                    
         DC    AL1(LEVBLAST)                                                    
         DC    CL1' '                                                           
         DC    CL15'DEPARTMENT'                                                 
         DS    CL(TOTBKLNQ)        PACKED AREA                                  
*                                                                               
         DC    AL1(SVGLEV)                                                      
         DC    AL1(SVGPMODE)                                                    
         DC    AL1(SVGPMODE)                                                    
         DC    CL1' '                                                           
         DC    CL15'SERVICE'                                                    
         DS    CL(TOTBKLNQ)        PACKED AREA                                  
*                                                                               
         DC    AL1(LVBLEV)                                                      
         DC    AL1(LEVAFRST)                                                    
         DC    AL1(LEVALAST)                                                    
         DC    CL1' '                                                           
         DC    CL15'OFFICE'                                                     
         DS    CL(TOTBKLNQ)        PACKED AREA                                  
*                                                                               
         DC    AL1(REPLEV)                                                      
         DC    AL1(REQFRST)                                                     
         DC    AL1(REQLAST)                                                     
         DC    CL1' '                                                           
         DC    CL15'REPORT'                                                     
         DS    CL(TOTBKLNQ)        PACKED AREA                                  
*                                                                               
TOTNUM   EQU   5                   NUMBER OF LEVELS HERE                        
TYPLEV   EQU   1                                                                
DEPLEV   EQU   2                                                                
SVGLEV   EQU   3                                                                
LVBLEV   EQU   4                                                                
REPLEV   EQU   5                                                                
*                                                                               
SVGPMODE EQU   212                 FAKE MODE FOR SERVICE GROUP                  
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
CLITAB   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL4(CLILNQ)         LENGTH OF ENTRY                              
         DC    AL1(0)              DISP. TO KEY                                 
         DC    AL3(CLIKLNQ)        KEY LENGTH                                   
         DC    AL4(CLIMAX)         MAX IN TABLE                                 
         DC    AL1(0)              NUMBER OF BUCKETS - NO BUCKETS               
         DC    AL1(0)              DISPLACEMENT TO FIRST BUCKET                 
         DS    (CLIMAX*CLILNQ)XL1  TABLE                                        
*                                                                               
CLIMAX   EQU   200                                                              
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    RC                                                           *         
*  P2    A(ITEM TO BE ADDED)                                          *         
*  P3    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIND,R5                                                          
BINADD   NMOD1 0,**BIN**                                                        
         L     RC,0(R1)                                                         
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
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R6,R6                                                            
         ICM   R6,1,BINFST         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         LR    R5,R4               A(RECORD FOUND)                              
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
         AP    0(8,R4),0(8,R3)     ADD TO COUNTER                               
*                                                                               
BINXIT   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD A LIST OF NEW BUSINESS/PROBONO ACCOUNTS       *          
**********************************************************************          
         SPACE 1                                                                
CLTNP    DS    0H                                                               
         NMOD1 0,**CLTN**                                                       
         L     RC,0(R1)                                                         
         USING ACTRECD,R6                                                       
         LA    R6,SVKEY                                                         
         USING BIND,R3                                                          
         L     R3,ACLITAB                                                       
         XC    BININ,BININ            CLEAR TABLE                               
         USING CLITBLD,R2                                                       
         LA    R2,CLIWRK           CLIENT TABLE WORK AREA                       
         XC    CLIWRK,CLIWRK                                                    
         MVC   ACTKEY(ACTRFST-ACTKEY),SPACES                                    
         MVC   ACTKCPY,RCCOMPFL                                                 
         L     R4,ADCMPEL                                                       
         USING CPYELD,R4                                                        
         MVC   ACTKUNT(2),CPYPROD     UL FOR CLI/PRO/JOB                        
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY      SAME C/U/L?                    
         BNE   CLTNPX                                                           
         DROP  R4                                                               
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)                                           
         USING ACLELD,R5                                                        
         LA    R5,IO                                                            
         MVI   ELCODE,ACLELQ                                                    
         BAS   RE,GETEL3                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   CLILEN,ACLVALS      GET CLIENT LENGTH                            
         DROP  R5                                                               
*                                                                               
CLTNP10  LA    R6,SVKEY            BUMP TO NEXT ACCOUNT                         
         SR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         LA    R1,ACTKACT(R1)      BUMP TO JOB FIELD                            
         MVI   0(R1),X'FF'                                                      
         GOTO1 =A(DMHIGHDR),DMCB,(RC)                                           
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY      SAME C/U/L?                    
         BNE   CLTNPX                                                           
*                                                                               
         LA    R6,IO                                                            
         MVC   SVKEY,IOKEY         UPDATE SVKEY                                 
*                                                                               
         USING GETOPTD,R5                                                       
         L     R5,ADGOBLOC                                                      
         MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELCUL,ACTKCULA           CUL                                  
         SR    R1,R1                                                            
         IC    R1,CLILEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   GOSELCLI(0),ACTKACT       MOVE IN CLIENT CODE                    
*                                                                               
         MVC   GOADM,DATAMGR                                                    
         LA    R6,IO                                                            
         ST    R6,GOAKEY                                                        
         GOTO1 GETOPT,DMCB,GETOPTD                                              
*                                                                               
         CLI   GOCTYPE,C'B'        NEW BUSINESS                                 
         BE    CLTNP20                                                          
         CLI   GOCTYPE,C'P'        PRO BONO??                                   
         BNE   CLTNP10             NO                                           
*                                                                               
CLTNP20  MVC   CLICDE,GOSELCLI                                                  
         MVC   CLIID,GOCTYPE       SET ID TO 'B' OR 'P'                         
         GOTO1 =A(BINADD),DMCB,(RC),CLIWRK,ACLITAB                              
         B     CLTNP10                                                          
*                                                                               
CLTNPX   XIT1                                                                   
         DROP  R2,R3,R5,R6                                                      
         EJECT                                                                  
**********************************************************************          
* GETEL # 3                                                          *          
**********************************************************************          
         SPACE 1                                                                
         GETELN R5,DISP2,ELCODE,3                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* HEADHOOK - CALLED FROM ACPRINT                                     *          
**********************************************************************          
         SPACE 1                                                                
HDHOOK   NMOD1 0,*HEADHK                                                        
         L     R2,=A(BOXRC)                                                     
         ICM   RC,15,0(R2)                                                      
*                                                                               
         L     R5,ADBOX                                                         
         USING BOXD,R5                                                          
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         MVI   BOXROWS+6,C'T'      SET ROWS                                     
         MVI   BOXROWS+9,C'M'                                                   
         MVI   BOXROWS+56,C'B'                                                  
*                                                                               
         MVI   BOXCOLS,C'L'                                                     
         MVI   BOXCOLS+21,C'C'                                                  
         MVI   BOXCOLS+32,C'C'                                                  
         MVI   BOXCOLS+43,C'C'                                                  
         MVI   BOXCOLS+54,C'C'                                                  
         MVI   BOXCOLS+65,C'C'                                                  
         MVI   BOXCOLS+76,C'C'                                                  
         MVI   BOXCOLS+87,C'C'                                                  
         MVI   BOXCOLS+98,C'C'                                                  
         MVI   BOXCOLS+109,C'C'                                                 
         MVI   BOXCOLS+120,C'R'                                                 
         CLI   RCSUBPRG,1                                                       
         BNE   HDH50                                                            
         MVI   BOXCOLS+120,C'C'                                                 
         MVI   BOXCOLS+131,C'R'                                                 
*                                                                               
HDH50    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
         MVC   HEAD9+24(1),PRWEEK1    VARIBLE HEADER DATA                       
         MVC   HEAD9+35(1),PRWEEK2                                              
         MVC   HEAD9+46(1),PRWEEK3                                              
         MVC   HEAD9+57(1),PRWEEK4                                              
         CLI   PRWEEK1,C'1'                                                     
         BE    *+8                                                              
         MVI   HEAD9+29,C'S'       PLURAL DAYS                                  
         MVC   HEAD9+68(1),PRWEEK5                                              
         CLI   PRWEEK5,C'1'                                                     
         BE    *+8                                                              
         MVI   HEAD9+73,C'S'                                                    
*                                                                               
         MVC   HEAD3+58(8),=C'MONTH OF'                                         
         MVC   HEAD3+67(6),MOSPRNT                                              
*                                                                               
         MVC   HEAD5+1(15),LEVADSC                                              
         MVC   HEAD5+16(6),LEVACDE                                              
         MVC   HEAD5+23(36),LEVANME                                             
         LA    R3,L'LEVADSC+L'LEVACDE+L'LEVANME                                 
         GOTO1 ADSQUASH,DMCB,HEAD5+1,(R3)                                       
*                                                                               
         MVC   HEAD5+59(5),=C'AS OF'                                            
         MVC   HEAD5+65(8),ASOFPRNT                                             
*                                                                               
         MVC   HEAD8+1(15),THISLEV  NAME OF LEVEL REPORTING AT                  
HDHX     XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GO TO GETPROF WITH A COMPOSITE KEY SO IT RETURNS YOU A             *          
* COMPOSITE PROFILE.                                                 *          
*        P1 IS RC                                                    *          
*        P2 IS A  OF ACCOUNT                                         *          
*        0(R3) IS A(ACCOUNT YOU WANT TO GET A PROFILE FOR)           *          
**********************************************************************          
         SPACE 1                                                                
BUILDPRO NMOD1 0,*BPROF                                                         
         L     RC,0(R1)                                                         
         L     R3,4(R1)                                                         
         USING PROFKD,R2                                                        
         LA    R2,WORK                                                          
         XC    PROFKEY,PROFKEY                                                  
         XC    PROFILES,PROFILES                                                
         MVI   PROFKSYS,C'A'                                                    
         MVC   PROFKPGM,=C'0R5'                                                 
         MVC   PROFKAGY,ALPHAID                                                 
         MVC   PROFKUNL,=C'1R'                                                  
         LTR   R3,R3               WAS AN ACCOUNT PASSED                        
         BZ    *+10                NO                                           
         MVC   PROFKACC,0(R3)                                                   
         GOTO1 GETPROF,DMCB,PROFKEY,PROFILES,DATAMGR                            
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCDIR ',SVKEY,IO,0                       
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* NEWCAL - SET BINUMS - N'WORKING DAYS PER WEEK IN PERIOD(MONTH)     *          
*              DATETAB - BUCKET DATES                                *          
*              NWKDYS - NUMBER OF WORKING DAYS IN A PERIOD (MONTH)   *          
*              PRWKS  - EBCDIC VERSION OF BINUMS                     *          
*              IF YOU FIND A CALENDER RECORD                         *          
*           P1 IS RC                                                 *          
*           P2 IS A(OFFICE) OR 0 FOR REQUEST LEVEL                   *          
**********************************************************************          
         SPACE 1                                                                
NEWCAL   NMOD1 NCWORKL,**NCAL**                                                 
         LR    R8,RC                                                            
         L     RC,0(R1)                                                         
         L     R4,4(R1)            A(OFFICE) OR 0                               
*                                                                               
         TM    FLGCST,FLGNUCST     ARE WE RUNNING ON NEW COST?                  
         BZ    NEWC30              NO - RUN OLD CALENDAR                        
*                                                                               
         USING NCWORKD,R8                                                       
         GOTO1 VGETPER,DMCB,MOSEND,ACALKEYS,NCPERDAT,(R4),ACALREC               
*                                                                               
         USING GPOPD,R2                                                         
         LA    R2,NCPERDAT                                                      
         CLI   GPONPER,0           ANY PERIOD DATA RETURNED                     
         BE    NEWC30              NOPE, SET UP USING TABLES                    
*                                                                               
         MVC   NWKDYS,GPONWORK                                                  
         SR    R1,R1                                                            
         IC    R1,GPONPER          NUMBER OF PERIODS DEFINED                    
         CLI   GPONPER,8           GT 8                                         
         BNH   *+6                                                              
         DC    H'0'                DIE UNTILL I EXPAND DATETAB, ETC             
*                                                                               
         STC   R1,DTENUM           SAVE NUMBER OF PERIODS                       
         LA    R4,DATETAB                                                       
         LA    R3,BINUMS                                                        
         XC    DATETAB(DATELNQ),DATETAB                                         
         XC    BINUMS(BIWKLNQ),BINUMS                                           
         LA    R2,GPOPERS                                                       
*                                                                               
         USING GPOPERD,R2                                                       
NEWC10   MVC   0(1,R3),GPOPNWRK                                                 
         MVC   0(3,R4),GPOPEND                                                  
         LA    R2,GPOPLN1Q(R2)                                                  
         LA    R3,1(R3)                                                         
         LA    R4,3(R4)                                                         
         BCT   R1,NEWC10                                                        
*                                                                               
* NOTE FOLLOWING WON'T WORK IF BINUMS ENTRY IS GT 9                             
*                                                                               
         LA    R1,NPRWKS                                                        
         LA    R2,BINUMS                                                        
         LA    R3,PRWKS            CONVERT BINARY TO EBCDIC                     
*                                                                               
NEWC20   MVI   0(R3),X'F0'                                                      
         MVN   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,NEWC20                                                        
         B     NEWCX                                                            
*                                                                               
NEWC30   DS    0H                                                               
         GOTO1 ACAL,DMCB,(RC)      GENERATE THE REPORT CALENDAR                 
NEWCX    XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SUB-ROUTINE TO BUILD CALENDAR VALUES FOR REPORT                    *          
* ON EXIT, BINUMS CONTAINS NUMBER OF WORKING DAYS IN EACH            *          
*          WEEK OF MONTH                                             *          
*          DATETAB CONTAINS LAST DAY IN THE FIRST FOUR WEEK          *          
*          OF MONTH                                                  *          
*          NDAYS=N'DAYS IN MONTH  NWKDYS=N'WORKING DAYS IN MONTH     *          
*          PRWKS=PRINTABLE N'DAYS IN EACH WEEK                       *          
**********************************************************************          
         SPACE 1                                                                
CAL      NMOD1 0,**CAL**                                                        
         L     RC,0(R1)                                                         
         MVC   WORK,MOSSTR         PACKED YYMM                                  
         MVI   WORK+2,X'01'        DD FOT GETDAY                                
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+10)                                 
         GOTO1 GETDAY,DMCB,WORK+10,WORK   FIRST DAY OF WEEK INTO WORK           
*                                                                               
         ZAP   DUB,=P'0'                                                        
         MVO   DUB(8),MOSEND+1(1)  PACKED MONTH                                 
         CVB   R1,DUB              NOW BINARY                                   
         BCTR  R1,0                MAKE IT AN OFFSET                            
         LA    R2,MONTAB           TABLE OF DAYS IN MONTH                       
         AR    R2,R1                                                            
         XC    NDAYS(2),NDAYS      CLEAR THE # OF DAYS/WKDY IN MNTH FLD         
         MVC   NDAYS,0(R2)         MOVE IN NUMBER OF DAYS IN MONTH              
         CLI   MOSEND+1,2          IS THIS FEB                                  
         BNE   CAL10               NO, I'M OK                                   
*                                                                               
         ZAP   DUB,=P'0'           CHECH FOR LEAP YEAR                          
         MVO   DUB(8),MOSEND(1)    PACK THE YEAR                                
         DP    DUB,=PL2'4'         LEAP YEARS ARE EVENLY DEVISABLE BY 4         
         CP    DUB+6(2),=P'0'      ANY REMAINDER?                               
         BNE   CAL10               YES-NOT A LEAP YEAR                          
         MVI   NDAYS,29            LEAP                                         
*                                                                               
* IF NO REQUEST AS OF DATE SPECIFIED BUT THEY REQUESTED THE                     
* REPORT WITH A MOS, USE LAST DATE OF MOS MONTH AS ASOF DATE                    
* THUS, CURRENT MONTH REPORTS SHOULD NOT BE REQUESTED WITH A MOS                
*                                                                               
CAL10    CLC   QEND,SPACES         DID THEY SPECIFY AN ASOF DATE                
         BNE   CAL20               YES, USE IT                                  
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         CLI   ACMMEND,X'FF'          ANY MOS END                               
         BE    CAL20               NO, DATES ARE SET                            
         CLC   ACMMEND,RUNDTE         IS REQUEST FOR THIS MONTH                 
         BE    CAL20               YES, LEAVE ASOF AS TODAY                     
*                                                                               
         MVC   RUNDTE,ACMMEND         AS OF YYMM OF MOS REQUESTED               
         SR    R1,R1                                                            
         IC    R1,NDAYS            CONVERT BINARY TO PACKED                     
         CVD   R1,DUB                                                           
         MVO   HALF(2),DUB+6(2)    MAKE X'0XXC' INTO X'XXC0'                    
         MVC   RUNDTE+2(1),HALF    LAST DAY OF MOS MONTH                        
*                                                                               
CAL20    LA    R2,DAYTABL          MONACCS TABLE OF DAYS                        
         LA    R3,NUMDAYTB         TABLE OF DAYS IN WEEK 1                      
         LA    R4,LASTTAB          TABLE OF DAYS IN WEEK 5                      
         LA    R1,7                                                             
CAL30    CLC   WORK(3),0(R2)       WORK IS FIRST DAY OF MONTH                   
         BE    CAL40                                                            
         LA    R2,3(R2)            NEXT DAY                                     
         LA    R3,1(R3)            FIRST WEEK TABLE                             
         LA    R4,4(R4)            LAST WEEK TABLE                              
         BCT   R1,CAL30                                                         
         DC    H'0'                                                             
CAL40    MVC   BIWK1,0(R3)         SAVE # OF DAYS IN FIRST WEEK                 
*                                                                               
         LA    R1,28               GET # OF DAYS IN LAST WEEK                   
         LA    R0,4                                                             
CAL50    CLM   R1,1,NDAYS                                                       
         BE    CAL60                                                            
         LA    R4,1(R4)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CAL50                                                         
*                                                                               
CAL60    MVC   BIWK5,0(R4)            STORE DAYS IN WEEK 5                      
         MVC   BIWKLST,BIWK5          SAVE 5TH WEEK AS LAST WEEK                
         MVC   BIWKMID(3),=X'050505'  MAKE MIDDLE THREE WEEKS 5                 
         LA    R0,5                RECALC DAYS IN MONTH                         
         SR    R1,R1               FROM PHYSICAL DAYS TO WORKING DAYS           
         SR    R3,R3                                                            
         LA    R2,BINUMS                                                        
CAL70    IC    R1,0(R2)                                                         
         AR    R3,R1                                                            
         LA    R2,1(R2)                                                         
         BCT   R0,CAL70                                                         
         STC   R3,NWKDYS                                                        
*                                                                               
         LA    R1,NPRWKS                                                        
         LA    R2,BINUMS                                                        
         LA    R3,PRWKS            CONVERT BINARY TO EBCDIC                     
         IC    R4,0(R2)                                                         
CAL80    MVI   0(R3),X'F0'                                                      
         MVN   0(1,R3),0(R2)                                                    
         LA    R2,1(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R1,CAL80                                                         
*                                                                               
* CREATE BUCKET DATES                                                           
* NOTE: THE NUMBER OF DAYS IN WEEK CALCULATED UP TO THE FIRST                   
* FRIDAY OF THE MONTH, THE REVENUE FOR WEEK ONE IS ALL REVENUE                  
* THRU THE FIRST SUNDAY AFTER THE FIRST FRIDAY, CAPISE                          
*                                                                               
         LA    R2,DATETAB          PRIME DATETAB WITH THIS MONTH                
         LA    R1,NDATES           NUMBER OF DATES                              
         MVC   0(2,R2),MOSEND                                                   
         LA    R2,3(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BIWK1                                                         
         CLC   WORK(3),=C'SUN'     MONTH START ON SUNDAY                        
         BNE   *+8                                                              
         LA    R1,1(R1)            ADD 1 DAY                                    
         CLC   WORK(3),=C'SAT'     MONTH START ON SATURDAY                      
         BNE   *+8                                                              
         LA    R1,2(R1)            ADD 2 DAYS                                   
*                                                                               
         CLI   PRFORMAT,C'A'       MONDAY THRU FRIDAY WEEK?                     
         BNE   *+8                 NO USE SAT THRU MONDAY                       
         LA    R1,2(R1)            ADD 2 FOR WEEKEND                            
*                                                                               
         LA    R2,DATETAB                                                       
         LA    R0,NDATES                                                        
CAL90    CVD   R1,DUB                                                           
         MVO   HALF(2),DUB+6(2)    MAKE X'0XXC' INTO X'XXC0'                    
         MVC   2(1,R2),HALF        SAVE THE XX AS YYMMXX                        
         LA    R1,7(R1)                                                         
         LA    R2,3(R2)                                                         
         BCT   R0,CAL90                                                         
*                                                                               
         LA    R0,7                GET NUMBER OF WORKING DAYS IN THIS           
         SR    R1,R1               REQUEST                                      
         LA    R2,DAYTABL                                                       
CAL100   CLC   0(3,R2),WORK        GET FIRST DAY OF MONTH NUMBER                
         BE    CAL110                                                           
         LA    R2,3(R2)                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,CAL100                                                        
         DC    H'0'                                                             
*                                                                               
CAL110   STC   R1,BYTE             SAVE DAY NUMBER IN BYTE                      
         ZAP   DUB,=P'0'                                                        
         MVO   DUB(8),RUNDTE+2(1)  PACKED DAY                                   
         CVB   R3,DUB              NOW BINARY                                   
         BCTR  R3,0                MAKE IT AN OFFSET                            
         MH    R3,=H'7'                                                         
         ZIC   R1,BYTE             RESTORE DAY NUMBER                           
         AR    R3,R1               R3= OFFSET INTO TABLE FOR WORK DAYS          
         LA    R2,WORKTAB          THIS MONTH                                   
         AR    R2,R3                                                            
         MVC   NWKDYS,0(R2)       SAVE # OF WORK DAYS FOR THIS REQUEST          
*                                                                               
*  ADJUST NUMBERS IF THERE IS A FEDERAL HOLIDAY IN THIS MONTH                   
*                                                                               
         LA    R3,HOLTAB                                                        
         L     R2,VEXTRAS                                                       
         USING RUNXTRAD,R2                                                      
         L     R2,ADMASTD                                                       
         USING MASTD,R2                                                         
         CLI   MCAGCTRY,CTRYCAN    IS THIS A CANADIAN AGENCY                    
         BNE   *+8                 NO                                           
         LA    R3,HOLTABCA         USE THE LIST OF CANADIAN HOLIDAYS            
*                                                                               
CAL120   CLI   0(R3),X'FF'         TEST FOR EOT                                 
         BE    CALX                                                             
*                                                                               
         CLC   RUNDTE(1),0(R3)   FIRST FIND YEAR IN TABLE                       
         BE    CAL130                                                           
         SR    R0,R0                                                            
         IC    R0,1(R3)            GET NEXT TABLE ENTRY                         
         AR    R3,R0                                                            
         B     CAL120                                                           
*                                                                               
CAL130   SR    R0,R0                                                            
         IC    R0,1(R3)            MAKE R0 # OF DATES IN TABLE                  
         SH    R0,=H'2'            ADJUST LENGTH FOR YEAR/LENGTH                
         SRL   R0,1                DIVIDE BY TWO                                
         LA    R3,2(R3)            POINT TO FIRST HOLIDAY                       
*                                                                               
         CLC   RUNDTE+1(1),0(R3)   MATCH ON MONTH OF HOLIDAY                    
         BE    CAL150              YES                                          
CAL140   LA    R3,2(R3)                                                         
         BCT   R0,*-14             DO NEXT COMPARE                              
         B     CALX                NO FEDERAL HOLIDAYS THIS MONTH               
*                                                                               
CAL150   CLC   RUNDTE+2(1),1(R3)   MATCH ON DAY OF HOLIDAY                      
         BL    CAL160              HOLIDAY NOT INCLUDED HERE                    
         SR    RE,RE                                                            
         IC    RE,NWKDYS                                                        
         BCTR  RE,0                DECREMENT N'WORKING DAYS IN MO.              
         STC   RE,NWKDYS                                                        
*                                                                               
CAL160   LA    R2,NDATES                                                        
         LA    RE,DATETAB          DECREMENT DAYS IN HOLIDAYS'S                 
         LA    RF,BINUMS           WEEK                                         
         LA    R1,PRWKS                                                         
*                                                                               
CAL170   CLC   0(2,R3),1(RE)       TEST AGAINST MONTH AND DAY                   
         BNH   CAL180                                                           
         LA    RE,L'DATETAB(RE)                                                 
         LA    RF,L'BINUMS(RF)                                                  
         LA    R1,L'PRWKS(R1)                                                   
         BCT   R2,CAL170                                                        
*                                                                               
CAL180   SR    R4,R4                                                            
         IC    R4,0(RF)                                                         
         BCTR  R4,0                DECREMENT N'DAYS IN WEEK                     
         STC   R4,0(RF)                                                         
         MVN   0(1,R1),0(RF)                                                    
         B     CAL140                                                           
*                                                                               
CALX     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* TABLES FOR DATE CALCULATION (DAYS IN WEEK1, MONTHS)                *          
**********************************************************************          
         SPACE 1                                                                
NUMDAYTB DC    AL1(5,4,3,2,1,5,5)                                               
*                                                                               
MONTAB   DC    AL1(31,28,31,30,31,30,31,31,30,31,30,31)                         
*                                                                               
         DS    0H                                                               
LASTTAB  DC    AL1(0,1,2,3)       MON                                           
         DC    AL1(1,2,3,4)       TUE       DAYS IN LAST WEEK OF                
         DC    AL1(2,3,4,5)       WED       MONTH                               
         DC    AL1(3,4,5,5)       THU                                           
         DC    AL1(4,5,5,5)       FRI                                           
         DC    AL1(0,0,0,1)       SAT                                           
         DC    AL1(0,0,1,2)       SUN                                           
*                                                                               
* TABLE OF WORKING DAYS                                                         
* FIRST DAY OF MONTH BY RUN DATE                                                
WORKTAB  DC    AL1(1,1,1,1,1,0,0) MON TUE WED ...                 1             
         DC    AL1(2,2,2,2,1,0,1)                                 2             
         DC    AL1(3,3,3,2,1,1,2)                                 3             
         DC    AL1(4,4,3,2,2,2,3)                                 4             
         DC    AL1(5,4,3,3,3,3,4)                                 5             
         DC    AL1(5,4,4,4,4,4,5)                                 6             
         DC    AL1(5,5,5,5,5,5,5)                                 7             
         DC    AL1(6,6,6,6,6,5,5)                                 8             
         DC    AL1(7,7,7,7,6,5,6)                                 9             
         DC    AL1(8,8,8,7,6,6,7)                                10             
         DC    AL1(9,9,8,7,7,7,8)                                11             
         DC    AL1(10,9,8,8,8,8,9)                               12             
         DC    AL1(10,9,9,9,9,9,10)                              13             
         DC    AL1(10,10,10,10,10,10,10)                         14             
         DC    AL1(11,11,11,11,11,10,10)                         15             
         DC    AL1(12,12,12,12,11,10,11)                         16             
         DC    AL1(13,13,13,12,11,11,12)                         17             
         DC    AL1(14,14,13,12,12,12,13)                         18             
         DC    AL1(15,14,13,13,13,13,14)                         19             
         DC    AL1(15,14,14,14,14,14,15)                         20             
         DC    AL1(15,15,15,15,15,15,15)                         21             
         DC    AL1(16,16,16,16,16,15,15)                         22             
         DC    AL1(17,17,17,17,16,15,16)                         23             
         DC    AL1(18,18,18,17,16,16,17)                         24             
         DC    AL1(19,19,18,17,17,17,18)                         25             
         DC    AL1(20,19,18,18,18,18,19)                         26             
         DC    AL1(20,19,19,19,19,19,20)                         27             
         DC    AL1(20,20,20,20,20,20,20)                         28             
         DC    AL1(21,21,21,21,21,20,20)                         29             
         DC    AL1(22,22,22,22,21,20,21)                         30             
         DC    AL1(23,23,23,22,21,21,22)                         31             
*                                                                               
HOLTAB   DS    0C                                                               
         DC    X'90',AL1(18),X'01010219053007040903112211231225'                
         DC    X'91',AL1(18),X'01010218052707040902112811291225'                
         DC    X'92',AL1(18),X'01010217052507030907112611271225'                
         DC    X'93',AL1(18),X'01010215053107050906112511261224'                
         DC    X'94',AL1(18),X'01030221053007040905112411251226'                
         DC    X'95',AL1(18),X'01020220052907040904112311241225'                
         DC    X'FF'                                                            
*                                                                               
HOLTABCA DS    0C                  CANADIAN HOLIDAYS                            
         DC    X'91',AL1(22),X'0101032905200624070108050912101412251226X        
               '                                                                
         DC    X'92',AL1(22),X'0101041705180701080309071012122512251228X        
               '                                                                
         DC    X'93',AL1(20),X'010104090524070108020906101112271228'            
         DC    X'94',AL1(20),X'010304080523070108010905101012261227'            
         DC    X'95',AL1(20),X'010204140522070308010904100912251226'            
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* TABLES OF 1R NAME DATA, KEPT AT EACH LEVEL                         *          
* TABLE IS-H(NUMBER IN TABLE), Y(MAX IT TABLE), C(TABLE DATA)        *          
* TABLE DATA IS COVERED BY LISTD DSECT, BUILT BY BUILDLST            *          
**********************************************************************          
         SPACE 1                                                                
LVBLIST  DS    0H                                                               
         DS    H                                                                
         DC    Y(LVBMAX)                                                        
         DC    Y(0)                                                             
         DS    (LVBMAX*LISTDLN)C                                                
LVBMAX   EQU   255                                                              
*                                                                               
LVALIST  DS    0H                                                               
         DS    H                                                                
         DC    Y(LVAMAX)                                                        
         DC    Y(0)                                                             
         DS    (LVAMAX*LISTDLN)C                                                
LVAMAX   EQU   50                                                               
         EJECT                                                                  
*********************************************************************           
* DSECT FOR STORAGE AREA                                            *           
*********************************************************************           
         SPACE 1                                                                
ACR5D    DSECT                                                                  
MYDUB    DS    D                                                                
ATYPES   DS    0A                                                               
ACLITAB  DS    A                   CLIENT TABLE BINSEARCH PARAMETERS            
AOFFNAME DS    A                   LEVEL B LIST                                 
ALVALST  DS    A                   LEVEL A LIST                                 
ACAL     DS    A                   OLD CALENDAR ROUTINE                         
ANEWCAL  DS    A                   NEW CALENDAR ROUTINE                         
ABLDPRO  DS    A                   BUILD PROFILE ROUTINE                        
UNDERLIN DS    V                                                                
ACLIST   DS    V                                                                
CENTER   DS    V                                                                
VGETPER  DS    V                   GET PERIOD INFO                              
VGETSTD  DS    V                   GET STDHRS INFO                              
ATYPLNQ  EQU   *-ATYPES                                                         
*                                                                               
ACALKEYS DS    A                   A(NEW CALENDER KEY TABLE)                    
ASTDKEYS DS    A                   A(STD HRS KEY TABLE)                         
ACALREC  DS    A                   A(CALENDER RECORD) (FOR GETSTD)              
ASTDHRS  DS    A                   A(STDHRS O/P BUFFER)                         
ADBOX    DS    A                                                                
ALOWTAB  DS    A                   ADDRESS OF LOWEST TABLE I NEED               
DISP2    DS    H                   DATADISP2                                    
*                                                                               
FLGCST   DS    XL1                 FLAG FOR COST (NEW/OLD)                      
FLGNUCST EQU   X'80'               COMPANY ON NEW COST                          
*                                                                               
NUMNON   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
PREVSVG  DS    CL(L'LEVACDE+1)     PREVIOUS LVA/SERVICE GROUP                   
MOSEND   DS    CL2                 MOS START                                    
MOSSTR   DS    CL2                 MOS END                                      
BILLED   DS    CL1                 FULLY BILLED SWITCH                          
RUNDTE   DS    CL3                 RUN DATE TO FILTER TRANSACTIONS              
ASOFPRNT DS    CL8                 ASOF DATE TO PRINT IN REPORT HEADER          
MOSPRNT  DS    CL6                 ASOF DATE TO PRINT IN REPORT HEADER          
MOS      DS    CL2                 MONTH OF SERVICE FROM SJ TRANS               
M1START  DS    PL2                 MOS START DATE PACKED                        
M1END    DS    PL2                 MOS END DATE PACKED                          
CURMON   DS    CL3                 PACKED CURRENT MONTH                         
CURMONP  DS    CL6                 PRINT CURRENT MONTH                          
FRMMONP  DS    CL6                 PRINT "FROM" MONTH                           
TRANSTAT DS    XL1                                                              
GOTACC   EQU   X'80'                                                            
GOTLEVC  EQU   X'40'                                                            
GOTLEVB  EQU   X'20'                                                            
GOTSVGP  EQU   X'10'                                                            
GOTLEVA  EQU   X'08'                                                            
GOTREQ   EQU   X'04'                                                            
*                                                                               
SJOFFLEV DS    CL1                 LEVEL TO PRINT SJ OFFICE NAMES AT            
SVOFF    DS    CL2                 SAVED AREA FOR OFFICE FOR NEWCAL             
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
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
LEVLNQS  DS    0XL4                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
LEVSCDE  DS    0CL6                LEVEL CODES                                  
LEVACDE  DS    CL6                 LEVEL A CODE                                 
LEVSVGP  DS    0CL1                SAVE GROUP                                   
LEVBCDE  DS    CL6                 LEVEL B CODE                                 
LEVCCDE  DS    CL6                 LEVEL C CODE                                 
LEVDCDE  DS    CL7                 LEVEL D CODE                                 
LVCDLNQ  EQU   *-LEVSCDE                                                        
*                                                                               
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVSDSC  EQU   *-LEVSCDE           TOTAL LEVEL CODES AND NAMES                  
*                                                                               
CLILEN   DS    XL1                 LENGTH OF CLIENT LEVEL                       
PROLEN   DS    XL1                 LENGTH OF PRODUCT LEVEL                      
SVDTE    DS    PL3                 SAVED AREA FOR DATE                          
*                                                                               
* BUDGET AREA                      THREE LEVELS, COVERED BY BUDD                
*                                                                               
BUDAREA  DS    0C                                                               
BUSTDHRS DS    XL3                                                              
         DS    PL8                                                              
BUSTDRAT DS    XL3                                                              
         DS    PL8                                                              
BUTRGPCT DS    XL3                                                              
         DS    PL8                                                              
*                                                                               
BUDSAVE  DS    PL8                 SAVED BUDGET AMOUNT                          
*                                                                               
PROFILES DS    0CL16               PROFILES SAVED AT THE LEDGER LEVEL           
PRBRSVGP DS    CL1                 BREAK ON SERVICE GROUP                       
PRNEWPRO DS    CL1                 PRINT NEW BUSINESS/PROBONO                   
PRSTDHR  DS    CL1                 STANDARD HOURS BUD NUM                       
PRSTDRAT DS    CL1                 STANDARD RATE BUD NUM                        
PRTRGPCT DS    CL1                 TARGET 5 BUD NUM GROUP                       
PRFORMAT DS    CL1                 BUCKET FORMAT                                
         DS    CL10                SPARE                                        
PRLVCNAM DS    CL15                                                             
THISLEV  DS    CL15                                                             
PRSVG    DS    CL1                 SERVICE GROUP                                
PRSVGNM  DS    CL36                                                             
*                                                                               
ACCBUCKS DS    0PL8                ACCUMULATORS FOR LOW LEVEL STUFF             
ACCWEEK1 DS    PL8                                                              
ACCWEEK2 DS    PL8                                                              
ACCWEEK3 DS    PL8                                                              
ACCWEEK4 DS    PL8                                                              
ACCWEEK5 DS    PL8                                                              
ACCWEEK6 DS    PL8                                                              
ACCWEEK7 DS    PL8                                                              
ACCWEEK8 DS    PL8                                                              
ACCRTIME DS    PL8                 NUMBER OF PL8'S HERE MUST = NUMBUCKS         
ACCBUDG  DS    PL8                                                              
ACCNEWB  DS    PL8                 NEW BUSINESS                                 
ACCPROB  DS    PL8                 PRO BONO                                     
*                                                                               
BINUMS   DS    0CL1                                                             
BIWK1    DS    CL1                 BINARY NUMBER OF DAYS IN WEEK 1              
BIWKMID  DS    0CL6                WEEKS 2,3,4,5,6,& 7                          
BIWK2    DS    CL1                 BINARY NUMBER OF DAYS IN WEEK 2              
BIWK3    DS    CL1                 BINARY NUMBER OF DAYS IN WEEK 3              
BIWK4    DS    CL1                 BINARY NUMBER OF DAYS IN WEEK 4              
BIWK5    DS    CL1                 BINARY NUMBER OF DAYS IN WEEK 5              
BIWK6    DS    CL1                 BINARY NUMBER OF DAYS IN WEEK 6              
BIWK7    DS    CL1                 BINARY NUMBER OF DAYS IN WEEK 7              
BIWKLST  DS    CL1                 # OF DAYS IN LAST WEEK                       
BIWKLNQ  EQU   *-BINUMS                                                         
*                                                                               
NDAYS    DS    CL1                 # OF DAYS IN MONTH                           
NWKDYS   DS    CL1                 # OF WORKING DAYS IN MONTH                   
*                                                                               
PRWKS    DS    0CL1                                                             
PRWEEK1  DS    CL1                 EBCDIC NUMBER OF DAYS IN WEEK 1              
PRWEEK2  DS    CL1                 DAYS IN WEEKS 2                              
PRWEEK3  DS    CL1                 DAYS IN WEEKS 3                              
PRWEEK4  DS    CL1                 DAYS IN WEEKS 4                              
PRWEEK5  DS    CL1                 DAYS IN WEEKS 5                              
PRWEEK6  DS    CL1                 DAYS IN WEEKS 6                              
PRWEEK7  DS    CL1                 DAYS IN WEEKS 7                              
PRWKLST  DS    CL1                 # OF DAYS IN LAST WEEK                       
NPRWKS   EQU   (*-PRWKS)/L'PRWKS                                                
*                                                                               
SAVEDATE DS    CL2                 MDATE                                        
DTENUM   DS    XL1                 # OF PERIODS RETURNED FROM GETPER            
*                                                                               
CLISTAT  DS    XL1                 CLIENT STATUS BYTE                           
CLINB    EQU   X'80'               CLIENT IS NEW BUSINESS                       
CLIPB    EQU   X'40'               CLIENT IS PRO BONO                           
*                                                                               
DATETAB  DS    0CL3                TABLE OF PACKED DATES FOR                    
DATE1    DS    CL3                 PUTTING REVENUE IN BUCKETS                   
DATE2    DS    CL3                                                              
DATE3    DS    CL3                                                              
DATE4    DS    CL3                                                              
DATE5    DS    CL3                                                              
DATE6    DS    CL3                                                              
DATE7    DS    CL3                                                              
DATE8    DS    CL3                                                              
NDATES   EQU   (*-DATETAB)/L'DATETAB                                            
DATELNQ  EQU   (*-DATETAB)                                                      
*                                                                               
SAVETRDT DS    CL3                 TRNDATE                                      
SAVEHOUR DS    PL(L'TOTBUCKS)                                                   
SAVERATE DS    PL(L'TOTBUCKS)                                                   
SUMTOTAL DS    4PL(L'TOTBUCKS)                                                  
*                                                                               
PL16     DS    PL16                FOR MULTIPLYING RATE                         
*                                                                               
MYMODE   DS    XL1                                                              
SUMMARY  EQU   X'80'                                                            
*                                                                               
MATCHKEY DS    CL35                                                             
SVKEY    DS    CL49                                                             
CLIWRK   DS    CL7                 CLIENT TABLE WORK AREA                       
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
         DS    CL2000              ACTUAL IO                                    
IOLNQ    EQU   *-IO                                                             
         EJECT                                                                  
**********************************************************************          
* DSECT FOR PRINT LINE                                               *          
**********************************************************************          
         SPACE 1                                                                
PRTD     DSECT                                                                  
PRREC    DS    0CL132                                                           
         DS    CL1                                                              
PRNAME   DS    CL20                                                             
         DS    CL1                                                              
PRWK1    DS    CL10                                                             
         DS    CL1                                                              
PRWK2    DS    CL10                                                             
         DS    CL1                                                              
PRWK3    DS    CL10                                                             
         DS    CL1                                                              
PRWK4    DS    CL10                                                             
         DS    CL1                                                              
PRWK5    DS    CL10                                                             
         DS    CL1                                                              
PRTOT    DS    CL10                                                             
         DS    CL1                                                              
PRRATE   DS    CL10                                                             
         DS    CL1                                                              
PRBUD    DS    CL10                                                             
         DS    CL1                                                              
PRRTIME  DS    CL10                                                             
         DS    CL1                                                              
PRPROBON DS    CL10                                                             
         DS    CL1                                                              
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER GETPROFS KEY                                        *          
**********************************************************************          
         SPACE 1                                                                
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
         EJECT                                                                  
***********************************************************************         
* DSECT FOR BINSRCH PARAMETERS                                        *         
***********************************************************************         
         SPACE 1                                                                
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
***********************************************************************         
* DSECT FOR CLIENT TABLE                                              *         
***********************************************************************         
         SPACE 1                                                                
CLITBLD  DSECT                                                                  
CLICDE   DS    CL3                 CLIENT CODE                                  
CLIKLNQ  EQU   *-CLICDE            KEY LENGTH                                   
CLIID    DS    CL1                 CLIENT ID(P-PRO BONO/B-NEW BUSINESS)         
CLILNQ   EQU   *-CLICDE                                                         
         SPACE 1                                                                
***********************************************************************         
* DSECT FOR BUDGET AREA                                               *         
***********************************************************************         
         SPACE 1                                                                
BUDD     DSECT                                                                  
BUDNUM   DS    CL1                                                              
BUDMODE  DS    CL1                                                              
BUTYPE   DS    XL1                 TYPE OF BUDGET                               
BUSTDHR  EQU   X'80'                                                            
BUSTDRT  EQU   X'40'                                                            
BUTRGPC  EQU   X'20'                                                            
BUDAMNT  DS    PL8                                                              
BUDDLN   EQU   *-BUDD                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR LIST TABLE                                                *         
***********************************************************************         
         SPACE 1                                                                
LISTD    DSECT                                                                  
LISTREC  DS    0C                                                               
LISTKEY  DS    CL12                                                             
LISTNAME DS    CL36                                                             
LISTDLN  EQU   *-LISTD                                                          
         SPACE 1                                                                
***********************************************************************         
* DSECT FOR SUBTOTAL TABLE                                            *         
***********************************************************************         
         SPACE 1                                                                
TOTTBLD  DSECT                                                                  
TOTLEV   DS    AL1                 SUB TOTAL LEVEL                              
TOTFMODE DS    AL1                 MODE TO ZAP THIS BUCKET                      
TOTLMODE DS    AL1                 MODE TO PRINT THIS BUCKET                    
TOTTLEN  DS    CL1                 LENGTH OF TOTTYPE                            
TOTTYPE  DS    CL15                SUB TOTAL TYPE (FROM LEDGER HEIR EL)         
TOTBUCKS DS    0PL8                A(FIRST BUCKET)                              
TOTWEEK1 DS    PL8                 REVENUE PER WEEK                             
TOTWEEK2 DS    PL8                                                              
TOTWEEK3 DS    PL8                                                              
TOTWEEK4 DS    PL8                                                              
TOTWEEK5 DS    PL8                                                              
TOTWEEK6 DS    PL8                                                              
TOTWEEK7 DS    PL8                                                              
TOTWEEK8 DS    PL8                                                              
TOTRTIME DS    PL8                 UNBILLABLE HOURS                             
TOTBUDG  DS    PL8                 BUDGET TOTAL                                 
TOTNEWB  DS    PL8                 NEW BUSINESS                                 
TOTPROB  DS    PL8                 PRO BONO COMPANY                             
TOTBKLNQ EQU   *-TOTBUCKS          TOTAL LENGTH OF BUCKETS                      
TOTTBLLN EQU   *-TOTTBLD                                                        
NUMBUCKS EQU   (*-TOTBUCKS)/L'TOTBUCKS                                          
         EJECT                                                                  
***********************************************************************         
* ++INCLUDES                                                          *         
***********************************************************************         
         SPACE 1                                                                
* ACGETSTDD                                                                     
* ACPERCALLD                                                                    
       ++INCLUDE ACGETSTDD                                                      
       ++INCLUDE ACPERCALLD                                                     
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
         SPACE 1                                                                
BUFFSIZE EQU   (GSBUFSZQ+GPBUFSZQ+IOLNQ+GSOPLENQ)                               
         EJECT                                                                  
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
         SPACE 1                                                                
* DDLOGOD                                                                       
* ACGENFILE                                                                     
* ACGENPOST                                                                     
* ACREPWORKD                                                                    
* ACGENMODES                                                                    
* ACREPPROFD                                                                    
* ACMASTD                                                                       
* ACBIGPRNTD                                                                    
* DDBIGBOX                                                                      
* DDCNTRL                                                                       
* DDREPXTRAD                                                                    
* DDREPMASTD                                                                    
* DDBOXEQUS                                                                     
* DDREMOTED                                                                     
GETOPTD  DSECT                                                                  
       ++INCLUDE ACGOBLOCK                                                      
       ++INCLUDE DDCTRYEQUS                                                     
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPPROFD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPR502 12/11/09'                                      
         END                                                                    
