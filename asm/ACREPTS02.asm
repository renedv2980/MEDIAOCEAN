*          DATA SET ACREPTS02  AT LEVEL 026 AS OF 12/11/09                      
*PHASE ACTS02A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE '- TIMESHEET SUMMARY REPORT'                                     
ACTS02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACTS**,R9                                                    
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACTSD,RC            RC=A(LOCAL W/S)                              
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,PROCRCVR       PROCESS RECOVERY FILE RECORD                 
         BE    PREC                                                             
         CLI   MODE,RUNFRST        FIRST FOR RUN                                
         BE    RUNF                                                             
         CLI   MODE,RUNLAST        LAST FOR RUN                                 
         BE    RUNL                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* RUN FIRST ROUTINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         MVI   RCSUBPRG,1                                                       
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         LA    R0,PKFLDLNQ         NUMBER OF PACKED FIELDS                      
         LA    R1,PKFLDS           R1=A(PACKED FIELDS)                          
         ZAP   0(L'PKFLDS,R1),=P'0'     CLEAR FIELDS                            
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         XC    SVSEQ,SVSEQ         SET SEQUENCE NUMBER                          
*                                                                               
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
*                                                                               
         LA    R5,TIMELNQ          GETMAIN AREA FOR SORTING ELEMENT             
         L     RE,=A(TIMEMAX)      GROUPS WITHIN A PERSON CODE                  
         MR    R4,RE                                                            
         ST    R5,TIMETBLN                                                      
         L     R0,TIMETBLN                                                      
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ATIMETAB         START OF AREA                                
*                                                                               
         USING BIND,RE                                                          
         L     RE,ATIMETAB                                                      
         XC    BININ,BININ         CLEAR BIN TABLE                              
         LA    R1,TIMELNQ                                                       
         STCM  R1,15,BINLEN        ENTRY LENGTH                                 
         XC    BINDISP,BINDISP     DISPLACEMENT TO KEY                          
         LA    R1,TIMKLNQ                                                       
         STC   R1,BINKEY+2         LENGTH OF KEY                                
         L     R1,=A(TIMEMAX)                                                   
         STCM  R1,15,BINMAX        MAXIMUM NUMBER OF ENTRIES                    
         XC    BINNUM,BINNUM       NUMBER OF BUCKETS                            
         XC    BINFST,BINFST       DISPLACEMENT TO FIRST BUCKET                 
         DROP  RE                                                               
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
         ST    R1,MCUSRDMP         SAVE   NEW START                             
         A     R1,TIMETBLN         LENGTH OF PHASE + R1=(GETMAIN)               
         ST    R1,MCUSRDMP+4                                                    
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         DROP  RF                                                               
*                                                                               
         MVC   LEVSCDE(LEVSLNQ),SPACES     CLEAR LEVEL CODES AREA               
RUNFX    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS A RECOVERY FILE RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
PREC     DS    0H                                                               
         L     R2,ADTRANS                                                       
         USING RCVRECD,R2             R2=A(RECOVERY HEADER)                     
         CLI   RCVPRGNO,RCVPCAPQ      TEST TIMESHEET PROGRAM                    
         BE    PREC10                                                           
         CLI   RCVPRGNO,RCVPCBLQ      OR CBILL PROGRAM                          
         BNE   PRECX                                                            
                                                                                
PREC10   CLI   RCVFILTY,RCVFAMST      TEST ACCMST RECORD                        
         BE    PREC20                                                           
         CLI   RCVFILTY,RCVFAACC      TEST ACCFIL RECORD                        
         BNE   PRECX                                                            
         GOTO1 VACCEMU,DMCB,EMUOLDN,,,RCVRECRD                                  
         ORG   *-2                                                              
         LR    R2,R1                  (EMU USES R2 FOR PLIST ADDRESS)           
         O     R2,=X'FF000000'                                                  
         BASR  RE,RF                                                            
         L     R2,ADTRANS                                                       
                                                                                
PREC20   DS    0H                                                               
         CLI   RCVRECTY,RCVRADDQ      ONLY ADDS                                 
         BNE   PRECX                                                            
*                                                                               
         USING TIMRECD,R3             R3=A(TRANSACTION KEY)                     
         LA    R3,RCVRECRD            IF THERE ARE NO TIMELS (X'8B')            
         TM    TIMRSTA,X'80'          IF RECORD IS MARKED DELETED               
         BO    PRECX                  DON'T PROCESS                             
         CLC   TIMKREF,=C'*TIME*'  IS IT A TIME RECORD?                         
         BNE   PRECX                                                            
         CLI   0(R3),TSWKTYPQ         SKIP PASSIVE POINTERS                     
         BE    PRECX                                                            
         CLI   0(R3),CAHKTYPQ         SKIP COST ALLOC HISTORY RECS              
         BE    PRECX                                                            
*                                                                               
*        LR    R4,R3                  DON'T NEED TO PROCESS THE RECORD          
*        MVI   ELCODE,TIMELQ                                                    
*        BAS   RE,GETEL                                                         
*        BNE   PRECX                                                            
*                                                                               
         USING TIMELD,R4                                                        
         LA    R4,TIMRFST                                                       
*                                                                               
         XC    SVPID,SVPID         SAVED AREA FOR PID NUMBER                    
         LR    R6,R4               LOOP THROUGH TIME REC FOR PIDEL              
PREC30   CLI   0(R6),0             AT THE END OF THE RECORD?                    
         BE    PREC50                                                           
         CLI   0(R6),PIDELQ                                                     
         BE    PREC40                                                           
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     PREC30                                                           
*                                                                               
         USING PIDELD,R6                                                        
PREC40   MVC   SVPID,PIDNO         SAVE PID NUMBER TO FILL IN LINE              
         DROP  R6                                                               
*                                                                               
PREC50   CLI   0(R4),0             AT THE END OF THE ELEMENT                    
         BE    PRECX                                                            
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   PREC99                   ELSE LOOP                               
         CLI   TIMETYP,TIMEINP     ONLY PROCESS INPUT DETAILS                   
         BNE   PREC99                   ELSE LOOP                               
*                                                                               
         USING TIMED,R5                                                         
         LA    R5,TIMEWRK                                                       
         MVC   TIMEWRK,SPACES                                                   
*                                                                               
         MVC   TIME1RA,TIMKACT     MOVE 1R ACCOUNT TO BINTABLE                  
         MVC   TIMECPY,TIMKCPY     COMPANY CODE FOR LATER READS                 
         MVC   TIMEPEDT,TIMKPEDT   PERIOD END DATE                              
         MVC   TIMESEQ,SVSEQ                                                    
         SR    R1,R1                                                            
         ICM   R1,8,SVSEQ                                                       
         LA    R1,R1                                                            
         STCM  R1,8,SVSEQ                                                       
*                                                                               
         XC    TIMETLN#,TIMETLN#   TEMPO LINE NUMBER                            
         XC    TIMEAPDT,TIMEAPDT   ACTUAL START DATE                            
         XC    TIMESTA,TIMESTA     CLEAR STATUS BYTE                            
         TM    TIMSTAT,TIMTEMPO    IS THIS A TEMPO LINE                         
         BNO   *+8                                                              
         OI    TIMESTA,TIMTEMPO    SET ISFROMTEMPO BIT                          
         LR    R6,R4               SAVE OFF R4                                  
PREC60   SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1               BUMP TO TEMPO EXTRA                          
         CLI   0(R4),0             AT THE END OF THE RECORD?                    
         BE    PREC70                                                           
         CLI   TIMEL,TIMELQ        MAKE SURE WE ARE AT AN 8B                    
         BNE   PREC60                                                           
         CLI   TIMETYP,TIMEXTRA    IS THE THE TEMPO EXTRA?                      
         BNE   PREC60                                                           
         MVC   TIMETLN#,TIMXTLN#   TEMPO LINE #                                 
         MVC   TIMEAPDT,TIMXTPDT   ACTUAL PERIOD END DATE                       
PREC70   LR    R4,R6               RESET R4                                     
*                                                                               
PREC80   OC    TIMESTA,TIMIND      TURN ON INDICATOR BITS                       
         MVC   TIMEPID,SVPID                                                    
         MVC   TIMETTYP,TIMTTYP    TYPE OF TIME                                 
         MVC   TIMELN#,TIMLINE#    TMS LINE NUMBER                              
         MVC   TIMETSK,TIMTSK      TASK CODE                                    
         MVC   TIMEPOFF,TIMOFF     PRODUCTION OFFICE                            
         MVC   TIMEMOA,TIMMOA      MONTH OF ACTIVITY                            
         MVC   TIMEADTE,TIMADAT    ACTIVITY DATE                                
         MVC   TIMEACT,TIMACC      SJ/1N ACCOUNT                                
         ZAP   TIMEBKT,TIMHRS      HOURS                                        
         CLC   TIMKCUNT(2),=C'1C'  DO NOT GIVE THEM 1N ACCOUNTS                 
         BNE   *+10                                                             
         MVC   TIMECAC,TIMKCACT    1C CONTRA                                    
*                                                                               
         ZAP   TIMERTE,=P'0'                                                    
         CLI   TIMLN,TIMILN1Q      ANY BILLABLE INFO?                           
         BNH   PREC90                                                           
         MVC   TIMEINC,TIMINC      INCOME ACCOUNT                               
         MVC   TIMERTE,TIMRATE     RATE                                         
*                                                                               
PREC90   DS    0H                                                               
         GOTO1 ABINADD,DMCB,(RC),TIMEWRK,ATIMETAB   ADD TABLE ENTRY             
*                                                                               
PREC99   SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R4,R1                                                            
         B     PREC50                                                           
*                                                                               
PRECX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R3,R4,R5                                                      
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
RUNL     DS    0H                                                               
         LA    R5,XP                                                            
         MVC   PRTLINE(PLINELNQ),XSPACES                                        
*                                                                               
         USING BIND,R1                                                          
         L     R1,ATIMETAB         R1=A(TRANSACTION TABLE)                      
         ICM   R3,15,BININ                                                      
         BZ    RUNLX                                                            
         USING TIMED,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         XC    LSTBINKY,LSTBINKY   CLEAR SAVED AREA FOR KEY COMPARE             
         XC    LSTTLN#,LSTTLN#     CLEAR SAVED AREA FOR TEMPO LINE#             
         MVI   FLAG,0                                                           
RUNL10   DS    0H                                                               
         CLI   QOPT1,C'Y'          ELIMINATE ALL NONE TEMPO PEOPLE?             
         BNE    *+14                                                            
         OC    TIMEPID,TIMEPID     IF NO PID SKIP T/S                           
         BZ    RUNL100                                                          
         CLC   LSTBINKY,TIMEKEY    SAME 1R ACT/TIMESHEET?                       
         BE    RUNL20                                                           
         NI    FLAG,X'FF'-FLGTMP                                                
         BAS   RE,GETCPY           GET COMPANY INFO                             
         BAS   RE,GETLDG           READ LEDGER RECORD AND GETLEVELS             
         BAS   RE,SETCDE           SET LEVEL CODES                              
         BAS   RE,READACC          READ ACCOUNT RECORD                          
         BAS   RE,GETTMP           GET TEMPO INFO (IF ANY)                      
*                                                                               
         CLI   QOPT2,C'Y'          ARE WE DOWNLOADING?                          
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
         TM    TIMESTA,TIMETMP     IS THIS TIMESHEET A TEMPO T/S                
         BNO   RUNL20                                                           
         OI    FLAG,FLGTMP         SET FLAG FOR HEADLINES/DWNLD                 
*                                                                               
RUNL20   OC    TIMETLN#,TIMETLN#   ANY TEMPO LINE NUMBER?                       
         BZ    RUNL30                  NO - SKIP CHECKING                       
         CLC   LSTTLN#,TIMETLN#    SAME TEMPO LINE NUMBER?                      
         BNE   RUNL30                                                           
         CLC   LSTBINKY,TIMEKEY    SAME 1R ACT/TIMESHEET?                       
         BNE   RUNL30                                                           
         MVC   PDUP,=C'**DUP**'    MARK LINE AS DUPLICATE                       
         OI    FLAG,FLGDUP         SET FLAG FOR DUP                             
*                                                                               
RUNL30   MVC   LSTTLN#,TIMETLN#    SAVE OFF TEMPO LINE NUM FOR NEXT CLC         
         MVC   LSTBINKY,TIMEKEY    SAVE OFF FOR LATER COMPARE                   
         EDIT  TIMELN#,PLN#        TMS LINE NUMBER                              
         EDIT  TIMETLN#,PTMPLN#    TEMPO LINE NUMBER                            
*        GOTO1 DATCON,DMCB,(1,TIMEADTE),(X'20',PADDDTE)  ACTIVITY DATE          
         OC    SVSTPDT,SVSTPDT     ANY ACTUAL START DATE?                       
         BZ    RUNL40                                                           
         GOTO1 DATCON,DMCB,(1,SVSTPDT),(X'20',PASTDTE)  ACTUAL ST DTE           
RUNL40   OC    TIMEAPDT,TIMEAPDT   ANY ACTUAL END DATE?                         
         BZ    RUNL50                                                           
         GOTO1 DATCON,DMCB,(1,TIMEAPDT),(X'20',PAENDTE) ACTUAL END DTE          
*                                                                               
RUNL50   CLC   TIMEAUL,=C'SJ'      1N OR SJ?                                    
         BNE   RUNL60                                                           
         MVC   PCLI,TIMEACLI       CLIENT CODE                                  
         MVC   PPROD,TIMEAPRD      PRODUCT CODE                                 
         MVC   PJOB,TIMEAJOB       JOB CODE                                     
         B     *+10                                                             
RUNL60   MVC   P1NACT,TIMEACT+2    1N ACCOUNT                                   
         MVC   PTASK,TIMETSK       TASK CODE                                    
         MVC   PCONTRA,TIMECAC     CONTRA ACCOUNT                               
         GOTO1 DATCON,DMCB,(1,TIMEPEDT),(8,SVPEDT)                              
*                                                                               
         OC    TIMEPID,TIMEPID     ANY PID?                                     
         BZ    RUNL70                                                           
         LA    R1,PKPIDCNT         INCREMENT EITHER TMS PID                     
         TM    TIMESTA,TIMTEMPO    IS IT A TEMPO TIMESHEET?                     
         BNO   *+8                                                              
         LA    R1,PKTPDCNT            OR TEMPO PID REC COUNTER                  
         AP    0(L'PKFLDS,R1),=P'1'                                             
         EDIT  TIMEPID,PPID                                                     
*                                                                               
RUNL70   CLC   TIMERTE,SPACES                                                   
         BE    RUNL80                                                           
         EDIT  TIMERTE,PRATE,2,FLOAT=-                                          
RUNL80   MVC   PINCACT,TIMEINC     INCOME ACCOUNT                               
         MVC   POFF,TIMEPOFF       OFFICE                                       
         MVC   WORK,TIMEMOA                                                     
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(9,PMOA)                                    
         GOTO1 (RF),DMCB,,(X'20',WORK)                                          
         MVC   SVMOA,WORK                                                       
*                                                                               
         EDIT  TIMEBKT,PHRS,2,FLOAT=-                                           
*                                                                               
         MVI   PTYPE,C'B'                                                       
         CLI   TIMETTYP,TIMTCB            B TIME                                
         BE    RUNL90                                                           
         MVI   PTYPE,C'R'                                                       
         CLI   TIMETTYP,TIMTCR            R TIME                                
         BE    RUNL90                                                           
         MVI   PTYPE,C'N'                                                       
*                                                                               
RUNL90   MVC   PTEMPO,=C'*TMS*'    SET TMS AS DEFAULT                           
         TM    TIMESTA,TIMTEMPO    IS IT A TEMPO TIMESHEET?                     
         BNO   *+20                                                             
         MVC   PTEMPO,=CL5'TEMPO'                                               
         AP    PKTMPCNT,=P'1'      INCREMENT TEMPO REC COUNTER                  
         MVI   PTYPE+1,C'T'        MARK TYPE AS TEMPO                           
*                                                                               
         TM    TIMESTA,TIMIADJ     IS IT ADJUSTED                               
         BNO   *+8                                                              
         MVI   PTYPE+1,C'A'        MARK TYPE AS ADJUSTED                        
*                                                                               
         TM    TIMESTA,TIMIWO      IS IT A WRITE-OFF                            
         BNO   *+8                                                              
         MVI   PTYPE+1,C'W'        MARK TYPE AS A WRITE-OFF                     
*                                                                               
         BAS   RE,PRINTIT                                                       
*                                                                               
RUNL100  LA    R2,TIMELNQ(R2)                                                   
         BCT   R3,RUNL10                                                        
*                                                                               
         CLI   QOPT2,C'Y'          WAS DOWNLOADING SELECTED?                    
         BE    RUNLX                  IF SO DO NOT PRINT TOTALS                 
         CP    PKCOUNT,=P'0'                                                    
         BE    RUNLX                                                            
         MVC   PPROD(36),=CL36'TOTAL NUMBER OF T/S              : '             
         EDIT  PKCOUNT,PRATE,ZERO=NOBLANK                                       
         BAS   RE,PRINTIT                                                       
         MVC   PPROD(36),=CL36'TOTAL NUMBER OF TMS T/S          : '             
         ZAP   PKFLDS,PKCOUNT                                                   
         SP    PKFLDS,PKTMPCNT     SUBTRACT OUT TEMPO                           
         EDIT  PKFLDS,PRATE,ZERO=NOBLANK                                        
         BAS   RE,PRINTIT                                                       
         MVC   PPROD(36),=CL36'TOTAL NUMBER OF TEMPO T/S        : '             
         EDIT  PKTMPCNT,PRATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         MVC   PPROD(36),=CL36'TOTAL NUMBER OF T/S W/PIDS       : '             
         ZAP   PKFLDS,PKPIDCNT                                                  
         AP    PKFLDS,PKTPDCNT                                                  
         EDIT  PKFLDS,PRATE,ZERO=NOBLANK                                        
         BAS   RE,PRINTIT                                                       
         MVC   PPROD(36),=CL36'TOTAL NUMBER OF TMS T/S W/PIDS   : '             
         EDIT  PKPIDCNT,PRATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
         MVC   PPROD(36),=CL36'TOTAL NUMBER OF TEMPO T/S W/PIDS : '             
         EDIT  PKTPDCNT,PRATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
RUNLX    TM    FLAG,FLGDUP         DO WE HAVE ANY DUPS                          
         BNO   *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* GET COMPANY RECORD AND READ FOR COMPANY ELEMENT                     *         
*      R2 = A(BINTABLE ENTRY)                                         *         
***********************************************************************         
         USING TIMED,R2                                                         
         USING CPYRECD,R3                                                       
GETCPY   NTR1                                                                   
         MVC   SVCPYNM,SPACES                                                   
         LA    R3,SVKEY                                                         
         MVC   CPYKEY,SPACES       CLEAR KEY                                    
         MVC   CPYKCPY,TIMECPY     COMPANY CODE                                 
         GOTO1 =A(DMREADDR),DMCB,(RC)     READ HIGH                             
         CLC   SVKEY(1),IOKEY                   SAME KEY?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET COMPANY RECORD                    
         LA    R3,IO                                                            
*                                                                               
         LA    R5,CPYRFST          GET X'10' ELEMENT COMPANY ELEM               
GETC10   CLI   0(R5),0             EOR                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R5),CPYELQ        X'10' -  COMPANY ELEMENT                     
         BE    GETC30                                                           
         CLI   0(R5),NAMELQ        X'20' -  NAME    ELEMENT                     
         BE    GETC40                                                           
GETC20   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     GETC10                                                           
*                                                                               
         USING CPYELD,R5                                                        
GETC30   MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         B     GETC20                                                           
*                                                                               
         USING NAMELD,R5                                                        
GETC40   SR    R1,R1                                                            
         IC    R1,1(R5)                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         LA    R2,L'SVCPYNM-1                                                   
         CR    R1,R2               DON'T MOVE IN MORE THAN ALLOWED              
         BNH   *+6                                                              
         LR    R1,R2                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCPYNM(0),NAMEREC        SAVE OFF NAME                          
*                                                                               
GETCX    B     EXIT                                                             
         DROP  R2,R3,R5                                                         
***********************************************************************         
* READ FOR LEDGER RECORD AND GET LEVELS OF ACCOUNT                    *         
*      R2 = A(BINTABLE ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMED,R2                                                         
         USING LDGRECD,R3                                                       
GETLDG   NTR1                                                                   
         LA    R3,SVKEY                                                         
         MVC   LDGKEY,SPACES       CLEAR KEY                                    
         MVC   LDGKCPY,TIMECPY     COMPANY                                      
         MVC   LDGKUNT(2),=C'1R'   UNIT/LEDGER                                  
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         CLC   SVKEY(3),IOKEY                   SAME KEY?                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET LEDGER RECORD                     
         LA    R3,IO                                                            
*                                                                               
         BAS   RE,GETLEVS                                                       
*                                                                               
GETLX    B     EXIT                                                             
         DROP  R2,R3                                                            
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
*     R3 = A(LEDGER RECORD)                                          *          
**********************************************************************          
         SPACE 1                                                                
         USING LDGRECD,R3                                                       
GETLEVS  NTR1                                                                   
         LA    R4,LDGRFST                                                       
GLEV10   CLI   0(R4),0             ARE WE AT THE EOR?                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'16'         ACCOUNT LENGTHS ELEMENT                      
         BE    GLEV20                                                           
         SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GLEV10                                                           
*                                                                               
         USING ACLELD,R4                                                        
GLEV20   MVC   LEVELS(LEVLNQ),SPACES                                            
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
GLEV30   ICM   R4,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV40              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R4,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R4,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV30                                                        
         B     GLEVX                                                            
*                                                                               
GLEV40   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SET LEVEL CODES                                                     *         
*      R2 = A(BINTABLE ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMED,R2                                                         
SETCDE   NTR1                                                                   
         MVC   LEVSCDE(LEVSLNQ),SPACES     CLEAR LEVEL CODES AREA               
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R1,TIME1RA          FULL ACCOUNT CODE                            
         LA    R2,LEVLNQS          FIRST LEVEL LENGTHS                          
         LA    R3,LEVSCDE          FIRST LEVEL CODE                             
*                                                                               
SETCDE10 SR    R4,R4                                                            
         IC    R4,0(R2)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   0(0,R3),0(R1)                                                    
         LA    R1,1(R4,R1)         BUMP TO NEXT LEVEL IN ACCOUNT CODE           
         LA    R3,L'LEVSCDE(R3)    BUMP TO NEXT LEVEL CODE AREA                 
         LA    R2,1(R2)            BUMP TO NEXT INDIVIUAL LEV LENGTH            
         BCT   R0,SETCDE10                                                      
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE THROUGH ACCOUNT LEVELS AND SAVE NAMES                       *         
*      R2 = A(BINTABLE ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMED,R2                                                         
READACC  NTR1                                                                   
         MVC   SVACCT,SPACES       CLEAR ACCOUNT FIELD                          
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS/LOOPS                       
         LA    R4,TIME1RA          FULL ACCOUNT CODE                            
         LA    R5,SVACCT                                                        
         LA    R6,LEVELS           FIRST LEVEL LENGTHS                          
         LA    R7,LEVNMES                                                       
*                                                                               
READA10  SR    R1,R1                                                            
         IC    R1,0(R6)            CURRENT INDIVIDUAL LEV LENGTH                
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R5),0(R4)                                                    
         STC   R1,BYTE                                                          
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,SVKEY                                                         
         MVC   ACTKEY,SPACES              CLEAR KEY                             
         MVC   ACTKCPY,TIMECPY            COMPANY                               
         MVC   ACTKUNT(2),=C'1R'          UNIT/LEDGER                           
         MVC   ACTKACT,SVACCT                                                   
         GOTO1 =A(DMREADDR),DMCB,(RC)     READ HIGH                             
         CLC   SVKEY(ACTKEND),IOKEY       SAME KEY?                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)     GET COMPANY RECORD                    
         LA    R3,IO                                                            
*                                                                               
         MVC   0(L'LEVNMES,R7),SPACES                                           
         LA    RE,ACTRFST                                                       
READA20  CLI   0(RE),0             EOR?                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),X'20'         NAME ELEMENT                                 
         BE    READA30                                                          
         SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         AR    RE,R1                                                            
         B     READA20                                                          
*                                                                               
         USING NAMELD,RE                                                        
READA30  SR    R1,R1                                                            
         IC    R1,1(RE)                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         LA    R3,L'LEVNMES-1                                                   
         CR    R1,R3               DON'T MOVE IN MORE THAN ALLOWED              
         BNH   *+6                                                              
         LR    R1,R3                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R7),NAMEREC     SAVE OFF NAME                                
         DROP  RE                                                               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R6,L'LEVELS(R6)     BUMP TO NEXT LEV LENGTH                      
         LA    R7,L'LEVNMES(R7)                                                 
         BCT   R0,READA10                                                       
*                                                                               
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* PRINT THE DATA                                                      *         
***********************************************************************         
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* GET TEMPO INFO (IF ANY)                                             *         
*      R2 = A(BINTABLE ENTRY)                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TIMED,R2                                                         
         USING TSXRECD,R3                                                       
GETTMP   NTR1                                                                   
         LA    R3,SVKEY                                                         
         MVC   TSXKEY,SPACES       CLEAR KEY                                    
         MVI   TSXKTYP,TSXKTYPQ    X'3E'                                        
         MVI   TSXKSUB,TSXKSUBQ    X'13'                                        
         MVC   TSXKCPY,TIMECPY     COMPANY CODE                                 
         MVC   TSXKPER,LEVDCDE     PERSON CODE                                  
         MVC   TSXKEND,TIMEPEDT    PERIOD END DATE                              
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         CLC   SVKEY(TSXKODS-TSXKEY),IOKEY      SAME RECORD?                    
         BNE   GETTX                                                            
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
         LA    R3,IOKEY                                                         
*                                                                               
         USING TIMELD,R4                                                        
         LA    R4,TSXRFST                                                       
*                                                                               
         XC    SVPERIOD,SVPERIOD   PERIOD NUMBER                                
         XC    SVSTPDT,SVSTPDT     PERIOD START DATE                            
*                                                                               
GETT10   CLI   0(R4),0             AT THE END OF THE ELEMENT                    
         BE    GETTX               READ NEXT RECORD                             
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   GETTMP99                 ELSE LOOP                               
         CLI   TIMETYP,TIMEXREF    ONLY PROCESS TEMPO XREF ELMS                 
         BNE   GETTMP99                 ELSE LOOP                               
*                                                                               
         EDIT  TIMXPED#,SVPERIOD                                                
         MVC   SVSTPDT,TIMXPSDT                                                 
*                                                                               
GETTMP99 SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R4,R1                                                            
         B     GETT10                                                           
*                                                                               
GETTX    DS    0H                                                               
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
*                                                                               
         USING HEADD,R3                                                         
         LA    R3,XHEAD2                                                        
         MVC   HEADDESC,=CL15'COMPANY CODE'                                     
         MVC   HEADCODE(L'SVCLOGO),SVCLOGO                                      
         MVC   HEADNAME(L'SVCPYNM),SVCPYNM                                      
*                                                                               
         LA    R3,XHEAD3                                                        
         MVC   HEADDESC,LEVADSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVACDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVANME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD4                                                        
         MVC   HEADDESC,LEVBDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVBCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVBNME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD5                                                        
         MVC   HEADDESC,LEVCDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVCCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVCNME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD6                                                        
         MVC   HEADDESC,LEVDDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVDCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVDNME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD7                                                        
         MVC   HEADDESC,=CL15'PERIOD'           LEVEL DESCRIPTION               
         MVC   HEADCODE(L'SVPERIOD),SVPERIOD    PERIOD NUMBER                   
         MVC   HEADNAME(L'SVPEDT),SVPEDT        PERIOD END DATE                 
*                                                                               
         TM    FLAG,FLGTMP         IS T/S A TEMPO T/S?                          
         BNO   EXIT                                                             
         MVC   HEADNAME+L'HEADNAME(9),=C'**TEMPO**'    EYECATCHER               
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ACCMST   DC    CL8'ACCMST'                                                      
EMUOLDN  DC    C'OLDN'                                                          
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
         DC    A(BOXRC)                                                         
         DC    A(BXHOOK)                                                        
         DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
*                                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
         DC    V(DLFLD)            DOWNLOAD ROUTINE                             
         SPACE 2                                                                
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
* ADD ITEM TO BINSRCH TABLE AND ACCUMULATE TOTALS                     *         
*  P1    A(ITEM TO BE ADDED)                                          *         
*  P2    A(TABLE)                                                     *         
***********************************************************************         
         SPACE 1                                                                
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
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         L     R4,DMCB             A(RECORD FOUND)                              
         SR    R0,R0                                                            
         ICM   R0,1,BINNUM         NUMBER OF BUCKETS                            
         BZ    BINXIT                NO BUCKETS - EXIT                          
         SR    R6,R6                                                            
         IC    R6,BINFST           DISPLACEMENT TO FIRST BUCKET                 
         AR    R4,R6               BUMP TO FIRST BUCKET IN TABLE                
         AR    R3,R6               BUMP TO FIRST BUCKET IN NEW ITEM             
BINA10   AP    0(TIMEBKLN,R4),0(TIMEBKLN,R3) ADD TO BUCKET                      
         LA    R3,TIMEBKLN(R3)     BUMP TO NEXT ENTRY IN NEW ITEM               
         LA    R4,TIMEBKLN(R4)     BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R0,BINA10                                                        
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
***********************************************************************         
* DUMP RECORDS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DUMP     NMOD1 0,**DMP**                                                        
         L     RC,0(R1)                                                         
         CLI   QOPT7,C'Y'                                                       
         BNE   DUMPX                                                            
         LA    R0,L'MSG                                                         
         LA    R2,MSG                                                           
         L     R3,4(R1)                                                         
         L     R4,8(R1)                                                         
*                                                                               
         LA    R5,=C'2D'                                                        
         GOTO1 PRNTBL,DMCB,((R0),(R2)),(R3),C'DUMP',(R4),(R5),         X        
               (C'P',PRINT)                                                     
*                                                                               
DUMPX    XIT1                                                                   
*        MVC   MSG,=CL10'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* BOX HOOK                                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R8                                                      
         USING BOXD,R4                                                          
BXHOOK   DS    0D                                                               
         NMOD1 0,*BHOOK                                                         
         L     RC,BOXRC            RESTORE REG C                                
         L     R8,VBIGPRNT                                                      
         L     R4,ADBOX                                                         
*                                                                               
         CLI   RCSUBPRG,1          IF NOT SET TO 1-NO BOXES                     
         BNE   BXXITX                                                           
*                                                                               
         MVI   BOXCOLS+(PTMPLN#-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PPID-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PASTDTE-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PAENDTE-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PCLI-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PPROD-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PJOB-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(P1NACT-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PTASK-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PTYPE-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PRATE-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PINCACT-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(POFF-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PHRS-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PTEMPO-PRTLINE-1),C'C'                                  
         MVI   BOXCOLS+(PCONTRA-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PMOA-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PDUP-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+PLINELNQ,C'R'                                            
*                                                                               
         MVI   BOXROWS+8,C'T'                                                   
         MVI   BOXROWS+12,C'M'                                                  
         MVI   BOXCOLS+1,C'L'                                                   
*                                                                               
BXXIT    MVI   BOXYORN,C'Y'                                                     
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXBLANK,C'N'                                                    
*                                                                               
BXXITX   XMOD1 1                                                                
*                                                                               
BOXRC    DC    A(0)                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DATAMGR INTERFACE                                                  *          
**********************************************************************          
         SPACE 1                                                                
DMSEQDR  NMOD1 0,SEQ               READ SEQUENTIAL                              
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRSEQ),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMHIGHDR NMOD1 0,HIGH              READ HIGH                                    
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMRDHI),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMREADDR NMOD1 0,READ              READ                                         
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,(X'88',DMREAD),=C'ACCDIR ',SVKEY,IO,0               
         B     DMX                                                              
*                                                                               
DMGETREC NMOD1 0,GREC              GET RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         USING ACCRECD,R3                                                       
         LA    R3,IO                                                            
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',ACCKDA,IO,DMWORK                  
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',ACCKDA,IO,DMWORK            
*                                                                               
DMX      XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACTSD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
VTYPES   DS    0A                                                               
ABINADD  DS    A                   ROUTINE TO ADD TO BINSEARCH TABLE            
ABOXRC   DS    A                                                                
ABXHOOK  DS    A                                                                
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD ROUTINE                             
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
TIMETBLN DS    F                   LENGTH OF AREA                               
ATIMETAB DS    A                   A(START) OF GETMAIN                          
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO KEY                          
ELCODE   DS    CL1                 ELEMENT CODE                                 
*                                                                               
PKFLDS   DS    PL4                                                              
PKCOUNT  DS    PL4                 TOTAL RECORD COUNTER                         
PKTMPCNT DS    PL4                 TEMPO RECORD COUNTER                         
PKPIDCNT DS    PL4                 TMS W/PID RECORD COUNTER                     
PKTPDCNT DS    PL4                 TEMPO W/PID RECORD COUNTER                   
PKFLDLNQ EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
*                                                                               
FLAG     DS    XL1                                                              
FLGTMP   EQU   X'80'               T/S IS TEMPO T/S                             
FLGDUP   EQU   X'40'               DUPLICATE TEMPO LINES FOUND                  
*                                                                               
LASTKEY  DS    CL49                                                             
*                                                                               
LSTBINKY DS    0CL16               LAST BINTABLE KEY                            
LSTCPY   DS    XL1                 LAST COMPANY CODE                            
LST1RA   DS    CL12                LAST 1R LOCATION CODE                        
LSTPEDT  DS    PL3                 LAST PERIOD END DATE                         
LSTTLN#  DS    XL2                 LAST TEMPO LINE NUMBER                       
*                                                                               
SVKEY    DS    CL49                                                             
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
SVPID    DS    XL2                                                              
SVTMPLN  DS    XL2                 SAVED AREA FOR TEMPO LINE #                  
SVPEDT   DS    CL8                 SAVED AREA FOR PERIOD END                    
SVPERIOD DS    CL2                 SAVED AREA FOR TIMESHEET PERIOD              
SVSTPDT  DS    PL3                 ACTUAL PERIOD START DATE                     
SVMOA    DS    CL4                 MOA - YYMM                                   
SVCPYNM  DS    CL36                SAVED AREA FOR COMPANY NAME                  
SVACCT   DS    CL12                SAVED AREA FOR ACCOUNT CODE                  
SVSEQ    DS    XL3                 SAVE SEQUENCE NUMBER                         
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
*                                                                               
PRTSIZE  DS    XL1                 PRINT AREA LENGTH                            
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
LEVLNQ   EQU   *-LEVELS                                                         
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
LEVNMES  DS    0CL36               LEVEL NAMES                                  
LEVANME  DS    CL36                LEVEL A NAME                                 
LEVBNME  DS    CL36                LEVEL B NAME                                 
LEVCNME  DS    CL36                LEVEL C NAME                                 
LEVDNME  DS    CL36                LEVEL D NAME                                 
*                                                                               
SVPRNT   DS    CL(L'XP)                                                         
*                                                                               
TMPWRK   DS    CL(TMPOLNQ)                                                      
TIMEWRK  DS    CL(TIMELNQ)                                                      
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
TIMEMAX  EQU    70000                                                           
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT LINE                                                *         
***********************************************************************         
         SPACE 1                                                                
PLINED   DSECT                                                                  
PRTLINE  DS    0C                                                               
PLN#     DS    CL5                 LINE NUMBER                                  
         DS    CL2                                                              
PTMPLN#  DS    CL5                 TEMPO LINE NUMBER                            
         DS    CL2                                                              
PPID     DS    CL6                 ACTIVITY DATE                                
         DS    CL2                                                              
PASTDTE  DS    CL6                 ACTUAL START DATE                            
         DS    CL2                                                              
PAENDTE  DS    CL6                 ACTUAL END DATE                              
         DS    CL2                                                              
PCLI     DS    CL3                 CLIENT CODE                                  
         DS    CL2                                                              
PPROD    DS    CL3                 PRODUCT CODE                                 
         DS    CL2                                                              
PJOB     DS    CL6                 JOB CODE                                     
         DS    CL2                                                              
P1NACT   DS    CL12                1N ACCOUNT                                   
         DS    CL2                                                              
PTASK    DS    CL2                 TASK CODE                                    
         DS    CL2                                                              
PTYPE    DS    CL2                 TIME TYPE                                    
         DS    CL2                                                              
PRATE    DS    CL8                 BILLING RATE                                 
         DS    CL2                                                              
PINCACT  DS    CL12                INCOME ACCOUNT                               
         DS    CL2                                                              
POFF     DS    CL2                 OFFICE CODE                                  
         DS    CL2                                                              
PHRS     DS    CL10                BILLABLE HOURS                               
         DS    CL2                                                              
PTEMPO   DS    CL5                 TEMPO EYECATCHER                             
         DS    CL2                                                              
PCONTRA  DS    CL12                CONTRA ACCOUNT(ONLY 1C)                      
         DS    CL2                                                              
PMOA     DS    CL6                 MOA                                          
         DS    CL2                                                              
PDUP     DS    CL7                 **DUP**                                      
PLINELNQ EQU   *-PRTLINE                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR HEADLINES                                                 *         
***********************************************************************         
         SPACE 1                                                                
HEADD    DSECT                                                                  
         DS    CL1                                                              
HEADDESC DS    CL15                                                             
         DS    CL1                                                              
HEADCODE DS    CL6                 1R OFFICE                                    
         DS    CL1                                                              
HEADNAME DS    CL36                DECSRIPTION                                  
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TIME TABLE WORK AREA                                          *         
***********************************************************************         
         SPACE 1                                                                
TIMED    DSECT                                                                  
TIMEKEY  DS    0C                  START OF BIN KEY                             
TIMECPY  DS    XL1                 COMPANY CODE                                 
TIME1RA  DS    CL12                1R LOCATION                                  
TIMEPEDT DS    XL3                 PERIOD END DATE                              
TIMETLN# DS    XL2                 TMS TEMPO LINE# (IF ANY)                     
TIMELN#  DS    XL2                 TMS LINE#                                    
TIMESEQ  DS    XL3                 SEQUENCE NUMBER                              
TIMKLNQ  EQU   *-TIMED                                                          
TIMEPID  DS    XL2                 PERSONAL ID                                  
TIMETTYP DS    XL1                 TYPE OF TIME                                 
TIMESTA  DS    XL1                 STATUS BYTE(TEMPO/ADJ/WO)                    
TIMETMP  EQU   X'10'               TIMESHEET IS A TEMPO TIMESHEET               
*                                  ALSO USED - TIMTEMPO - X'80'                 
*                                            - TIMIWO   - X'40'                 
*                                            - TIMIADJ  - X'20'                 
TIMETSK  DS    CL2                 TASK CODE                                    
TIMEPOFF DS    CL2                 PRODUCTION OFFICE                            
TIMEMOA  DS    PL2                 MOA                                          
TIMEADTE DS    PL3                 TIME LINES ACTIVITY DATE                     
TIMEAPDT DS    PL3                 TIMESHEET ACTUAL END DATE(TEMPO)             
TIMEACT  DS    0CL14               SJ ACCOUNT/1N ACCOUNT                        
TIMEAUL  DS    CL2                   UNIT/LEDGER                                
TIMEACLI DS    CL3                    CLIENT                                    
TIMEAPRD DS    CL3                     PRODUCT                                  
TIMEAJOB DS    CL6                      JOB                                     
TIMEINC  DS    CL12                INCOME/SUSPENSE                              
TIMECAC  DS    CL12                CONTRA (NOT 1N)                              
TIMERTE  DS    PL4                 RATE                                         
TIMEPRD  DS    XL1                 PERIOD NUMBER                                
TIMEBKT  DS    PL8                 TIME BUCKETS                                 
TIMEBKLN EQU   *-TIMEBKT           BUCKET LENGTH                                
TIMEBKCT EQU   (*-TIMEBKT)/TIMEBKLN                                             
TIMELNQ  EQU   *-TIMED                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TEMPO T/S WORK AREA                                           *         
***********************************************************************         
         SPACE 1                                                                
TMPTBD   DSECT                                                                  
TMPPEDT  DS    XL3                 PERIOD END DATE                              
TMPKLNQ  EQU   *-TMPTBD                                                         
TMPOLNQ  EQU   *-TMPTBD                                                         
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
*               ++INCLUDES                                            *         
***********************************************************************         
*                                                                               
* ACRCVRECD                                                                     
* ACBIGPRINTD                                                                   
* ACREPWORKD                                                                    
* ACGENFILE                                                                     
* ACGENMODES                                                                    
* DDLOGOD                                                                       
* ACMASTD                                                                       
* DDMASTD                                                                       
* DDBIGBOX                                                                      
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRCVRECD                                                      
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDDLCB                                                         
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026ACREPTS02 12/11/09'                                      
         END                                                                    
*          DATA SET ACREPZJ02  AT LEVEL 169 AS OF 12/05/00                      
*          DATA SET ACREPZJ02  AT LEVEL 169 AS OF 12/05/00                      
