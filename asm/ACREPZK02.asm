*          DATA SET ACREPZK02  AT LEVEL 073 AS OF 03/29/01                      
***********************************************************************         
* OPTIONS:                                                            *         
* --------                                                            *         
*         QOPT1:'Y'= WRITE BACK ANY RECORDS THAT SHOULD BE CHANGED    *         
*         QOPT2:'Y'= ELIMINATE ANY NON TEMPO T/S                      *         
*         QOPT3:'Y'= SHOW ONLY LINES THAT WERE CHANGED                *         
*                                                                     *         
***********************************************************************         
*PHASE ACZK02A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE '- TIMESHEET SUMMARY REPORT'                                     
ACZK02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZK**,R9                                                    
         USING ACWORKD,RA          RA=A(GLOBAL W/S)                             
         LA    RC,SPACEND                                                       
         USING ACZKD,RC            RC=A(LOCAL W/S)                              
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                      INITIALIZATIONS FOR THE RUN.           
         CLI   MODE,REQFRST              REQUEST FIRST                          
         BE    REQF                                                             
         CLI   MODE,LEDGFRST                                                    
         BE    LDGF                                                             
         CLI   MODE,LEVAFRST                                                    
         BE    LEVAF                                                            
         CLI   MODE,LEVBFRST                                                    
         BE    LEVBF                                                            
         CLI   MODE,LEVCFRST                                                    
         BE    LEVCF                                                            
         CLI   MODE,PROCACC                                                     
         BE    PACC                                                             
         CLI   MODE,RUNLAST              RUN LAST                               
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
         L     R2,ABOXRC           SET UP BOX ROUTINE                           
         ST    RC,0(R2)                                                         
         L     R2,ABXHOOK                                                       
         ST    R2,HEADHOOK                                                      
*                                                                               
         L     RF,ADMASTC                                                       
         USING MASTD,RF                                                         
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
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         XC    START,START                                                      
         CLC   QSTART,SPACES       ANY START DATE                               
         BE    REQF10                                                           
         MVC   WORK(L'QSTART),QSTART                                            
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)                                   
*                                                                               
REQF10   MVC   END,=X'FFFFFF'                                                   
         CLC   QEND,SPACES         ANY END DATE                                 
         BE    REQFX                                                            
         MVC   WORK(L'QEND),QEND                                                
         CLC   WORK+4(2),SPACES                                                 
         BNE   REQF20                                                           
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
REQF20   GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING CPYELD,R5                                                        
         L     R5,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         DROP  R5                                                               
*                                                                               
         L     R2,ADCMPNAM                                                      
         LA    R3,SVCPYNM                                                       
         BAS   RE,GETNME                                                        
*                                                                               
         BAS   RE,GETLEVS          GET LEVELS                                   
*                                                                               
LDGFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL A FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVAF    DS    0H                                                               
         L     R2,ADLVANAM                                                      
         LA    R3,LEVANME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVAFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL B FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVBF    DS    0H                                                               
         L     R2,ADLVBNAM                                                      
         LA    R3,LEVBNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVBFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEVEL C FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
LEVCF    DS    0H                                                               
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         L     R2,ADLVCNAM                                                      
         LA    R3,LEVCNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
LEVCFX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS ACCOUNT                                                    *          
**********************************************************************          
         SPACE 1                                                                
PACC     DS    0H                                                               
         USING BIND,R2                                                          
         L     R2,ATIMETAB         TIME LINE TABLE                              
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,ATMPTAB          TEMPO TABLE                                  
         XC    BININ,BININ         CLEAR BIN TABLE                              
         L     R2,ADUPTAB          DUPLICATE TEMPO LINE TABLE                   
         XC    BININ,BININ         CLEAR BIN TABLE                              
         DROP  R2                                                               
*                                                                               
         LA    R0,PKFLDLNQ         NUMBER OF PACKED FIELDS                      
         LA    R1,PKFLDS           R1=A(PACKED FIELDS)                          
         ZAP   0(L'PKFLDS,R1),=P'0'     CLEAR FIELDS                            
         LA    R1,L'PKFLDS(R1)                                                  
         BCT   R0,*-10                                                          
*                                                                               
         BAS   RE,SETCDE           SET LEVEL CODES                              
*                                                                               
         L     R2,ADACCNAM                                                      
         LA    R3,LEVDNME                                                       
         BAS   RE,GETNME                                                        
*                                                                               
PACC20   GOTO1 APTIME,DMCB,(RC)    READ TIME RECORDS                            
*                                                                               
PACCX    B     EXIT                                                             
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
*                                                                               
RUNLX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
*     R3 = A(LEDGER RECORD)                                          *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R4                                                        
GETLEVS  NTR1                                                                   
         L     R4,ADLDGHIR                                                      
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
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* SET LEVEL CODES                                                    *          
**********************************************************************          
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
SETCDE   NTR1                                                                   
         L     R5,ADACC            A(ACCOUNT RECORD)                            
         MVC   LEVSCDE(LEVSLNQ),SPACES     CLEAR LEVEL CODES AREA               
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
* GET NAME ROUTINE                                                   *          
*     R2 = NAME ELEMENT (SOURCE)                                     *          
*     R3 = NAME FIELD   (DESTINATION)                                *          
**********************************************************************          
         SPACE 1                                                                
         USING NAMELD,R2                                                        
GETNME   NTR1                                                                   
         MVC   0(L'LEVNMES,R3),SPACES                                           
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GETNX                                                            
         EX    R1,*+4                                                           
         MVC   0(0,R3),NAMEREC                                                  
*                                                                               
GETNX    B     EXIT                                                             
         DROP  R2                                                               
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
         DC    A(PTIME)            ROUTINE TO PROCESS TIME RECS                 
*                                                                               
         DC    A(TIMETAB)          TIME LINE TABLE                              
         DC    A(TMPTAB)           TEMPO LINE TABLE                             
         DC    A(DUPTAB)           DUPLICATE TEMPO LINE TABLE                   
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
*        MVC   MSG,=CL15'TRNS  REC'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* PROCESS TIME                                                       *          
*         READ THROUGH TIME RECORDS                                  *          
**********************************************************************          
         SPACE 1                                                                
PTIME    DS    0D                                                               
         NMOD1 0,*PTIME*                                                        
         L     RC,0(R1)                                                         
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,ADACC                                                         
*                                                                               
         USING TIMRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   TIMKEY,SPACES       CLEAR KEY                                    
         MVC   TIMKCPY,ACTKCPY     COMPANY CODE                                 
         MVC   TIMKUNT(2),=C'1R'   U/L                                          
         MVC   TIMKACT,ACTKACT     MOVE IN ACCOUNT CODE                         
*                                                                               
*        MVC   MSG,=CL15'ACCOUNT'                                               
*        GOTO1 ADUMP,DMCB,(RC),ACTKEY,15                                        
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     PTIME20                                                          
*                                                                               
PTIME10  GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
PTIME20  CLC   SVKEY(TIMKOFF-TIMKEY),IOKEY      SAME KEY?                       
         BNE   PTIME999                                                         
*                                                                               
         LA    R2,IOKEY                                                         
         CLC   TIMKREF,=C'*TIME*'  LOOKING FOR ONLY TIME TRANSACTIONS           
         BNE   PTIME10                                                          
*                                                                               
         CLC   TIMKPEDT,START                                                   
         BL    PTIME10             IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    PTIME10             IGNORE AFTER END DATE OF REQUEST             
*                                                                               
         GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
*                                                                               
         MVC   MSG,=CL15'TIME REC IN'                                           
         SR    R6,R6                                                            
         ICM   R6,3,TIMRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
*                                                                               
         USING TIMELD,R4                                                        
         LA    R4,TIMRFST                                                       
*                                                                               
         NI    FLAG,X'FF'-FLGCHG   CLEAR FLAG FOR CHANGE                        
         XC    SVPID,SVPID         SAVED AREA FOR PID NUMBER                    
         LR    R6,R4               LOOP THROUGH TIME REC FOR PIDEL              
PTIME30  CLI   0(R6),0             AT THE END OF THE RECORD?                    
         BE    PTIME50                                                          
         CLI   0(R6),PIDELQ                                                     
         BE    PTIME40                                                          
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     PTIME30                                                          
*                                                                               
         USING PIDELD,R6                                                        
PTIME40  MVC   SVPID,PIDNO         SAVE PID NUMBER TO FILL IN LINE              
         DROP  R6                                                               
*                                                                               
PTIME50  CLI   0(R4),0             AT THE END OF THE ELEMENT                    
         BNE   PTIME60                                                          
*                                                                               
         TM    FLAG,FLGCHG         WAS RECORD CHANGED?                          
         BNO   PTIME10             READ SEQ                                     
*                                                                               
         MVC   MSG,=CL15'TIME REC OUT'                                          
         SR    R6,R6                                                            
         ICM   R6,3,TIMRLEN                                                     
         GOTO1 ADUMP,DMCB,(RC),(R2),(R6)                                        
         BAS   RE,WRTREC           PUT FIXED RECORD BACK                        
         B     PTIME10             READ SEQ                                     
*                                                                               
PTIME60  NI    FLAG,X'FF'-FLGTMP#  CLEAR FLAG FOR LINE # CHANGE                 
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   PTIME150                 ELSE LOOP                               
         CLI   TIMETYP,TIMEINP     ONLY PROCESS INPUT DETAILS                   
         BNE   PTIME150                 ELSE LOOP                               
*                                                                               
         USING TIMED,R5                                                         
         LA    R5,TIMEWRK                                                       
         MVC   TIMEWRK,SPACES                                                   
*                                                                               
         USING TMPTBD,R3                                                        
         LA    R3,TMPWRK                                                        
         MVC   TMPWRK,SPACES                                                    
*                                                                               
         USING DUPTBD,R7                                                        
         LA    R7,DUPWRK                                                        
         MVC   DUPWRK,SPACES                                                    
*                                                                               
         MVC   TIME1RA,TIMKACT     MOVE 1R ACCOUNT TO BINTABLE                  
         MVC   TIMECPY,TIMKCPY     COMPANY CODE FOR LATER READS                 
         MVC   TIMEPEDT,TIMKPEDT   PERIOD END DATE                              
*                                                                               
         XC    TIMETLN#,TIMETLN#   TEMPO LINE #                                 
         XC    TIMEOTLN,TIMEOTLN   ORIGINAL TEMPO LINE #                        
         XC    TIMEAPDT,TIMEAPDT   ACTUAL START DATE                            
         XC    TIMESTA,TIMESTA     CLEAR STATUS BYTE                            
         MVC   SVLN#,TIMLINE#      SAVE OFF TMS LINE NUMBER                     
         TM    TIMSTAT,TIMTEMPO    IS THIS A TEMPO LINE                         
         BNO   PTIME130                                                         
         MVC   TMPPEDT,TIMKPEDT    SAVE OFF PER END DATE IN TEMPO TAB           
         OI    TIMESTA,TIMTEMPO    SET ISFROMTEMPO BIT                          
         LR    R6,R4               SAVE OFF R4                                  
PTIME70  SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1               BUMP TO TEMPO EXTRA                          
         CLI   0(R4),0             AT THE END OF THE RECORD?                    
         BE    PTIME120                                                         
         CLI   TIMEL,TIMELQ        MAKE SURE WE ARE AT AN 8B                    
         BNE   PTIME70                                                          
         CLI   TIMETYP,TIMEXTRA    IS THE THE TEMPO EXTRA?                      
         BNE   PTIME70                                                          
*                                                                               
* FILL OUT DUP FIELDS AND TRY AND ADD THEM TO THE DUP TABLE                     
*      IF YOU CANNOT ADD THEM THAT MEANS THEY ARE DUPS AND NEED                 
*      TO BE REPLACED.  MAKE A CALL TO BINADD WITH FLAG SET TO FLGCHKT          
*      AND THEN REPLACE IT IN BINADD AND THEN CHANGE IT IN THE RECORD.          
*                                                                               
         MVC   DUP1RA,TIMKACT      ACCOUNT                                      
         MVC   DUPPEDT,TIMKPEDT    PERIOD END DATE                              
         MVC   DUPTLN#,TIMXTLN#    TEMPO LINE NUMBER                            
*                                                                               
         USING BIND,RE                                                          
         L     RE,ADUPTAB          R1=A(DUPLICATE TABLE)                        
         ICM   R0,15,BININ                                                      
         BZ    PTIME100            NO ENTRIES IN DUPL TABLE                     
         LA    RF,BINTAB                                                        
         DROP  RE                                                               
*                                                                               
PTIME80  DS    0H                                                               
         CLC   DUPWRK,0(RF)                                                     
         BNE   PTIME90                                                          
         OI    FLAG,FLGCHG+FLGTMP# MARK RECORD AND LINE AS CHANGED              
         MVC   TIMEOTLN,TIMXTLN#   SAVE ORIGINAL TEMPO LINE NUMBER              
         MVC   TIMXTLN#,SVLN#      SAVE OFF TMS LINE# AS TEMPO LINE #           
         B     PTIME110                                                         
*                                                                               
PTIME90  LA    RF,DUPLNQ(RF)                                                    
         BCT   R0,PTIME80                                                       
*                                                                               
PTIME100 GOTO1 ABINADD,DMCB,(RC),DUPWRK,ADUPTAB     ADD TABLE ENTRY             
*                                                                               
PTIME110 MVC   TIMETLN#,TIMXTLN#   TEMPO LINE #                                 
         MVC   TIMEAPDT,TIMXTPDT   ACTUAL PERIOD END DATE                       
PTIME120 LR    R4,R6               RESET R4                                     
*                                                                               
PTIME130 OC    TIMESTA,TIMIND      TURN ON INDICATOR BITS                       
         TM    FLAG,FLGTMP#        WAS TEMPO LINE # CHANGED                     
         BNO   *+8                                                              
         OI    TIMESTA,TIMECHG     MARK LINE AS CHANGED                         
*                                                                               
         MVC   TIMEPID,SVPID                                                    
         MVC   TIMETTYP,TIMTTYP    TYPE OF TIME                                 
         MVC   TIMELN#,TIMLINE#    TMS LINE NUMBER                              
         MVC   TIMETSK,TIMTSK      TASK CODE                                    
         MVC   TIMEPOFF,TIMOFF     PRODUCTION OFFICE                            
         MVC   TIMEMOA,TIMMOA      MONTH OF ACTIVITY                            
         MVC   TIMEADTE,TIMADAT    ACTIVITY DATE                                
         MVC   TIMEACT,TIMACC      SJ/1N ACCOUNT                                
         ZAP   TIMEBKT,TIMHRS      HOURS                                        
         MVC   TIMECAC,TIMKCACT    1C CONTRA                                    
*                                                                               
         ZAP   TIMERTE,=P'0'                                                    
         CLI   TIMLN,TIMILN1Q      ANY BILLABLE INFO?                           
         BNH   PTIME140                                                         
         MVC   TIMEINC,TIMINC      INCOME ACCOUNT                               
         MVC   TIMERTE,TIMRATE     RATE                                         
*                                                                               
PTIME140 DS    0H                                                               
         GOTO1 ABINADD,DMCB,(RC),TIMEWRK,ATIMETAB   ADD TABLE ENTRY             
*                                                                               
         CLC   TMPPEDT,SPACES      IS THIS LINE A TEMPO LINE?                   
         BE    PTIME150                                                         
         GOTO1 ABINADD,DMCB,(RC),TMPWRK,ATMPTAB     ADD TABLE ENTRY             
*                                                                               
PTIME150 SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R4,R1                                                            
         B     PTIME50                                                          
*                                                                               
PTIME999 DS    0H                                                               
         BAS   RE,ADJTIME          MAKE TIME TABLE AS TEMPO                     
         BAS   RE,ACCL             SIMULATE ACCOUNT LAST W/ACC INFO             
*                                                                               
PTIMEX   XMOD1                                                                  
         DROP  R2,R3,R4,R5,R7                                                   
         EJECT                                                                  
**********************************************************************          
* REREAD RECORD AND FIX DUPLICATE LINE NUMBERS                       *          
**********************************************************************          
         SPACE 1                                                                
WRTREC   NTR1                                                                   
         CLI   QOPT1,C'Y'          DO WE WANT TO UPDATE?                        
         BNE   WRTRECX                                                          
         CLI   RCWRITE,C'N'                                                     
         BE    WRTRECX                                                          
         GOTO1 =A(DMPUTREC),DMCB,(RC)     WRITE RECORD BACK                     
*                                                                               
WRTRECX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* MARK TIME TABLE T/S AS TEMPO WHERE NEEDED                          *          
**********************************************************************          
         SPACE 1                                                                
ADJTIME  NTR1                                                                   
         USING BIND,R1                                                          
         L     R1,ATMPTAB          R1=A(TEMPO TABLE)                            
         ICM   R0,15,BININ                                                      
         BZ    ADJTX               NO ENTRIES IN TMPO TABLE                     
         USING TMPTBD,R2                                                        
         LA    R2,BINTAB                                                        
*                                                                               
ADJT10   L     R1,ATIMETAB         R1=A(TIME LINE TABLE)                        
         ICM   R6,15,BININ                                                      
         BZ    ADJTX                                                            
         USING TIMED,R3                                                         
         LA    R3,BINTAB                                                        
*                                                                               
ADJT20   CLC   TMPPEDT,TIMEPEDT    MATCH ON PERIOD END DATES                    
         BNE   *+8                                                              
         OI    TIMESTA,TIMETMP     MARK T/S AS A TEMPO T/S                      
         LA    R3,TIMELNQ(R3)      BUMP TIME LINE TABLE TO NEXT ENTRY           
         BCT   R6,ADJT20                                                        
*                                                                               
         LA    R2,TMPOLNQ(R2)      BUMP TEMPO TABLE TO NEXT ENTRY               
         BCT   R0,ADJT10                                                        
*                                                                               
ADJTX    XIT1                                                                   
         DROP  R1,R2,R3                                                         
         EJECT                                                                  
**********************************************************************          
* ACCOUNT LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING PLINED,R5                                                        
ACCL     NTR1                                                                   
         LA    R5,XP                                                            
         MVC   PRTLINE(PLINELNQ),XSPACES                                        
*                                                                               
         USING BIND,R1                                                          
         L     R1,ATIMETAB         R1=A(TRANSACTION TABLE)                      
         ICM   R3,15,BININ                                                      
         BZ    ACCLX                                                            
         USING TIMED,R2                                                         
         LA    R2,BINTAB                                                        
         DROP  R1                                                               
*                                                                               
         XC    LSTBINKY,LSTBINKY   CLEAR SAVED AREA FOR KEY COMPARE             
         XC    LSTTLN#,LSTTLN#     CLEAR SAVED AREA FOR TEMPO LINE#             
         MVI   FLAG,0                                                           
ACCL10   DS    0H                                                               
         CLI   QOPT2,C'Y'          ELIMINATE ALL NONE TEMPO PEOPLE?             
         BNE   *+14                                                             
         OC    TIMEPID,TIMEPID     IF NO PID SKIP T/S                           
         BZ    ACCL100                                                          
*                                                                               
         CLC   LSTBINKY,TIMEKEY    SAME 1R ACT/TIMESHEET?                       
         BE    ACCL20                                                           
         NI    FLAG,X'FF'-FLGTMP                                                
         BAS   RE,GETTMP           GET TEMPO INFO (IF ANY)                      
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
         TM    TIMESTA,TIMETMP     IS THIS TIMESHEET A TEMPO T/S                
         BNO   ACCL20                                                           
         OI    FLAG,FLGTMP         SET FLAG FOR HEADLINES/DWNLD                 
*                                                                               
ACCL20   TM    TIMESTA,TIMECHG     WAS THIS LINE CHANGED?                       
         BNO   *+10                                                             
         MVC   PERR,=C'**CHG**'    MARK LINE AS CHANGED                         
         OC    TIMETLN#,TIMETLN#   ANY TEMPO LINE NUMBER?                       
         BZ    ACCL30                  NO - SKIP CHECKING                       
         CLC   LSTTLN#,TIMETLN#    SAME TEMPO LINE NUMBER?                      
         BNE   ACCL30                                                           
         CLC   LSTBINKY,TIMEKEY    SAME 1R ACT/TIMESHEET?                       
         BNE   ACCL30                                                           
         MVC   PERR,=C'**DUP**'    MARK LINE AS DUPLICATE                       
         OI    FLAG,FLGDUP         SET FLAG FOR DUP                             
*                                                                               
ACCL30   MVC   LSTTLN#,TIMETLN#    SAVE OFF TEMPO LINE NUM FOR NEXT CLC         
         MVC   LSTBINKY,TIMEKEY    SAVE OFF FOR LATER COMPARE                   
         EDIT  TIMELN#,PLN#        TMS LINE NUMBER                              
         EDIT  TIMETLN#,PTMPLN#    TEMPO LINE NUMBER                            
         EDIT  TIMEOTLN,PTMPOLN#   ORIGINAL TEMPO LINE NUMBER                   
*        GOTO1 DATCON,DMCB,(1,TIMEADTE),(X'20',PADDDTE)  ACTIVITY DATE          
         OC    SVSTPDT,SVSTPDT     ANY ACTUAL START DATE?                       
         BZ    ACCL40                                                           
         GOTO1 DATCON,DMCB,(1,SVSTPDT),(X'20',PASTDTE)  ACTUAL ST DTE           
ACCL40   OC    TIMEAPDT,TIMEAPDT   ANY ACTUAL END DATE?                         
         BZ    ACCL50                                                           
         GOTO1 DATCON,DMCB,(1,TIMEAPDT),(X'20',PAENDTE) ACTUAL END DTE          
*                                                                               
ACCL50   CLC   TIMEAUL,=C'SJ'      1N OR SJ?                                    
         BNE   ACCL60                                                           
         MVC   PCLI,TIMEACLI       CLIENT CODE                                  
         MVC   PPROD,TIMEAPRD      PRODUCT CODE                                 
         MVC   PJOB,TIMEAJOB       JOB CODE                                     
         B     *+10                                                             
ACCL60   MVC   P1NACT,TIMEACT+2    1N ACCOUNT                                   
         MVC   PTASK,TIMETSK       TASK CODE                                    
         MVC   PCONTRA,TIMECAC     CONTRA ACCOUNT                               
         GOTO1 DATCON,DMCB,(1,TIMEPEDT),(8,SVPEDT)                              
*                                                                               
         OC    TIMEPID,TIMEPID     ANY PID?                                     
         BZ    ACCL70                                                           
         LA    R1,PKPIDCNT         INCREMENT EITHER TMS PID                     
         TM    TIMESTA,TIMTEMPO    IS IT A TEMPO TIMESHEET?                     
         BNO   *+8                                                              
         LA    R1,PKTPDCNT            OR TEMPO PID REC COUNTER                  
         AP    0(L'PKFLDS,R1),=P'1'                                             
         EDIT  TIMEPID,PPID                                                     
*                                                                               
ACCL70   CLC   TIMERTE,SPACES                                                   
         BE    ACCL80                                                           
         EDIT  TIMERTE,PRATE,2,FLOAT=-                                          
ACCL80   MVC   PINCACT,TIMEINC     INCOME ACCOUNT                               
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
         BE    ACCL90                                                           
         MVI   PTYPE,C'R'                                                       
         CLI   TIMETTYP,TIMTCR            R TIME                                
         BE    ACCL90                                                           
         MVI   PTYPE,C'N'                                                       
*                                                                               
ACCL90   MVC   PTEMPO,=C'*TMS*'    SET TMS AS DEFAULT                           
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
         CLI   QOPT3,C'Y'          DO WE WANT TO SEE ONLY CHANGED LINES         
         BNE   *+12                                                             
         TM    TIMESTA,TIMECHG     WAS THIS LINE CHANGED?                       
         BNO   *+8                                                              
         BAS   RE,PRINTIT                                                       
         MVC   XP,XSPACES                                                       
*                                                                               
ACCL100  LA    R2,TIMELNQ(R2)                                                   
         BCT   R3,ACCL10                                                        
*                                                                               
         CP    PKCOUNT,=P'0'                                                    
         BE    ACCLX                                                            
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
ACCLX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* PRINT THE DATA                                                      *         
***********************************************************************         
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         XIT1                                                                   
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
GETTX    XIT1                                                                   
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
         BNO   HEADUPX                                                          
         MVC   HEADNAME+L'HEADNAME(9),=C'**TEMPO**'    EYECATCHER               
*                                                                               
HEADUPX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
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
         MVI   BOXCOLS+(PTMPOLN#-PRTLINE-1),C'C'                                
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
         MVI   BOXCOLS+(PERR-PRTLINE-1),C'C'                                    
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
         MVC   SVDA,ACCKDA         SAVE OFF DISK ADDRESS                        
         GOTO1 DATAMGR,DMCB,DMGET,=C'ACCMST ',SVDA,IO,DMWORK                    
         B     DMX                                                              
*                                                                               
DMPUTREC NMOD1 0,PREC              PUT RECORD                                   
         L     RC,0(R1)            RESET RC                                     
         GOTO1 DATAMGR,DMCB,=CL8'PUTREC',=C'ACCMST',SVDA,IO,DMWORK              
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
* TABLES AND BUFFERS                                                 *          
**********************************************************************          
         SPACE 1                                                                
         DC    C'***TMS***'                                                     
TIMETAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 1               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TIMELNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TIMKLNQ)            KEY LENGTH                               
         DC    AL4(TIMEMAX)            MAX IN TABLE                             
         DC    AL1(TIMEBKCT)           NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(TIMEBKT-TIMED)      DISPLACEMENT TO FIRST BUCKET             
         DS    (TIMEMAX*TIMELNQ)XL1    TABLE                                    
*                                                                               
         DC    C'***TMP***'                                                     
TMPTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 2               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TMPOLNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TMPKLNQ)            KEY LENGTH                               
         DC    AL4(TMPMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (TIMEMAX*TIMELNQ)XL1    TABLE                                    
*                                                                               
         DC    C'***DUP***'                                                     
DUPTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 3               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(DUPLNQ)             LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(DUPKLNQ)            KEY LENGTH                               
         DC    AL4(DUPMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (DUPMAX*DUPLNQ)XL1      TABLE                                    
*                                                                               
TIMEMAX  EQU   10000                                                            
TMPMAX   EQU    1000                                                            
DUPMAX   EQU   10000                                                            
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZKD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
VTYPES   DS    0A                                                               
ABINADD  DS    A                   ROUTINE TO ADD TO BINSEARCH TABLE            
ABOXRC   DS    A                                                                
ABXHOOK  DS    A                                                                
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
APTIME   DS    A                   ROUTINE TO PROCESS TIME RECORDS              
*                                                                               
ATIMETAB DS    A                   TIME LINE TABLE                              
ATMPTAB  DS    A                   TEMPO LINE TABLE                             
ADUPTAB  DS    A                   DUPLICATE TEMPO LINE TABLE                   
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD ROUTINE                             
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
TIMETBLN DS    F                   LENGTH OF AREA                               
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
MSG      DS    CL15                DUMP MESSAGE                                 
*                                                                               
FLAG     DS    XL1                                                              
FLGTMP   EQU   X'80'               T/S IS TEMPO T/S                             
FLGDUP   EQU   X'40'               DUPLICATE TEMPO LINES FOUND                  
FLGCHG   EQU   X'20'               MARK FLAG AS CHANGED TO CHANGE REC           
FLGTMP#  EQU   X'10'               TEMPO LINE NUMBER WAS CHANGED                
*                                                                               
LASTKEY  DS    CL49                                                             
*                                                                               
LSTBINKY DS    0CL15               LAST BINTABLE KEY                            
LST1RA   DS    CL12                LAST 1R LOCATION CODE                        
LSTPEDT  DS    PL3                 LAST PERIOD END DATE                         
LSTTLN#  DS    XL2                 LAST TEMPO LINE NUMBER                       
*                                                                               
SVDA     DS    F                   SVAED ARE FOR DISK ADDRESS                   
SVKEY    DS    CL49                                                             
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
SVPID    DS    XL2                                                              
SVLN#    DS    XL2                 SAVED AREA FOR TMS LINE #                    
SVPEDT   DS    CL8                 SAVED AREA FOR PERIOD END                    
SVPERIOD DS    CL2                 SAVED AREA FOR TIMESHEET PERIOD              
SVSTPDT  DS    PL3                 ACTUAL PERIOD START DATE                     
SVMOA    DS    CL4                 MOA - YYMM                                   
SVCPYNM  DS    CL36                SAVED AREA FOR COMPANY NAME                  
SVACCT   DS    CL12                SAVED AREA FOR ACCOUNT CODE                  
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
DUPWRK   DS    CL(DUPLNQ)                                                       
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
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
PTMPOLN# DS    CL5                 ORIGINAL TEMPO LINE NUMBER                   
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
PERR     DS    CL7                 **DUP**/**CHG**                              
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
TIME1RA  DS    CL12                1R LOCATION                                  
TIMEPEDT DS    XL3                 PERIOD END DATE                              
TIMETLN# DS    XL2                 TEMPO LINE (IF NOT THERE TMS #)              
TIMELN#  DS    XL2                 TMS LINE#                                    
TIMKLNQ  EQU   *-TIMED                                                          
TIMEOTLN DS    XL2                 ORIGINAL TEMPO LINE IF DUP                   
TIMECPY  DS    XL1                 COMPANY CODE                                 
TIMEPID  DS    XL2                 PERSONAL ID                                  
TIMETTYP DS    XL1                 TYPE OF TIME                                 
TIMESTA  DS    XL1                 STATUS BYTE(TEMPO/ADJ/WO)                    
TIMETMP  EQU   X'80'               TIMESHEET IS A TEMPO TIMESHEET               
*                                  ALSO USED - TIMIWO   - X'40'                 
*                                            - TIMIADJ  - X'20'                 
TIMECHG  EQU   X'10'               TEMPO LINE NUMBER WAS CHANGED                
*                                            - TIMTEMPO - X'08'                 
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
* DSECT DUP TABLE WORK AREA                                           *         
***********************************************************************         
         SPACE 1                                                                
DUPTBD   DSECT                                                                  
DUPKEY   DS    0C                  START OF BIN KEY                             
DUP1RA   DS    CL12                1R LOCATION                                  
DUPPEDT  DS    XL3                 PERIOD END DATE                              
DUPTLN#  DS    XL2                 TEMPO LINE                                   
DUPKLNQ  EQU   *-DUPTBD                                                         
DUPLNQ   EQU   *-DUPTBD                                                         
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
**PAN#1  DC    CL21'073ACREPZK02 03/29/01'                                      
         END                                                                    
*          DATA SET ACREPZJ02  AT LEVEL 169 AS OF 12/05/00                      
*          DATA SET ACREPZJ02  AT LEVEL 169 AS OF 12/05/00                      
