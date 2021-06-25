*          DATA SET ACREPZJ02  AT LEVEL 002 AS OF 01/20/12                      
*PHASE ACZJ02A,*                                                                
*INCLUDE DLFLD                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'TIMESHEET COUNT REPORT'                                         
**********************************************************************          
* ACZJ - OPTIONS                                                     *          
* --------------                                                     *          
*       OPTION 1 = Y TO ELIMINATE ALL T/S WITHOUT PIDEL STAMPED      *          
*                = T TO ELIMINATE ALL NONE TEMPO PEOPLE              *          
*                = B TO ELIMINATE ALL NONE BRANDO PEOPLE             *          
*       OPTION 2 = D DWNLD TO DATASET (DEFAULT IS P/O)               *          
*                = Q DWNLD DIRECT TO QUEUE NOT DATASET               *          
*       OPTION 3 = ACCFILE NUMBER                                    *          
*                = Y - PRINT DUPS FOR TEMPO TIME                     *          
*       OPTION 4 = A - RUN OFF ACTIVITY DATE                         *          
*       OPTION 5 = A/I - CHECK TIMESHEET STATUS                      *          
*       OPTION 6 = N - N TIME TO THE JOB                             *          
*       OPTION 6 = E - EXCLUDE CERTAIN CLI/PROD BASED ON XPERTAB     *          
*       OPTION 7 = Y - PRINTABLES                                    *          
*                                                                    *          
*       QSELECT  = TYPE OF TIME                                      *          
*       QDUEST   = ACTIVITY DATE                                     *          
**********************************************************************          
         SPACE 1                                                                
ACZJ02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZJ**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZJD,RC                                                         
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
*        CLI   MODE,ACCLAST              ACCOUNT LAST                           
*        BE    ACCL                                                             
         CLI   MODE,RUNLAST              RUN LAST                               
         BE    RUNL                                                             
*                                                                               
EXIT     XMOD1 1                                                                
         EJECT                                                                  
**********************************************************************          
* RUN FIRST                                                          *          
**********************************************************************          
         SPACE 1                                                                
RUNF     DS    0H                                                               
         MVC   VTYPES(VTYPLNQ),ADCONS        RELOCATE ADDRESSES                 
         MVI   FLAG,0                                                           
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
         MVI   RCREQREP,C'N'       DONT PRINT ADDITIONAL REQUEST PAGES          
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
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
         L     R4,MCBXAREA                                                      
         ST    R4,ADBOX                                                         
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
*                                                                               
         L     RF,ADWNBUF             RF=A(DOWNLOAD BUFFER)                     
         XC    0(L'DWNBUF,RF),0(RF)   CLEAR DOWNLOAD BUFFER                     
         XC    DWNSTAT,DWNSTAT        CLEAR DOWNSTAT BYTE                       
*                                                                               
RUNFX    B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
**********************************************************************          
* REQUEST FIRST                                                      *          
**********************************************************************          
         SPACE 1                                                                
REQF     DS    0H                                                               
         MVI   RCSUBPRG,1                                                       
*                                                                               
         CLC   QSELECT,SPACES      DO WE HAVE ANY FILTERING?                    
         BNH   REQF020                                                          
*                                                                               
         USING TYPTABD,RE                                                       
         L     RE,ATYPTAB          FIND MATCHING TYPE                           
REQF010  CLI   0(RE),EOF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TYPCODE,QSELECT                                                  
         BE    *+12                                                             
         LA    RE,TYPTBLNQ(RE)                                                  
         B     REQF010                                                          
         MVC   TYPEFLT,TYPBINC                                                  
         DROP  RE                                                               
*                                                                               
REQF020  CLI   QOPT2,C'D'          DOWNLOAD TO DATASET?                         
         BNE   REQF040                                                          
         CLI   QOPT3,X'40'         ANY FILE SPECIFIED?                          
         BE    REQF040                                                          
         CLC   OUTCNT,=H'0'        IS THIS THE FIRST REQUEST                    
         BNE   REQF040             NO                                           
         LH    R2,OUTCNT                                                        
         LA    R2,1(R2)                                                         
         STH   R2,OUTCNT                                                        
         L     R1,AACFLTAB                                                      
REQF030  CLI   0(R1),X'FF'         EOT                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),QOPT3                                                    
         BE    *+12                                                             
         LA    R1,L'ACFLTAB(R1)                                                 
         B     REQF030                                                          
*                                                                               
         MVC   DSPARM+13(2),1(R1)   FILL IN TAPE DATASET NAME                   
         GOTO1 DYNALLOC,DMCB,(0,DDPARM),((R2),DSPARM)                           
         OPEN  (OUTP,(OUTPUT))                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQF040  XC    START,START                                                      
         CLC   QSTART,SPACES       ANY START DATE                               
         BE    REQF050                                                          
         MVC   WORK(L'QSTART),QSTART                                            
         CLC   WORK+4(2),SPACES                                                 
         BNE   *+10                                                             
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,START)                                   
*                                                                               
REQF050  MVC   END,=X'FFFFFF'                                                   
         CLC   QEND,SPACES         ANY END DATE                                 
         BE    REQF070                                                          
         MVC   WORK(L'QEND),QEND                                                
         CLC   WORK+4(2),SPACES                                                 
         BNE   REQF060                                                          
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
REQF060  GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
*                                                                               
REQF070  MVC   SMOA,=X'0000'                                                    
         CLC   QMOSSTRT,SPACES      ANY START DATE                              
         BE    REQF080                                                          
         MVC   WORK(L'QMOSSTRT),QMOSSTRT                                        
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,SMOA)                                    
*                                                                               
REQF080  MVC   EMOA,=X'FFFF'                                                    
         CLC   QMOSEND,SPACES      ANY END DATE                                 
         BE    REQF090                                                          
         MVC   WORK(L'QMOSEND),QMOSEND                                          
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(1,EMOA)                                    
*                                                                               
REQF090  XC    ACTSTRT,ACTSTRT                                                  
         CLI   QOPT4,C'A'          RUN OFF ACTIVITY DATE?                       
         BNE   REQF100                                                          
         CLC   QDUEST,SPACES       ANY ACTIVITY DATE                            
         BE    REQF100                                                          
         MVC   WORK(L'QDUEST),QDUEST                                            
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACTSTRT)                                 
*                                                                               
REQF100  CLI   QOPT2,C'Q'                 NO BOXES FOR DOWNLOADING              
         BNE   REQFX                                                            
         MVI   RCSUBPRG,9                 SET PRINT RTE AS INVALID              
         GOTO1 ADWNL,DMCB,(RC),DWNINIT    INITIALIZE DOWNLOAD RTE               
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
LDGF     DS    0H                                                               
         USING CPYELD,R1                                                        
         L     R1,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         DROP  R1                                                               
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
         L     R1,ATSTIDTB         EXCLUDE ANY IDS IN TEST TAB                  
PACC10   CLI   0(R1),EOF                                                        
         BE    PACC20                                                           
         CLC   ALPHAID,0(R1)                                                    
         BE    PACCX                                                            
         LA    R1,L'TSTIDTB(R1)                                                 
         B     PACC10                                                           
*                                                                               
PACC20   DS    0H                                                               
         GOTO1 APTIME,DMCB,(RC)    READ TIME RECORDS                            
*                                                                               
PACCX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* RUN LAST                                                           *          
**********************************************************************          
         SPACE 1                                                                
RUNL     DS    0H                                                               
         CLC   OUTCNT,=H'0'        WAS A TAPE OPENED                            
         BE    RUNLX               NO                                           
         CLOSE (OUTP)                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   QOPT2,C'Q'                 NO BOXES FOR DOWNLOADING              
         BNE   RUNLX                                                            
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOR     DOWNLOAD EOR MARKER                   
*                                                                               
RUNLX    B     EXIT                                                             
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
         EX    R1,*+8                                                           
         B     *+10                                                             
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
         L     R5,ADACC            A(ACCOUNT RECORD)                            
*                                                                               
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
         EX    R4,*+8                                                           
         B     *+10                                                             
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
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),NAMEREC                                                  
*                                                                               
GETNX    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
AACFLTAB DC    A(ACFLTAB)          ACC FILE TABLE                               
ATSTIDTB DC    A(TSTIDTB)          TEST ID TABLE                                
ATYPTAB  DC    A(TYPTAB)           TYPE TABLE                                   
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
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
         DC    A(DWNBUF)           DOWNLOAD BUFFER                              
         DC    A(DWNRTE)           ROUTINE TO DWNLD RECS (QUE/DATASET)          
         DC    A(DWNL)             ROUTINE TO INTERFACE BTW DWNR&DLFLD          
         DC    A(PTIME)            ROUTINE TO PROCESS TIME RECS                 
*                                                                               
         DC    A(TIMETAB)          TIME LINE TABLE                              
         DC    A(TMPTAB)           TEMPO LINE TABLE                             
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
**********************************************************************          
* GETEL                                                              *          
**********************************************************************          
         SPACE 1                                                                
DDPARM   DC    CL8'OUTP'                                                        
DSPARM   DC    CL20'ACCTAPE.AC0ZJXX1' THE XX WILL BECOME THE ACC FILE           
OUTCNT   DC    H'0'                   RELATIVE GENERATION FOR DYNALLOC          
*                                                                               
OUTP     DCB   DDNAME=OUTP,DSORG=PS,RECFM=VB,LRECL=L'TPREC,MACRF=PM,   X        
               BLKSIZE=0                                                        
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
BINA10   GOTO1 BINSRCH,DMCB,(X'01',(R3)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
*                                                                               
         CLI   DMCB,1              RECORD WAS ADDED                             
         BE    BINXIT                                                           
*                                                                               
         TM    FLAG,FLGTIM ARE WE DOING A TIME ADD?                             
         BNO   BINXIT                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TIMEDUP-TIMED(R3)                                             
         AHI   R1,1                                                             
         STC   R1,TIMEDUP-TIMED(R3)                                             
         B     BINA10                                                           
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
         MVC   SV1RACC,ACTKACT     SAVE OFF ACCOUNT CODE                        
*                                                                               
         USING TIMRECD,R2                                                       
         LA    R2,SVKEY                                                         
         MVC   TIMKEY,SPACES       CLEAR KEY                                    
         MVC   TIMKCPY,ACTKCPY     COMPANY CODE                                 
         MVC   TIMKUNT(2),=C'1R'   U/L                                          
         MVC   TIMKACT,ACTKACT     MOVE IN ACCOUNT CODE                         
*                                                                               
*        MVC   MSG,=CL10'ACCOUNT'                                               
*        GOTO1 ADUMP,DMCB,(RC),ACTKEY,15                                        
         DROP  R3                                                               
*                                                                               
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         B     PTIME015                                                         
*                                                                               
PTIME010 GOTO1 =A(DMSEQDR),DMCB,(RC)            READ SEQ                        
PTIME015 CLC   SVKEY(TIMKOFF-TIMKEY),IOKEY      SAME KEY?                       
         BNE   PTIME999                                                         
*                                                                               
         LA    R2,IOKEY                                                         
         TM    TIMKSTAT,TIMSDELT   IS THIS RECORD DELETED?                      
         BO    PTIME010                                                         
         CLC   TIMKREF,=C'*TIME*'  LOOKING FOR ONLY TIME TRANSACTIONS           
         BNE   PTIME010                                                         
*                                                                               
         CLC   TIMKPEDT,START                                                   
         BL    PTIME010            IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    PTIME010            IGNORE AFTER END DATE OF REQUEST             
         CLI   QOPT5,C'A'          ONLY APPROVED TIMESHEETS?                    
         BNE   *+12                                                             
         TM    TIMKSTAT,TIMSFAPP                                                
         BNO   PTIME010            IGNORE IF NOT A MATCH                        
         CLI   QOPT5,C'I'          ONLY IN PROGRESSS TIMESHEETS?                
         BNE   *+12                                                             
         CLI   TIMKSTAT,0                                                       
         BNE   PTIME010            IGNORE IF NOT A MATCH                        
*                                                                               
         CLI   QOPT6,C'E'          EXCLUDE CERTAIN FULLY APPROVED               
         BNE   PTIME030                                                         
         LA    R1,XPERTAB                                                       
PTIME020 CLI   0(R1),EOF                                                        
         BE    PTIME030                                                         
         CLC   TIMKACT+6(6),0(R1)                                               
         BE    PTIME010                                                         
         LA    R1,L'XPERTAB(R1)                                                 
         B     PTIME020                                                         
*                                                                               
PTIME030 GOTO1 =A(DMGETREC),DMCB,(RC)   GET RECORD                              
*                                                                               
         USING TIMELD,R4                                                        
         LA    R4,TIMRFST                                                       
*                                                                               
         XC    SVPID,SVPID         SAVED AREA FOR PID NUMBER                    
         XC    SVLMADT,SVLMADT     LINE MANAGER SUBMIT DATE                     
         XC    SVCLADT,SVCLADT     CLIENT LEVEL SUBMIT DATE                     
         LR    R6,R4               LOOP THROUGH TIME REC FOR PIDEL              
PTIME040 CLI   0(R6),0             AT THE END OF THE RECORD?                    
         BE    PTIME080                                                         
         CLI   0(R6),PIDELQ                                                     
         BE    PTIME060                                                         
         CLI   0(R6),GDAELQ                                                     
         BE    PTIME070                                                         
PTIME050 SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     PTIME040                                                         
*                                                                               
         USING PIDELD,R6                                                        
PTIME060 MVC   SVPID,PIDNO         SAVE PID NUMBER TO FILL IN LINE              
         B     PTIME050                                                         
         DROP  R6                                                               
*                                                                               
         USING GDAELD,R6                                                        
PTIME070 LA    RE,SVCLADT          ASSUME ITS CLIENT APPROVAL                   
         CLI   GDATYPE,GDACLSUB                                                 
         BE    *+8                                                              
         LA    RE,SVLMADT          LINE MANAGER APPROVAL                        
         MVC   0(L'SVCLADT,RE),GDADATE                                          
         B     PTIME050                                                         
         DROP  R6                                                               
*                                                                               
PTIME080 CLI   0(R4),0             AT THE END OF THE ELEMENT                    
         BE    PTIME010            READ NEXT RECORD                             
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   PTIME140                 ELSE LOOP                               
         CLI   TIMETYP,TIMEINP     ONLY PROCESS INPUT DETAILS                   
         BNE   PTIME140                 ELSE LOOP                               
*                                                                               
         USING TIMED,R5                                                         
         LA    R5,TIMEWRK                                                       
         MVC   TIMEWRK,SPACES                                                   
         XC    TIMEDUP,TIMEDUP                                                  
*                                                                               
         USING TMPTBD,R3                                                        
         LA    R3,TMPWRK                                                        
         MVC   TMPWRK,SPACES                                                    
*                                                                               
         MVC   TIMEPEDT,TIMKPEDT   PERIOD END DATE                              
         MVC   TIMELMDT,SVLMADT    LINE MANAGER APPROVED DATE                   
         MVC   TIMECLDT,SVCLADT    CLIENT LEVEL APPROVED DATE                   
         MVC   TIMEDA,SVDA         DISK ADDRESS                                 
*                                                                               
         OC    TIMESTA,TIMIND      TURN ON INDICATOR BITS                       
         MVC   TIMEPID,SVPID                                                    
         MVC   TIMETTYP,TIMTTYP    TYPE OF TIME                                 
         CLI   TYPEFLT,0           DO WE WANT TO FILTER ON TYPE                 
         BE    PTIME090                                                         
         CLC   TYPEFLT,TIMETTYP    DO WE WANT THIS?                             
         BNE   PTIME140                                                         
*                                                                               
PTIME090 MVC   TIMELN#,TIMLINE#    TMS LINE NUMBER                              
         MVC   TIMETSK,TIMTSK      TASK CODE                                    
         MVC   TIMEPOFF,TIMOFF     PRODUCTION OFFICE                            
         CLC   TIMMOA,SMOA                                                      
         BL    PTIME140                                                         
         CLC   TIMMOA,EMOA                                                      
         BH    PTIME140                                                         
         MVC   TIMEMOA,TIMMOA      MONTH OF ACTIVITY                            
         MVC   TIMEACDT,TIMADAT    ACTIVITY DATE                                
         MVC   TIMEACT,TIMACC      SJ/1N ACCOUNT                                
         ZAP   TIMEBKT,TIMHRS      HOURS                                        
         MVC   TIMECAC,TIMKCACT    1C CONTRA                                    
*                                                                               
         ZAP   TIMERTE,=P'0'                                                    
         CLI   TIMLN,TIMILN1Q      ANY BILLABLE INFO?                           
         BNH   *+16                                                             
         MVC   TIMEINC,TIMINC      INCOME ACCOUNT                               
         MVC   TIMERTE,TIMRATE     RATE                                         
*                                                                               
         XC    TIMETLN#,TIMETLN#   TEMPO LINE NUMBER                            
         XC    TIMEAEDT,TIMEAEDT   ACTUAL END DATE                              
         XC    TIMESTA,TIMESTA     CLEAR TIMEL STATUS BYTE                      
         MVC   TIMESTAT,TIMRSTAT   RECORD STATUS BYTE                           
         TM    TIMSTAT,TIMTEMPO    IS THIS A TEMPO LINE                         
         BNO   *+14                                                             
         MVC   TMPPEDT,TIMKPEDT    SAVE OFF PER END DATE IN TEMPO TAB           
         OI    TIMESTA,TIMTEMPO    SET ISFROMTEMPO BIT                          
         TM    TIMSTAT,TIMSMCS     IS THIS A BRANDO LINE                        
         BNO   *+8                                                              
         OI    TIMESTA,TIMEBRO     MARK T/S AS A BRANDO T/S                     
         LR    R6,R4               SAVE OFF R4                                  
PTIME100 SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1               BUMP TO TEMPO EXTRA                          
         CLI   0(R4),0             AT THE END OF THE RECORD?                    
         BE    PTIME130                                                         
         CLI   TIMEL,TIMELQ        MAKE SURE WE ARE AT AN 8B                    
         BNE   PTIME100                                                         
         CLI   TIMETYP,TIMEINP     ONLY PROCESS INPUT DETAILS                   
         BNE   *+10                     ELSE LOOP                               
         LR    R4,R6               RESET R4                                     
         B     PTIME130                                                         
*                                                                               
         CLI   TIMETYP,TIMETIME    IS THE THE BRANDO ELEMENT?                   
         BNE   PTIME110                                                         
         MVC   TIMEAEDT,TIMETPDT   ACTUAL PERIOD END DATE                       
         MVC   TIMEASDT,TIMETDT1   BRANDO START DATE                            
         MVC   TIMETLN#,TIMEIDNO   BRANDO TIME ROW #                            
         B     PTIME100                                                         
*                                                                               
PTIME110 CLI   TIMETYP,TIMEMOV#    IS THE THE TIME MOVE ELEMENT?                
         BNE   PTIME120                                                         
         OC    TIMMREF#,TIMMREF#   ANY REVERSAL #?                              
         BZ    PTIME120                                                         
         MVC   TIMEREV#,TIMMREF#   SAVE OFF TIME MOVE #                         
         B     PTIME100                                                         
*                                                                               
PTIME120 CLI   TIMETYP,TIMEXTRA    IS THE THE TEMPO EXTRA?                      
         BNE   PTIME100                                                         
         OI    FLAG,FLGTIM                                                      
         MVC   TIMETLN#,TIMXTLN#   TEMPO LINE #                                 
         MVC   TIMEAEDT,TIMXTPDT   ACTUAL PERIOD END DATE                       
         MVC   SVENPDT,TIMXTPDT    ACTUAL PERIOD END DATE                       
         B     PTIME100                                                         
*                                                                               
PTIME130 GOTO1 ABINADD,DMCB,(RC),TIMEWRK,ATIMETAB   ADD TABLE ENTRY             
         NI    FLAG,X'FF'-FLGTIM                                                
         CLC   TMPPEDT,SPACES      IS THIS LINE A TEMPO LINE?                   
         BE    PTIME140                                                         
         GOTO1 ABINADD,DMCB,(RC),TMPWRK,ATMPTAB     ADD TABLE ENTRY             
*                                                                               
PTIME140 SR    R1,R1                                                            
         IC    R1,TIMLN                                                         
         AR    R4,R1                                                            
         B     PTIME080                                                         
*                                                                               
PTIME999 DS    0H                                                               
         BAS   RE,ADJTIME          MAKE TIME TABLE AS TEMPO                     
         BAS   RE,ACCL             SIMULATE ACCOUNT LAST W/ACC INFO             
*                                                                               
PTIMEX   XMOD1                                                                  
         DROP  R2,R3,R4,R5                                                      
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
         XC    LSTPEDT,LSTPEDT     CLEAR SAVED AREA FOR T/S COMPARE             
         NI    FLAG,X'FF'-(FLGTMP+FLGBRO+FLGBROBT)                              
ACCL10   DS    0H                                                               
         MVC   PRTLINE(PLINELNQ),XSPACES                                        
         CLI   QOPT1,C'Y'          ELIMINATE ALL NONE TEMPO PEOPLE?             
         BNE    *+14                                                            
         OC    TIMEPID,TIMEPID     IF NO PID SKIP T/S                           
         BZ    ACCL110                                                          
         CLI   QOPT1,C'T'          ELIMINATE ALL NONE TEMPO T/S?                
         BNE    *+12                                                            
         TM    TIMESTA,TIMETMP     IF NOT TEMPO - SKIP                          
         BZ    ACCL110                                                          
         CLI   QOPT1,C'B'          ELIMINATE ALL NONE BRANDO T/S?               
         BNE    *+12                                                            
         TM    TIMESTA,TIMEBRO     IF NOT BRANDO - SKIP                         
         BZ    ACCL110                                                          
         CLI   QOPT4,C'A'          ONLY INCLUDE THOSE T/S FOR REQ DTE           
         BNE    *+14                                                            
         CLC   TIMEACDT,ACTSTRT    MATCH ON ACTIVITY DATE                       
         BL    ACCL110                                                          
         CLI   TIMETTYP,TIMTCB     DOES IT HAVE B TIME                          
         BNE   *+8                                                              
         OI    FLAG,FLGBROBT                                                    
         CLI   QOPT6,C'N'          N TIME TO THE JOB ONLY                       
         BNE   ACCL12                                                           
         CLI   TIMETTYP,X'03'      N TIME                                       
         BE    *+12                                                             
         CLI   TIMETTYP,X'16'      NC TIME                                      
         BNE   ACCL110                                                          
         CLC   TIMEAJOB,SPACES                                                  
         BNH   ACCL110                                                          
ACCL12   CLC   LSTPEDT,TIMEPEDT    SAME TIMESHEET?                              
         BE    ACCL15                                                           
         MVC   SVPMOA,TIMEMOA                                                   
         MVC   SVSTAT,TIMESTAT     SAVE OFF STATUS BYTE                         
         NI    FLAG,X'FF'-(FLGTMP+FLGBRO+FLGBROBT)                              
         MVC   LSTPEDT,TIMEPEDT    SAVE OFF FOR LATER COMPARE                   
         MVI   FORCEHED,C'Y'       FORCE TO NEW PAGE                            
         TM    TIMESTA,TIMETMP     IS THIS TIMESHEET A TEMPO T/S                
         BNO   *+12                                                             
         BAS   RE,GETTMP           GET TEMPO INFO (IF ANY)                      
         OI    FLAG,FLGTMP         SET FLAG FOR HEADLINES/DWNLD                 
         TM    TIMESTA,TIMEBRO     IS THIS TIMESHEET A BRANDO T/S               
         BNO   *+8                                                              
         OI    FLAG,FLGBRO         SET FLAG FOR HEADLINES/DWNLD                 
         CLI   TIMETTYP,TIMTCB     DOES IT HAVE B TIME                          
         BNE   *+8                                                              
         OI    FLAG,FLGBROBT                                                    
*                                                                               
ACCL15   CLI   QOPT8,C'Y'          FILTER ON DIFFERENT MOAS?                    
         BNE   ACCL15A                                                          
         TM    FLAG,FLGBRO                                                      
         BNO   ACCL110                                                          
         TM    FLAG,FLGBROBT                                                    
         BNO   ACCL110                                                          
         CLC   SVPMOA,TIMEMOA                                                   
         BE    ACCL110                                                          
ACCL15A  MVC   SVPMOA,TIMEMOA                                                   
*                                                                               
         XC    SVLMADT,SVLMADT     LINE MANAGER SUBMIT DATE                     
         XC    SVCLADT,SVCLADT     CLIENT LEVEL SUBMIT DATE                     
         EDIT  TIMELN#,PLN#        TMS LINE NUMBER                              
         EDIT  TIMETLN#,PTMPLN#    TEMPO LINE NUMBER                            
         GOTO1 DATCON,DMCB,(1,TIMEACDT),(X'20',PADDDTE)  ACTIVITY DATE          
         OC    TIMEASDT,TIMEASDT   ANY ACTUAL START DATE?                       
         BZ    *+10                                                             
         MVC   SVSTPDT,TIMEASDT                                                 
         OC    SVSTPDT,SVSTPDT     ANY ACTUAL START DATE?                       
         BZ    ACCL17                                                           
ACCL16   GOTO1 DATCON,DMCB,(1,SVSTPDT),(X'20',PASTDTE) ACTUAL ST DTE            
         B     ACCL20                                                           
ACCL17   OC    TIMELMDT,TIMELMDT   ANY LINE MANAGER SUBMIT DATE?                
         BZ    ACCL20                                                           
         GOTO1 DATCON,DMCB,(1,TIMELMDT),(X'20',PASTDTE) LMA SUBMIT DATE         
         MVC   SVLMADT,TIMELMDT    SAVE OFF LINE MAN APP DTE FOR HEADUP         
ACCL20   OC    TIMEAEDT,TIMEAEDT   ANY ACTUAL END DATE?                         
         BZ    ACCL25                                                           
         GOTO1 DATCON,DMCB,(1,TIMEAEDT),(X'20',PAENDTE) ACTUAL END DTE          
         B     ACCL30                                                           
ACCL25   OC    TIMECLDT,TIMECLDT   ANY CLIENT APPROVAL SUBMIT DATE?             
         BZ    ACCL30                                                           
         GOTO1 DATCON,DMCB,(1,TIMECLDT),(X'20',PAENDTE) CLA SUBMIT DATE         
         MVC   SVCLADT,TIMECLDT    SAVE OFF CLI LEV APP DTE FOR HEADUP          
*                                                                               
ACCL30   DS    0H                                                               
*        MVC   MSG,=CL10'TIMED'                                                 
*        GOTO1 ADUMP,DMCB,(RC),(R2),TIMELNQ                                     
*                                                                               
*        MVC   MSG,=CL10'PLINED B4'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R5),PLINELNQ                                    
*                                                                               
         MVC   PACT,TIMEACT        SJ/1N ACCOUNT                                
         CLC   TIMEREV#,SPACES     ANY REVISION #?                              
         BE    ACCL40                                                           
         EDIT  TIMEREV#,PREV#                                                   
*                                                                               
ACCL40   MVC   PTASK,TIMETSK       TASK CODE                                    
         MVC   PCONTRA,TIMECAC     CONTRA ACCOUNT                               
         GOTO1 DATCON,DMCB,(1,TIMEPEDT),(8,SVPEDT)                              
         GOTO1 HEXOUT,DMCB,TIMEDA,PDUP,L'TIMEDA                                 
*                                                                               
*        MVC   MSG,=CL10'PLINED AF'                                             
*        GOTO1 ADUMP,DMCB,(RC),(R5),PLINELNQ                                    
*                                                                               
         OC    TIMEPID,TIMEPID     ANY PID?                                     
         BZ    ACCL50                                                           
         LA    R1,PKPIDCNT         INCREMENT EITHER TMS PID                     
         TM    TIMESTA,TIMTEMPO    IS IT A TEMPO TIMESHEET?                     
         BNO   *+8                                                              
         LA    R1,PKTPDCNT            OR TEMPO PID REC COUNTER                  
         AP    0(L'PKFLDS,R1),=P'1'                                             
         SR    R7,R7                                                            
         ICM   R7,3,TIMEPID                                                     
         EDIT  (R7),PPID                                                        
*                                                                               
ACCL50   CLC   TIMERTE,SPACES                                                   
         BE    ACCL60                                                           
         EDIT  TIMERTE,PRATE,FLOAT=-                                            
ACCL60   MVC   PINCACT,TIMEINC     INCOME ACCOUNT                               
         MVC   POFF,TIMEPOFF       OFFICE                                       
         MVC   WORK,TIMEMOA                                                     
         MVI   WORK+2,X'01'                                                     
         GOTO1 DATCON,DMCB,(1,WORK),(9,PMOA)                                    
         GOTO1 (RF),DMCB,,(X'20',WORK)                                          
         MVC   SVMOA,WORK                                                       
*                                                                               
         MVI   PBHRS+L'PBHRS-1,C'0'       SET BUCKET TO ZERO                    
         CLI   TIMETTYP,TIMTCB            B TIME                                
         BNE   ACCL70                                                           
         MVI   PTYPE,C'B'                                                       
         EDIT  TIMEBKT,PBHRS,FLOAT=-                                            
*                                                                               
ACCL70   MVI   PRHRS+L'PRHRS-1,C'0'       SET BUCKET TO ZERO                    
         CLI   TIMETTYP,TIMTCR            R TIME                                
         BNE   ACCL80                                                           
         MVI   PTYPE,C'R'                                                       
         EDIT  TIMEBKT,PRHRS,FLOAT=-                                            
*                                                                               
ACCL80   MVI   PNHRS+L'PNHRS-1,C'0'       SET BUCKET TO ZERO                    
         CLI   TIMETTYP,TIMTCN            N TIME                                
         BNE   ACCL90                                                           
         MVI   PTYPE,C'N'                                                       
         EDIT  TIMEBKT,PNHRS,FLOAT=-                                            
*                                                                               
ACCL90   MVI   PNCHRS+L'PNCHRS-1,C'0'     SET BUCKET TO ZERO                    
         CLI   TIMETTYP,TIMTNC            NON CLIENT TIME                       
         BNE   ACCL100                                                          
         MVC   PTYPE,=C'NC'                                                     
         EDIT  TIMEBKT,PNCHRS,FLOAT=-                                           
*                                                                               
ACCL100  MVC   PTEMPO,=C'*TMS*'    SET TMS AS DEFAULT                           
         TM    TIMESTA,TIMEBRO     IS IT A BRANDO TIMESHEET?                    
         BNO   *+10                                                             
         MVC   PTEMPO,=CL5'*BRO*'                                               
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
         CLI   QOPT3,C'Y'          DO THEY WANT DUPS DELINEATED?                
         BNE   *+18                                                             
         CLI   TIMEDUP,0           IS IT A DUPLICATE                            
         BE    *+10                                                             
         MVC   PDUP,=C'**DUP**'                                                 
*                                                                               
         BAS   RE,PRINTIT                                                       
*                                                                               
ACCL110  LA    R2,TIMELNQ(R2)                                                   
         BCT   R3,ACCL10                                                        
*                                                                               
         CP    PKCOUNT,=P'0'                                                    
         BE    ACCLX                                                            
         MVC   PACT(36),=CL36'TOTAL NUMBER OF T/S              : '              
         EDIT  PKCOUNT,PRATE,ZERO=NOBLANK                                       
         BAS   RE,PRINTIT                                                       
         MVC   PACT(36),=CL36'TOTAL NUMBER OF TMS T/S          : '              
         ZAP   PKFLDS,PKCOUNT                                                   
         SP    PKFLDS,PKTMPCNT     SUBTRACT OUT TEMPO                           
         EDIT  PKFLDS,PRATE,ZERO=NOBLANK                                        
         BAS   RE,PRINTIT                                                       
         MVC   PACT(36),=CL36'TOTAL NUMBER OF TEMPO T/S        : '              
         EDIT  PKTMPCNT,PRATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         MVC   PACT(36),=CL36'TOTAL NUMBER OF T/S W/PIDS       : '              
         ZAP   PKFLDS,PKPIDCNT                                                  
         AP    PKFLDS,PKTPDCNT                                                  
         EDIT  PKFLDS,PRATE,ZERO=NOBLANK                                        
         BAS   RE,PRINTIT                                                       
         MVC   PACT(36),=CL36'TOTAL NUMBER OF TMS T/S W/PIDS   : '              
         EDIT  PKPIDCNT,PRATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
         MVC   PACT(36),=CL36'TOTAL NUMBER OF TEMPO T/S W/PIDS : '              
         EDIT  PKTPDCNT,PRATE,ZERO=NOBLANK                                      
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
ACCLX    XIT1                                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT THE DATA                                                     *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         CLI   QOPT2,C'D'          WAS DOWNLOAD W/ TAPE SELECTED?               
         BE    PRINT10                                                          
         CLI   QOPT2,C'Q'          WAS STRAIGHT DOWNLOAD SELECTED?              
         BNE   PRINT20                                                          
PRINT10  GOTO1 ADWNRTE,DMCB,(RC)   IF SO POINT TO DOWNLOAD                      
         B     PRINTX              SKIP HEAD UP ROUTINE                         
*                                                                               
PRINT20  BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
*                                                                               
PRINTX   XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* GET TEMPO INFO (IF ANY)                                            *          
*     R2 = A(BINTABLE ENTRY)                                         *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMED,R2                                                         
         USING TSXRECD,R3                                                       
GETTMP   NTR1                                                                   
         LA    R3,SVKEY                                                         
         XC    TSXKEY,TSXKEY       CLEAR KEY                                    
         MVC   TSXKEY(TSXKODS-TSXKEY+L'TSXKODS),SPACES                          
*        MVC   TSXKEY,SPACES                                                    
         MVI   TSXKTYP,TSXKTYPQ    X'3E'                                        
         MVI   TSXKSUB,TSXKSUBQ    X'13'                                        
         MVC   TSXKCPY,RCCOMPFL    COMPANY CODE                                 
         MVC   TSXKPER,LEVDCDE     PERSON CODE                                  
         MVC   TSXKEND,TIMEPEDT    PERIOD END DATE                              
         SR    R1,R1                                                            
         IC    R1,LEVC                                                          
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TSXKODS(0),SV1RACC         SET UP O/D/S                          
         GOTO1 =A(DMHIGHDR),DMCB,(RC)     READ HIGH                             
         CLC   SVKEY(TSXKODS-TSXKEY+L'TSXKODS),IOKEY  SAME RECORD?              
         BNE   GETTMPX                                                          
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
GETTMP10 CLI   0(R4),0             AT THE END OF THE ELEMENT                    
         BE    GETTMPX             READ NEXT RECORD                             
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
         B     GETTMP10                                                         
*                                                                               
GETTMPX  DS    0H                                                               
         XIT1                                                                   
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
**********************************************************************          
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   XHEAD2+1(10),=CL10'LOGIN ID :'                                   
         MVC   XHEAD2+12(L'SVCLOGO),SVCLOGO  COMPANY'S MAIN ID                  
*                                                                               
         USING HEADD,R3                                                         
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
         TM    SVSTAT,TIMSFAPP                                                  
         BZ    *+10                                                             
         MVC   HEADSTAT,=CL24'FULLY APPROVED'                                   
         TM    SVSTAT,TIMSSUBM                                                  
         BZ    *+10                                                             
         MVC   HEADSTAT,=CL24'SUBMITTED'                                        
         TM    SVSTAT,TIMSPAPP                                                  
         BZ    *+10                                                             
         MVC   HEADSTAT,=CL24'PART APPROVED'                                    
*        TM    SVSTAT,TIMSAWAP                                                  
*        BZ    *+10                                                             
*        MVC   HEADSTAT,=CL24'AWAITING LINE MANGR APPL'                         
         TM    SVSTAT,TIMSREJE                                                  
         BZ    *+10                                                             
         MVC   HEADSTAT,=CL24'REJECTED'                                         
*        TM    SVSTAT,TIMSMAAP                                                  
*        BZ    *+10                                                             
*        MVC   HEADSTAT,=CL24'LINE MANAGER APPROVED'                            
         CLI   SVSTAT,0                                                         
         BNE   *+10                                                             
         MVC   HEADSTAT,=CL24'IN PROGRESS'                                      
*                                                                               
         MVC   XHEAD11+27(7),=CL7'PER STR'                                      
         OC    SVSTPDT,SVSTPDT     ANY ACTUAL START DATE?                       
         BNZ   HEADUP10                                                         
         OC    SVLMADT,SVLMADT     ANY LINE MANAGER SUBMIT DATE?                
         BZ    HEADUP10                                                         
         MVC   XHEAD11+27(7),=CL7'LMA SUB'                                      
HEADUP10 MVC   XHEAD11+35(7),=CL7'PER END'                                      
         OC    SVENPDT,SVENPDT     ANY ACTUAL END DATE?                         
         BNZ   HEADUP20                                                         
         OC    SVCLADT,SVCLADT     ANY SLIENT LEVEL SUBMIT DATE?                
         BZ    HEADUP20                                                         
         MVC   XHEAD11+35(7),=CL7'CLA SUB'                                      
*                                                                               
HEADUP20 TM    FLAG,FLGTMP         IS T/S A TEMPO T/S?                          
         BNO   *+10                                                             
         MVC   HEADNAME+L'HEADNAME(9),=C'**TEMPO**'      EYECATCHER             
         TM    FLAG,FLGBRO         IS T/S A BRANDO T/S?                         
         BNO   HEADUPX                                                          
         MVC   HEADNAME+L'HEADNAME(10),=C'**BRANDO**'    EYECATCHER             
*                                                                               
HEADUPX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
XPERTAB  DS    0CL6                                                             
         DC    CL6'HALSHA'                                                      
         DC    CL6'CRAARL'                                                      
         DC    CL6'PARALE'                                                      
         DC    CL6'ROJLEO'                                                      
         DC    CL6'SAMMIK'                                                      
         DC    CL6'STUPET'                                                      
         DC    CL6'ZHATAO'                                                      
         DC    CL6'ALLJOV'                                                      
         DC    CL6'CEPNIL'                                                      
         DC    CL6'DROELE'                                                      
         DC    CL6'GALNIC'                                                      
         DC    CL6'KOLMIC'                                                      
         DC    CL6'MACCRA'                                                      
         DC    CL6'MIDJEF'                                                      
         DC    CL6'ROWCRA'                                                      
         DC    CL6'VANRUS'                                                      
         DC    CL6'YANJER'                                                      
         DC    CL6'COWSHA'                                                      
         DC    CL6'DANYUL'                                                      
         DC    CL6'JULJOR'                                                      
         DC    CL6'KIMROY'                                                      
         DC    CL6'MCCCHR'                                                      
         DC    CL6'RATJOS'                                                      
         DC    CL6'ZLOSTA'                                                      
         DC    CL6'DOJJAS'                                                      
         DC    CL6'THOMIC'                                                      
         DC    CL6'ACCTHO'                                                      
         DC    CL6'VIELUI'                                                      
         DC    CL6'PALMAR'                                                      
         DC    CL6'BYLNUA'                                                      
         DC    CL6'BASNAT'                                                      
         DC    CL6'FALPHI'                                                      
         DC    CL6'FERJAC'                                                      
         DC    CL6'ROSCIN'                                                      
         DC    CL6'SHIGIL'                                                      
         DC    CL6'TSIJOA'                                                      
         DC    CL6'BURALI'                                                      
         DC    CL6'FAGJIL'                                                      
         DC    CL6'GOKPRA'                                                      
         DC    CL6'CLUTOM'                                                      
         DC    CL6'FRAJAM'                                                      
         DC    CL6'GALKIR'                                                      
         DC    CL6'HOPRIT'                                                      
         DC    CL6'JORMAR'                                                      
         DC    CL6'LEEKEE'                                                      
         DC    CL6'PETJUL'                                                      
         DC    CL6'DIBJOS'                                                      
         DC    CL6'PERMAR'                                                      
         DC    CL6'TWYMAT'                                                      
         DC    CL6'VICSCO'                                                      
         DC    CL6'ARMBLA'                                                      
         DC    CL6'LAMJEN'                                                      
         DC    AL1(EOF)                                                         
         EJECT                                                                  
**********************************************************************          
* LITERALS                                                           *          
**********************************************************************          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD ROUTINE                                                   *          
*          R2   - ADRESS OF BINTABLE ENTRY                           *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMED,R2                                                         
         USING PLINED,R5                                                        
DWNRTE   DS    0D                                                               
         NMOD1 0,*DWNR*                                                         
         L     RC,0(R1)                                                         
         LA    R5,XP                                                            
*                                                                               
         MVC   SVPRNT,XP           SAVE OFF PRINTLINE FOR LATER                 
         LA    R4,RECBUF                                                        
         MVC   RECBUF,XSPACES                                                   
*                                                                               
         CLI   QOPT2,C'D'          DID THEY REQUEST DWNLD W/ A TAPE?            
         BNE   DWNR130                                                          
         XC    TPRECHD,TPRECHD     CLEAR TAPE HEADER                            
*                                                                               
* ALPHAID                                                                       
*                                                                               
         MVC   0(L'ALPHAID,R4),ALPHAID                                          
         LA    R4,L'ALPHAID(R4)                                                 
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* PERIOD END DATE                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIMEPEDT),(10,WORK)                               
         MVC   0(8,R4),WORK                                                     
         LA    R4,8(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* 1R OFFICE CODE                                                                
*                                                                               
         LA    R3,LEVACDE+L'LEVACDE-1 POINT TO END OF FILED                     
         LA    R1,L'LEVACDE        FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LEVACDE                                                  
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* 1R DEPARTMENT CODE                                                            
*                                                                               
         LA    R3,LEVBCDE+L'LEVBCDE-1 POINT TO END OF FILED                     
         LA    R1,L'LEVBCDE        FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LEVBCDE                                                  
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* 1R SUB DEPARTMENT CODE                                                        
*                                                                               
         LA    R3,LEVCCDE+L'LEVCCDE-1 POINT TO END OF FILED                     
         LA    R1,L'LEVCCDE        FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LEVCCDE                                                  
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* PERSON CODE                                                                   
*                                                                               
         LA    R3,LEVDCDE+L'LEVDCDE-1 POINT TO END OF FILED                     
         LA    R1,L'LEVDCDE        FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LEVDCDE                                                  
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* PID NUMBER                                                                    
*                                                                               
         CLC   PPID,SPACES         ANY PID?                                     
         BNE   *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR10                                                           
*                                                                               
         LA    R3,PPID             POINT TO STR OF FILED                        
         LA    R1,L'PPID           FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* PERIOD NUMBER                                                                 
*                                                                               
DWNR10   LA    R3,SVPERIOD         POINT TO STR OF FILED                        
         LA    R1,L'SVPERIOD       FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* TIMESHEET STATE TEMPO/TMS                                                     
*                                                                               
         MVC   0(5,R4),=C'TEMPO'                                                
         TM    FLAG,FLGTMP                                                      
         BO    *+10                                                             
         MVC   0(5,R4),=C'*TMS*'                                                
         LA    R4,5(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* LINE NUMBER - TEMPO OR TMS                                                    
*                                                                               
         LA    R3,PLN#             TMS LINE # IS DEFAULT                        
         OC    TIMETLN#,TIMETLN#   TEMPO LINE IS OVERRIDE                       
         BZ    *+8                                                              
         LA    R3,PTMPLN#                                                       
         LA    R1,L'PLN#           FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* LOCATION START DATE                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(1,SVSTPDT),(10,WORK)                                
         MVC   0(8,R4),WORK                                                     
         LA    R4,8(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* LOCATION END DATE                                                             
*                                                                               
         OC    TIMEAEDT,TIMEAEDT   IF NO END DATE SKIP                          
         BNZ   *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR50                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIMEAEDT),(10,WORK)                               
         MVC   0(8,R4),WORK                                                     
         LA    R4,8(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* ACCOUNT (IF ANY)                                                              
*                                                                               
DWNR50   CLC   PACT,SPACES                                                      
         BH    *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR60                                                           
*                                                                               
         LA    R3,PACT+L'PACT-1    POINT TO END OF FILED                        
         LA    R1,L'PACT           FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PACT                                                     
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* TASK CODE                                                                     
*                                                                               
DWNR60   CLC   PTASK,SPACES        ANY TASK CODE?                               
         BNE   *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR70                                                           
*                                                                               
         MVC   0(2,R4),PTASK                                                    
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* TYPE CODE                                                                     
*                                                                               
DWNR70   DS    0H                                                               
         LA    R3,PTYPE+L'PTYPE-1  POINT TO END OF FILED                        
         LA    R1,L'PTYPE          FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PTYPE                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* RATE                                                                          
*                                                                               
         CP    TIMERTE,=P'0'                                                    
         BNZ   *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR80                                                           
*                                                                               
         LA    R3,PRATE            POINT TO STR OF FILED                        
         LA    R1,L'PRATE          FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* INCOME/SUSPENSE ACCOUNT                                                       
*                                                                               
DWNR80   CLC   PINCACT,SPACES      ANY INCOME/SUSPENS?                          
         BH    *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR90                                                           
*                                                                               
         LA    R3,PINCACT+L'PINCACT-1 POINT TO END OF FILED                     
         LA    R1,L'PINCACT        FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PINCACT                                                  
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* PRODUCTION OFFICE                                                             
*                                                                               
DWNR90   CLC   POFF,SPACES         ANY OFFICE?                                  
         BNE   *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR100                                                          
*                                                                               
         LA    R3,POFF+L'POFF-1    POINT TO END OF FILED                        
         LA    R1,L'POFF           FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),POFF                                                     
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* BILLABLE HOURS                                                                
*                                                                               
DWNR100  LA    R3,PBHRS            POINT TO STR OF FILED                        
         LA    R1,L'PBHRS          FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* CHARGEABLE HOURS                                                              
*                                                                               
         LA    R3,PRHRS            POINT TO STR OF FILED                        
         LA    R1,L'PRHRS          FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* NON-BILLIABLE HOURS                                                           
*                                                                               
         LA    R3,PNHRS            POINT TO STR OF FILED                        
         LA    R1,L'PNHRS          FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* NON-CLIENT HOURS                                                              
*                                                                               
         LA    R3,PNCHRS           POINT TO STR OF FILED                        
         LA    R1,L'PNCHRS         FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* TIME LINE ATTRIBUTE (TEMPO/TMS)                                               
*                                                                               
         MVC   0(5,R4),PTEMPO                                                   
         LA    R4,5(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* CONTRA ACCOUNT                                                                
*                                                                               
         CLC   PCONTRA,SPACES      ANY CONTRA?                                  
         BNE   *+16                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         B     DWNR110                                                          
*                                                                               
         LA    R3,PCONTRA+L'PCONTRA-1 POINT TO END OF FILED                     
         LA    R1,L'PCONTRA        FIND OUT LENGTH OF ACTUAL DATA               
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         BCTR  R1,0                DECREMENT FOR EX                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),PCONTRA                                                  
         LA    R4,1(R1,R4)                                                      
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
* MOA (BROKEN OUT INTO TWO FLDS YY AND MM)                                      
*                                                                               
DWNR110  CLC   SVMOA,SPACES        ANY MOA?                                     
         BNE   *+20                                                             
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         B     DWNR120                                                          
*                                                                               
         MVC   0(2,R4),SVMOA                                                    
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
*                                                                               
         MVC   0(2,R4),SVMOA+2                                                  
         LA    R4,2(R4)                                                         
         MVI   0(R4),X'5E'         SEMI-COLON SEPARATORS                        
         LA    R4,1(R4)            BUMP PAST SEPARATOR                          
         MVI   0(R4),X'0D'         MARK END OF RECORD                           
         LA    R4,1(R4)            BUMP PAST EOR MARKER                         
*                                                                               
DWNR120  DS    0H                                                               
         LA    RE,RECBUF           FIND ACTUAL LENGTH OF RECORD                 
         LR    RF,R4                                                            
         SR    RF,RE                                                            
         AHI   RF,4                ADD 4 BYTES FOR TAPE HEADER                  
         STCM  RF,3,TPRECLN        STORE RECORD LENGTH                          
         MVC   TPBUF,RECBUF        SAVE OFF BUFFER IN TAPE BUFFER               
         L     R3,=A(OUTP)                                                      
         PUT   (R3),TPREC                                                       
         B     DWNRX               EXIT                                         
*                                                                               
DWNR130  DS    0H                  DOWNLOADING STRAIGHT TO THE QUEUE            
         CLI   QOPT2,C'Q'          DOWNLOAD DIRECTLY TO QUEUE?                  
         BNE   DWNRX                                                            
         LA    R5,SVPRNT           POINT TO SVPRNT-XP IS USED BY DLFLD          
*                                                                               
         USING BOXD,R1                                                          
         L     R1,ADBOX                                                         
         MVI   BOXOFF,C'Y'         NO BOXES IF DOWNLOADING                      
         DROP  R1                                                               
*                                                                               
*        TM    DWNSTAT,DWNHDLN     WERE THE HEADLINES DOWNLOADED?               
*        BO    *+8                                                              
*        BAS   RE,DWNHEAD          DOWNLOAD HEADLINES                           
*                                                                               
* ALPHAID                                                                       
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(L'ALPHAID),ALPHAID  MOVE AGENCY ID TO DWN FLD             
         LA    R1,L'ALPHAID               ALPHAID LENGTH                        
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* PERIOD END DATE                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIMEPEDT),(10,WORK)                               
         MVI   PRTSIZE,8                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(8),WORK             MOVE PER END DTE TO DWN FLD           
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* 1R OFFICE CODE                                                                
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,LEVACDE+L'LEVACDE-1     POINT TO END OF FILED                 
         LA    R1,L'LEVACDE               FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),LEVACDE          MOVE LEVA CODE TO DWN FLD             
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* 1R DEPARTMENT CODE                                                            
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,LEVBCDE+L'LEVBCDE-1     POINT TO END OF FILED                 
         LA    R1,L'LEVBCDE               FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),LEVBCDE          MOVE LEVB CODE TO DWN FLD             
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* 1R SUB DEPARTMENT CODE                                                        
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,LEVCCDE+L'LEVCCDE-1     POINT TO END OF FIELD                 
         LA    R1,L'LEVCCDE               FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),LEVCCDE          MOVE LEVC CODE TO DWN FLD             
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* PERSON CODE                                                                   
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,LEVDCDE+L'LEVDCDE-1     POINT TO END OF FIELD                 
         LA    R1,L'LEVDCDE               FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),LEVDCDE          MOVE LEVD CODE TO DWN FLD             
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* PID NUMBER                                                                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLC   PPID,SPACES                ANY PID?                              
         BE    DWNR140                                                          
*                                                                               
         LA    R3,PPID                    POINT TO STR OF FILED                 
         LA    R1,L'PPID                  FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),PPID             MOVE AGENCY ID TO DWN FLD             
DWNR140  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* PERIOD NUMBER                                                                 
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,SVPERIOD                POINT TO STR OF FILED                 
         LA    R1,L'SVPERIOD              FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE PERIOD NUM TO DWN FLD            
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* TIMESHEET STATE TEMPO/TMS                                                     
*                                                                               
         MVI   PRTSIZE,5                  SET COLUMN LEN=5                      
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(5),=C'TEMPO'        MOVE TEMPO TO DWN FLD                 
         TM    FLAG,FLGTMP                                                      
         BO    *+10                                                             
         MVC   DWNFLD(5),=C'*TMS*'        MOVE *TMS* TO DWN FLD                 
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* LINE NUMBER - TEMPO OR TMS                                                    
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,PLN#                    TMS LINE # IS DEFAULT                 
         OC    TIMETLN#,TIMETLN#          TEMPO LINE IS OVERRIDE                
         BZ    *+8                                                              
         LA    R3,PTMPLN#                                                       
         LA    R1,L'PLN#                  FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE TIME LINE TO DWN FLD             
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* LOCATION START DATE                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(1,SVSTPDT),(10,WORK)                                
         MVI   PRTSIZE,8                  SET COLUMN LEN=8                      
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(8),WORK             MOVE PER END DTE TO DWN FLD           
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* LOCATION END DATE                                                             
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         OC    TIMEAEDT,TIMEAEDT          IF NO END DATE SKIP                   
         BZ    DWNR150                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIMEAEDT),(10,WORK)                               
         MVI   PRTSIZE,8                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD(8),WORK             MOVE PER END DTE TO DWN FLD           
DWNR150  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* ACCOUNT                                                                       
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLC   PACT,SPACES                                                      
         BE    DWNR190                                                          
*                                                                               
         LA    R3,PACT+L'PACT-1    POINT TO END OF FILED                        
         LA    R1,L'PACT           FIND OUT LEN OF ACTUAL DATA                  
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),PACT             MOVE 1NACCT CODE TO DWN FLD           
DWNR190  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* TASK CODE                                                                     
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLC   PTASK,SPACES               ANY TASK CODE?                        
         BE    DWNR200                                                          
*                                                                               
         LA    R1,L'PTASK                                                       
         STC   R1,PRTSIZE                                                       
         MVC   DWNFLD(L'PTASK),PTASK      MOVE TASK CODE TO DWN FLD             
DWNR200  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* TYPE CODE                                                                     
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,PTYPE+L'PTYPE-1         POINT TO END OF FILED                 
         LA    R1,L'PTYPE                 FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),PTYPE            MOVE TYPE CODE TO DWN FLD             
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* RATE                                                                          
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CP    TIMERTE,=P'0'                                                    
         BZ    DWNR210                                                          
*                                                                               
         LA    R3,PRATE                   POINT TO STR OF FILED                 
         LA    R1,L'PRATE                 FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE RATE TO DWN FLD                  
DWNR210  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* INCOME/SUSPENSE ACCOUNT                                                       
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLC   PINCACT,SPACES             ANY INCOME/SUSPENS?                   
         BNH   DWNR220                                                          
*                                                                               
         LA    R3,PINCACT+L'PINCACT-1     POINT TO END OF FILED                 
         LA    R1,L'PINCACT               FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),PINCACT          MOVE INC/SUSPENSE TO DWN FLD          
DWNR220  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* PRODUCTION OFFICE                                                             
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLC   POFF,SPACES                ANY OFFICE?                           
         BE    DWNR230                                                          
*                                                                               
         LA    R3,POFF+L'POFF-1           POINT TO END OF FILED                 
         LA    R1,L'POFF                  FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE PRD OFF TO DWN FLD               
DWNR230  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* BILLABLE HOURS                                                                
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,PBHRS                   POINT TO STR OF FILED                 
         LA    R1,L'PBHRS                 FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE BILLABLE HRS TO DWN FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* CHARGEABLE HOURS                                                              
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,PRHRS                   POINT TO STR OF FILED                 
         LA    R1,L'PRHRS                 FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE CHRGABLE HRS TO DWN FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* NON-BILLIABLE HOURS                                                           
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,PNHRS                   POINT TO STR OF FILED                 
         LA    R1,L'PNHRS                 FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE NON-BILL HRS TO DWN FLD          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* NON-CLIENT HOURS                                                              
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         LA    R3,PNCHRS                  POINT TO STR OF FILED                 
         LA    R1,L'PNCHRS                FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BH    *+12                                                             
         LA    R3,1(R3)                                                         
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),0(R3)            MOVE NON-CLI HRS TO DWN FLD           
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* TIME LINE ATTRIBUTE (TEMPO/TMS)                                               
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         MVC   DWNFLD,PTEMPO              MOVE TEMPO TO DWN FLD                 
         LA    R1,L'PTEMPO                                                      
         STC   R1,PRTSIZE                                                       
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* CONTRA ACCOUNT                                                                
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLC   PCONTRA,SPACES             ANY CONTRA?                           
         BE    DWNR240                                                          
*                                                                               
         LA    R3,PCONTRA+L'PCONTRA-1     POINT TO END OF FILED                 
         LA    R1,L'PCONTRA               FIND OUT LEN OF ACTUAL DATA           
         CLI   0(R3),X'40'                                                      
         BNE   *+12                                                             
         SHI   R3,1                                                             
         BCT   R1,*-12                                                          
*                                                                               
         STC   R1,PRTSIZE                                                       
         BCTR  R1,0                       DECREMENT FOR EX                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DWNFLD(0),PCONTRA          MOVE CONTRA ACCT TO DWN FLD           
DWNR240  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
* MOA (BROKEN OUT INTO TWO FLDS YY AND MM)                                      
*                                                                               
         MVI   PRTSIZE,0                  SET COLUMN LEN=0-NO PADDING           
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         CLC   SVMOA,SPACES               ANY MOA?                              
         BNE   DWNR250                                                          
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         B     DWNR260                                                          
*                                                                               
DWNR250  MVI   PRTSIZE,2                  SET COLUMN LEN=2                      
         MVC   DWNFLD(2),SVMOA            MOVE YY ACCT TO DWN FLD               
         GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
         MVI   PRTSIZE,2                  SET COLUMN LEN=2                      
         MVC   DWNFLD(2),SVMOA+2          MOVE MM ACCT TO DWN FLD               
DWNR260  GOTO1 ADWNL,DMCB,(RC),DWNTEXT    DOWNLOAD TEXT                         
*                                                                               
         MVC   DWNFLD,SPACES              SPACE OUT DOWNLOAD FIELD              
         GOTO1 ADWNL,DMCB,(RC),DWNEOL     DOWNLOAD EOL MARKER                   
*                                                                               
DWNRX    MVC   XP,XSPACES          CLEAR PRINT LINE                             
         XMOD1                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD MODULE                                                    *          
*          PARM1 - RC                                                *          
*          PARM2 - ACTION                                            *          
**********************************************************************          
         SPACE 1                                                                
DWNL     DS    0D                                                               
         NMOD1 0,**DOWN**                                                       
         L     RC,0(R1)                                                         
         L     RF,4(R1)                                                         
         STC   RF,DWNMODE          SAVE CURRENT MODE                            
         USING DLCBD,R5                                                         
         L     R5,ADWNBUF                                                       
*                                                                               
         CLI   DWNMODE,DWNINIT     INITIALIZE                                   
         BE    DWNL10                                                           
         CLI   DWNMODE,DWNTEXT     DOWN-LOAD TEXT                               
         BE    DWNL20                                                           
         CLI   DWNMODE,DWNNUM      DOWN-LOAD NUMBER                             
         BE    DWNL30                                                           
         CLI   DWNMODE,DWNPACK     DOWN-LOAD NUMBER (PACKED)                    
         BE    DWNL40                                                           
         MVI   DLCBACT,DLCBEOL                                                  
         CLI   DWNMODE,DWNEOL      END OF LINE                                  
         BE    DWNL50                                                           
         MVI   DLCBACT,DLCBEOR                                                  
         CLI   DWNMODE,DWNEOR      END OF REPORT                                
         BE    DWNL50                                                           
         DC    H'0'                                                             
*                                                                               
* INITIALIZATION                                                                
*                                                                               
DWNL10   TM    DWNSTAT,DWNINTZ     HAS IT ALREADY BEEN INITIALIZED?             
         BO    DWNLX               YES - EXIT                                   
         MVI   DLCBACT,DLCBINIT    DOWN LOAD ACTION IS START                    
         LA    RE,XP               PRINT LINE                                   
         ST    RE,DLCBAPL                                                       
         LA    RE,DWNHOOK          POINT TO HOOK FOR APPLICATION                
         ST    RE,DLCBAPR                                                       
         MVC   DLCXMAXL,=Y(L'XP)                                                
         MVI   DLCXDELC,C' '       DELIMITER                                    
         MVI   DLCXEOTC,C'"'       TEXT DELIMITER                               
         MVI   DLCXEOTA,C''''      ALTERNATE TEXT DELIMITER                     
         MVI   DLCXEOLC,X'5E'      SEMI-COLON, END-OF-LINE                      
         MVI   DLCXEORC,C':'       END-OF-REPORT                                
         GOTO1 DLFLD,(R5)                                                       
         MVI   FORCEHED,C'Y'       EXCEPT FIRST TIME IN                         
         GOTO1 ACREPORT                                                         
         MVC   DLCBFLD,SPACES      MUST CLEAR FIRST TIME IN                     
*                                  TURN OFF DOWN-LOAD ROW FLDS AS C' '          
         OI    DWNSTAT,DWNINTZ     TURN ON INITIALIZED BYTE                     
         B     DWNLX               EXIT                                         
*                                                                               
* DOWNLOAD A RECORD - TEXT                                                      
*                                                                               
DWNL20   MVC   DLCBLEN,PRTSIZE     LEN OF FIELD-SET TO 0 (NO PADDING)           
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBTXT     TYPE   IS TEXT                               
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER                                                    
*                                                                               
DWNL30   MVI   DLCBLEN,0           LEN OF FIELD-SET TO 0 (NO PADDING)           
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBTYP,DLCBNUM     TYPE   IS NUMBER                             
         MVI   DLCBLEN,16          YES, USE MAXIMUM LENGTH OF NUMERICS          
         MVC   DLCBFLD,SPACES                                                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         B     DWNL50              DOWN-LOAD FIELD                              
*                                                                               
* DOWNLOAD A RECORD - NUMBER (PACKED)                                           
*                                                                               
DWNL40   MVI   DLCBTYP,DLCBPACF    PACKED DATA                                  
         OI    DLCBFLG1,DLCBFTRM   DOWN-LOAD WITH TRAILING MINUS                
         MVI   DLCBACT,DLCBPUT     ACTION IS PUT                                
         MVI   DLCBLEN,L'PKFLDS    YES, USE MAXIMUM LENGTH OF NUMERICS          
         XC    DLCBFLD,DLCBFLD     CLEAN DWNLOAD FIELD TO 0'S                   
         MVC   DLCBFLD(L'DWNFLD),DWNFLD                                         
         NC    DLCBFLD,DLCBFLD     YES, MAKE SURE NUMERIC FLD NOT ZEROS         
         BNZ   DWNL50              NOT  ZERO, DOWN-LOAD FIELD                   
         MVI   DLCBLEN,1           ZERO, SET LENGTH 1 TO DOWN-LOAD A 0          
*                                                                               
* END OF LINE/END OF RECORD                                                     
*                                                                               
DWNL50   GOTO1 DLFLD,(R5)          DOWN-LOAD FIELD                              
*                                                                               
DWNLX    XMOD1                                                                  
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD HOOK                                                      *          
**********************************************************************          
         SPACE 1                                                                
DWNHOOK  MVI   FORCEHED,C'N'       NEVER HEAD UP                                
         MVI   SPACING,1                                                        
         MVI   LINE,1                                                           
         L     RF,ACREPORT                                                      
         BR    RF                                                               
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
         MVI   BOXCOLS+(PPID-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PADDDTE-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PASTDTE-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PAENDTE-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(PACT-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PREV#-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PTASK-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PTYPE-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PRATE-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PINCACT-PRTLINE-1),C'C'                                 
         MVI   BOXCOLS+(POFF-PRTLINE-1),C'C'                                    
         MVI   BOXCOLS+(PBHRS-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PRHRS-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PNHRS-PRTLINE-1),C'C'                                   
         MVI   BOXCOLS+(PNCHRS-PRTLINE-1),C'C'                                  
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
         TM    FLAG,FLGBRO         IS T/S A BRANDO T/S?                         
         BNO   BXXIT                                                            
         MVC   XHEAD11+7(5),=CL5'BRAND'                                         
         MVC   XHEAD12+7(5),=CL5'ROW #'                                         
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
ACFLTAB  DS    0CL3                                                             
         DC    C'101'                                                           
         DC    C'202'                                                           
         DC    C'303'                                                           
         DC    C'404'                                                           
         DC    C'505'                                                           
         DC    C'606'                                                           
         DC    C'707'                                                           
         DC    C'808'                                                           
         DC    C'909'                                                           
         DC    C'A10'                                                           
         DC    C'B11'                                                           
         DC    C'C12'                                                           
         DC    C'D13'                                                           
         DC    C'Q14'                                                           
         DC    C'T15'                                                           
         DC    C'V16'                                                           
         DC    C'Y17'                                                           
         DC    AL1(EOF)                                                         
*                                                                               
TYPTAB   DS    0C                                                               
         DC    C'B ',AL1(TIMTCB)                                                
         DC    C'R ',AL1(TIMTCR)                                                
         DC    C'N ',AL1(TIMTCN)                                                
         DC    C'NC',AL1(TIMTNC)                                                
         DC    AL1(EOF)                                                         
*                                                                               
TSTIDTB  DS    0CL2                                                             
         DC    C'SJ'                                                            
         DC    C'TC'                                                            
         DC    C'TD'                                                            
         DC    C'TG'                                                            
         DC    AL1(EOF)                                                         
*                                                                               
* BINTABLE 1 - TRANSACTION TABLE                                                
*                                                                               
         DC    C'***TMS***'                                                     
TIMETAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
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
TMPTAB   DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TMPOLNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TMPKLNQ)            KEY LENGTH                               
         DC    AL4(TMPMAX)             MAX IN TABLE                             
         DC    AL1(0)                  NUMBER OF BUCKETS - NO BUCKETS           
         DC    AL1(0)                  DISPLACEMENT TO FIRST BUCKET             
         DS    (TIMEMAX*TIMELNQ)XL1    TABLE                                    
*                                                                               
TIMEMAX  EQU   20000                                                            
TMPMAX   EQU    1000                                                            
*                                                                               
         DS    0D                  DOWNLOAD BUFFER                              
DWNBUF   DS    CL250                                                            
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZJD    DSECT                                                                  
ADBOX    DS    A                   ADDRESS OF BOX ROUTINE                       
*                                                                               
VTYPES   DS    0A                                                               
ABINADD  DS    A                   ROUTINE TO ADD TO BINSEARCH TABLE            
ABOXRC   DS    A                                                                
ABXHOOK  DS    A                                                                
ADUMP    DS    A                   ROUTINE TO DITTO RECORDS                     
ADWNBUF  DS    A                   DOWNLOAD BUFFER                              
ADWNRTE  DS    A                   ROUTINE TO PRINTOUT/PUT RECORDS              
ADWNL    DS    A                   ROUTINE TO DWNLD RECORDS                     
APTIME   DS    A                   ROUTINE TO PROCESS TIME RECORDS              
*                                                                               
ATIMETAB DS    A                   TIME LINE TABLE                              
ATMPTAB  DS    A                   TEMPO LINE TABLE                             
*                                                                               
PRNTBL   DS    V                   PRINT DATA                                   
DLFLD    DS    V                   DOWNLOAD ROUTINE                             
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
SVDA     DS    F                                                                
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
FLGTIM   EQU   X'40'                                                            
FLGBRO   EQU   X'20'                                                            
FLGBROBT EQU   X'10'               TIME SHEET HAS A B TIME LINE                 
*                                                                               
LASTKEY  DS    CL49                                                             
*                                                                               
LSTPEDT  DS    PL3                 LAST PERIOD END DATE                         
SVKEY    DS    CL49                                                             
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
SVPID    DS    XL2                                                              
SVTMPLN  DS    XL2                 SAVED AREA FOR TEMPO LINE #                  
SVPEDT   DS    CL8                 SAVED AREA FOR PERIOD END                    
SVPERIOD DS    CL2                 SAVED AREA FOR TIMESHEET PERIOD              
SVSTPDT  DS    PL3                 ACTUAL PERIOD START DATE                     
SVENPDT  DS    PL3                 ACTUAL PERIOD END   DATE                     
SVMOA    DS    CL4                 MOA - YYMM                                   
SVPMOA   DS    PL2                 MOA - YYMM (PACKED)                          
SV1RACC  DS    CL12                SAVED AREA 1R ACCOUNT                        
SVSTAT   DS    XL1                 SAVED AREA FOR RECORD STATUS                 
SVLMADT  DS    PL3                 LINE MANAGER APPROVAL SUB DATE               
SVCLADT  DS    PL3                 CLIENT LEVEL APPROVAL SUB DATE               
*                                                                               
TYPEFLT  DS    XL1                 SAVED AREA FOR FILTERING BY TYPE             
DWNSTAT  DS    XL1                 DOWNLOAD STATUS                              
DWNINTZ  EQU   X'80'               DOWNLOAD INITIALIZED                         
DWNHDLN  EQU   X'40'               DOWNLOAD HEADLINES                           
*                                                                               
DWNMODE  DS    XL1                 DOWNLOAD MODE                                
DWNINIT  EQU   1                      DOWN-LOAD INITIALIZATION                  
DWNEOL   EQU   2                      MARK END OF LINE                          
DWNEOR   EQU   3                      MARK END OF REPORT                        
DWNTEXT  EQU   4                      DOWN-LOAD TEXT                            
DWNNUM   EQU   5                      DOWN-LOAD NUMBER                          
DWNPACK  EQU   6                      DOWN-LOAD NUMBER (PACKED)                 
*                                                                               
DWNFLD   DS    CL36                SAVED AREA FOR FIELD TO BE DWNLOADED         
PRTSIZE  DS    XL1                 PRINT AREA LENGTH                            
*                                                                               
START    DS    XL3                                                              
END      DS    XL3                                                              
ACTSTRT  DS    XL3                                                              
SMOA     DS    XL2                 START MOA                                    
EMOA     DS    XL2                 END MOA                                      
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
SVPRNT   DS    CL(L'XP)            SAVED AREA FOR PRINTLINE                     
RECBUF   DS    CL(L'XP)            BUFFER AREA FOR DWNLOAD                      
*                                                                               
TMPWRK   DS    CL(TMPOLNQ)                                                      
TIMEWRK  DS    CL(TIMELNQ)                                                      
*                                                                               
TPREC    DS    0CL(L'RECBUF+4)     VARIABLE LENGTH TAPE RECORD                  
TPRECHD  DS    0CL4                RECORD HEADER - LENGTH AND SPARE             
TPRECLN  DS    CL2                 RECORD LENGTH                                
         DS    CL2                 SPARE                                        
TPBUF    DS    CL(L'RECBUF)                                                     
*                                                                               
IO       DS    0CL2042                                                          
IOKEY    DS    CL42                KEY                                          
IODATA   DS    CL2000              DATA                                         
IOLNQ    EQU   *-IO                LENGTH                                       
*                                                                               
EOF      EQU   X'FF'               END OF FILE MARKER                           
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
         DS    CL1                                                              
PPID     DS    CL5                 PID                                          
         DS    CL2                                                              
PADDDTE  DS    CL6                 ACTIVITY DATE                                
         DS    CL1                                                              
PASTDTE  DS    CL6                 ACTUAL START DATE                            
         DS    CL2                                                              
PAENDTE  DS    CL6                 ACTUAL END DATE                              
         DS    CL2                                                              
PACT     DS    CL14                SJ/1N ACCOUNT                                
         DS    CL2                                                              
PREV#    DS    CL10                REVERSAL #                                   
         DS    CL5                                                              
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
PBHRS    DS    CL10                BILLABLE HOURS                               
         DS    CL2                                                              
PRHRS    DS    CL10                CHARGEABLE HOURS                             
         DS    CL2                                                              
PNHRS    DS    CL10                NON-BILLABLE HOURS                           
         DS    CL2                                                              
PNCHRS   DS    CL10                NON-CLIENT HOURS                             
         DS    CL2                                                              
PTEMPO   DS    CL5                 TEMPO EYECATCHER                             
         DS    CL2                                                              
PCONTRA  DS    CL12                CONTRA ACCOUNT(ONLY 1C)                      
         DS    CL2                                                              
PMOA     DS    CL6                 MOA                                          
         DS    CL1                                                              
PDUP     DS    CL8                 DUPLICATE FLAG                               
         DS    CL1                                                              
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
HEADNAME DS    0CL36               DECSRIPTION                                  
         DS    CL12                                                             
HEADSTAT DS    CL24                                                             
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TIME TABLE WORK AREA                                          *         
***********************************************************************         
         SPACE 1                                                                
TIMED    DSECT                                                                  
TIMEPEDT DS    XL3                 PERIOD END DATE                              
TIMETLN# DS    XL2                 TMS TEMPO LINE# (IF ANY)                     
TIMELN#  DS    XL2                 TMS LINE#                                    
TIMEDUP  DS    XL1                 DUP BYTE                                     
TIMKLNQ  EQU   *-TIMED                                                          
TIMESTAT DS    XL1                 RECORD STATUS BYTE                           
*        EQU   X'40'               FULLY APPROVED            (TIMSFAPP)         
*        EQU   X'20'               SUBMITTED                 (TIMSSUBM)         
*        EQU   X'10'               PART APPROVED             (TIMSPAPP)         
*        EQU   X'08'               AWAITING LINE MGR APPL    (TIMSAWAP)         
*        EQU   X'02'               REJECTED                  (TIMSREJE)         
*        EQU   X'01'               RETRACTED                 (TIMSRETR)         
*        EQU   X'01'               LINE MANAGER APPROVED     (TIMSMAAP)         
TIMEPID  DS    XL2                 PERSONAL ID                                  
TIMETTYP DS    XL1                 TYPE OF TIME                                 
TIMESTA  DS    XL1                 STATUS BYTE(TEMPO/ADJ/WO)                    
TIMETMP  EQU   X'10'               TIMESHEET IS A TEMPO TIMESHEET               
*                                  ALSO USED - TIMTEMPO - X'80'                 
*                                            - TIMIWO   - X'40'                 
*                                            - TIMIADJ  - X'20'                 
*                                            - TIMTEMPO - X'08'                 
TIMEBRO  EQU   X'04'               TIMESHEET IS A BRANDO TIMESHEET              
TIMETSK  DS    CL2                 TASK CODE                                    
TIMEPOFF DS    CL2                 PRODUCTION OFFICE                            
TIMEMOA  DS    PL2                 MOA                                          
TIMEACDT DS    PL3                 TIME LINES ACTIVITY DATE                     
TIMEASDT DS    PL3                 TIMESHEET ACTUAL START DATE(TEMPO)           
TIMEAEDT DS    PL3                 TIMESHEET ACTUAL END DATE(TEMPO)             
TIMELMDT DS    PL3                 LINE MANAGER APPROVAL SUB DATE               
TIMECLDT DS    PL3                 CLIENT LEVEL APPROVAL SUB DATE               
TIMEACT  DS    0CL14               SJ ACCOUNT/1N ACCOUNT                        
TIMEAUL  DS    CL2                   UNIT/LEDGER                                
TIMEACLI DS    CL3                    CLIENT                                    
TIMEAPRD DS    CL3                     PRODUCT                                  
TIMEAJOB DS    CL6                      JOB                                     
TIMEINC  DS    CL12                INCOME/SUSPENSE                              
TIMECAC  DS    CL12                CONTRA (NOT 1N)                              
TIMERTE  DS    PL4                 RATE                                         
TIMEPRD  DS    XL1                 PERIOD NUMBER                                
TIMEREV# DS    XL4                 REVERSAL #                                   
TIMEDA   DS    XL4                 DISK ADDRESS                                 
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
* DSECT TYPE TABLE                                                    *         
***********************************************************************         
         SPACE 1                                                                
TYPTABD  DSECT                                                                  
TYPCODE  DS    CL2                 CHARACTER CODE                               
TYPBINC  DS    XL1                 BINARY    CODE                               
TYPTBLNQ EQU   *-TYPTABD                                                        
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
**PAN#1  DC    CL21'002ACREPZJ02 01/20/12'                                      
         END                                                                    
