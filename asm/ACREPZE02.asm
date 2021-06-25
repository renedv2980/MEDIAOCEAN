*          DATA SET ACREPZE02  AT LEVEL 168 AS OF 12/07/00                      
*PHASE ACZE02A                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'FIND DUPLICATE TEMPO TIME SHEETS'                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* QOPT1 = YES to print TMS item for missing time sheet                *         
* QOPT2 = NO  to suppress detail printed                              *         
* QOPT3 = YES to include missing xtra element reporting               *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
ACZE02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACZE**,R9                                                    
         L     RA,0(,R1)                                                        
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACZED,RC                                                         
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
         CLI   MODE,PROCTIME                                                    
         BE    PTIME                                                            
         CLI   MODE,ACCLAST              ACCOUNT LAST                           
         BE    ACCL                                                             
         CLI   MODE,REQLAST                                                     
         BE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
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
*                                                                               
         USING ACCRECD,RE                                                       
         LA    RE,IO                                                            
         MVC   DISP2,=Y(ACCRFST-ACCKEY)      SET DISP2                          
         DROP  RE                                                               
*                                                                               
         MVI   FCSUPOFC,C'Y'                                                    
         MVI   FCGETOPT,C'N'                                                    
*                                                                               
         LA    RE,IO               RE=A(IO AREA)                                
         LA    RF,IOLNQ            RF=(LENGTH OF IO AREA)                       
         SR    R1,R1                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         USING MASTD,R2                                                         
         USING BOXD,R4                                                          
         L     R2,ADMASTC                                                       
         L     R4,MCBXAREA                                                      
         MVC   BOXWIDTH,=F'198'         SET WIDTH FOR REPORT                    
*                                                                               
         LA    RE,ACFDTOT          START  OF ACCUMS                             
         LA    RF,TOT#ACCM         NUMBER OF ACCUMS                             
RUNF22   ZAP   0(L'ACFDTOT,RE),=P'0'                                            
         LA    RE,L'ACFDTOT(,RE)                                                
         BCT   RF,RUNF22                                                        
*                                                                               
         LA    RE,ACFXTOT           START  OF ACCUMS                            
         LA    RF,TOT#ACCM          NUMBER OF ACCUMS                            
RUNF24   ZAP   0(L'ACFXTOT,RE),=P'0'                                            
         LA    RE,L'ACFXTOT(,RE)                                                
         BCT   RF,RUNF24                                                        
         ZAP   ACFTMPO,=P'0'                                                    
         ZAP   ACFMISS,=P'0'                                                    
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
         MVI   FLAG,0              INITIALIZE FLAG                              
*        MVI   RCREQREP,NO                                                      
*        TM    FLAG,FLGONCE                                                     
*        BO    *+8                                                              
*        MVI   RCREQREP,YES                                                     
*        OI    FLAG,FLGONCE                                                     
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
         BE    REQF21                                                           
         MVC   WORK(L'QEND),QEND                                                
         CLC   WORK+4(2),SPACES                                                 
         BNE   REQF20                                                           
         MVC   WORK+4(2),=C'31'                                                 
         GOTO1 DATCON,DMCB,(X'30',WORK),(0,WORK),(1,0) LAST DAY OF MO           
*                                                                               
REQF20   GOTO1 DATCON,DMCB,(0,WORK),(1,END)                                     
*                                                                               
REQF21   LA    RE,CPYDTOT          START  OF ACCUMS                             
         LA    RF,TOT#ACCM         NUMBER OF ACCUMS                             
REQF22   ZAP   0(L'CPYDTOT,RE),=P'0'                                            
         LA    RE,L'CPYDTOT(,RE)                                                
         BCT   RF,REQF22                                                        
*                                                                               
         LA    RE,CPYXTOT          START  OF ACCUMS                             
         LA    RF,TOT#ACCM         NUMBER OF ACCUMS                             
REQF24   ZAP   0(L'CPYXTOT,RE),=P'0'                                            
         LA    RE,L'CPYXTOT(,RE)                                                
         BCT   RF,REQF24                                                        
         ZAP   CPYTMPO,=P'0'                                                    
         ZAP   CPYMISS,=P'0'                                                    
         ZAP   CPYPRSN,=P'0'                                                    
         ZAP   CPYPER#,=P'0'                                                    
         ZAP   CPYMPER#,=P'0'                                                   
*                                                                               
REQFX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* LEDGER FIRST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING CPYELD,R1                                                        
LDGF     DS    0H                                                               
         L     R1,ADCMPEL          GET X'10' ELEMENT COMPANY ELEM               
         TM    CPYSTAT4,CPYSOFF2   IS NEW 2 CHAR OFFICE IN USE                  
         BNO   *+8                                                              
         OI    FLAG,FLGNOFF        NEW 2 CHAR OFFICE IN USE                     
         MVC   SVCLOGO,CPYLOGO     GET LOGIN ID / CONNECT ID                    
         BAS   RE,GETLEVS          GET LEVELS                                   
*                                                                               
LDGFX    B     EXIT                                                             
         DROP  R1                                                               
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
         L     R2,ATIMETAB         TRANSACTION TABLE                            
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
         NI    FLAG,TURNOFF-FLGPER CLEAR FLAG                                   
         MVC   SVACCT,SPACES       CLEAR ACCOUNT                                
         XC    SVPEDT,SVPEDT       CLEAR PERIOD END DATE                        
         XC    SVPID,SVPID         CLEAR PID NUMBER                             
         MVC   LASTKEY,SPACES      LAST KEY FOR EVERY NEW KEY                   
*                                                                               
PACCX    B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* PROCESS TIME                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING TIMELD,R4                                                        
XREF     USING TIMELD,R5                                                        
         USING ACMD,R7                                                          
*                                                                               
PTIME    DS    0H                                                               
         MVI   ERRSTAT,0                                                        
         L     R7,AMONACC                                                       
*                                                                               
         L     R7,ACMALTN          GET ADDRESS OF TIME RECORD                   
         USING TIMRECD,R7                                                       
*                                                                               
         CLC   TIMKPEDT,START                                                   
         BL    PTIMEX              IGNORE OLD ONES                              
         CLC   TIMKPEDT,END                                                     
         BH    PTIMEX              IGNORE AFTER END DATE OF REQUEST             
*                                                                               
         L     R4,ADTRANS                                                       
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   PTIMEX                                                           
         CLI   TIMETYP,TIMEINP          MUST BE INPUT TYPE                      
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         TM    TIMSTAT,TIMTEMPO        INPUT FROM TEMPO UPLOAD ?                
         BZ    PTIMEX                  NO, SO DON'T CARE                        
         AP    ACFTMPO,=P'1'                                                    
         AP    CPYTMPO,=P'1'                                                    
         LR    R5,R4                                                            
         SR    R1,R1                                                            
PTIME02  IC    R1,XREF.TIMLN            GET ELEMENT LENGTH                      
         AR    R5,R1                                                            
         CLI   XREF.TIMEL,0             EOR                                     
         BNE   PTIME03                                                          
         SR    R5,R5                                                            
         B     PTIME08                                                          
*                                                                               
PTIME03  CLI   XREF.TIMETYP,TIMEINP     SHOULD NOT OF FOUND THIS                
         BNE   PTIME04                                                          
         SR    R5,R5                                                            
         B     PTIME08                                                          
*                                                                               
PTIME04  CLI   XREF.TIMETYP,TIMEXTRA    TEMPO INFO                              
         BNE   PTIME02                                                          
*                                                                               
BLK      USING TIMED,TIMEWRK                                                    
*                                                                               
PTIME08  DS    0H                                                               
         LTR   R5,R5                                                            
         BNZ   PTIME09             Found extra                                  
         CLI   QOPT3,YES           Inlcude one missing extra elem ?             
         BNE   PTIMEX              No                                           
         LA    R5,DUMXTRA          Yes, Fake extra-element                      
*                                                                               
PTIME09  MVC   TIMEWRK,SPACES                                                   
*        MVC   BLK.TIMEPER,LEVDCDE     PERSON CODE                              
         MVC   BLK.TIMETMP#,XREF.TIMXTLN#                                       
         MVC   BLK.TIMEPEDT,TIMKPEDT   PERIOD END DATE                          
         MVC   BLK.TIMESTA,TIMSTAT     SAVE STATUS BYTES                        
         MVC   BLK.TIMEACC,TIMACC                                               
         MVC   BLK.TIMETSK,TIMTSK                                               
         MVC   BLK.TIMEOFF,TIMOFF                                               
         MVC   BLK.TIMETTYP,TIMTTYP                                             
         MVC   BLK.TIMEIND,TIMIND                                               
         MVC   BLK.TIMEMOA,TIMMOA                                               
         MVC   BLK.TIMETMS#,TIMLINE#                                            
         ZAP   BLK.TIMEHRS,TIMHRS                                               
         MVC   BLK.TIMEADTE,TIMADAT                                             
         ZAP   BLK.TIMEAMNT,=P'0'                                               
         ZAP   BLK.TIMERATE,=P'0'                                               
         MVI   BLK.TIMEDUP,0                                                    
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   PTIME20                                                          
         MVC   BLK.TIMEINC,TIMINC                                               
         ZAP   BLK.TIMERATE,TIMRATE                                             
         ZAP   BLK.TIMEAMNT,TIMAMNT                                             
         DROP  BLK                                                              
*                                                                               
PTIME20  OC    XREF.TIMXTLN#,XREF.TIMXTLN#                                      
         BNZ   PTIME21                                                          
         OI    ERRSTAT,ERRMISS                                                  
*                                                                               
PTIME21  GOTO1 ABINADD,DMCB,(RC),TIMEWRK,ATIMETAB   ADD TABLE ENTRY             
         CLI   DMCB,1              RECORD ADDED ?                               
         BE    PTIME22             Yes                                          
         MVC   ABUFFNTY,DMCB                                                    
         OI    ERRSTAT,ERRDUP                                                   
         B     PTIME25                                                          
*                                                                               
PTIME22  TM    ERRSTAT,ERRMISS                                                  
         BZ    PTIME50             DON'T WANT THIS ONE AT ALL                   
         CLI   QOPT1,YES           WANT MISSING ?                               
         BNE   PTIME50             NO                                           
*                                                                               
         USING PLINED,R2                                                        
PTIME25  LA    R2,XP                                                            
         MVC   PCODE,LEVDCDE                                                    
         MVC   PNAME,LEVDNME                                                    
         GOTO1 DATCON,DMCB,(1,TIMKPEDT),(8,PPEDT)                               
*                                                                               
         USING TIMED,R6                                                         
         TM    ERRSTAT,ERRDUP      DUPLICATE ?                                  
         BZ    PTIME30                                                          
         MVC   PERR,=CL15'DUP LINE#'                                            
         L     R6,ABUFFNTY         GET ENTRY IN BUFFALO                         
         ZIC   R3,R3                                                            
         IC    R3,TIMEDUP                                                       
         AHI   R3,1                                                             
         STC   R3,TIMEDUP                                                       
         EDIT  (R3),PDUPNUM                                                     
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*        CHECK FOR EXACT DUPLICATION NOW                                        
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         MVI   DIFFCODE,C'T'       Type                                         
         CLC   TIMTTYP,TIMETTYP                                                 
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'C'       Contra                                       
         CLC   TIMACC,TIMEACC                                                   
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'W'       Task                                         
         CLC   TIMTSK,TIMETSK                                                   
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'O'       Office                                       
         CLC   TIMOFF,TIMEOFF                                                   
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'M'       MOA                                          
         CLC   TIMMOA,TIMEMOA                                                   
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'H'       Hours                                        
         CP    TIMHRS,TIMEHRS                                                   
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'R'       Rate                                         
         ZAP   DUB,=P'0'                                                        
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   *+10                                                             
         ZAP   DUB,TIMRATE                                                      
         CP    DUB,TIMERATE                                                     
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'A'       Activity date                                
         CLC   TIMADAT,TIMEADTE                                                 
         BNE   PTIME28                                                          
*                                                                               
         MVI   DIFFCODE,C'I'       Income account                               
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   PTIME27                                                          
         CLC   TIMINC,TIMEINC                                                   
         BNE   PTIME28                                                          
*                                                                               
PTIME27  MVI   DIFFCODE,C'$'                                                    
         ZAP   DUB,=P'0'                                                        
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   *+10                                                             
         ZAP   DUB,TIMAMNT                                                      
         CP    DUB,TIMEAMNT                                                     
         BNE   PTIME28                                                          
         OI    ERRSTAT,ERRXDUP     SET TO SAY EXACT DUPLICATE                   
         NI    ERRSTAT,TURNOFF-ERRDUP                                           
*                                                                               
PTIME28  MVI   PSTAR,C'*'          FLAG WHEN PRINTING NOT EXACT                 
         MVC   PDCODE,DIFFCODE                                                  
         TM    ERRSTAT,ERRXDUP                                                  
         BZ    PTIME30                                                          
         MVI   PDCODE,C' '                                                      
         MVI   PSTAR,C' '          REMOVE PRINTING FLAG                         
         MVC   PERR,=CL15'EXACT DUP'                                            
*                                                                               
PTIME30  MVI   PTTYPE,C'B'                                                      
         CLI   TIMETTYP,TIMTCB                                                  
         BE    PTIME35                                                          
         MVI   PTTYPE,C'R'                                                      
         CLI   TIMETTYP,TIMTCR                                                  
         BE    PTIME35                                                          
         MVI   PTTYPE,C'N'                                                      
         CLI   TIMETTYP,TIMTCN                                                  
         BE    PTIME35                                                          
         MVI   PTTYPE,C'C'                                                      
         CLI   TIMETTYP,TIMTNC                                                  
         BE    PTIME35                                                          
         MVI   PTTYPE,C'?'                                                      
*                                                                               
PTIME35  MVC   WORK,TIMEMOA                                                     
         MVI   WORK+2,01                                                        
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOA)                                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIMEADTE),(8,PACTDTE)                             
*                                                                               
         MVC   PSJAC,TIMEACC                                                    
         MVC   PTASK,TIMETSK                                                    
         MVC   POFFICE,TIMEOFF                                                  
         MVC   PINCOME,TIMEINC                                                  
*                                                                               
         EDIT  TIMERATE,PRATE,2,ZERO=BLANK                                      
         EDIT  TIMEHRS,PHOURS,2,ZERO=NOBLANK                                    
         EDIT  TIMEAMNT,PAMNT,2,ZERO=NOBLANK                                    
*                                                                               
PTIME35A SR    R3,R3                                                            
         ICM   R3,3,TIMETMS#                                                    
         EDIT  (R3),PLINE1                                                      
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,XREF.TIMXTLN#                                               
         EDIT  (R3),PLINE2                                                      
         TM    ERRSTAT,ERRMISS+ERRXDUP                                          
         BNO   PTIME36                                                          
         MVC   PERR,=CL15'X-DUP && NO XTRA'                                     
         B     PTIME37                                                          
*                                                                               
PTIME36  TM    ERRSTAT,ERRMISS+ERRDUP                                           
         BNO   PTIME37                                                          
         MVC   PERR,=CL15'DUP && NO XTRA'                                       
         DROP  R6                                                               
*                                                                               
PTIME37  CLI   QOPT2,NO            Suppress detial                              
         BE    *+8                                                              
         BAS   RE,PRINTIT          PRINT RECORD FORM BUFFALO                    
*                                                                               
PTIME38  MVC   XP,XSPACES                                                       
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
*        PRINT CURRENT TMS (TEMPO CREATED) RECORD                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
         SPACE 1                                                                
PTIME40  MVI   PTTYPE,C'B'                                                      
         CLI   TIMTTYP,TIMTCB                                                   
         BE    PTIME45                                                          
         MVI   PTTYPE,C'R'                                                      
         CLI   TIMTTYP,TIMTCR                                                   
         BE    PTIME45                                                          
         MVI   PTTYPE,C'N'                                                      
         CLI   TIMTTYP,TIMTCN                                                   
         BE    PTIME45                                                          
         MVI   PTTYPE,C'C'                                                      
         CLI   TIMTTYP,TIMTNC                                                   
         BE    PTIME45                                                          
         MVI   PTTYPE,C'?'                                                      
*                                                                               
PTIME45  MVC   WORK,TIMMOA                                                      
         MVI   WORK+2,01                                                        
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMOA)                                    
*                                                                               
         GOTO1 DATCON,DMCB,(1,TIMADAT),(8,PACTDTE)                              
*                                                                               
         MVC   PSJAC,TIMACC                                                     
         MVC   PTASK,TIMTSK                                                     
         MVC   POFFICE,TIMOFF                                                   
*                                                                               
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   *+10                                                             
         MVC   PINCOME,TIMINC                                                   
*                                                                               
         EDIT  TIMHRS,PHOURS,2,ZERO=NOBLANK                                     
         ZAP   PKAMT,=P'0'                                                      
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   *+10                                                             
         ZAP   PKAMT,TIMAMNT                                                    
         EDIT  PKAMT,PAMNT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,=P'0'                                                      
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   *+10                                                             
         ZAP   PKAMT,TIMRATE                                                    
         EDIT  PKAMT,PRATE,2,ZERO=BLANK                                         
*                                                                               
PTIME45A SR    R3,R3                                                            
         ICM   R3,3,TIMLINE#                                                    
         EDIT  (R3),PLINE1                                                      
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,3,XREF.TIMXTLN#                                               
         EDIT  (R3),PLINE2                                                      
         TM    ERRSTAT,ERRDUP+ERRXDUP                                           
         BNZ   *+10                                                             
         MVC   PERR,=CL15'MISSING XTRA'                                         
         TM    ERRSTAT,ERRDUP+ERRMISS                                           
         BNO   PTIME46                                                          
*                                                                               
PTIME46  CLI   QOPT2,NO                                                         
         BE    PTIME47                                                          
         BAS   RE,PRINTIT                                                       
*                                                                               
PTIME47  MVC   XP,XSPACES                                                       
*                                                                               
PTIME50  TM    ERRSTAT,ERRXDUP                                                  
         BZ    PTIME52                                                          
         GOTO1 ADDTOT,CPYXTOT                                                   
         GOTO1 ADDTOT,ACFXTOT                                                   
*        TM    FLAG,FLGPER                                                      
*        BO    PTIMEX                                                           
*        AP    CPYPRSN,=P'1'                                                    
*        OI    FLAG,FLGPER                                                      
         B     PTIMEX                                                           
*                                                                               
PTIME52  TM    ERRSTAT,ERRDUP                                                   
         BZ    PTIME54                                                          
         GOTO1 ADDTOT,CPYDTOT                                                   
         GOTO1 ADDTOT,ACFDTOT                                                   
*        TM    FLAG,FLGPER                                                      
*        BO    PTIMEX                                                           
*        AP    CPYPRSN,=P'1'                                                    
*        OI    FLAG,FLGPER                                                      
         B     PTIMEX                                                           
*                                                                               
PTIME54  TM    ERRSTAT,ERRMISS                                                  
         BZ    PTIMEX                                                           
         AP    CPYMISS,=P'1'                                                    
         AP    ACFMISS,=P'1'                                                    
*                                                                               
PTIMEX   B     EXIT                                                             
         DROP  R2,R4,R7                                                         
         DROP  XREF                                                             
         EJECT                                                                  
**********************************************************************          
* ACCOUNT LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
         USING BIND,R2                                                          
         USING TIMED,R4                                                         
NXT      USING TIMED,R5                                                         
                                                                                
ACCL     DS    0H                                                               
         MVI   BYTE,NO                                                          
         L     R2,ATIMETAB         Time table                                   
         L     R3,BININ            Number of entries                            
         LTR   R3,R3                                                            
         BZ    ACCLX                                                            
*                                                                               
         LA    R4,BINTAB           Point to first entry                         
ACCL05   CLI   TIMEDUP,0                                                        
         BNE   ACCL10                                                           
         A     R4,BINLEN                                                        
         BCT   R3,ACCL05                                                        
         B     ACCLX               None to add                                  
*                                                                               
ACCL10   LA    RE,CPYPER#                                                       
         OC    TIMETMP#,TIMETMP#   Was there an xtra elem.?                     
         BNZ   *+8                 No, so don't count                           
         LA    RE,CPYMPER#                                                      
         AP    0(L'CPYPER#,RE),=P'1'      Found the first                       
         MVI   BYTE,YES                                                         
         SHI   R3,1                                                             
         BZ    ACCLX               Only one                                     
*                                                                               
         LR    R5,R4                                                            
ACCL15   A     R5,BINLEN                                                        
         CLI   NXT.TIMEDUP,0                                                    
         BNE   ACCL20                                                           
         BCT   R3,ACCL15                                                        
         B     ACCLX               No more                                      
*                                                                               
*        R4 = current record, R5 = next record                                  
*                                                                               
ACCL20   CLC   TIMEPEDT,NXT.TIMEPEDT                                            
         BE    ACCL22                                                           
         LA    RE,CPYPER#                                                       
         OC    NXT.TIMETMP#,NXT.TIMETMP#   Was there an xtra elem.?             
         BNZ   *+8                         No, so don't count                   
         LA    RE,CPYMPER#                                                      
         AP    0(L'CPYPER#,RE),=P'1'                                            
         MVI   BYTE,YES                                                         
*                                                                               
ACCL22   LR    R4,R5               Restart process                              
         SHI   R3,1                                                             
         BP    ACCL15                                                           
*                                                                               
ACCLX    CLI   BYTE,YES                                                         
         BNE   *+10                                                             
         AP    CPYPRSN,=P'1'                                                    
         NI    FLAG,TURNOFF-FLGPER  RESET FLAG                                  
         B     EXIT                                                             
         DROP  R2,R4                                                            
         DROP  NXT                                                              
         EJECT                                                                  
**********************************************************************          
* RUN     LAST                                                       *          
**********************************************************************          
         SPACE 1                                                                
REQL     DS    0H                                                               
         BAS   RE,PRTTOTS                                                       
         MVI   RCREQREP,NO                                                      
         NI    FLAG,TURNOFF-FLGNOFF CLEAR FLAG                                  
*                                                                               
REQLX    B     EXIT                                                             
         EJECT                                                                  
RUNL     DS    0H                                                               
         BAS   RE,PRTTOTS                                                       
*                                                                               
RUNLX    B     EXIT                                                             
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* PUT TOTAL IN ACCUMS                                                           
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
         SPACE 1                                                                
         USING TOTALD,R1                                                        
         USING TIMELD,R4                                                        
ADDTOT   NTR1                                                                   
         AP    TOTDUP,=P'1'                                                     
         AP    TOTTHRS,TIMHRS                                                   
*                                                                               
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   *+10                                                             
         AP    TOTBHRS,TIMHRS                                                   
*                                                                               
         CLI   TIMTTYP,TIMTCR                                                   
         BNE   *+10                                                             
         AP    TOTRHRS,TIMHRS                                                   
*                                                                               
         CLI   TIMTTYP,TIMTCN                                                   
         BNE   *+10                                                             
         AP    TOTNHRS,TIMHRS                                                   
*                                                                               
         CLI   TIMTTYP,TIMTNC                                                   
         BNE   *+10                                                             
         AP    TOTCHRS,TIMHRS                                                   
*                                                                               
         CLI   TIMLN,TIMILN2Q                                                   
         BNE   ADDTOT20                                                         
         AP    TOTTAMT,TIMAMNT                                                  
*                                                                               
         CLI   TIMTTYP,TIMTCB                                                   
         BNE   *+10                                                             
         AP    TOTBAMT,TIMAMNT                                                  
*                                                                               
         CLI   TIMTTYP,TIMTCR                                                   
         BNE   *+10                                                             
         AP    TOTRAMT,TIMAMNT                                                  
*                                                                               
         CLI   TIMTTYP,TIMTCN                                                   
         BNE   *+10                                                             
         AP    TOTNAMT,TIMAMNT                                                  
*                                                                               
         CLI   TIMTTYP,TIMTNC                                                   
         BNE   *+10                                                             
         AP    TOTCAMT,TIMAMNT                                                  
*                                                                               
ADDTOT20 TM    ERRSTAT,ERRMISS                                                  
         BZ    ADDTOTX                                                          
         AP    TOTBOTH,=P'1'                                                    
*                                                                               
ADDTOTX  B     EXIT                                                             
         DROP  R1,R4                                                            
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
D        USING TOTALD,R4                                                        
X        USING TOTALD,R6                                                        
         USING PTOTAL,R2                                                        
PRTTOTS  NTR1                                                                   
         MVI   RCSUBPRG,2                                                       
         LA    R4,CPYDTOT                                                       
         LA    R6,CPYXTOT                                                       
         ZAP   TOTTMPO,CPYTMPO                                                  
         ZAP   TOTMISS,CPYMISS                                                  
         CLI   MODE,REQLAST        Print company totals                         
         BE    PRTTOT05                                                         
         MVI   RCSUBPRG,3                                                       
         LA    R4,ACFDTOT                                                       
         LA    R6,ACFXTOT                                                       
         ZAP   TOTTMPO,ACFTMPO                                                  
         ZAP   TOTMISS,ACFMISS                                                  
         CLI   MODE,RUNLAST        Print ACCFILE totals                         
         BE    PRTTOT05                                                         
         DC    H'00'                                                            
*                                                                               
PRTTOT05 LA    R2,XP                                                            
         MVI   FORCEHED,C'Y'                                                    
         CP    D.TOTTHRS,=P'0'                                                  
         BE    PRTTOT06                                                         
         MVC   PTOTDIS,=CL36'Dup-line total hours'                              
         MVI   PCOLON,C':'                                                      
         EDIT  D.TOTBHRS,PBAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTNHRS,PNAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTRHRS,PRAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTCHRS,PCAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTTHRS,PTAMT,2,ZERO=NOBLANK                                   
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT06 CP    X.TOTTHRS,=P'0'                                                  
         BE    PRTTOT07                                                         
         MVC   PTOTDIS,=CL36'Exact dup total hours'                             
         MVI   PCOLON,C':'                                                      
         EDIT  X.TOTBHRS,PBAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTNHRS,PNAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTRHRS,PRAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTCHRS,PCAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTTHRS,PTAMT,2,ZERO=NOBLANK                                   
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT07 CP    D.TOTTHRS,=P'0'                                                  
         BNE   *+10                                                             
         CP    X.TOTTHRS,=P'0'                                                  
         BE    PRTTOT10                                                         
         MVC   PTOTDIS,=CL36'Both total hours'                                  
         MVI   PCOLON,C':'                                                      
         ZAP   PKAMT,D.TOTBHRS                                                  
         AP    PKAMT,X.TOTBHRS                                                  
         EDIT  PKAMT,PBAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTNHRS                                                  
         AP    PKAMT,X.TOTNHRS                                                  
         EDIT  PKAMT,PNAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTRHRS                                                  
         AP    PKAMT,X.TOTRHRS                                                  
         EDIT  PKAMT,PRAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTCHRS                                                  
         AP    PKAMT,X.TOTCHRS                                                  
         EDIT  PKAMT,PCAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTTHRS                                                  
         AP    PKAMT,X.TOTTHRS                                                  
         EDIT  PKAMT,PTAMT,2,ZERO=NOBLANK                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT10 CP    D.TOTTAMT,=P'0'                                                  
         BE    PRTTOT11                                                         
         MVC   PTOTDIS,=CL36'Line-dup total amount'                             
         MVI   PCOLON,C':'                                                      
         EDIT  D.TOTBAMT,PBAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTNAMT,PNAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTRAMT,PRAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTCAMT,PCAMT,2,ZERO=NOBLANK                                   
         EDIT  D.TOTTAMT,PTAMT,2,ZERO=NOBLANK                                   
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT11 CP    X.TOTTAMT,=P'0'                                                  
         BE    PRTTOT12                                                         
         MVC   PTOTDIS,=CL36'Exact dup total amount'                            
         MVI   PCOLON,C':'                                                      
         EDIT  X.TOTBAMT,PBAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTNAMT,PNAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTRAMT,PRAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTCAMT,PCAMT,2,ZERO=NOBLANK                                   
         EDIT  X.TOTTAMT,PTAMT,2,ZERO=NOBLANK                                   
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT12 CP    D.TOTTAMT,=P'0'                                                  
         BNE   *+10                                                             
         CP    X.TOTTAMT,=P'0'                                                  
         BE    PRTTOT15                                                         
         MVC   PTOTDIS,=CL36'Both total amount'                                 
         MVI   PCOLON,C':'                                                      
         ZAP   PKAMT,D.TOTBAMT                                                  
         AP    PKAMT,X.TOTBAMT                                                  
         EDIT  PKAMT,PBAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTNAMT                                                  
         AP    PKAMT,X.TOTNAMT                                                  
         EDIT  PKAMT,PNAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTRAMT                                                  
         AP    PKAMT,X.TOTRAMT                                                  
         EDIT  PKAMT,PRAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTCAMT                                                  
         AP    PKAMT,X.TOTCAMT                                                  
         EDIT  PKAMT,PCAMT,2,ZERO=NOBLANK                                       
*                                                                               
         ZAP   PKAMT,D.TOTTAMT                                                  
         AP    PKAMT,X.TOTTAMT                                                  
         EDIT  PKAMT,PTAMT,2,ZERO=NOBLANK                                       
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT15 CP    TOTMISS,=P'0'                                                    
         BE    PRTTOT16                                                         
         MVC   PTOTDIS,=CL36'Total missing XTRA element'                        
         MVI   PCOLON,C':'                                                      
         EDIT  TOTMISS,PTOTAMT,ZERO=NOBLANK                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT16 CP    D.TOTDUP,=P'0'                                                   
         BE    PRTTOT18                                                         
         MVC   PTOTDIS,=CL36'Total duplicate lines'                             
         MVI   PCOLON,C':'                                                      
         EDIT  D.TOTDUP,PTOTAMT,ZERO=NOBLANK                                    
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT18 CP    X.TOTDUP,=P'0'                                                   
         BE    PRTTOT20                                                         
         MVC   PTOTDIS,=CL36'Total exact duplicates'                            
         MVI   PCOLON,C':'                                                      
         EDIT  X.TOTDUP,PTOTAMT,ZERO=NOBLANK                                    
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT20 CP    D.TOTBOTH,=P'0'                                                  
         BE    PRTTOT22                                                         
         MVC   PTOTDIS,=CL36'Total line-dup && miss. xtra'                      
         MVI   PCOLON,C':'                                                      
         EDIT  D.TOTBOTH,PTOTAMT,ZERO=NOBLANK                                   
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT22 CP    X.TOTBOTH,=P'0'                                                  
         BE    PRTTOT24                                                         
         MVC   PTOTDIS,=CL36'Total x-dup && miss. xtra'                         
         MVI   PCOLON,C':'                                                      
         EDIT  X.TOTBOTH,PTOTAMT,ZERO=NOBLANK                                   
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT24 CP    TOTTMPO,=P'0'                                                    
         BE    PRTTOT26                                                         
         MVC   PTOTDIS,=CL36'Total tempo line processed'                        
         MVI   PCOLON,C':'                                                      
         EDIT  TOTTMPO,PTOTAMT,ZERO=NOBLANK                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOT26 CP    CPYPRSN,=P'0'                                                    
         BE    PRTTOTX                                                          
         CLI   MODE,REQLAST        Print company totals                         
         BNE   PRTTOTX                                                          
         MVC   PTOTDIS,=CL36'Total number of people'                            
         MVI   PCOLON,C':'                                                      
         EDIT  CPYPRSN,PTOTAMT,ZERO=NOBLANK                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
         MVC   PTOTDIS,=CL36'Total time sheets with duplicates'                 
         MVI   PCOLON,C':'                                                      
         EDIT  CPYPER#,PTOTAMT,ZERO=NOBLANK                                     
         BAS   RE,PRINTIT                                                       
*                                                                               
         CP    CPYMPER#,=P'0'                                                   
         BE    PRTTOTX                                                          
         MVC   PTOTDIS,=CL36'Total time sheets missing xtra elem'               
         MVI   PCOLON,C':'                                                      
         EDIT  CPYMPER#,PTOTAMT,ZERO=NOBLANK                                    
         BAS   RE,PRINTIT                                                       
*                                                                               
PRTTOTX  B     EXIT                                                             
         DROP  R2                                                               
         DROP  D,X                                                              
         EJECT                                                                  
**********************************************************************          
* PRINT ROUTINE                                                      *          
**********************************************************************          
         SPACE 1                                                                
PRINTIT  NTR1                                                                   
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT                                                         
         B     EXIT                                                             
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
* SETUP HEADLINES                                                    *          
**********************************************************************          
         SPACE 1                                                                
HEADUP   NTR1                                                                   
         MVC   XHEAD3+1(10),=CL10'LOGIN ID :'                                   
         MVC   XHEAD3+12(L'SVCLOGO),SVCLOGO  COMPANY'S MAIN ID                  
*                                                                               
         USING HEADD,R3                                                         
         CLI   RCSUBPRG,1                                                       
         BNE   EXIT                                                             
         LA    R3,XHEAD5                                                        
         MVC   HEADDESC,LEVADSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVACDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVANME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD6                                                        
         MVC   HEADDESC,LEVBDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVBCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVBNME      LEVEL NAME                                 
*                                                                               
         LA    R3,XHEAD7                                                        
         MVC   HEADDESC,LEVCDSC      LEVEL DESCRIPTION                          
         MVC   HEADCODE,LEVCCDE      LEVEL CODE                                 
         MVC   HEADNAME,LEVCNME      LEVEL NAME                                 
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* CONSTANTS                                                          *          
**********************************************************************          
         SPACE 1                                                                
ATIMETAB DC    A(TIMETAB)          TRANSACTION TABLE                            
ABINADD  DC    A(BINADD)           ROUTINE TO ADD TO BINSEARCH TABLE            
ADUMP    DC    A(DUMP)             ROUTINE TO DITTO RECORDS                     
*                                                                               
ACCMST   DC    CL8'ACCMST'                                                      
DUMXTRA  DC    AL1(TIMELQ,TIMXTLNQ,0,TIMEXTRA)                                  
         DC    X'0000'                                                          
         DC    X'700101'                                                        
         DC    XL7'00'                                                          
         DC    X'00'                                                            
         SPACE 2                                                                
**********************************************************************          
* RELOCATACBLES                                                      *          
**********************************************************************          
         SPACE 1                                                                
ADCONS   DS    0F                                                               
         DC    V(PRNTBL)           PRINT DATA                                   
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
*&&DO                                                                           
         L     R4,DMCB             A(RECORD FOUND)                              
         LA    R6,TIMKLNQ          BUMP PAST KEY                                
         AR    R4,R6                  IN TABLE ENTRY                            
         AR    R3,R6                  IN NEW ENTRY                              
         OC    0(1,R4),0(R3)       TURN ON STATUS                               
         B     BINXIT                                                           
*&&                                                                             
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
* TABLES                                                             *          
**********************************************************************          
        SPACE 1                                                                 
*                                                                               
* BINTABLE 1 - TRANSACTION TABLE                                                
*                                                                               
         DC    C'***BINT***'                                                    
TIMETAB  DS    0D                  BINTABLE CONSTANTS FOR TABLE 5               
         DC    F'0'                    NUMBER IN TABLE                          
         DC    AL4(TIMELNQ)            LENGTH OF ENTRY                          
         DC    AL1(0)                  DISP. TO KEY                             
         DC    AL3(TIMKLNQ)            KEY LENGTH                               
         DC    AL4(TIMEMAX)            MAX IN TABLE                             
         DS    (TIMEMAX*TIMELNQ)XL1    TABLE                                    
                                                                                
TIMEMAX  EQU   20000                                                            
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
        SPACE 1                                                                 
ACZED    DSECT                                                                  
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
TURNOFF  EQU   X'FF'                                                            
ABUFFNTY DS    A                                                                
*                                                                               
VTYPES   DS    0A                                                               
PRNTBL   DS    V                   PRINT DATA                                   
VTYPLNQ  EQU   *-VTYPES                                                         
*                                                                               
DISP2    DS    H                   DISPLACEMENT TO KEY                          
ELCODE   DS    CL1                 ELEMENT CODE                                 
*                                                                               
PKAMT    DS    PL8                                                              
*                                                                               
PKFLDS   DS    PL4                                                              
PKCOUNT  DS    PL4                 TOTAL RECORD COUNTER                         
PKTMPCNT DS    PL4                 TEMPO RECORD COUNTER                         
PKPIDCNT DS    PL4                 TMS W/PID RECORD COUNTER                     
PKTPDCNT DS    PL4                 TEMPO W/PID RECORD COUNTER                   
PKFLDLNQ EQU   (*-PKFLDS)/L'PKFLDS                                              
*                                                                               
*   COMPANY TOTALS                                                              
CPYDTOT  DS    (TOT#ACCM)PL8       Line  duplicates - company wide              
CPYXTOT  DS    (TOT#ACCM)PL8       Exact duplicates - company wide              
ACFDTOT  DS    (TOT#ACCM)PL8       Line  duplicates - Accfile wide              
ACFXTOT  DS    (TOT#ACCM)PL8       Exact duplicates - Accfile wide              
*                                                                               
CPYTMPO  DS    PL8                 Tempo elements processed                     
ACFTMPO  DS    PL8                                                              
TOTTMPO  DS    PL8                                                              
CPYMISS  DS    PL8                 Just missing xtra x'8B' element              
ACFMISS  DS    PL8                                                              
TOTMISS  DS    PL8                 Just missing xtra x'8B' element              
CPYPRSN  DS    PL8                 Number of persons process for cpy            
CPYPER#  DS    PL8                 Number of duplicate time sheets              
CPYMPER# DS    PL8                 Number of missing xtra elem. time            
*                                                                               
MSG      DS    CL10                DUMP MESSAGE                                 
DIFFCODE DS    CL1                                                              
*                                                                               
FLAG     DS    XL1                                                              
FLGPER   EQU   X'80'               PERSON WAS DONE ALREADY                      
FLGNOFF  EQU   X'40'               NEW OFFICE IN USE                            
*                                                                               
ERRSTAT  DS    XL1                                                              
ERRMISS  EQU   X'80'               MISSING XTRA X'8B' ELEMENT                   
ERRDUP   EQU   X'40'               DUPLICATE TEMPO LINE # ONLY                  
ERRXDUP  EQU   X'20'               EXACT DUPLICATE                              
*                                                                               
SVKEY    DS    CL49                                                             
SVACCT   DS    CL12                                                             
SVPEDT   DS    CL3                                                              
SVCLOGO  DS    CL7                 COMPANY'S LOGIN ID (LOGO)                    
SVPID    DS    XL2                                                              
*                                                                               
LASTKEY  DS    CL(TIMKEND)                                                      
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
TIMEWRK  DS    CL(TIMELNQ)                                                      
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
PSTAR    DS    CL1                                                              
PDCODE   DS    CL1                                                              
         DS    CL1                                                              
PDUPNUM  DS    CL2                 DUPLCATION FACTOR                            
         DS    CL1                                                              
PCODE    DS    CL10                PERSON CODE                                  
         DS    CL1                                                              
PNAME    DS    CL24                NAME                                         
         DS    CL2                                                              
PPEDT    DS    CL8                 PERIOD END DATE                              
         DS    CL2                                                              
PTTYPE   DS    CL1                 B,N,R,C                                      
         DS    CL2                                                              
PMOA     DS    CL6                 MOA                                          
         DS    CL2                                                              
PHOURS   DS    CL6                 HOURS                                        
         DS    CL2                                                              
PAMNT    DS    CL8                 AMOUNT                                       
         DS    CL2                                                              
PLINE1   DS    CL6                 TMS   LINE #                                 
         DS    CL2                                                              
PLINE2   DS    CL6                 TEMPO LINE #                                 
         DS    CL2                                                              
PSJAC    DS    CL14                                                             
         DS    CL2                                                              
PINCOME  DS    CL14                                                             
         DS    CL2                                                              
PTASK    DS    CL2                                                              
         DS    CL2                                                              
POFFICE  DS    CL2                                                              
         DS    CL2                                                              
PACTDTE  DS    CL8                                                              
         DS    CL2                                                              
PERR     DS    CL15                                                             
         DS    CL1                                                              
PRATE    DS    CL8                                                              
PLINELNQ EQU   *-PRTLINE                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT FOR PRINT TOTAL DETAIL                                        *         
***********************************************************************         
         SPACE 1                                                                
PTOTAL   DSECT                                                                  
PRTTOTL  DS    0C                                                               
PTOTDIS  DS    CL36                DISCRIPTION                                  
         DS    CL1                                                              
PCOLON   DS    CL1                                                              
         DS    CL1                                                              
PTOTAMT  DS    0CL12                                                            
PBAMT    DS    CL12                B                                            
         DS    CL2                                                              
PNAMT    DS    CL12                N                                            
         DS    CL2                                                              
PRAMT    DS    CL12                R                                            
         DS    CL2                                                              
PCAMT    DS    CL12                C                                            
         DS    CL2                                                              
PTAMT    DS    CL12                Total                                        
PLIN2LNQ EQU   *-PRTTOTL                                                        
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
TOTALD   DSECT                                                                  
TOTBHRS  DS    PL8                                                              
TOTNHRS  DS    PL8                                                              
TOTRHRS  DS    PL8                                                              
TOTCHRS  DS    PL8                                                              
TOTTHRS  DS    PL8                                                              
TOTBAMT  DS    PL8                                                              
TOTNAMT  DS    PL8                                                              
TOTRAMT  DS    PL8                                                              
TOTCAMT  DS    PL8                                                              
TOTTAMT  DS    PL8                                                              
TOTDUP   DS    PL8                                                              
TOTBOTH  DS    PL8                                                              
TOT#ACCM EQU   (*-TOTALD)/8                                                     
         SPACE 1                                                                
***********************************************************************         
* DSECT TIME TABLE WORK AREA                                          *         
***********************************************************************         
         SPACE 1                                                                
TIMED    DSECT                                                                  
*IMEPER  DS    CL12                PERSON CODE                                  
TIMEPEDT DS    XL3                 PERIOD END DATE                              
TIMETMP# DS    XL2                 TEMPO LINE#                                  
TIMKLNQ  EQU   *-TIMED                                                          
TIMEACC  DS    CL14                                                             
TIMETSK  DS    CL2                                                              
TIMEOFF  DS    CL2                                                              
TIMETTYP DS    CL1                                                              
TIMEIND  DS    XL1                 WO OR ADJ                                    
TIMEMOA  DS    XL2                                                              
TIMETMS# DS    XL2                                                              
TIMEHRS  DS    PL3                                                              
TIMERATE DS    PL4                                                              
TIMEADTE DS    XL3                                                              
TIMEINC  DS    CL14                INCOME ACCOUNT                               
TIMEAMNT DS    PL6                 BILLING AMOUNT IF APPLICABLE                 
TIMESTA  DS    XL1                 STATUS BYTE                                  
TIMEDUP  DS    XL1                 DUPLICATE FACTOR                             
TIMELNQ  EQU   *-TIMED                                                          
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
**PAN#1  DC    CL21'168ACREPZE02 12/07/00'                                      
         END                                                                    
