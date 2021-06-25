*          DATA SET SPEZF09    AT LEVEL 114 AS OF 11/04/20                      
*PHASE T23009A                                                                  
***********************************************************************         
*                                                                     *         
*  REGISTER USAGE                                                     *         
*  R0 - OS & WORK                                                     *         
*  R1 - OS & WORK                                                     *         
*  R2 - WORK                                                          *         
*  R3 - WORK                                                          *         
*  R4 - WORK                                                          *         
*  R5 - WORK                                                          *         
*  R6 - SECOND BASE                                                   *         
*  R7 - EZBLOCKD                                                      *         
*  R8 - SPOOLD                                                        *         
*  R9 - SYSD                                                          *         
*  RA - TWAD                                                          *         
*  RB - FIRST BASE REG                                                *         
*  RC - GEND                                                          *         
*  RD - STD REG SAVE CHAIN                                            *         
*  RE - OS & WORK                                                     *         
*  RF - OS & WORK                                                     *         
*                                                                     *         
*  AIO1  WORKER INDEX                                                 *         
*  SYSD  WORKER BUFFER                                                *         
*  AIO2  EZBLOCK                                                      *         
*  AIO3  STATION REC BUFFER                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPEZF09 - EASI - INVOICE REPORT'                                
         PRINT NOGEN                                                            
T23009   CSECT                                                                  
         NMOD1 0,T23009,R6,RR=R2                                                
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
         ST    RC,SVRC                                                          
         MVC   AIO,AIO1                                                         
         L     R7,AIO2                                                          
         USING EZBLOCKD,R7                                                      
         L     RF,=A(SAVREGS)      SAVE REGS FOR EZPRINT                        
         A     RF,RELO                                                          
         STM   R6,RC,0(RF)                                                      
*                                                                               
         BRAS  RE,INIT                                                          
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LRR                                                              
*                                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
NEQXIT   LTR   RB,RB                                                            
EXIT     DS   0H                                                                
         XIT1                                                                   
*                                                                               
* VALIDATE WHAT DATA IS TO BE REPORTED *                                        
*                                                                               
VKEY     MVI   ERROR,INVACT        ONLY VALID ACTION IS LIST                    
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTCHA                                                    
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTDEL                                                    
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTREST                                                   
         BE    TRAPERR                                                          
         CLI   ACTNUM,ACTDIS                                                    
         BE    TRAPERR                                                          
*                                                                               
         CLI   CONSEMPL,0          DON'T DO UNLESS SOON, OV, DDS                
         BNE   VK050                NO                                          
*                                                                               
         MVI   CONSEMPL,05         DEFAULT TO MPL1                              
*                                                                               
         CLI   IRPSYS,0            IS RESIDENT SYSTEM FROM ONLINE               
         BE    VK050                NO                                          
*                                                                               
         MVC   CONSEMPL,IRPSYS                                                  
*                                                                               
VK050    DS   0H                                                                
         XC    RQOPTS,RQOPTS       CLEAR OPTS                                   
* READ PROFILE TO DETERMINE WHETHER SORT BY CLIENT                              
*                                                                               
         MVI   SRTCLT,C'N'                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0EZ'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         CLI   WORK+16+14,C'Y'                                                  
         BNE   VK060                                                            
         CLI   25(RA),X'00'                                                     
         BE    NOTNOWER                                                         
*                                                                               
         MVI   SRTCLT,C'Y'                                                      
         MVI   SRTOPT,C'Y'                                                      
*                                                                               
VK060    DS    0H                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SEZ2'                                                 
         NI    WORK,X'FF'-X'40'    LOWERCASE 'S'                                
         MVC   WORK+4(2),AGENCY                                                 
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
         MVC   PGDISP,EZ2PGDSC-EZ2PROF+WORK+16                                  
         MVC   AGYDISC,EZ2AGYDS-EZ2PROF+WORK+16                                 
         MVC   EZ2MINUS,EZ2NEGTV-EZ2PROF+WORK+16                                
*                                                                               
         GOTO1 VALIMED             DUMMY - TO NET OR SPOT                       
*                                                                               
         LA    R2,IRPSTAH          STATION (REQUIRED) - ALL IS OK               
         XC    SVSTA,SVSTA                                                      
         GOTO1 ANY                                                              
*                                                                               
         CLC   =C'ALL',WORK                                                     
         BNE   VK100                                                            
         CLI   5(R2),3                                                          
         BNE   VK100                                                            
         MVC   SVSTA(3),=C'ALL'                                                 
         B     VK120                                                            
*                                                                               
VK100    GOTO1 VREADSTA                                                         
         MVC   SVSTA,FLDRDSTA                                                   
*                                                                               
VK120    GOTO1 VEQVSTA             BUILD EQUIVALENT STATION TABLE               
*                                                                               
* OPTIONS LINE                                                                  
*                                                                               
         LA    R2,IRPOPTH                                                       
         CLI   5(R2),0                                                          
         BE    VK140                                                            
*                                                                               
         BRAS  RE,VOPT                                                          
*                                                                               
VK140    LA    R2,IRPDTEH          BATCH DATE (REQUIRED)                        
         XC    SVDTES,SVDTES                                                    
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK144                                                            
*                                                                               
*  OPTION TODAYC - CONVERTED TODAY  OR OPTION TODAY LOADED TODAY                
*                                                                               
         TM    OPTNSW,OPTTDYC+OPTTDYL                                           
         BNZ   VK200                                                            
*                                                                               
VK144    GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         ICM   R3,15,DMCB                                                       
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDTESTR)                                
*                                                                               
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK160                YES                                         
         LA    R3,1+8(R2,R3)                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
         OC    DMCB,DMCB                                                        
         BZ    BADATE                                                           
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDTEEND)                                
         B     VK200                                                            
*                                                                               
VK160    MVC   SVDTEEND,SVDTESTR                                                
*                                                                               
VK200    LA    R2,IRPSEQH          BATCH SEQ (OPTIONAL)                         
         XC    SVBSEQ,SVBSEQ                                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    VK300                                                            
*                                                                               
         MVC   WORK(8),=8C'0'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   NOTNUMER                                                         
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
         STCM  R0,15,SVBSEQ                                                     
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
VK300    LA    R2,IRPINVH          INVOICE (OPTIONAL)                           
         XC    SVINV,SVINV                                                      
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
         MVC   SVINV,8(R2)                                                      
*                                                                               
VKXIT    MVC   IRPSYS,CONSEMPL                                                  
         MVI   IRPSYSH+5,1                                                      
         XC    KEY,KEY                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    EXIT                                                             
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         B     EXIT                                                             
*                                                                               
* VALKEY ERRORS                                                                 
*                                                                               
NOTNOWER L     R1,=A(NOTNOWMS)                                                  
         LA    R2,CONWHENH                                                      
         XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         ZIC   RE,0(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CONHEAD(0),1(R1)                                                 
         MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
*                                                                               
*                                                                               
*                                                                               
* PRINT THE INVOICE REPORT (INITIALIZE) *                                       
*                                                                               
LRR      DS    0H                                                               
         BRAS  RE,LRRINIT                                                       
*                                                                               
         XC    EZWKRIND,EZWKRIND                                                
         BRAS  RE,CLRALL           CLEAR ALL LINE SAVES                         
*                                                                               
* READ THROUGH WORKER FILE, LOOKING FOR SELECTED INVOICES *                     
*                                                                               
LRR200   DS    0H                                                               
         XCEF  EZSTFRST,'EZSTDLEN'                                              
         XCEF  EZSBFRST,'EZSBDLEN'                                              
*                                                                               
         LAY   R5,EZWRKIOB                                                      
         USING WRKIOB,R5                                                        
*                                                                               
         CLI   OPTEXP,C'Y'                                                      
         BE    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
         MVI   WRKIACTN,WRKIANDX                                                
         GOTO1 EZWRKIO,(R5)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LRR205                                                           
         CLI   WRKIERRS,0                                                       
         BE    LRR210                                                           
         DC    H'0'                                                             
         DROP  R5                  WRKIOB,R5                                    
*                                                                               
* EOF HERE                                                                      
*                                                                               
LRR205   CLI   SRTOPT,C'Y'         SORT REQUIRED                                
         BE    LRR240               YES - GET RECORDS FROM SORTER               
*                                                                               
         B     EXIT                 NO, ALL DONE                                
*                                                                               
* GOT WORKER FILE INDEX HERE                                                    
*                                                                               
LRR210   DS    0H                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
         LA    R4,EZWKRIND                                                      
         USING UKRECD,R4                                                        
         CLI   UKDAY,X'99'         TEST AN EASI FILE                            
         BNE   LRR200                                                           
*                                                                               
         CLC   UKUSRID,TWAORIG     TEST RIGHT ID                                
         BNE   LRR200                                                           
*                                                                               
         CLI   OPTNKP,C'Y'         ONLY NON-KEEP BATCHES?                       
         BNE   *+12                                                             
         TM    UKSTAT,X'08'        BATCH STATUS = KEEP?                         
         BO    LRR200                                                           
*                                                                               
* SEE IF IN DATE RANGE                                                          
*                                                                               
         OC    SVDTES,SVDTES       WERE DATES ENTERED                           
         BZ    LRR210A                                                          
*                                                                               
         CLC   UKAGELD,SVDTESTR                                                 
         BL    LRR200                                                           
         CLC   UKAGELD,SVDTEEND                                                 
         BH    LRR200                                                           
*                                                                               
LRR210A  DS    0H                                                               
         TM    OPTNSW,OPTTDYL      OPTION LOADED TODAY                          
         BZ    *+14                                                             
         CLC   UKAGELD,OPTODAYL   WAS THIS LOADED TODAY?                        
         BNE   LRR200                                                           
*                                                                               
         OC    OPTBMOS,OPTBMOS     FILTERING ON MOS                             
         BZ    LRR211                                                           
*                                                                               
         LAY   R5,EZWRKIOB                                                      
         USING WRKIOB,R5                                                        
*                                                                               
         OC    WRKEZMOS,WRKEZMOS                                                
         BZ    *+14                                                             
         MVC   HALF,WRKEZMOS                                                    
         B     LRR210B                                                          
*                                                                               
         CLI   WRKEZUDT,X'00'                                                   
         JE    *+2                 NO MOS IN BATCH INDEX                        
*                                                                               
         MVC   BYTE,WRKEZUDT                                                    
         BRAS  RE,NEW2OLD                                                       
         DROP  R5                  WRKIOB,R5                                    
*                                                                               
LRR210B  DS    0H                                                               
         CLC   HALF,OPTBMOS                                                     
         BNE   LRR200                                                           
*                                                                               
LRR211   DS    0H                                                               
         USING EZWKRIXD,R4                                                      
*                                                                               
         BRAS  RE,CLRALL                                                        
         LA    R1,ALLPTTL                                                       
         LHI   R0,ALLPTTN                                                       
         BRAS  RE,ZAPTOT                                                        
*                                                                               
         MVC   SRCESTA(4),EZWISTN         STATION                               
         CLI   SRCESTA+3,C' '                                                   
         BH    *+8                                                              
         MVI   SRCESTA+3,C' '                                                   
         MVC   SRCESTA+4(1),EZWIMED                                             
*                                                                               
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION CODE               
*                                                                               
         MVC   EZSPTNET,SPOTNETS                                                
         MVC   EZEQVSTA,EQUISTA5                                                
         MVC   EZBAGYMD,BAGYMD                                                  
*                                                                               
         GOTO1 VFILTSYS                                                         
         BNE   LRR200                                                           
*                                                                               
         MVC   WORK(5),EQUISTA5        STATION                                  
         CLI   WORK+3,0                                                         
         BNE   *+8                                                              
         MVI   WORK+3,C' '                                                      
*                                                                               
         CLC   =C'ALL',SVSTA                                                    
         BE    LRR218                                                           
*                                                                               
         CLC   WORK(5),SVSTA                                                    
         BNE   LRR200                                                           
*                                                                               
LRR218   DS    0H                                                               
         CLI   OPTMEDIA,0          FILTER BY MEDIA                              
         BE    LRR220               NO                                          
*                                                                               
         CLC   EQVMED,OPTMEDIA                                                  
         BNE   LRR200                                                           
*                                                                               
LRR220   DS    0H                                                               
         OC    OPTMGRP,OPTMGRP                                                  
         BZ    LRR221                                                           
*                                                                               
         BRAS  RE,FTRMGR           SEE IF THIS QUALIFIES                        
         BNE   LRR200                                                           
*                                                                               
LRR221   DS    0H                                                               
         OC    OPTMKT,OPTMKT                                                    
         BZ    LRR222                                                           
*                                                                               
         BRAS  RE,FTRMKT           SEE IF THIS QUALIFIES                        
         BNE   LRR200                                                           
*                                                                               
LRR222   DS    0H                                                               
*                                  READ FIRST RECORD                            
*                                                                               
LRR222R  LAY   R5,EZWRKIOB                                                      
         USING WRKIOB,R5                                                        
*                                                                               
         MVIY  WRKIACTN,WRKIAGET                                                
         GOTO1 EZWRKIO,(R5)                                                     
         CLI   WRKIERRS,WRKIEEOF                                                
         BE    LRR200                                                           
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         LAY   R5,WRKEZKEY                                                      
         USING WRKEZKEY,R5                                                      
*                                                                               
* IF FILTERS MATCH ON FILTER                                                    
*                                                                               
LRR223   OC    SVBSEQ,SVBSEQ         SEQ                                        
         BZ    *+14                                                             
         CLC   SVBSEQ,WRKEZSQN                                                  
         BNE   LRR200                                                           
*                                                                               
* FILTER BY SOURCE                                                              
*                                                                               
         MVC   SVSRCE,WRKEZDSC+EZWCSRCE-EZWKRCMD                                
         DROP  R5                                                               
*                                                                               
         OC    SVSRCE,SPACES       TO UPPERCASE                                 
*                                                                               
         CLC   OPTSRCE,SPACES      DO WE HAVE SOURCE FILTER?                    
         BNH   LRR227              NO, NO "SOURCE=" CARD                        
         CLC   =C'ALL ',OPTSRCE    REQUESTING FOR ALL SOURCES?                  
         BE    LRR228              YES                                          
*                                                                               
         CLC   =C'SDI ',OPTSRCE    FILTERING ON SDIX?                           
         BNE   LRR225              NO - COMPARE FOR THE FULL 4 CHARS            
         CLC   =C'SDI',SVSRCE      DOES THE SOURCE START WITH "SDI"             
         BNE   LRR200              NO - SKIP IT                                 
         B     LRR228              YES - PROCESS THIS BATCH                     
*                                                                               
LRR225   DS    0H                  CHECK IF SAME SOURCE AS THE FILTER           
         CLC   OPTSRCE,SVSRCE                                                   
         BNE   LRR200                                                           
         B     LRR228                                                           
*                                                                               
* NO "SOURCE=" CARD SUPPLIED.                                                   
* CHECK IF WE NEED TO SUPPRESS THIS SOURCE                                      
*                                                                               
LRR227   DS    0H                                                               
         BRAS  RE,SUPPRESS                                                      
         BNE   LRR200                                                           
*                                                                               
LRR228   DS    0H                                                               
         MVC   EZPYFRST(L'SPACES),SPACES                                        
         MVC   EZPYFRST+L'SPACES(EZPYDLEN-L'SPACES),SPACES                      
*                                                                               
         CLI   SRTOPT,C'Y'         THIS INPUT PASS FOR SORT                     
         BNE   *+12                 NO FULL LOOKUP IN EZMOD                     
         MVI   EZLOOKSW,X'E0'                                                   
         B     *+8                                                              
         MVI   EZLOOKSW,X'20'                                                   
*                                                                               
         OC    OPTCC,OPTCC         IF FILTERING BY CLIENT                       
         BZ    *+8                                                              
         NI    EZLOOKSW,X'FF'-X'80' FORCE CLIENT LOOKUP                         
*                                                                               
         OI    EZLOOKSW,EZVALACT                                                
*                                                                               
* NOTE DURING INPUT PHASE OF SORT, GO TO EZMOD TO GET INVOICE AND MOS           
*                                                                               
         GOTO1 VEZMOD,DMCB,EZBLOCKD                                             
*                                                                               
LRR229   DS    0H                                                               
         AP    BATUSED,=P'1'                                                    
*                                                                               
         CLI   SRTOPT,0            THIS A SORTED REQUEST                        
         BE    LRR200                NO                                         
*                                                                               
         CLI   SRTOPT,C'Z'         THIS OUTPUT PASS FOR SORT                    
         BE    LRR250               YES                                         
*                                                                               
         CLI   SRTCLT,C'Y'         SORTING BY CLIENT?                           
         BE    LRR200              YES - SORTER INPUT IN EZMPROC                
*                                                                               
         CLI   SRTOPT,C'Y'         THIS INPUT PASS FOR SORT                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   SRTOPN,C'Y'         SORT OPENED                                  
         BE    LRR230               YES                                         
*                                                                               
         MVI   SRTOPN,C'Y'         SET SORT OPENED                              
*                                                                               
         BRAS  RE,SETCARDS                                                      
         LM    RE,RF,=A(SORTCARD,RECCARD)                                       
         A     RE,RELO                                                          
         A     RF,RELO                                                          
         GOTO1 SORTER,DMCB,(RE),(RF)                                            
*                                                                               
LRR230   DS    0H                                                               
         BRAS  RE,ADDTOSRT                                                      
*                                                                               
         B     LRR200              NEXT INDEX                                   
*                                                                               
* SORTER OUTPUT PHASE                                                           
*                                                                               
LRR240   MVI   SRTOPT,C'Z'         SET TO OUTPUT PASS FOR SORT                  
         XC    LASTCLSQ,LASTCLSQ                                                
*                                                                               
         MVC   WORK(4),VEZMOD                                                   
         MVC   WORK+4(4),EZWRKIO                                                
         BRAS  RE,INITEZB          INITIALIZE EZBLOCKD                          
         MVC   VEZMOD,WORK                                                      
         MVC   EZWRKIO,WORK+4                                                   
*                                                                               
LRR250   CLI   SRTOPN,C'Y'         SORT OPENED                                  
         BNE   EXIT                 NO                                          
         GOTO1 SORTER,DMCB,=C'GET'                                              
*                                                                               
         L     RF,4(,R1)                                                        
         LTR   RF,RF               END OF SORTED RECS                           
         BZ    ENDSORT               YES                                        
*                                                                               
         CLC   LASTCLSQ,0(RF)                                                   
         BE    LRR250                                                           
         MVC   LASTCLSQ,0(RF)                                                   
*                                                                               
         AP    SORTGET,=P'1'                                                    
         MVC   SORTENT(SORTRLQ),0(RF)                                           
         MVC   SRCESTA,SORTSTA                                                  
*                                                                               
LRR251   DS    0H                                                               
         GOTO1 VEQVSTA             GO GET EQUIVALENT STATION CODE               
*                                                                               
         MVC   EZBAGYMD,BAGYMD                                                  
         MVC   EZSPTNET,SPOTNETS                                                
         MVC   EZEQVSTA,EQUISTA5                                                
         MVC   EZBAGYMD,BAGYMD                                                  
*                                                                               
LRR255   DS    0H                                                               
         LAY   R5,EZWRKIOB                                                      
         USING WRKIOB,R5                                                        
*                                                                               
         MVC   WRKEZUID,TWAORIG                                                 
         MVC   WRKEZSQN,SORTSQN                                                 
         OI    WRKINDS,WRKISEQQ                                                 
         CLI   OPTEXP,C'Y'                                                      
         BE    *+8                                                              
         OI    WRKINDS,WRKINOXQ                                                 
*                                                                               
         XCEF  EZSTFRST,'EZSTDLEN'                                              
         XCEF  EZSBFRST,'EZSBDLEN'                                              
*                                                                               
         MVI   WRKIACTN,WRKIANDX                                                
         GOTO1 EZWRKIO,(R5)                                                     
         CLI   WRKIERRS,0                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   SORTSQN,WRKEZSQN                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R5                                                               
*                                                                               
         BRAS  RE,FAKEIND                                                       
*                                                                               
         B     LRR222R                                                          
         DROP  R4                                                               
*                                                                               
ENDSORT  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
*        EZMOD PROCESSING ROUTINE                                               
*                                                                               
* ORDER OF RECORDS WILL BE INVOICE             (31)                             
*                          INVOICE TOP COMMENT (32)                             
*                          SCHEDULE LINE       (41)                             
*                          BROADCAST DETAIL    (51)                             
*                          INVOICE TOP COMMENT (32)                             
*                          INVOICE BOTTOM CMT  (33)                             
*                          INVOICE TOTAL       (34)                             
*                                                                               
         DS    0H                                                               
EZMPROC  NTR1                                                                   
         CLI   EZMODE,EZINVP       (31) PROCESS INVOICE                         
         BE    INVPRTN                                                          
         CLI   SRTOPT,C'Y'         IF SORT INPUT, NO PRINTING                   
         BE    EXIT                                                             
         CLI   EZMODE,EZSPTP       (51) PROCESS SPOT                            
         BE    STPPRTN                                                          
         CLI   EZMODE,EZSPTL            LAST FOR SPOT                           
         BE    STPLRTN                                                          
         CLI   EZMODE,EZSCHP       (41) PROCESS SCHEDULE LINE                   
         BE    LINPRTN                                                          
         CLI   EZMODE,EZSCOMP      (42) PROCESS SCHEDULE COMMENT                
         BE    CHPCRTN                                                          
         CLI   EZMODE,EZSCHL       (  ) LAST FOR SCHED LINE                     
         BE    LINLRTN                                                          
         CLI   EZMODE,EZRECP       (51) PROCESS RECONCILIATION REMARK           
         BE    RECPRTN                                                          
         CLI   EZMODE,EZICCOMP     (32) PROCESS INVOICE COMMENT-TOP             
         BE    INVCRTN                                                          
         CLI   EZMODE,EZIBCOMP     (33) PROCESS INVOICE COMMENT-BOTTOM          
         BE    INVBRTN                                                          
         CLI   EZMODE,EZINVL       (34) LAST FOR INVOICE                        
         BE    INVLRTN                                                          
         CLI   EZMODE,EZERRP       PROCESS ERRORS                               
         BE    PROCERR                                                          
         B     EXIT                                                             
*                                                                               
*        PROCESS SPOT DETAIL                                                    
*                                                                               
STPPRTN  DS    0H                                                               
         BRAS  RE,TOPCOM           PRINT TOP STND COMS (IF NECESSARY)           
         BRAS  RE,CLRSPT           CLEAR SPOT LINE SAVES                        
         BRAS  RE,CLRRCN           AND RECONCILIATION REMARKS                   
*                                                                               
         LA    R5,SPTLIN1                                                       
         USING PSPOT,R5                                                         
         CLI   EZSNMED,C'N'        IF NET, TREAT AS SPECIAL                     
         BE    AP020                                                            
*                                                                               
         CLI   EZSNMED,C'C'        IF CABLE, TREAT AS SPECIAL                   
         BE    AP020                                                            
*                                                                               
         CLI   EZSNMED,C'S'        IF SYNDICATION, TREAT AS SPECIAL             
         BE    AP020                                                            
*                                                                               
         CLI   PGDISP,C'Y'                                                      
         BE    AP020                                                            
*                                                                               
         CLI   SRCESTA,C'0'        IS THIS LOCAL CABLE                          
         BL    AP040                                                            
*                                                                               
AP020    DS   0H                                                                
         OC    EZSPNET,EZSPNET                                                  
         BZ    AP030                                                            
*                                                                               
         CLC   EZSPNET,SPACES                                                   
         BE    AP030                                                            
*                                                                               
         MVC   PARATE+L'SPTLIN1-44(4),=C'NET='                                  
         MVC   PARATE+L'SPTLIN1-40(L'EZSPNET),EZSPNET                           
*                                                                               
AP030    DS   0H                                                                
*&&DO                                                                           
         OC    EZSPPRGN,EZSPPRGN                                                
         BZ    AP040                                                            
*                                                                               
         CLC   EZSPPRGN,SPACES                                                  
         BE    AP040                                                            
*                                                                               
         MVC   PARATE+L'SPTLIN1-34(4),=C'PGM='                                  
         MVC   PARATE+L'SPTLIN1-30(23),EZSPPRGN                                 
*&&                                                                             
*                                                                               
AP040    DS   0H                                                                
*&&DO                                                                           
         CLI   EZSPRUN,C'N'                                                     
         BE    AP050                                                            
*                                                                               
         OC    EZSPBINT,EZSPBINT   IS THERE AN INTEGRATION CHGE                 
         BZ    AP050                                                            
*                                                                               
         ICM   R0,15,EZSPBINT                                                   
         CVD   R0,DUB                                                           
         AP    INTTTL,DUB                                                       
*                                                                               
         LA    R2,PARATE+L'SPTLIN1+L'SPTLIN1-6                                  
         EDIT  (B4,EZSPBINT),(14,(R2)),2,COMMAS=YES                             
*                                                                               
         CLI   0(R2),C' '                                                       
         BH    *+12                                                             
         LA    R2,1(,R2)                                                        
         B     *-12                                                             
*                                                                               
         AHI   R2,-12                                                           
*                                                                               
         MVC   0(12,R2),=C'INTEGRATION='                                        
*&&                                                                             
*                                                                               
AP050    DS   0H                                                                
         MVC   PADATE,EZSPDDT                                                   
         OC    PADATE,SPACES                                                    
         MVC   PADAY,EZSPDDAY                                                   
         OC    PADAY,SPACES                                                     
*                                                                               
         CLI   SVERRNO,EZERROMN    WAS THIS AN OUT OF DATE ERROR                
         BNE   AP060                                                            
         MVC   PADATE+1+L'SPTLIN1(3),EZSPDDT+5 SHOW BAD YEAR                    
*                                                                               
         MVI   SVERRNO,0           RESET                                        
*                                                                               
AP060    DS   0H                                                                
         MVC   PATIME,EZSPDTIM                                                  
         OC    PATIME,SPACES                                                    
*                                                                               
AP070    MVC   PATYPE+1(3),EZSPTYP                                              
         OC    PATYPE+1(3),SPACES                                               
         MVC   PACLASS,EZSPCLS                                                  
         OC    PACLASS,SPACES                                                   
         MVC   PAMG,EZSPDMD1       FIRST MAKE GOOD DATE                         
         OC    PAMG,SPACES                                                      
*                                  ***NOTE- MORE IN REMARKS                     
AP090    OC    EZSPPGB,EZSPPGB                                                  
         BZ    AP100                                                            
         MVC   PAPB(3),EZSPPGB                                                  
         MVI   PAPB+3,C'-'                                                      
         MVC   PAPB+4(3),EZSPPGB+3                                              
*                                                                               
AP100    LA    R2,PAPF                                                          
         OC    EZSPCOPY,SPACES                                                  
         CLI   EZSPCOPY,C' '                                                    
         BNH   AP300                                                            
*                                                                               
         MVC   X,SPACES                                                         
         XC    DMCB(24),DMCB                                                    
         LA    RF,EZSPCOPY                                                      
         ST    RF,DMCB                                                          
         MVI   DMCB,L'EZSPCOPY                                                  
         LA    RF,X                                                             
         ST    RF,DMCB+4                                                        
         MVI   DMCB+4,L'PAPF                                                    
         LA    RF,3                                                             
         ST    RF,DMCB+8                                                        
         MVI   DMCB+8,L'PAPF                                                    
         GOTO1 CHOPPER,DMCB                                                     
*                                                                               
         MVC   PAPF,X                                                           
         LA    RF,X+L'PAPF         SEE IF ANY REAL DATA ON 2ND LINE             
         LA    R0,L'PAPF           FIX BIAS FILM BUG                            
         CLI   0(RF),C'A'          ALPHA OR NUMERIC                             
         BNL   *+16                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,*-12                                                          
         B     AP300                                                            
         MVC   PAPF+L'SPTLIN1,X+L'PAPF   2ND LINE                               
         MVI   PAPF+L'SPTLIN1-1,C' ' PUT A SPACE BETWEEN FILM AN PGM=           
*                                                                               
AP300    TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    AP380                YES                                         
*                                                                               
         CLI   EZSPRUN,C'N'        DID SPOT RUN                                 
         BE    AP320                NO                                          
*                                                                               
         ICM   R2,15,EZSPBRAT                                                   
*                                                                               
         ICM   RE,15,EZSPBDR       DEBT CREDIT                                  
         ICM   RF,15,EZSPBCR                                                    
         LR    R0,R2                                                            
         AR    R0,RE                                                            
         AR    R0,RF                                                            
         BZ    AP330               IF ALL NETS TO ZERO, PRINT N/C               
*                                                                               
         LR    R0,R2                                                            
         CVD   R0,DUB                                                           
         AP    ANNPTTL,DUB                                                      
*                                                                               
         C     R2,=F'9999999'      MORE THAN 99K $                              
         BNH   AP310                                                            
         CVD   R2,DUB                                                           
         NI    DUB+6,X'0F'                                                      
         CP    DUB+6(2),=P'0'      PENNIES ZERO                                 
         BNE   AP310                                                            
*                                                                               
         LTR   R2,R2                                                            
         BP    *+12                                                             
         CLI   EZ2MINUS,C'Y'                                                    
         BE    AP305                                                            
*                                                                               
         EDIT  (R2),(L'PARATE+1,ELEM),2                                         
         B     AP307                                                            
*                                                                               
AP305    DS    0H                                                               
         EDIT  (R2),(L'PARATE+1,ELEM),FLOAT=-                                   
*                                                                               
AP307    DS    0H                                                               
         MVC   PARATE,ELEM                                                      
         MVI   PARATE+L'PARATE-1,C'.'                                           
         B     AP320                                                            
*                                                                               
AP310    DS    0H                                                               
         LTR   R2,R2                                                            
         BP    *+12                                                             
         CLI   EZ2MINUS,C'Y'                                                    
         BE    AP312                                                            
*                                                                               
         EDIT  (R2),(L'PARATE,PARATE),2                                         
         B     AP320                                                            
*                                                                               
AP312    DS    0H                                                               
         EDIT  (R2),(L'PARATE,PARATE),2,FLOAT=-                                 
*                                                                               
AP320    ICM   R2,15,EZSPBDR       DEBT CREDIT                                  
         ICM   RF,15,EZSPBCR                                                    
         AR    R2,RF               COMBINE                                      
         BZ    AP380                                                            
         BM    AP340                                                            
         EDIT  (R2),(9,X),2,FLOAT=+                                             
         B     AP360                                                            
*                                                                               
AP330    MVC   PARATE+5(3),=C'N/C'                                              
         B     AP380               BYPASS PRINTING AND TOTALS                   
*                                                                               
AP340    DS    0H                                                               
         EDIT  (R2),(9,X),2,FLOAT=-                                             
*                                                                               
AP360    MVC   PADBCR,X+1                                                       
         CLI   X,C' '                                                           
         BNH   *+10                                                             
         MVC   PADBCR-1(9),X                                                    
         CVB   R2,DUB                                                           
         AP    RECPTTL,DUB                                                      
*                                                                               
AP380    DS    0H                                                               
         CLI   EZSNMED,C'N'        IF NET, TREAT AS SPECIAL                     
         BE    AP382                                                            
         CLI   EZSNMED,C'C'        IF CABLE, TREAT AS SPECIAL                   
         BE    AP382                                                            
         CLI   EZSNMED,C'S'        IF SYNDICATION, TREAT AS SPECIAL             
         BE    AP382                                                            
*                                                                               
         CLI   PGDISP,C'Y'                                                      
         BE    AP382                                                            
*                                                                               
         CLI   SRCESTA,C'0'        IS THIS LOCAL CABLE                          
         BL    AP385                                                            
*                                                                               
AP382    DS    0H                                                               
         OC    EZSPPRGN,EZSPPRGN                                                
         BZ    AP385                                                            
         CLC   EZSPPRGN,SPACES                                                  
         BE    AP385                                                            
*                                                                               
         GOTO1 =A(NEXTLINE),DMCB,PAPGM-PSPOT,27                                 
         BNE   AP385                                                            
*                                                                               
         LA    R1,EZSPPRGN                                                      
         ICM   R1,8,=AL1(L'EZSPPRGN)                                            
         BRAS  RE,FINDLEN                                                       
         BNE   AP385                                                            
*                                                                               
         L     R2,FULL                                                          
         AHI   R2,PAPGM-PSPOT                                                   
         MVC   0(4,R2),=C'PGM='                                                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R2),EZSPPRGN                                                 
*        MVC   4(23,R2),EZSPPRGN                                                
         OC    4(40,R2),SPACES                                                  
*                                                                               
AP385    DS   0H                                                                
         CLI   EZSPRUN,C'N'                                                     
         BE    AP400                                                            
*                                                                               
         OC    EZSPBINT,EZSPBINT   IS THERE AN INTEGRATION CHGE                 
         BZ    AP400                                                            
*                                                                               
         ICM   R0,15,EZSPBINT                                                   
         CVD   R0,DUB                                                           
         AP    INTTTL,DUB                                                       
*                                                                               
         GOTO1 =A(NEXTLINE),DMCB,PAPGM-PSPOT,L'EZSPPRGN                         
         BNE   AP400                                                            
*                                                                               
         L     RF,FULL                                                          
         EDIT  (B4,EZSPBINT),(14,PAINTEG-PSPOT(RF)),2,COMMAS=YES                
*                                                                               
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         LA    RF,1(,RF)                                                        
         B     *-12                                                             
*                                                                               
         AHI   RF,-12                                                           
*                                                                               
         MVC   0(12,RF),=C'INTEGRATION='                                        
*                                                                               
AP400    DS    0H                  ACTUALS                                      
         LA    R1,EZEQVSTA+4                                                    
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    EZMTFLAG-EZMEDTBD(RF),EZMTFDGQ                                   
         BZ    AP410                                                            
*                                                                               
         XC    X,X                                                              
         LA    RF,X                                                             
*                                                                               
         CLC   EZSPVCID,SPACES                                                  
         BNH   AP401                                                            
*                                                                               
         MVC   X(L'EZSPVCID),EZSPVCID                                           
         LA    RF,X+L'EZSPVCID                                                  
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         MVI   1(RF),C'|'                                                       
         LA    RF,2(RF)                                                         
*                                                                               
AP401    DS    0H                                                               
         CLC   EZSPACID,SPACES                                                  
         BNH   *+10                                                             
         MVC   0(L'EZSPACID,RF),EZSPACID                                        
         OC    X,SPACES                                                         
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 SCANNER,DMCB,(C'C',X),(3,WORK),C',=| '                           
         LLC   R3,DMCB+4                                                        
         CHI   R3,0                                                             
         BNE   AP406                                                            
*                                                                               
* INVALID DATA HERE                                                             
*                                                                               
         L     R2,ANXTRCN          REMARKS                                      
         MVC   0(4,R2),=C'ACT:'                                                 
         MVC   4(L'RCNLIN1-4,R2),EZSPVCID                                       
         LA    R2,L'RCNLIN1(R2)                                                 
         MVC   4(L'RCNLIN1-4,R2),EZSPACID                                       
         LA    R2,L'RCNLIN1(R2)                                                 
         ST    R2,ANXTRCN                                                       
         B     AP410                                                            
*                                                                               
AP406    DS    0H                                                               
         LA    R1,WORK                                                          
         USING SCANBLKD,R1                                                      
         L     R2,ANXTRCN                                                       
         MVC   0(4,R2),=C'ACT:'                                                 
*                                                                               
AP407    DS    0H                                                               
         LLC   RF,SC1STLEN                                                      
         LTR   RF,RF               ANYTHING?                                    
         BZ    AP408A              SKIP THIS SCANNER ENTRY                      
*                                                                               
         CHI   RF,L'RCNLIN1-4                                                   
         BNH   *+8                                                              
         LHI   RF,L'RCNLIN1-4                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R2),SC1STFLD                                                 
*                                                                               
AP408    LA    R2,L'RCNLIN1(R2)                                                 
AP408A   LA    R1,SCBLKLQ(R1)                                                   
         BCT   R3,AP407                                                         
*                                                                               
         ST    R2,ANXTRCN                                                       
*                                                                               
AP410    DS    0H                  MORE MAKEGOOD DATA                           
         CLI   EZSPDMD2,C' '       IF HAVE 2ND DATE                             
         BH    AP420                                                            
         CLI   EZSPMGT1,C' '       OR TIMES                                     
         BH    AP420                                                            
         CLI   EZSPMGT2,C' '                                                    
         BNH   APXIT                                                            
*                                                                               
AP420    L     R2,ANXTRCN          REMARKS                                      
         MVC   0(7,R2),=C'M/G FOR'                                              
         MVC   8(5,R2),EZSPDMD1                                                 
         CLI   EZSPDMD2,C' '                                                    
         BNH   AP422                                                            
*                                                                               
         MVI   13(R2),C'-'                                                      
         LA    R2,L'RCNLIN1(R2)    NEXT LINE                                    
         MVC   0(5,R2),EZSPDMD2                                                 
*                                                                               
AP422    DS    0H                                                               
         CLI   EZSPMGT1,C' '       DO WE HAVE TIMES                             
         BH    AP440                                                            
         CLI   EZSPMGT2,C' '                                                    
         BNH   APXIT                                                            
*                                                                               
AP440    LA    R2,L'RCNLIN1(R2)    NEXT LINE                                    
         MVC   0(11,R2),EZSPDMTM   FOR TIMES                                    
*                                                                               
         LA    R2,L'RCNLIN1(R2)                                                 
         ST    R2,ANXTRCN                                                       
*                                                                               
APXIT    B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
*                                                                               
*                                                                               
*        LAST FOR A SPOT                                                        
*                                                                               
STPLRTN  BAS   RE,PRTSPT           DO PRINTING FOR SPOT                         
         B     EXIT                                                             
*                                                                               
*        PROCESS SCHEDULE LINE                                                  
*                                                                               
LINPRTN  DS    0H                                                               
         BRAS  RE,TOPCOM           PRINT TOP STND COMS (IF NECESSARY)           
         BRAS  RE,CLRALL           CLEAR ALL LINE SAVES                         
*                                                                               
         LA    R5,SCHLIN1          FIRST LINE                                   
         USING PSCHED,R5                                                        
         MVC   PDAYS,EZSLDAYS                                                   
*                                                                               
*                                                                               
         CLC   =C'ESGI',EZSNSYS    IS THIS ENTERPRISE                           
         BE    LP10                                                             
*                                                                               
         CLC   =C'JD2',EZSNSYS     IS THIS THE JDS2000                          
         BNE   *+14                                                             
LP10     OC    EZSLBSTM(4),EZSLBSTM ARE BOTH TIMES ZERO                         
         BZ    LP30                                                             
*                                                                               
LP14     MVC   PTIME,EZSLDTIM                                                   
*                                                                               
LP30     MVC   PRLIN,EZSLLIN                                                    
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    LP60                 YES                                         
*                                                                               
         ICM   R2,15,EZSLBRAT                                                   
         BZ    LP40                                                             
*                                                                               
         C     R2,=F'999999'       MORE THAN 9K $                               
         BNH   LP35                NO - EDIT WITH 2 DECIMAL                     
*                                                                               
         CVD   R2,DUB                                                           
         NI    DUB+6,X'0F'                                                      
         CP    DUB+6(2),=P'0'      PENNIES = ZERO?                              
         BNE   LP35                NO - EDIT WITH 2 DECIMAL                     
*                                                                               
         EDIT  (R2),(L'PRATE+1,ELEM)                                            
         MVC   PRATE,ELEM                                                       
         MVI   PRATE+L'PRATE-1,C'.'                                             
         B     LP36                                                             
*                                                                               
LP35     DS    0H                                                               
         EDIT  (R2),(L'PRATE,PRATE),2                                           
*                                                                               
LP36     DS    0H                                                               
         MVC   PNTMS,EZSLNTMS                                                   
*                                                                               
LP40     DS    0H                                                               
         LA    R5,L'SCHLIN1(R5)    NEXT LINE                                    
*                                                                               
         CLI   EZSLRDET,C' '       TEST HAVE RATE DETAIL                        
         BNH   LP60                                                             
         MVC   3(12,R5),=C'RATE DETAIL='                                        
         MVC   16(3,R5),EZSLRDET                                                
         LA    R5,L'SCHLIN1(R5)                                                 
*                                                                               
LP60     DS    0H                                                               
         OC    EZSLSDT(12),EZSLSDT   TEST HAVE LINE DATES                       
         BZ    LP100                                                            
*                                                                               
* VALIDATE DATES TO BE AT LEAST NUMERIC                                         
*                                                                               
         LA    R0,12                                                            
         LA    R1,EZSLSDT                                                       
         CLI   0(R1),C'0'                                                       
         BL    LP100                                                            
         CLI   0(R1),C'9'                                                       
         BH    LP100                                                            
         LA    R1,1(,R1)                                                        
         BCT   R0,*-20                                                          
*                                                                               
         MVC   3(15,R5),=C'SCHEDULE DATES='                                     
*                                                                               
         OC    EZSLSDT,EZSLSDT   TEST HAVE START LINE DATE                      
         BZ    LP070                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,EZSLSDT),(4,19(R5))                               
*                                                                               
LP070    DS    0H                                                               
         MVI   24(R5),C'-'                                                      
*                                                                               
         OC    EZSLEDT,EZSLEDT   TEST HAVE END LINE DATE                        
         BZ    LP080                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(0,EZSLEDT),(4,25(R5))                               
LP080    DS    0H                                                               
         LA    R5,L'SCHLIN1(R5)                                                 
*                                                                               
LP100    DS    0H                                                               
         CLI   EZSLPLAN,C' '                                                    
         BNH   LP110                                                            
         MVC   3(5,R5),=C'PLAN='                                                
         MVI   8(R5),0             BLOCK BOX CHAR                               
         MVC   9(10,R5),EZSLPLAN                                                
         LA    R5,L'SCHLIN1(R5)                                                 
*                                                                               
LP110    DS    0H                                                               
LPXIT    B     EXIT                                                             
         DROP  R5                                                               
*                                                                               
*        PROCESS SCHEDULE COMMENT                                               
*                                                                               
CHPCRTN  DS    0H                                                               
         BAS   RE,PRTSCH           FINISH UP ANY SCHED LINE PRINTING            
         MVC   P1+2(130),EZSCCOM                                                
         BRAS  RE,FIXCOM                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
*        LAST FOR SCHEDULE LINE                                                 
*                                                                               
LINLRTN  DS    0H                                                               
         BAS   RE,PRTSCH           FINISH UP ANY SCHED LINE PRINTING            
         B     EXIT                                                             
*                                                                               
*        PROCESS RECONCILIATION REMARK                                          
*                                                                               
RECPRTN  DS    0H                                                               
         CLC   EZRRREM,SPACES                                                   
         BNH   EXIT                                                             
*                                                                               
         L     RF,ANXTRCN          A(NEXT SLOT)                                 
         LA    R0,RCNLINX          TEST WILL FIT                                
         CR    RF,R0                                                            
         BL    RECP4                                                            
*                                                                               
         AHI   RF,-L'RCNLIN1       NO, BACK UP TO LAST LINE                     
         MVC   0(L'PRREM,RF),SPACES                                             
         MVC   0(12,RF),=C'**OVERFLOW**'                                        
         B     EXIT                                                             
*                                                                               
RECP4    DS    0H                                                               
         XC    WORK,WORK                                                        
         OC    EZRRREM,SPACES                                                   
         GOTO1 CHOPPER,DMCB,(L'EZRRREM,EZRRREM),(L'RCNLIN1,WORK),(0,2)          
         CLI   DMCB+11,0           NUMBER OF LINES USED                         
         JE    *+2                 ZERO = SOMETHING'S WRONG                     
*                                                                               
         CLC   SPTLIN1+PARATE+5-PSPOT(3),=C'N/C'                                
         BNE   RECP6                                                            
*                                                                               
         CLC   =CL20'PKG ',EZRRREM   IS REMARK PACKAGE                          
         BNE   RECP6                  LEAVE N/C                                 
*                                                                               
         MVC   SPTLIN1+PARATE+5-PSPOT(3),SPACES                                 
*                                                                               
RECP6    DS    0H                                                               
         LLC   R0,DMCB+11          NUMBER OF LINES USED BY CHOPPER              
         LA    R1,WORK             SPLIT REMARK LINES                           
*                                                                               
RECP8    DS    0H                                                               
         L     RF,ANXTRCN          A(NEXT SLOT)                                 
         LA    RE,RCNLINX          TEST WILL FIT                                
         CR    RF,RE                                                            
         BL    RECP10                                                           
*                                                                               
         AHI   RF,-L'RCNLIN1       NO, BACK UP TO LAST LINE                     
         MVC   0(L'PRREM,RF),SPACES                                             
         MVC   0(12,RF),=C'**OVERFLOW**'                                        
         B     EXIT                                                             
*                                                                               
RECP10   DS    0H                                                               
         MVC   0(L'RCNLIN1,RF),0(R1) MOVE SPLIT LINE FROM WORK                  
*                                                                               
         LA    R1,L'RCNLIN1(R1)    BUMP WORK POINTER                            
         LA    RF,L'RCNLIN1(RF)                                                 
         ST    RF,ANXTRCN          BUMP SLOT                                    
         BCT   R0,RECP8                                                         
*                                                                               
         B     EXIT                                                             
*                                                                               
*        PROCESS INVOICE HEADER                                                 
*                                                                               
INVPRTN  DS    0H                                                               
* SUPPRESS LCISD'S IM INVOICES FOR SOURCE ISD - 12/18/08                        
         CLC   =X'31C1',EZWKRIND   ZDFPERE?                                     
         BE    IP010                                                            
         CLC   =X'2D98',EZWKRIND   LCISD?                                       
         BE    IP010                                                            
*                                                                               
         B     IP020                                                            
*                                                                               
IP010    DS    0H                                                               
         TM    EZIHCVST,EZIHIMQ    IM INVOICE?                                  
         BZ    IP020                                                            
         CLC   OPTSRCE,SPACES      HAVE SOURCE FILTER?                          
         BH    IP020               YES - SKIP THE ISD SUPPRESS CODE             
         CLC   SVSRCE,=C'ISD '                                                  
         BE    IP100                                                            
*                                                                               
IP020    DS    0H                                                               
         OC    OPTCC,OPTCC                                                      
         BZ    IP050                                                            
         OC    EZIHCVAD,EZIHCVAD   HAVE WE GOT CLT OVERRIDE?                    
         BZ    IP040                                                            
         CLC   OPTCC,EZIHLADC                                                   
         BNE   IP100                                                            
         B     IP050                                                            
*                                                                               
IP040    DS    0H                  NO OVERRIDE                                  
         CLC   OPTCC,EZIHAAID                                                   
         BNE   IP100                                                            
*                                                                               
IP050    DS    0H                                                               
         CLI   SRTOPT,C'Z'         SORTER OUTPUT PHASE?                         
         BNE   IP070                                                            
         CLI   SRTCLT,C'Y'                                                      
         BNE   IP070                                                            
*                                                                               
* SORTING BY CLIENT HERE. SORTER OUTPUT PHASE                                   
*                                                                               
         OC    EZIHCVAD,EZIHCVAD   HAVE WE GOT CLT OVERRIDE?                    
         BZ    IP060                                                            
         LA    R1,EZIHCVAD                                                      
         BRAS  RE,GETCLT                                                        
         L     R1,EZAREC                                                        
         MVC   BYTE,CPROF+6-CLTHDR(R1)                                          
         GOTO1 CLUNPK,DMCB,(BYTE,EZIHCVAD),X                                    
         CLC   SORTCLT,X                                                        
         BNE   IP100                                                            
         B     IP070                                                            
*                                                                               
IP060    DS    0H                  NO OVERRIDE                                  
         CLC   SORTCLT,EZIHAAID                                                 
         BNE   IP100                                                            
*                                                                               
IP070    DS    0H                                                               
         TM    OPTNSW,OPTDEL       REQUESTING DELETED ONLY                      
         BZ    IP120                                                            
         TM    EZIHCVST,EZIHCDEL   THIS A DELETED INVOICE                       
         BO    IP170                YES                                         
*                                                                               
IP100    MVI   EZMODE,EZINVL       SET TO BYPASS INVOICE                        
         B     EPXIT                                                            
*                                                                               
IP120    TM    OPTNSW,OPTDONE      REQUESTING DONE (DEL/CONVERT)                
         BZ    IP130                                                            
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    IP100                                                            
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL                                        
         BZ    IP100                                                            
         B     IP170                                                            
*                                                                               
IP130    TM    OPTNSW,OPTUNCV      ONLY UNCONVERTED                             
         BZ    IP140                                                            
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    IP170                                                            
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL                                        
         BNZ   IP100                                                            
         B     IP170                                                            
*                                                                               
IP140    TM    OPTNSW,OPTCONV       ONLY CONVERTED                              
         BZ    IP160                                                            
*                                                                               
         TM    EZIHCVST,EZIHCVQ                                                 
         BZ    IP100                                                            
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    IP100                                                            
*                                                                               
         OC    SVCONDT,SVCONDT                                                  
         BZ    IP160                                                            
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(3,WORK)                                
         CLC   SVCONDTS,WORK                                                    
         BH    IP100                                                            
         CLC   SVCONDTE,WORK                                                    
         BL    IP100                                                            
         B     IP170                                                            
*                                                                               
IP160    TM    OPTNSW,OPTTDYC      OPTION CONVERTED TODAY                       
         BZ    IP170                                                            
         CLC   EZIHCVDT,OPTODAYC   WAS THIS CONVERTED TODAY                     
         BNE   IP100                                                            
*                                                                               
IP170    OC    OPTMOS,OPTMOS       OPTION MONTH OF SERVICE                      
         BZ    IP200                                                            
         CLC   EZIHDMOS,OPTMOS     WAS THIS REQUESTED MOS                       
         BNE   IP100                                                            
*                                                                               
*                                  SET VARIOUS SWITCHES                         
*                                                                               
IP200    DS    0H                                                               
         MVI   BYPASS,C'N'                                                      
         AP    INVPRT,=P'1'                                                     
*                                                                               
         CLI   SRTOPT,C'Y'         IF SORT INPUT, NO PRINTING                   
         BNE   IP250                                                            
*                                                                               
         CLI   SRTCLT,C'Y'        SORTING BY CLIENT?                            
         BNE   EXIT                NO - JUST EXIT                               
* NOTE - SORTS BY STA,MOS, ETC ARE HANDLED IN LIST MODE                         
*                                                                               
         CLI   SRTOPN,C'Y'         SORT OPENED                                  
         BE    IP230                YES                                         
*                                                                               
* INITIALIZE SORTER                                                             
         MVI   SRTOPN,C'Y'         SET SORT OPENED                              
         BRAS  RE,SETCARDS                                                      
         LM    RE,RF,=A(SORTCARD,RECCARD)                                       
         A     RE,RELO                                                          
         A     RF,RELO                                                          
         GOTO1 SORTER,DMCB,(RE),(RF)                                            
*                                                                               
IP230    DS    0H                                                               
         BRAS  RE,ADDTOSRT                                                      
         B     EXIT                                                             
*                                                                               
IP250    DS    0H                                                               
         MVI   TCOMSW,C'N'         TOP STND COMS NOT DONE                       
         MVI   IBCSW,C'N'          NO INVOICE BOTTOM COMS YET                   
         BRAS  RE,CLRALL           CLEAR ALL LINE SAVES                         
*                                                                               
         MVI   PAGE+1,1                                                         
         MVI   NEWINV,C'N'                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                  SET UP BOXES PARAMETERS *                    
         USING BOXD,R1                                                          
         ICM   R1,15,ABOX          IS ABOX ZEROS                                
         BZ    IP300               YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
*                                                                               
* TURN BOXES OFF                                                                
         TM    OPTNSW,OPTNOBOX                                                  
         BZ    *+12                                                             
         MVI   BOXYORN,C'N'                                                     
         MVI   BOXOFF,C'Y'                                                      
*                                                                               
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
*                                                                               
         LA    R5,BOXCOLS                                                       
         USING PLINED,R5                                                        
         MVI   B1,C'L'                                                          
         MVI   B2,C'C'                                                          
         MVI   B3,C'C'                                                          
         MVI   B4,C'C'                                                          
         MVI   B5,C'C'                                                          
         MVI   B6,C'C'                                                          
         MVI   B7,C'C'                                                          
         MVI   B8,C'C'                                                          
         MVI   B9,C'C'                                                          
         MVI   B10,C'C'                                                         
         MVI   B11,C'C'                                                         
         MVI   B12,C'C'                                                         
         MVI   B13,C'C'                                                         
         MVI   B14,C'C'                                                         
         MVI   B15,C'C'                                                         
         MVI   B16,C'C'                                                         
         MVI   B17,C'C'                                                         
         MVI   B18,C'C'                                                         
         MVI   B19,C'R'                                                         
         DROP  R5                                                               
         MVI   BOXROWS+14,C'T'                                                  
         MVI   BOXROWS+14+3,C'M'                                                
         MVI   BOXROWS+14+44,C'B'                                               
*                                                                               
IP300    DS    0H                                                               
EPXIT    B     EXIT                                                             
*                                                                               
*        PROCESS INVOICE COMMENT (TOP)                                          
*                                                                               
INVCRTN  DS    0H                                                               
         BRAS  RE,TOPCOM           PRINT TOP STND COMS (IF NECESSARY)           
         MVC   P1+1(130),EZICCOM                                                
         BRAS  RE,FIXCOM                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
*                                                                               
*        PROCESS INVOICE COMMENT (BOTTOM)                                       
*                                                                               
INVBRTN  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*        LAST FOR INVOICE                                                       
*                                                                               
INVLRTN  DS    0H                                                               
         BAS   RE,PRTSCH           FINISH UP ANY SCHED PRINTING                 
*                                                                               
         CLI   IBCSW,C'Y'          TEST THIS IS FIRST                           
         BE    IL8                                                              
         GOTO1 SPOOL,DMCB,(R8)     YES, SKIP A LINE                             
         MVI   IBCSW,C'Y'                                                       
*                                                                               
IL8      DS    0H                                                               
         MVC   P1+1(130),EZIBCOM                                                
         BRAS  RE,FIXCOM                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         BAS   RE,BOTCOM           PRINT ANY BOTTOM STND COMMENTS               
*                                                                               
*  SET UP BOXES PARAMETERS *                                                    
*                                                                               
         USING BOXD,R1                                                          
         ICM   R1,15,ABOX          IS ABOX ZEROS                                
         BZ    IL100               YES/ ON-LINE SKIP BOXES                      
         LA    R5,BOXROWS                                                       
         ZIC   RE,LINE                                                          
         AR    R5,RE                                                            
         BCTR  R5,R0                                                            
         MVI   0(R5),C'B'                                                       
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
IL100    DS    0H                                                               
         MVI   ALLOWLIN,11         DONT START WITHOUT 11 LINES                  
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    IL130                YES                                         
*                                                                               
         ICM   R2,15,EZITBSCH                                                   
         BZ    IL110                                                            
*                                                                               
         MVC   P1+09(13),=C'TOTAL ORDERED'                                      
         EDIT  (R2),(9,P1+23),2                                                 
*                                                                               
IL110    MVC   P1+78(11),=C'TOTAL AIRED'                                        
         CP    ANNPTTL,=P'0'                                                    
         BNL   *+12                                                             
         CLI   EZ2MINUS,C'Y'                                                    
         BE    IL111                                                            
*                                                                               
         EDIT  (P8,ANNPTTL),(11,P1+90),2                                        
         B     IL112                                                            
*                                                                               
IL111    DS    0H                                                               
         EDIT  (P8,ANNPTTL),(11,P1+90),2,FLOAT=-                                
*                                                                               
IL112    DS    0H                                                               
         MVC   P1+104(14),=C'RECONCILIATION'                                    
         CP    RECPTTL,=P'0'                                                    
         BNL   IL120                                                            
         EDIT  (P8,RECPTTL),(11,P1+121),2,FLOAT=-                               
         B     IL130                                                            
*                                                                               
IL120    EDIT  (P8,RECPTTL),(11,P1+121),2,FLOAT=+                               
*                                                                               
IL130    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CP    INTTTL,=P'0'                                                     
         BE    IL135                                                            
         MVC   P1+72(17),=C'TOTAL INTEGRATION'                                  
         EDIT  (P8,INTTTL),(11,P1+90),2                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CP    ANNPTTL,=P'0'                                                    
         BE    IL135                                                            
*                                                                               
         ZAP   DUB,ANNPTTL                                                      
         AP    DUB,INTTTL                                                       
         MVC   P1+72(17),=C'TOTAL AIRED + INT'                                  
         EDIT  (P8,DUB),(11,P1+90),2                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
IL135    DS    0H                                                               
         USING BOXD,R1                                                          
         ICM   R1,15,ABOX          IS ABOX ZEROS                                
         BZ    IL150               YES/ ON-LINE SKIP BOXES                      
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXYORN,C'N'                                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
IL150    DS    0H                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    IL180                YES                                         
*                                                                               
         OC    EZITBGST,EZITBGST   IS THERE ANY CANADIAN GST TAX                
         BZ    IL160                                                            
         MVC   P2+44(7),=C'GST TAX'                                             
         ICM   R2,15,EZITBGST                                                   
         EDIT  (R2),(11,P2+52),2,FLOAT=-                                        
*                                                                               
IL160    DS    0H                                                               
         OC    EZITBPST,EZITBPST   IS THERE ANY CANADIAN PST TAX                
         BZ    IL170                                                            
         MVC   P3+44(7),=C'GST TAX'                                             
         MVI   P3+44,C'P'                                                       
*                                                                               
         ICM   R2,15,EZITBPST                                                   
         EDIT  (R2),(11,P3+52),2,FLOAT=-                                        
*                                                                               
IL170    DS    0H                                                               
         MVC   P1+66(17),=C'AGENCY COMMISSION'                                  
         CLI   AGYDISC,C'Y'                                                     
         BNE   *+10                                                             
         MVC   P1+66(17),=C'AGENCY DISCOUNT  '                                  
*                                                                               
         LA    R2,EZBLOCKD                                                      
         AHI   R2,EZITPAGC-EZBLOCKD                                             
         EDIT  (P8,0(R2)),(11,P1+90),2,FLOAT=-                                  
         MVC   P2+66(9),=C'STATE TAX'                                           
         LA    R2,EZBLOCKD                                                      
         AHI   R2,EZITPSTX-EZBLOCKD                                             
         EDIT  (P8,0(R2)),(11,P2+90),2,FLOAT=-                                  
         MVC   P3+66(9),=C'LOCAL TAX'                                           
         LA    R2,EZBLOCKD                                                      
         AHI   R2,EZITPLTX-EZBLOCKD                                             
         EDIT  (P8,0(R2)),(11,P3+90),2,FLOAT=-                                  
         MVC   P4+66(7),=C'NET DUE'                                             
         LA    R2,EZBLOCKD                                                      
         AHI   R2,EZITPDUE-EZBLOCKD                                             
         EDIT  (P8,0(R2)),(11,P4+90),2,FLOAT=-                                  
         B     IL184                                                            
*                                                                               
IL180    MVI   P3,0                FORCE P4 TO PRINT                            
*                                                                               
IL184    MVC   P1+0(12),=C'REP CONTRACT'                                        
         MVC   P1+22(10),EZIHRORD                                               
         MVC   P2+0(16),=C'STATION CONTRACT'                                    
         MVC   P2+22(10),EZIHSORD                                               
*                                                                               
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BZ    IL190                                                            
         MVC   P4+22(13),=C'* CONVERTED *'                                      
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(5,P4+36)                               
         B     *+10                                                             
IL190    MVC   P4+22(15),=C'* UNCONVERTED *'                                    
*                                                                               
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BZ    *+10                                                             
         MVC   P4+40(11),=C'* DELETED *'                                        
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         OC    EZSNGST,EZSNGST     ANY CANADIAN GST REGISTRATION                
         BZ    IL194                NO                                          
         CLC   EZSNGST,SPACES      ANY CANADIAN GST REGISTRATION                
         BE    IL194                NO                                          
*                                                                               
         MVC   P1+2(8),=C'GST REG='                                             
         MVC   P1+11(20),EZSNGST                                                
*                                                                               
IL194    DS   0H                                                                
         OC    EZSNQST,EZSNQST     ANY CANADIAN QST REGISTRATION                
         BZ    IL196                NO                                          
         CLC   EZSNQST,SPACES      ANY CANADIAN QST REGISTRATION                
         BE    IL196                NO                                          
*                                                                               
         MVC   P1+40(8),=C'GST REG='                                            
         MVI   P1+40,C'Q'                                                       
         MVC   P1+49(20),EZSNQST                                                
*                                                                               
IL196    DS   0H                                                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    EZITBPNT,EZITBPNT                                                
         BNZ   IL204                                                            
         OC    EZITBPGR,EZITBPGR                                                
         BZ    IL206                                                            
*                                                                               
IL204    DS    0H                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    IL206                YES                                         
*                                                                               
* SPEC-30121 - JAN/2020 CANADA GOES $NET                                        
*                                                                               
         CLI   SPOTCAN,C'C'                                                     
         BNE   IL205                                                            
         CLC   =C'2001',EZIHMON    JAN/2020                                     
         BH    IL205                                                            
         CLI   EQVMED,C'T'                                                      
         BE    *+12                                                             
         CLI   EQVMED,C'R'                                                      
         BNE   IL205                                                            
         MVC   P1+66(19),=C'PRIOR NET BALANCE  '                                
         ICM   R2,15,EZITBPGR                                                   
         EDIT  (R2),(11,P1+89),2,FLOAT=-                                        
         B     IL206                                                            
*                                                                               
IL205    DS    0H                                                               
         MVC   P1+66(19),=C'PRIOR GROSS BALANCE'                                
         ICM   R2,15,EZITBPGR                                                   
         EDIT  (R2),(11,P1+89),2,FLOAT=-                                        
         MVC   P2+66(17),=C'PRIOR NET BALANCE'                                  
         ICM   R2,15,EZITBPNT                                                   
         EDIT  (R2),(11,P2+89),2,FLOAT=-                                        
*                                                                               
IL206    DS    0H                                                               
         LA    R3,P1                                                            
*                                                                               
         ICM   R2,15,EZIHBSTP                                                   
         BZ    IL208                                                            
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    IL208                YES                                         
*                                                                               
         MVC   0(17,R3),=C'SALES TAX PERCENT'                                   
         EDIT  (R2),(10,22(R3)),3,ALIGN=LEFT,ZERO=BLANK,DROP=3                  
         LA    R3,132(R3)                                                       
*                                                                               
IL208    DS    0H                                                               
         ICM   R2,15,EZIHBAUD                                                   
         BZ    IL210                                                            
*                                                                               
         MVC   0(16,R3),=C'AUDIENCE PERCENT'                                    
         EDIT  (R2),(10,22(R3)),3,ALIGN=LEFT,ZERO=BLANK,DROP=3                  
         LA    R3,132(R3)                                                       
*                                                                               
IL210    DS    0H                                                               
         CLI   EZIHACF,C'Y'                                                     
         BNE   IL212                                                            
*                                                                               
         MVC   0(21,R3),=C'AGENCY COMMISSION=YES'                               
         CLI   AGYDISC,C'Y'                                                     
         BNE   *+10                                                             
         MVC   0(21,R3),=C'AGENCY DISCOUNT=YES  '                               
*                                                                               
IL212    DS    0H                                                               
         CLI   EZIHNPKG,C' '                                                    
         BNH   IL216                                                            
         MVC   30(12,R3),=C'NET PACKAGE='                                       
         MVC   42(3,R3),EZIHNPKG                                                
         OC    42(3,R3),SPACES                                                  
*                                                                               
IL216    DS    0H                                                               
         TMY   EZIHSFL2,EZIHF2NOPKGQ  NO-PACKAGE '#' OVERRIDE?                  
         BZ    *+14                                                             
         MVC   42(3,R3),=C'#  '                                                 
         B     IL220                                                            
*                                                                               
         CLI   EZIHCVNP,0                                                       
         BE    IL220                                                            
         MVC   30(12,R3),=C'NET PACKAGE='                                       
         ZIC   R0,EZIHCVNP                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  42(3,R3),DUB+6(2)                                                
*                                                                               
IL220    DS    0H                                                               
         CLC   0(L'P,R3),SPACES                                                 
         BNH   *+8                                                              
         LA    R3,132(R3)                                                       
*                                                                               
         CLC   EZIHAAID,SPACES                                                  
         BE    IL230                                                            
         OC    EZIHAAID,EZIHAAID                                                
         BNZ   IL240                                                            
*                                                                               
IL230    CLC   EZIHAPID,SPACES                                                  
         BE    IL260                                                            
         OC    EZIHAPID,EZIHAPID                                                
         BZ    IL260                                                            
IL240    MVC   0(21,R3),=C'SPOTPAK CLT/PRD CODES'                               
*                                                                               
         CLI   SPOTNETS,C'N'       THIS NET                                     
         BNE   *+10                                                             
         MVC   0(4,R3),=C' NET'                                                 
*                                                                               
         MVC   23(L'EZIHAAID,R3),EZIHAAID                                       
*                                                                               
         LA    R1,23+L'EZIHAAID(,R3)                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'/'                                                       
         MVC   2(L'EZIHAPID,R1),EZIHAPID                                        
*                                                                               
         LA    R1,1+L'EZIHAPID(,R1)                                             
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         CLC   EZIHEST,SPACES                                                   
         BNH   IL250                                                            
*                                                                               
         MVI   1(R1),C'-'                                                       
         MVC   2(L'EZIHEST,R1),EZIHEST                                          
*                                                                               
         LA    R1,2+L'EZIHEST(,R1)                                              
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
IL250    DS   0H                                                                
         MVC   3(12,R1),=C'FROM STATION'                                        
         OC    0(L'P,R3),SPACES                                                 
*                                                                               
IL260    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R3,P1                                                            
         TM    EZIHCVST,EZIHCOVR   CLIENT PRODUCT OVERRIDE                      
         BZ    IL280                                                            
         MVC   0(21,R3),=C'SPOTPAK CLT/PRD CODES'                               
*                                                                               
         CLI   SPOTNETS,C'N'       THIS NET                                     
         BNE   *+10                                                             
         MVC   0(4,R3),=C' NET'                                                 
*                                                                               
         MVC   23(L'EZIHLADC,R3),EZIHLADC                                       
         LA    R1,26(,R3)                                                       
         CLI   25(R3),C' '                                                      
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'/'          CLIENT PRODUCT SEPARATOR                     
*                                                                               
         MVC   1(L'EZIHLPRC,R1),EZIHLPRC                                        
         LA    R1,3(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         OC    EZIHLP2C,EZIHLP2C                                                
         BZ    IL264                                                            
         MVI   0(R1),C','           PRODUCT PRODUCT SEPARATOR                   
         MVC   1(L'EZIHLP2C,R1),EZIHLP2C                                        
         LA    R1,3(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
*                                                                               
IL264    CLI   EZIHCVES,0                                                       
         BE    IL266                                                            
         ZIC   R0,EZIHCVES                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R1),C'-'           PRODUCT ESTIMATE SEPARATOR                  
         UNPK  1(3,R1),DUB                                                      
         LA    R1,4(,R1)                                                        
IL266    MVC   3(8,R1),=C'OVERRIDE'                                             
*                                                                               
IL280    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   P1(10),=C'BATCH DATE'                                            
         GOTO1 DATCON,DMCB,(3,EZBTBRDT),(5,P1+23)                               
*                                                                               
         MVC   P1+34(8),=C'SOURCE ='                                            
*                                                                               
         MVC   P1+43(4),SVSRCE                                                  
*                                                                               
         OC    EZSNSYS,SPACES                                                   
         MVC   P1+50(L'EZSNSYS),EZSNSYS                                         
*                                                                               
         CLC   SRCESTA,EQUISTA5    THIS AN EQUIVALENT STATION                   
         BE    IL300                                                            
*                                                                               
         MVC   P2+39(10),=C'EQUIVALENT'                                         
         MVC   P2+50(7),PRTSTA7C                                                
*                                                                               
IL300    MVC   P1+66(15),=C'NUMBER OF SPOTS'                                    
         ICM   R2,15,EZIHTSPN                                                   
         EDIT  (R2),(5,P1+95)                                                   
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWINV,C'Y'         NEW INVOICE NEXT                             
*                                                                               
         BRAS  RE,CLRALL                                                        
         LA    R1,ALLPTTL                                                       
         LHI   R0,ALLPTTN                                                       
         BRAS  RE,ZAPTOT                                                        
*                                                                               
ILXIT    DS    0H                                                               
         AP    SORTUSED,=P'1'                                                   
         B     EXIT                                                             
*                                                                               
*        PROCERR - SAVE ERROR CODES PASSED BY EZMOD                             
*                                                                               
PROCERR  DS    0H                                                               
         CLI   EZERRCD,EZERROMN    THIS AN OUT OF MONTH ERROR                   
         BNE   EXIT                                                             
         MVI   SVERRNO,EZERROMN    SAVE IT                                      
         B     EXIT                                                             
*                                                                               
*        BOTCOM - PRINT BOTTOM STANDARD COMMENTS                                
*                                                                               
BOTCOM   DS    0H                                                               
*        CLI   EZSBCOM,C' '        SKIP IF NONE                                 
         LA    RF,SYSD                                                          
         LHI   R0,BCOMSV-SYSD                                                   
         AR    RF,R0                                                            
         CLI   0(RF),C' '          SKIP IF NONE                                 
         BNHR  RE                                                               
*                                                                               
BOTC     NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)     SKIP A LINE FIRST                            
*                                                                               
*        LA    R2,EZSBCOM          FIRST ONE                                    
         LA    R2,SYSD                                                          
         LHI   RF,BCOMSV-SYSD                                                   
         AR    R2,RF                                                            
         LA    R3,EZSBCMN          COUNT                                        
*                                                                               
BC4      DS    0H                                                               
         CLI   0(R2),C' '          FIRST BLANK ONE IS END                       
         BNH   BCX                                                              
         MVC   P1+1(L'EZSBCOM),0(R2)                                            
         BRAS  RE,FIXCOM                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,L'EZSBCOM(R2)                                                 
         BCT   R3,BC4                                                           
*                                                                               
BCX      DS    0H                                                               
         B     EXIT                                                             
*                                                                               
*        PRTSPT - DO PRINTING FOR SPOT DETAIL                                   
*                                                                               
PRTSPT   DS    0H                                                               
         LR    R0,RE                                                            
         MVC   P1,SPACES                                                        
         LA    R1,RCNLIN1                                                       
         ST    R1,ANXTRCN                                                       
*                                                                               
PRTSPT2  DS    0H                                                               
         MVI   NEXTFLAG,C'N'                                                    
*                                                                               
         BAS   RE,NXTSPT           LOOK FOR SOMETHING TO PRINT                  
         BNE   *+8                                                              
         MVI   NEXTFLAG,C'Y'                                                    
*                                                                               
         BAS   RE,NXTRCN                                                        
         BNE   *+8                                                              
         MVI   NEXTFLAG,C'Y'                                                    
*                                                                               
         CLI   NEXTFLAG,C'Y'                                                    
         BNE   PRTSPT4                                                          
*                                                                               
         CLC   P1,SPACES           ANYTHING TO PRINT                            
         BE    PRTSPT2                                                          
         BAS   RE,NXTSCH                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRTSPT2                                                          
*                                                                               
PRTSPT4  DS    0H                                                               
         BRAS  RE,CLRSPT                                                        
         BRAS  RE,CLRRCN                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*        NXTSPT - GET NEXT SPOT LINE                                            
*                                                                               
NXTSPT   DS    0H                                                               
         L     R1,ANXTSPT                                                       
         LA    RF,SPTLINX                                                       
         CR    R1,RF                                                            
         BL    *+8                                                              
         LTR   RB,RB               UNEQ COND CODE                               
         BR    RE                                                               
*                                                                               
         MVC   P1+PSPOT-PLINED(L'PSPOT),0(R1)                                   
         LA    R1,L'SPTLIN1(R1)                                                 
         ST    R1,ANXTSPT                                                       
         CR    RB,RB               EQ CONDITION CODE                            
         BR    RE                                                               
*                                                                               
*                                                                               
*                                                                               
*        NXTRCN - GET NEXT RECONCILIATION REMARK                                
*                                                                               
NXTRCN   DS    0H                                                               
         L     R1,ANXTRCN                                                       
         LA    RF,RCNLINX                                                       
         CR    R1,RF                                                            
         BL    *+8                                                              
         LTR   RB,RB               UNEQ COND CODE                               
         BR    RE                                                               
*                                                                               
         MVC   P1+PRREM-PLINED(L'PRREM),0(R1)                                   
         LA    R1,L'RCNLIN1(R1)                                                 
         ST    R1,ANXTRCN                                                       
         CR    RB,RB               EQ CONDITION CODE                            
         BR    RE                                                               
*                                                                               
*        PRTSCH - FINISH UP ANY PRINTING FOR SCHED                              
*                                                                               
PRTSCH   DS    0H                                                               
         LR    R0,RE                                                            
         MVC   P1,SPACES                                                        
*                                                                               
PRTSCH2  DS    0H                                                               
         BAS   RE,NXTSCH           LOOK FOR SOMETHING TO PRINT                  
         CLC   P1,SPACES           ANYTHING TO PRINT                            
         BE    PRTSCH4             NO, DONE- DONT BOTHER WITH SCHED             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PRTSCH2                                                          
*                                                                               
PRTSCH4  DS    0H                                                               
         BRAS  RE,CLRSCH                                                        
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*        NXTSCH - GET NEXT SCHEDULE LINE                                        
*                                                                               
NXTSCH   DS    0H                                                               
         L     R1,ANXTSCH                                                       
         LA    RF,SCHLINX                                                       
         CR    R1,RF                                                            
         BNLR  RE                                                               
         CLC   0(L'SCHLIN1,R1),SPACES                                           
         BER   RE                                                               
         MVC   P1+PSCHED-PLINED(L'PSCHED),0(R1)                                 
         LA    R1,L'SCHLIN1(R1)                                                 
         ST    R1,ANXTSCH                                                       
         BR    RE                                                               
*                                                                               
NOTNUMER MVI   ERROR,NOTNUM                                                     
         B     TRAPERR                                                          
*                                                                               
BADATE   MVI   ERROR,INVDATE                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
NUMLINS  EQU   16                                                               
         DC    AL1(L'OPTHLPMS-1)                                                
OPTHLPMS DC    C'OPTIONS=CONV/DELETE/DONE/MOS/NOCOST/SORT/SOURCE/UNCONVC        
               '                                                                
         DC    AL1(L'TOOMANMS-1)                                                
TOOMANMS DC    C'* ERROR * ENTER ONLY ONE OF DONE, DELETE, CONV, UNCONVC        
                *'                                                              
         DC    AL1(L'TOOBIGMS-1)                                                
TOOBIGMS DC    C'* ERROR * ENTRY IS TOO LARGE *'                                
         DC    AL1(L'OPTSRTMS-1)                                                
OPTSRTMS DC    C'* ERROR * SORT=MKT/STATION *'                                  
         DC    AL1(L'SORTERMS-1)                                                
SORTERMS DC    C'* ERROR * SORT ONLY ALLOWED SOON, OV, OR DDS *'                
         DC    AL1(L'MOSERMS-1)                                                 
MOSERMS  DC    C'* ERROR * ENTER MOS MON/YR OR MONYR *'                         
         DC    AL1(L'MKTERMS-1)                                                 
MKTERMS  DC    C'* ERROR * INVALID MARKET *'                                    
         DC    AL1(L'INVMEDMS-1)                                                
INVMEDMS DC    C'* ERROR * ENTER MEDIA=T/R/N/X ONLY *'                          
         DC    AL1(L'NOMEDMS-1)                                                 
NOMEDMS  DC    C'* ERROR * MUST ENTER MEDIA FIRST *'                            
         DC    AL1(L'MGRERMS-1)                                                 
MGRERMS  DC    C'* ERROR * ENTER MGR XNNNN *'                                   
         DC    AL1(L'NOMGRPMS-1)                                                
NOMGRPMS DC    C'* ERROR * MRGP NOT FOUND *'                                    
         DC    AL1(L'NOTNOWMS-1)                                                
NOTNOWMS DC    C'* ERROR * CAN''T RUN "NOW" WHEN SORTING BY CLIENT *'           
*                                                                               
*                   1---5----10---15---20---25---30---35---40                   
SORTCARD DC    CL80'SORT FIELDS=(1,XX,A),FORMAT=BI,WORK=1 '                     
         ORG   SORTCARD                                                         
         DS    CL15                                                             
SRTCDLEN DS    CL2                                                              
         ORG                                                                    
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=XX '                                   
         ORG   RECCARD                                                          
         DS    CL21                                                             
RECCDLEN DS    CL2                                                              
         ORG                                                                    
         DROP  R6,RB,RC                                                         
*                                                                               
* VALIDATE OPTIONS                                                              
*                                                                               
VOPT     NMOD1 0,**VOPT**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         CLI   8(R2),C'?'          HELP OPTION                                  
         BE    VKHLP                                                            
*                                                                               
         MVI   OPTNKP,C'N'                                                      
         MVI   OPTEXP,C'N'         DO NOT RETURN EXPIRED(DEFAULT)               
*                                                                               
         MVI   BYTE,0                                                           
         LA    R4,BLOCK                                                         
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(20,(R2)),(8,(R4)),0                                
         CLI   4(R1),0                                                          
         BE    TOOBIGER                                                         
*                                                                               
VOPT100  CLI   0(R4),0             END                                          
         BE    VOPTX                                                            
*                                                                               
         LLC   RE,BYTE                                                          
         AHI   RE,1                                                             
         STC   RE,BYTE                                                          
*                                                                               
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
*                                                                               
* GET ADDRESS OF OPTION VALIDATION RTN                                          
*                                                                               
         LA    RF,OPTABLE                                                       
         EX    R1,VOPTCLC                                                       
         BE    VOPTGO                                                           
         LA    RF,L'OPTABLE(,RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VKERR                                                            
*                                                                               
VOPTGO   L     RE,8(,RF)                                                        
         A     RE,RELO                                                          
         BR    RE                                                               
VOPTCLC  CLC   12(0,R4),0(RF)                                                   
VOPTCARD DS    CL80                                                             
*                                                                               
*                                  TRACE OPTION                                 
*                                                                               
VOPTTR   CLI   OFFLINE,C'Y'        IF OFFLINE, OK                               
         BE    *+12                                                             
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   VKERR                                                            
         MVC   TRCOPT,22(R4)                                                    
         B     VOPT200                                                          
*                                                                               
*                                  SORT OPTION                                  
*                                                                               
VOPTSRT  TM    WHEN,X'38'          ONLY SORT SOON, OV, DDS                      
         BZ    SORTERR                                                          
         MVI   SRTOPT,C'Y'                                                      
         MVI   SRTTYP,C'S'                                                      
         CLI   1(R4),0             ANY FOLLOWING = PARM                         
         BE    VOPT200              NO                                          
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VOPTSRTA         STATION                                      
         BE    VOPT200                                                          
         EX    R1,VOPTSRTB         MARKET                                       
         BE    VOPTSRT2                                                         
         EX    R1,VOPTSRTC         MKT                                          
         BNE   OPTSRTER             WHAT?                                       
VOPTSRT2 MVI   SRTTYP,C'M'                                                      
         B     VOPT200                                                          
VOPTSRTA CLC   22(0,R4),=C'STATION '                                            
VOPTSRTB CLC   22(0,R4),=C'MARKET '                                             
VOPTSRTC CLC   22(0,R4),=C'MKT '                                                
*                                                                               
*                                  DONE - CONVERTED + DELETED                   
*                                                                               
VOPTDN   TM    OPTNSW,OPTCONV+OPTDEL+OPTUNCV                                    
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTDONE                                                   
         B     VOPT200                                                          
*                                                                               
* NOTKEEP - ONLY BATHCES NOT KEEP                                               
*                                                                               
VOPTNKP  MVI   OPTNKP,C'Y'                                                      
         B     VOPT200                                                          
*                                                                               
*                                  CONVERTED BATCHES                            
*                                                                               
VOPTCN   TM    OPTNSW,OPTDONE+OPTDEL+OPTUNCV                                    
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTCONV                                                   
         XC    SVCONDT,SVCONDT                                                  
*                                                                               
         CLI   1(R4),0             ANY FOLLOWING = PARM                         
         BE    VOPT200              NO                                          
*                                                                               
         GOTO1 DATVAL,DMCB,(0,22(R4)),WORK                                      
         OC    DMCB(4),DMCB                                                     
         BZ    VKERR                                                            
         MVC   FULL,DMCB                                                        
         GOTO1 DATCON,DMCB,(0,WORK),(3,SVCONDTS)                                
         L     RF,FULL             LENGTH OF VALID DATE                         
         LA    RF,22(RF,R4)        ADVANCE PAST THE DATE                        
         CLI   0(RF),C'-'                                                       
         BNE   VKERR                                                            
         LA    RF,1(RF)                                                         
         GOTO1 DATVAL,DMCB,(0,0(RF)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    VKERR                                                            
         GOTO1 DATCON,DMCB,(0,WORK),(3,SVCONDTE)                                
         CLC   SVCONDTS,SVCONDTE                                                
         BH    VKERR                                                            
         B     VOPT200                                                          
*                                                                               
*                                  DELETED BATCHES                              
*                                                                               
VOPTDL   TM    OPTNSW,OPTDONE+OPTCONV+OPTUNCV                                   
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTDEL                                                    
         B     VOPT200                                                          
*                                                                               
*                                  MEDIA REQUEST                                
*                                                                               
VOPTMED  DS   0H                                                                
         CLI   22(R4),C'T'          TV                                          
         BE    VOPTMD10                                                         
         CLI   22(R4),C'R'          RADIO                                       
         BE    VOPTMD10                                                         
         CLI   22(R4),C'N'          NETWORK                                     
         BE    VOPTMD10                                                         
         CLI   22(R4),C'X'          NETWORK RADIO                               
         BNE   INVMEDER                                                         
VOPTMD10 MVC   OPTMEDIA,22(R4)                                                  
*                                                                               
* SET MEDIA STRAIGHT                                                            
*                                                                               
         NI    BAGYMD,X'F0'                                                     
         CLI   OPTMEDIA,C'T'                                                    
         BNE   VOPTMD14                                                         
         OI    BAGYMD,01                                                        
         B     VOPTMDX                                                          
*                                                                               
VOPTMD14 CLI   OPTMEDIA,C'R'                                                    
         BNE   VOPTMD20                                                         
         OI    BAGYMD,02                                                        
         B     VOPTMDX                                                          
*                                                                               
VOPTMD20 CLI   OPTMEDIA,C'N'                                                    
         BNE   VOPTMD40                                                         
         OI    BAGYMD,03                                                        
         B     VOPTMDX                                                          
*                                                                               
VOPTMD40 CLI   OPTMEDIA,C'X'                                                    
         BNE   VOPTMD50                                                         
         OI    BAGYMD,04                                                        
*                                                                               
VOPTMDX  DS    0H                                                               
         MVC   QMED,OPTMEDIA                                                    
         B     VOPT200                                                          
*                                                                               
VOPTMD50 DC    H'0'                                                             
         DC    CL10'BAD MEDIA'                                                  
*                                                                               
*                                  MARKET GROUP REQUEST                         
*                                                                               
VOPTMGR  DS   0H                                                                
         CLI   OPTMEDIA,0                                                       
         BE    NOMEDER                                                          
         CLI   22(R4),C'A'                                                      
         BL    MGRENTER                                                         
         CLI   22(R4),C'Z'                                                      
         BH    MGRENTER                                                         
*                                                                               
         MVC   OPTMGR,22(R4)                                                    
*                                                                               
         CLI   1(R4),1                                                          
         BNE   VOPTMGR2                                                         
         MVI   SRTTYP,C'G'                                                      
         MVI   SRTOPT,C'Y'                                                      
         B     VOPT200                                                          
*                                                                               
VOPTMGR2 CLI   1(R4),2                                                          
         BL    MGRENTER                                                         
         CLI   1(R4),5                                                          
         BH    MGRENTER                                                         
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                SUBTRACT 1 FOR 1ST LETTER                    
         LA    R1,23(,R4)                                                       
         LA    RE,DUB                                                           
         MVI   DUB,C'0'                                                         
         MVC   DUB+1(4),DUB                                                     
OPTMGR10 CLI   0(R1),C'0'                                                       
         BL    MGRENTER                                                         
         CLI   0(R1),C'9'                                                       
         BH    MGRENTER                                                         
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,OPTMGR10                                                      
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   OPTMGRP(2),WORK                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),BAGYMD    NO BCLT                                       
         MVC   KEY+8(1),22(R4)                                                  
         MVC   KEY+9(2),OPTMGRP                                                 
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         OC    KEY+5(3),KEY+5      FIND PRODUCT GROUP                           
         BZ    OPTMGR30                                                         
         CLC   KEY(5),KEYSAVE                                                   
         BE    OPTMGR20                                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   NOMGRPER                                                         
*                                                                               
OPTMGR20 CLC   KEY+8(1),22(R4)                                                  
         BNE   OPTMGR24                                                         
         CLC   KEY+9(2),OPTMGRP                                                 
         BE    OPTMGR30                                                         
OPTMGR24 MVC   KEY+8(1),22(R4)                                                  
         MVC   KEY+9(2),OPTMGRP                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
OPTMGR30 CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    OPTMGR40                                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
OPTMGR40 CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   NOMGRPER                                                         
*                                                                               
         LA    R3,MKTABLN                                                       
         L     R4,AMKTAB                                                        
*        XC    MKTAB,MKTAB                                                      
         XCEF  MKTAB,'MKTABLN'                                                  
*                                                                               
OPTMGR50 MVC   0(2,R4),KEY+11                                                   
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   OPTMGR60                                                         
         LA    R4,2(,R4)                                                        
         BCT   R3,OPTMGR50                                                      
         DC    H'0'                                                             
OPTMGR60 XC    FILENAME,FILENAME                                                
*                                                                               
         B     VOPT200                                                          
*                                                                               
*                                  MARKET REQUEST                               
*                                                                               
VOPTMKT  DS   0H                                                                
         CLI   OPTMEDIA,0                                                       
         BE    NOMEDER                                                          
         CLI   1(R4),4             BAD MARKET IF MORE THAN 4 DIGITS             
         BH    MKTERR                                                           
         ZIC   RE,1(R4)            GET LENGTH                                   
         BCTR  RE,0                                                             
*                                                                               
         MVC   WORK(4),=C'0000'                                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVN   WORK(0),22(R4)                                                   
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),22(R4)                                                   
         BNE   BADNUMER                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,22(0,R4) *EXECUTED*                                          
*                                                                               
         CVB   R0,DUB                                                           
         STCM  R0,3,OPTMKT                                                      
*                                                                               
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB            SET FOR USER TOO                             
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO1                     
*                                                                               
         CLI   8(R1),0             ANY ERROR                                    
         BNE   MKTERR                                                           
         B     VOPT200                                                          
*                                                                               
*                                  MOS/MONTH OF SERVICE                         
*                                                                               
VOPTMOS  DS   0H                                                                
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   MOSERR               YES, ERROR                                  
*                                                                               
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    MOSERR               NO, ERROR                                   
         GOTO1 DATCON,(R1),(0,WORK),(6,OPTMOS)                                  
         GOTO1 (RF),(R1),(0,WORK),(X'21',FULL)                                  
         MVC   OPTBMOS,FULL                                                     
*        NI    OPTBMOS,X'0F'                                                    
*                                                                               
         B     VOPT200                                                          
*                                                                               
*                                  UNCONVERTED BATCHES                          
*                                                                               
VOPTUC   TM    OPTNSW,OPTDEL+OPTCONV+OPTUNCV                                    
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTUNCV                                                   
         B     VOPT200                                                          
*                                                                               
*                                  SHOW ALL COST/RATES AS ZERO                  
*                                                                               
VOPTCS   OI    OPTNSW,OPTNOCS                                                   
         B     VOPT200                                                          
*                                                                               
*                                  SHOW ALL SOURCE XXXX                         
*                                                                               
VOPTSRCE MVC   OPTSRCE,22(R4)                                                   
         OC    OPTSRCE,SPACES                                                   
         B     VOPT200                                                          
*                                                                               
*                                  EXPIRED                                      
*                                                                               
VOPTEXP  MVI   OPTEXP,C'Y'                                                      
         B     VOPT200                                                          
*                                                                               
*                                  LOADED TODAY                                 
*                                                                               
VOPTTDY  OI    OPTNSW,OPTTDYL                                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,OPTODAYL)                              
         B     VOPT200                                                          
*                                                                               
*                                  CONVERTED TODAY                              
*                                                                               
VOPTDC   OI    OPTNSW,OPTTDYC                                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(1,OPTODAYC)                              
*                                                                               
         CLC   AGENCY,=C'JW'       IF JWT                                       
         BNE   VOPT200                                                          
*                                                                               
* READ Z5 PROFILE FOR MARKET GROUP                                              
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0Z5'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+16,DATAMGR                                
*                                                                               
         CLI   WORK+16+1,C'*'      IF NONE, BYPASS                              
         BE    VOPT200                                                          
*                                                                               
         MVC   OPTMGR,WORK+16+1                                                 
         MVI   SRTTYP,C'G'                                                      
         MVI   SRTOPT,C'Y'                                                      
         B     VOPT200                                                          
*                                  DONE - CONVERTED + DELETED                   
VOPTBOX  DS    0H                                                               
         OI    OPTNSW,OPTNOBOX                                                  
         B     VOPT200                                                          
*                                                                               
VOPTCC   DS    0H                                                               
         MVC   OPTCC,22(R4)                                                     
         B     VOPT200                                                          
*                                                                               
*                                                                               
* UP ERROR FIELD & NEXT BLOCK                                                   
VOPT200  DS    0H                                                               
         LA    R4,42(,R4)                                                       
         B     VOPT100                                                          
*                                                                               
VOPTX    DS    0H                                                               
         TM    OPTNSW,OPTTDYC                                                   
         JZ    EXIT                                                             
         OC    SVCONDT,SVCONDT                                                  
         BNZ   VKERR                                                            
                                                                                
         J     EXIT                                                             
*                                                                               
TOOBIGER L     R1,=A(TOOBIGMS)                                                  
         B     MYERRA                                                           
*                                                                               
SORTERR  L     R1,=A(SORTERMS)                                                  
         B     MYERRA                                                           
*                                                                               
INVMEDER L     R1,=A(INVMEDMS)                                                  
         B     MYERRA                                                           
*                                                                               
NOMEDER  L     R1,=A(NOMEDMS)                                                   
         B     MYERRA                                                           
*                                                                               
MKTERR   DS   0H                                                                
         L     R1,=A(MKTERMS)                                                   
         B     MYERRA                                                           
*                                                                               
VKHLP    L     R1,=A(OPTHLPMS)                                                  
         B     MYERRA                                                           
*                                                                               
MOSERR   L     R1,=A(MOSERMS)                                                   
         B     MYERRA                                                           
*                                                                               
MGRENTER L     R1,=A(MGRERMS)                                                   
         B     MYERRA                                                           
*                                                                               
NOMGRPER L     R1,=A(NOMGRPMS)                                                  
         B     MYERRA                                                           
*                                                                               
OPTSRTER L     R1,=A(OPTSRTMS)                                                  
         B     MYERRA                                                           
*                                                                               
TOOMANER L     R1,=A(TOOMANMS)                                                  
*                                                                               
MYERRA   LA    R2,IRPOPTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         A     R1,RELO                                                          
         BCTR  R1,0                                                             
         ZIC   RE,0(R1)                                                         
         EX    RE,MYERRMV                                                       
         MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
MYERRMV  MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
VKERR    OI    BYTE,X'F0'                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTERRMS),OPTERRMS                                     
         MVC   CONHEAD+16(1),BYTE                                               
         LA    R2,IRPOPTH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
BADNUMER MVI   ERROR,NOTNUM                                                     
         GOTO1 ERREX                                                            
*                                                                               
OPTERRMS DC    C'* ERROR * FIELD X INVALID OPTION *'                            
*                                                                               
         DS    0D                                                               
OPTABLE  DS    CL12                                                             
         DC    CL8'HELP    ',A(VKHLP)                                           
         DC    CL8'CONV    ',A(VOPTCN)                                          
         DC    CL8'DELETED ',A(VOPTDL)                                          
         DC    CL8'DONE    ',A(VOPTDN)   CONVERTED/DELETED                      
         DC    CL8'NOTKEEP ',A(VOPTNKP)  ONLY BATCHES NOT MARKED KEEP           
         DC    CL8'MARKET  ',A(VOPTMKT)                                         
         DC    CL8'MEDIA   ',A(VOPTMED)                                         
         DC    CL8'MGRP    ',A(VOPTMGR)                                         
         DC    CL8'MKT     ',A(VOPTMKT)                                         
         DC    CL8'MONTH   ',A(VOPTMOS)                                         
         DC    CL8'MOS     ',A(VOPTMOS)                                         
         DC    CL8'NOCOST  ',A(VOPTCS)                                          
         DC    CL8'SORT    ',A(VOPTSRT)                                         
         DC    CL8'SOURCE  ',A(VOPTSRCE)                                        
         DC    CL8'TODAY   ',A(VOPTTDY)   LOADED TODAY                          
         DC    CL8'TODAYC  ',A(VOPTDC)    CONVERTED TODAY                       
         DC    CL8'TRACE   ',A(VOPTTR)                                          
         DC    CL8'UNCONV  ',A(VOPTUC)                                          
         DC    CL8'NOBOXES ',A(VOPTBOX)                                         
         DC    CL8'CC      ',A(VOPTCC)                                          
         DC    CL8'EXPIRED ',A(VOPTEXP)                                         
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
         DROP  RB,RC                                                            
*                                                                               
* FILTER ON MARKET *                                                            
*                                                                               
FTRMKT   NMOD1 0,**FMKT**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
*                                                                               
         LA    R0,STTABL/STTABENT                                               
         L     R1,ASTTABC                                                       
FTRMKT10 OC    0(STTABENT,R1),0(R1)  EMPTY ENTRY                                
         BZ    FTRMKT30               YES                                       
*                                                                               
         CLC   WORK(5),0(R1)   THIS THIS STATION                                
         BE    FTRMKT20                                                         
         LA    R1,STTABENT(,R1)                                                 
         BCT   R0,FTRMKT10                                                      
         LA    R1,STTABL                                                        
         L     R0,ASTTABC                                                       
         SR    RF,RF                                                            
         MVCL  R0,RE               * NOTE - RE NOT REALLY CHANGED               
         B     FTRMKT30                                                         
*                                                                               
FTRMKT20 CLC   OPTMKT,5(R1)                                                     
         J     EXIT                                                             
*                                                                               
FTRMKT30 BRAS  RE,FMTSTA                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
*                                                                               
         CLI   8(R1),0             ANY ERROR                                    
         BE    FTRMKTTP             NO                                          
*                                                                               
         LTR   RB,RB                                                            
         J     EXIT                                                             
*                                                                               
FTRMKTTP DS    0H                                                               
         L     RF,AIO3                                                          
         MVC   FULL,SMKT-STARECD(RF)                                            
*                                                                               
FTRMKT50 LA    R0,STTABL/STTABENT                                               
         L     R1,ASTTABC                                                       
FTRMKT54 OC    0(STTABENT,R1),0(R1)  EMPTY ENTRY                                
         BZ    FTRMKT60               YES                                       
         LA    R1,STTABENT(,R1)                                                 
         BCT   R0,FTRMKT54                                                      
         DC    H'0'                                                             
*                                                                               
FTRMKT60 MVC   0(5,R1),WORK                                                     
         PACK  DUB,FULL                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,5(R1)                                                       
*                                                                               
         B     FTRMKT20                                                         
         DROP  RB,RC                                                            
*                                                                               
*        FORMAT STATION CALL LETTERS                                            
*                                                                               
FMTSTA   NMOD1 0,**FMTS**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
*                                                                               
         LA    R1,WORK+4                                                        
         ICM   R1,8,=AL1(EZMTBNDQ)                                              
         GOTO1 VGETMED                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   KEY+1(1),EZMTMED-EZMEDTBD(RF)                                    
*                                                                               
         MVC   KEY+2(4),WORK                                                    
         OI    KEY+5,X'40'         FORCE BLANK IF BINARY ZERO                   
*                                                                               
         CLI   KEY+5,X'4B'         SEE IF PERIOD                                
         BNE   *+8                                                              
         MVI   KEY+5,X'40'         FORCE BLANK IF PERIOD                        
*                                                                               
         MVC   KEY+6(1),EZMTPMED-EZMEDTBD(RF)                                   
         MVC   KEY+7(2),AGENCY                                                  
         J     EXIT                                                             
         DROP  RB,RC                                                            
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *               
*        HDHK - HEADHOOK ROUTINE                                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVRC                                                          
         USING GEND,RC                                                          
         MVC   H1+0(7),=C'AGENCY-'                                              
         MVC   H1+8(30),EZAGNAM                                                 
*                                                                               
         MVC   H1+41(8),=C'STATION-'                                            
         GOTO1 VPRTSTA,DMCB,SRCESTA,H1+50                                       
*                                                                               
         CLI   SRCESTA,C'0'        IS THIS LOCAL CABLE                          
         BL    HDHK20               NO                                          
*                                                                               
         MVC   H2+40(7),H1+50      MOVE STATION                                 
         MVC   H1+50(L'EZSNNAME),EZSNNAME                                       
*                                                                               
         CLC   EZIHNET,SPACES      WAS NETWORK IN HEADER                        
         BNH   HDHK20               NO                                          
*                                                                               
         MVC   H2+45(L'EZIHNET),EZIHNET                                         
*                                                                               
HDHK20   DS    0H                                                               
         CLI   SRTTYP,C'G'         SORTING BY MGR                               
         BNE   HDHK040                                                          
         MVC   H1+60(1),SORTMGR                                                 
         MVC   DUB+5(2),SORTMGR+1                                               
         MVI   DUB+7,X'0F'                                                      
         UNPK  DUB(5),DUB+5(3)                                                  
         MVC   H1+61(4),DUB                                                     
         B     HDHK050                                                          
HDHK040  CLI   SRTTYP,C'M'         SORTING BY MKT                               
         BNE   HDHK060                                                          
HDHK050  SR    R0,R0                                                            
         ICM   R0,3,SORTMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  H1+68(4),DUB                                                     
*                                                                               
HDHK060  MVC   H1+82(6),=C'PAYEE-'                                              
         MVC   H1+89(30),EZPYNAM                                                
*                                                                               
         MVC   H2+8(30),EZAGLIN1                                                
         MVC   H2+50(30),EZSNLIN1                                               
         MVC   H2+89(30),EZPYLIN1                                               
*                                                                               
         MVC   H3+8(30),EZAGLIN2                                                
         MVC   H3+50(30),EZSNLIN2                                               
         MVC   H3+89(30),EZPYLIN2                                               
*                                                                               
         MVC   H4+8(30),EZAGLIN3                                                
         MVC   H4+50(30),EZSNLIN3                                               
         MVC   H4+89(30),EZPYLIN3                                               
*                                                                               
         MVC   H5+8(30),EZAGLIN4                                                
         MVC   H5+50(30),EZSNLIN4                                               
         MVC   H5+89(30),EZPYLIN4                                               
*                                                                               
         MVC   H6+0(15),=C'INVOICE NUMBER-'                                     
         MVC   H6+16(10),EZIHINV                                                
         MVC   H6+28(5),=C'DATE-'                                               
         MVC   H6+34(8),EZIHDIDT                                                
         OC    EZIHISDT,EZIHISDT                                                
         BZ    HDHK100                                                          
         MVC   H6+43(7),=C'PERIOD-'                                             
         MVC   H6+51(8),EZIHDISD                                                
         MVI   H6+59,C'-'                                                       
         MVC   H6+60(8),EZIHDIED                                                
*                                                                               
HDHK100  DS    0H                                                               
         MVC   H6+84(15),=C'REPRESENTATIVE-'                                    
         MVC   H6+100(25),EZIHREP                                               
*                                                                               
         MVC   H7+84(12),=C'SALESPERSON-'                                       
         MVC   H7+100(25),EZIHSLSP                                              
*                                                                               
         MVC   H8+0(11),=C'ADVERTISER-'                                         
         MVC   H8+12(25),EZIHADVN                                               
         MVC   H8+38(16),=C'BROADCAST MONTH-'                                   
         MVC   H8+55(6),EZIHDMOS                                                
         MVC   H8+84(15),=C'AGENCY CONTACT-'                                    
         MVC   H8+100(25),EZIHACON                                              
*                                                                               
         MVC   H9+0(8),=C'PRODUCT-'                                             
         MVC   H9+12(25),EZIHPRDN                                               
         MVC   H9+38(15),=C'SCHEDULE DATES-'                                    
         OC    EZIHDSDT,EZIHDSDT                                                
         BZ    HDHK120                                                          
         MVC   H9+55(8),EZIHDSDT                                                
         MVI   H9+63,C'-'                                                       
         MVC   H9+64(8),EZIHDEDT                                                
*                                                                               
HDHK120  MVC   H9+84(15),=C'STATION CONTACT'                                    
         MVC   H9+100(25),EZIHSCON                                              
*                                                                               
         MVC   H10+0(9),=C'ESTIMATE-'                                           
         MVC   H10+12(10),EZIHEST                                               
         MVC   H10+38(15),=C'CONTRACT DATES-'                                   
         OC    EZIHDCSD,EZIHDCSD                                                
         BZ    HDHK140                                                          
         MVC   H10+55(8),EZIHDCSD                                               
         MVI   H10+63,C'-'                                                      
         MVC   H10+64(8),EZIHDCED                                               
*                                                                               
HDHK140  MVC   H10+84(11),=C'ORDER TYPE-'                                       
         MVC   H10+100(15),EZIHORD                                              
*                                                                               
         MVC   H11+38(10),=C'RATE CARD-'                                        
         MVC   H11+55(10),EZIHRC                                                
         MVC   H11+84(12),=C'BILLING INS-'                                      
         MVC   H11+100(25),EZIHINST                                             
*                                                                               
         MVC   H12+38(9),=C'DUE DATE-'                                          
         CLC   EZIHDUDT,SPACES                                                  
         BNH   HDHK200                                                          
         CLC   EZIHDUDT(2),=C'00'                                               
         BL    HDHK200                                                          
         CLC   EZIHDUDT(2),=C'99'                                               
         BH    HDHK200                                                          
         CLC   EZIHDUDT+2(2),=C'01'                                             
         BL    HDHK200                                                          
         CLC   EZIHDUDT+2(2),=C'12'                                             
         BH    HDHK200                                                          
         CLC   EZIHDUDT+4(2),=C'01'                                             
         BL    HDHK200                                                          
         CLC   EZIHDUDT+4(2),=C'31'                                             
         BH    HDHK200                                                          
         GOTO1 DATCON,DMCB,(0,EZIHDUDT),(5,H12+55)                              
*        MVC   H12+55(10),EZIHDUDT                                              
*                                                                               
HDHK200  DS    0H                                                               
         LA    R2,H12+84                                                        
         MVC   0(4,R2),=C'PAGE'                                                 
         SR    R0,R0                                                            
         ICM   R0,3,PAGE                                                        
         EDIT  (R0),(4,6(R2)),ALIGN=LEFT                                        
*                                                                               
HDHK220  DS    0H                                                               
         CLI   EZMODE,EZINVL       IF AT END OF INVOICE                         
         BE    HDHK230             SKIP MIDS                                    
*                                                                               
         MVC   H14+14(8),=C'SCHEDULE'                                           
         MVC   H14+54(16),=C'ACTUAL BROADCAST'                                  
         MVC   H14+111(14),=C'RECONCILIATION'                                   
*                                                                               
         LA    RF,MID1                                                          
         USING PLINED,RF                                                        
*                                                                               
         MVC   0(L'MID1,RF),SPACES                                              
         MVC   PRLIN(3),=C'LIN'                                                 
         MVC   PNTMS(3),=C'NO/'                                                 
         MVC   PAMG(3),=C'M/G'                                                  
*                                                                               
         LA    RF,MID2                                                          
         MVC   0(L'MID2,RF),SPACES                                              
*                                                                               
         MVC   PDAYS(6),=C'  DAYS'                                              
         MVC   PTIME(7),=C'   TIME'                                             
         MVC   PRLIN(3),=C'NO.'                                                 
         MVC   PRATE(6),=C'  RATE'                                              
         MVC   PNTMS(3),=C'MTH'                                                 
         MVC   PADATE(4),=C'DATE'                                               
         MVC   PADAY(3),=C'DAY'                                                 
         MVC   PATIME(5),=C' TIME'                                              
         MVC   PATYPE(4),=C'TYPE'                                               
         MVC   PACLASS(3),=C'CLS'                                               
         MVC   PAPB(5),=C'  P/B'                                                
         MVC   PAMG(3),=C'FOR'                                                  
         MVC   PAPF(4),=C'FILM'                                                 
         MVC   PARATE(7),=C'   RATE'                                            
         MVC   PRREM(7),=C'REMARKS'                                             
         MVC   PADBCR(5),=C'DR/CR'                                              
         B     HDHKX                                                            
*                                                                               
HDHK230  DS    0H                  AT END OF INVOICE KILL BOXES                 
         USING BOXD,R1                                                          
         ICM   R1,15,ABOX          IS ABOX ZEROS                                
         BZ    HDHKX               YES/ ON-LINE SKIP BOXES                      
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         MVI   BOXYORN,C'N'                                                     
*                                                                               
HDHKX    J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RC                                                            
*                                                                               
*        MYPRINT - PRINT ROUTINE FOR EZMOD TRACE                                
*                                                                               
MYPRINT  NMOD1 0,**+MYP**                                                       
*                                                                               
         LA    R5,SAVEP1                                                        
         LM    R6,RA,SAVREGS                                                    
         L     RC,SAVREGS+24                                                    
*                                                                               
         USING GEND,RC                                                          
*                                                                               
         MVC   0(132,R5),P1                                                     
         MVC   132(132,R5),P2                                                   
         MVC   264(132,R5),P3                                                   
         MVC   396(132,R5),P4                                                   
         MVC   P2,SPACES                                                        
         MVC   P3,SPACES                                                        
         MVC   P4,SPACES                                                        
         L     R2,0(,R1)                                                        
         MVC   P1,1(R2)       EZMODS LINE IS AT +1                              
*                                                                               
         L     R2,HEADHOOK                                                      
         XC    HEADHOOK,HEADHOOK                                                
         ZIC   R3,LINE                                                          
         MVI   LINE,1                                                           
         ZIC   R4,FORCEHED                                                      
         MVI   FORCEHED,0                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         ST    R2,HEADHOOK                                                      
         STC   R3,LINE                                                          
         STC   R4,FORCEHED                                                      
         MVC   P1,0(R5)                                                         
         MVC   P2,132(R5)                                                       
         MVC   P3,264(R5)                                                       
         MVC   P4,396(R5)                                                       
         J     EXIT                                                             
*                                                                               
SAVREGS  DS    7F                                                               
SAVEP1   DS    CL132                                                            
SAVEP2   DS    CL132                                                            
SAVEP3   DS    CL132                                                            
SAVEP4   DS    CL132                                                            
*                                                                               
         DROP  RB,RC                                                            
*                                                                               
* INITIALIZE EZBLOCK FOR EZMOD *                                                
*                                                                               
INITEZB  NMOD1 0,**+INT**                                                       
         L     RC,SVRC                                                          
*                                                                               
         L     RF,=A(HDHK)         HEADLINE ROUTINE                             
         A     RF,RELO                                                          
         ST    RF,HEADHOOK                                                      
*                                                                               
         USING GEND,RC                                                          
         LA    RE,EZBLOCKD                                                      
         LHI   RF,EZBLOCKL                                                      
         XCEFL                                                                  
*                                                                               
         MVI   EZWRKIOF,C'Y'                                                    
*                                                                               
         MVI   NEWINV,C'Y'         1ST TIME NEW INVOICE                         
         MVC   EZWKRFIL,EASIWK                                                  
         L     R1,AWRKFBUF                                                      
         ST    R1,EZWKRBUF                                                      
         L     RF,AIO1                                                          
         ST    RF,EZWKRREC                                                      
         LA    RF,2048(RF)                                                      
         ST    RF,EZAREC                                                        
         MVC   EZCOMFCS,ACOMFACS                                                
*        MVI   EZLOOKSW,X'20'+X'40'+X'80'  NO CMML/PRD/EST LOOKUP               
         MVI   EZLOOKSW,X'20'      DON'T LOOK UP COMMLS                         
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         MVC   EZAGY,SVAGYA                                                     
         MVC   EZUTL,UTL                                                        
         MVC   EZBAGYMD,BAGYMD                                                  
         MVC   EZSELINV,SVINV                                                   
         L     RF,=A(EZMPROC)                                                   
         A     RF,RELO                                                          
         ST    RF,EZHOOK                                                        
         L     RF,=A(MYPRINT)          PRINT ROUTINE FOR TRACE                  
         ST    RF,EZPRINT                                                       
*                                                                               
* INITIALIZE WORKIO BLOCK                                                       
*                                                                               
         LAY   R1,EZWRKIOB                                                      
         USING WRKIOB,R1                                                        
*                                                                               
         MVC   WRKEZUID,TWAORIG                                                 
         MVC   WRKIACOM,EZCOMFCS                                                
         MVC   WRKIAREC,EZWKRREC                                                
         MVC   WRKIABUF,EZWKRBUF                                                
         MVI   WRKIFTYP,WRKIFTEZ                                                
*                                                                               
         DROP  R1                                                               
*                                                                               
         CLI   EZ2MINUS,C'Y'                                                    
         BNE   *+8                                                              
         OI    EZFLAG,EZFNEGQ           NEGATIVE AMOUNTS                        
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
INIT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,SYSD                                                          
*                                                                               
         LHI   RF,STTABC-SYSD                                                   
         LA    R2,0(R1,RF)                                                      
         ST    R2,ASTTABC                                                       
*                                                                               
         LHI   RF,MKTAB-SYSD                                                    
         LA    R2,0(R1,RF)                                                      
         ST    R2,AMKTAB                                                        
*                                                                               
         LHI   RF,WRKFBUFR-SYSD                                                 
         LA    R2,0(R1,RF)                                                      
         ST    R2,AWRKFBUF                                                      
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* INITIALIZE FOR LIST MODE                                                      
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
LRRINIT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,COUNTERS                                                      
         LHI   R0,COUNTNQ                                                       
         ZAP   0(4,R1),=P'0'                                                    
         LA    R1,4(R1)                                                         
         BCT   R0,*-10                                                          
*                                                                               
         XC    LASTSEQ,LASTSEQ                                                  
         BRAS  RE,INITEZB          INITIALIZE EZBLOCKD                          
*                                                                               
         XC    DMCB,DMCB                                                        
         MVC   DMCB+4(4),=X'D9000A2C' DDWRKIO                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   EZWRKIO,0(R1)                                                    
*                                                                               
         GOTO1 CALLOV,DMCB,(X'10',0),ATWA  LOAD T23010 (EZMOD)                  
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         ST    RF,VEZMOD                                                        
*                                  SET OPTIONS FOR EZMOD IN EZBLOCK             
*                                  --------------------------------             
         MVI   EZTRACE,0           TRACE OPTION                                 
         CLI   TRCOPT,C'N'                                                      
         BE    LRRINI10                                                         
         CLI   TRCOPT,C' '                                                      
         BE    LRRINI10                                                         
*                                                                               
         CLI   TRCOPT,C'A'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'FF'       TRACE EVERYTHING                             
*                                                                               
         CLI   TRCOPT,C'Y'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'FF'       TRACE EVERYTHING                             
*                                                                               
         CLI   TRCOPT,C'E'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'20'       TRACE ERRORS                                 
*                                                                               
         CLI   TRCOPT,C'M'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'60'       TRACE MODES AND ERRORS                       
*                                                                               
         CLI   TRCOPT,C'F'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'E0'       TRACE FIELDS AND ERRORS AND MODES            
*                                                                               
         CLI   TRCOPT,C'R'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'70'       TRACE MODES, ERRORS, AND ADDED RECS          
*                                                                               
LRRINI10 DS    0H                                                               
*                                                                               
LRRINITX DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
*                                                                               
* GET MARKET FOR THIS STATION *                                                 
*                                                                               
GETMKT   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,FMTSTA                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
*                                                                               
         CLI   8(R1),0             ANY ERROR                                    
         BNE   GETMKNQX                                                         
         L     RF,AIO3                                                          
         PACK  DUB,SMKT-STARECD(4,RF)                                           
         CVB   R0,DUB                                                           
         STH   R0,HALF                                                          
*                                                                               
GETMKQX  DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
GETMKNQX DS    0H                                                               
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
* GET MARKET GROUP FOR THIS STATION *                                           
*                                                                               
GETMGR   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETMKT           GO GET MKT 1ST                               
         BNE   GETMGNQX            IF NO MKT, GONE                              
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(1),EZBAGYMD   NO BCLT                                      
         MVC   KEY+8(1),OPTMGR                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   GETMGNQX                                                         
*                                                                               
GETMGR10 CLC   HALF,KEY+11                                                      
         BE    GETMGQX                                                          
         GOTO1 SEQ                                                              
         CLC   KEY(9),KEYSAVE                                                   
         BE    GETMGR10                                                         
*                                                                               
GETMGQX  DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
GETMGNQX DS    0H                                                               
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* ADDS RECORD TO SORTER                                                         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
ADDTOSRT NTR1  BASE=*,LABEL=*                                                   
         XC    SORTENT(SORTRLQ),SORTENT                                         
*                                                                               
         CLI   SRTCLT,0                                                         
         BE    ADDTS08                                                          
         OC    EZIHCVAD,EZIHCVAD   HAVE WE GOT CLT OVERRIDE?                    
         BZ    ADDTS05                                                          
         LA    R1,EZIHCVAD                                                      
         BRAS  RE,GETCLT                                                        
         L     R1,EZAREC                                                        
         MVC   BYTE,CPROF+6-CLTHDR(R1)                                          
         GOTO1 CLUNPK,DMCB,(BYTE,EZIHCVAD),SORTCLT                              
         B     *+10                                                             
*                                                                               
ADDTS05  DS    0H                                                               
         MVC   SORTCLT,EZIHAAID                                                 
*                                                                               
         MVC   SORTSEQ,BATSEQ                                                   
         CLC   LASTSEQ,EZWKRIND+8  SAME BATCH AS LAST INVOICE?                  
         BE    ADDTS08                                                          
         SR    R0,R0                                                            
         ICM   R0,3,BATSEQ                                                      
         AHI   R0,1                                                             
         STCM  R0,3,BATSEQ                                                      
         MVC   SORTSEQ,BATSEQ      UPDATE SORTER BATCH COUNT                    
         MVC   LASTSEQ,EZWKRIND+8                                               
*                                                                               
ADDTS08  DS    0H                                                               
         MVC   SORTMONS,EZIHBMOS                                                
         MVC   SORTSTA(4),EZWKRIND+EZWISTN-EZWIKEY                              
         MVC   SORTSTA+4(1),EZWKRIND+EZWIMED-EZWIKEY                            
         MVC   SORTINV,EZIHINV                                                  
         MVC   SORTSQN,EZWKRIND+8                                               
         CLI   SRTTYP,C'S'         STATION                                      
         BE    ADDTS30                                                          
         CLI   SRTTYP,0            STATION                                      
         BE    ADDTS30                                                          
*                                                                               
         CLI   SRTTYP,C'M'         SORT BY MKT                                  
         BNE   ADDTS10                                                          
         BRAS  RE,GETMKT                                                        
         BNE   ADDTSX                                                           
         MVC   SORTMKT,HALF                                                     
         B     ADDTS30                                                          
*                                                                               
ADDTS10  CLI   SRTTYP,C'G'         SORT BY MGR                                  
         BNE   ADDTSX                                                           
         MVC   SORTMGR(3),=X'6F0000'                                            
         BRAS  RE,GETMGR                                                        
         BNE   ADDTS20                                                          
         MVC   SORTMGR(5),KEY+8    MOVE IN MGR + MKT                            
         B     ADDTS30                                                          
*                                                                               
ADDTS20  DS    0H                                                               
         XC    SORTMKT,SORTMKT                                                  
         BRAS  RE,GETMKT                                                        
         BNE   ADDTS30                                                          
         MVC   SORTMKT,HALF                                                     
*                                                                               
ADDTS30  GOTO1 SORTER,DMCB,=C'PUT',SORTENT                                      
*                                                                               
         AP    SORTCT,=P'1'                                                     
*                                                                               
ADDTSX   J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* R1 EXPECTED TO ADDRESS TWO-CHARACTER PACKED CLIENT CODE                       
* ON EXIT EZAREC I/O AREA CONTAINS CLIENT RECORD                                
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
GETCLT   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CLTHDR,R6                                                        
*        MVC   CKEYAM,BAGYMD                                                    
         MVC   CKEYAM,EZBAGYMD                                                  
         MVC   CKEYCLT,0(R1)                                                    
         MVC   KEYSAVE(L'CKEY),KEY                                              
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'SPTDIR',KEY,KEY                       
         CLI   8(R1),0                                                          
         BNE   GETCNEQX                                                         
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   GETCNEQX                                                         
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'SPTFIL',CKDA,EZAREC                   
         CLI   8(R1),0                                                          
         BNE   GETCNEQX                                                         
*                                                                               
GETCEQX  J     EQXIT                                                            
GETCNEQX J     NEQXIT                                                           
         LTORG                                                                  
         DROP  R6                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
* BYTE EXPECTED TO HAVE THE NEW MOS (X'BA' = OCT 2011)                          
* ON EXIT HALF WILL HAVE THE OLD FORMAT MOS (X'1110')                           
* DUB IS USED FOR CONVERSION                                                    
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                 
NEW2OLD  NTR1  BASE=*,LABEL=*                                                   
         ZIC   RF,BYTE                                                          
         NILL  GRF,X'000F'          ZERO OUT YEAR (RF - GRAND INSTR.)           
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF+1                                                        
*                                                                               
         ZIC   RF,BYTE                                                          
         SRL   RF,4                GET RID OF THE MONTH                         
         CVD   RF,DUB                                                           
         SR    RF,RF                                                            
         ICM   RF,3,DUB+6                                                       
         SRL   RF,4                                                             
         STC   RF,HALF                                                          
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
ZAPTOT   NTR1  BASE=*,LABEL=*                                                   
         ZAP   0(8,R1),=P'0'                                                    
         LA    R1,8(R1)                                                         
         BCT   R0,*-10                                                          
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*        FIXCOM - FIX UP COMMENT FOR PRINTING                                   
*                                                                               
FIXCOM   NTR1  BASE=*,LABEL=*                                                   
         OC    P(132),SPACES                                                    
         CLC   P(132),SPACES                                                    
         JE    EQXIT               NO COMMENT                                   
*                                                                               
         LA    R1,P+131            FIND LAST CHAR                               
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         LA    RF,P                                                             
         SR    R1,RF               FOR BCT                                      
         JNP   EQXIT                                                            
*                                                                               
FIXCOM4  DS    0H                                                               
         LA    RF,1(RF)            SKIP FIRST POS                               
         CLI   0(RF),C' '          SET SPACE                                    
         BNE   *+8                                                              
         MVI   0(RF),0             TO NULL TO BLOCK BOXES                       
         BCT   R1,FIXCOM4                                                       
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SUPPRESS NTR1  BASE=*,LABEL=*                                                   
         L     R5,AWRKFBUF                                                      
         USING W_RECD,R5                                                        
*                                                                               
         LA    R1,SUPTAB                                                        
         USING SUPTABD,R1                                                       
*                                                                               
* LOOK FOR BATCH SOURCE IN SOURCE LIST                                          
*                                                                               
SUP10    LA    R2,STSRCLST         LIST OF SOURCES                              
         USING STSRCLST,R2                                                      
*                                                                               
SUP20    CLI   0(R2),X'FF'         END OF SOURCE LIST?                          
         BE    SUP50               GO TO NEXT TABLE ENTRY                       
*                                                                               
         LLC   RF,STSRCLEN         LENGTH OF SOURCE                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVSRCE(0),STSOURCE                                               
         BE    SUP30               SEE IF WE'RE SUPPRESSING THIS SOURCE         
*                                                                               
         LA    R2,STSRCLQ(R2)      BUMP SOURCE LIST POINTER                     
         B     SUP20                                                            
         DROP  R2                                                               
*                                                                               
SUP30    CLI   0(R2),X'FF'         GO TO END OF SOURCE LIST                     
         BE    *+12                                                             
         LA    R2,STSRCLQ(R2)      BUMP SOURCE LIST POINTER                     
         B     *-12                                                             
*                                                                               
         LA    R2,1(R2)            BUMP PAST X'FF'                              
*                                                                               
         USING STIDLST,R2                                                       
SUP40    CLI   STTYPE,X'FF'        END OF ID LIST?                              
         BE    SUP50               NEXT TABLE ENTRY                             
*                                                                               
         CLI   STTYPE,STALLQ       EXCLUDING THIS SRC FOR ALL?                  
         JE    NEQXIT                                                           
*                                                                               
         LA    RF,AGENCY           POINT TO BATCH AGENCY                        
         CLI   STTYPE,STAGYQ       EXCLUDING AGENCY?                            
         BE    *+8                                                              
         LA    RF,W_USRID          POINT TO BATCH ID                            
*                                                                               
         CLC   STID,0(RF)                                                       
         JE    NEQXIT                                                           
*                                                                               
         LA    R2,STIDLQ(R2)       BUMP ID LIST POINTER                         
         B     SUP40                                                            
         DROP  R2                                                               
*                                                                               
SUP50    XR    RF,RF                                                            
         ICM   RF,3,STLEN          LENGTH OF ENTRY                              
         AR    R1,RF               BUMP TO NEXT ENTRY                           
         CLI   STLEN,X'FF'         END OF TABLE?                                
         JE    EQXIT                                                            
         B     SUP10                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
SUPTAB   DS    0X                  TABLE X'FF'-TERMINATED                       
*                                                                               
* LINE 1: 'SDIX', 'ISD ' FOR ALL                                                
*                                                                               
SUP1     DC    AL2(SUP1LQ)         LENGTH OF ENTRY                              
*                                                                               
         DC    X'03',C'SDI '       LIST OF SOURCES                              
         DC    X'04',C'ISD '                                                    
         DC    X'FF'                                                            
*                                                                               
         DC    AL1(STALLQ),X'0000' LIST OF IDS/POWERCODES                       
         DC    X'FF'                                                            
*                                                                               
SUP1LQ   EQU   *-SUP1              ENTRY LENGTH EQU                             
*                                                                               
* LINE 2: COZE, ICZE FOR TH(ZENITH)                                             
*                                                                               
SUP2     DC    AL2(SUP2LQ)                                                      
*                                                                               
         DC    X'04',C'COZE'                                                    
         DC    X'04',C'ICZE'                                                    
         DC    X'FF'                                                            
*                                                                               
         DC    AL1(STAGYQ),C'TH'                                                
         DC    X'FF'                                                            
*                                                                               
SUP2LQ   EQU   *-SUP2                                                           
*                                                                               
* LINE 3: IPZS, IPGS FOR                                                        
* OSNYREO, BROMRE, DFZRES, DFRES, ZEREP, ZDFPERE, GMMRE, GMPMRE                 
*                                                                               
SUP3     DC    AL2(SUP3LQ)                                                      
*                                                                               
         DC    X'04',C'IPZS'                                                    
         DC    X'04',C'IPGS'                                                    
         DC    X'FF'                                                            
*                                                                               
         DC    AL1(STIDQ),X'2FA7'  OSNYREO                                      
         DC    AL1(STIDQ),X'34BF'  BROMRE                                       
         DC    AL1(STIDQ),X'2F0B'  DFRES                                        
         DC    AL1(STIDQ),X'2F07'  DFZRES                                       
         DC    AL1(STIDQ),X'319B'  ZEREP                                        
         DC    AL1(STIDQ),X'31C1'  ZDFPERE                                      
         DC    AL1(STIDQ),X'23C6'  GMMRE                                        
         DC    AL1(STIDQ),X'3737'  GMPMRE                                       
         DC    X'FF'                                                            
*                                                                               
SUP3LQ   EQU   *-SUP3                                                           
*                                                                               
* LINE 4: IPSA FOR UB                                                           
*                                                                               
SUP4     DC    AL2(SUP4LQ)                                                      
*                                                                               
         DC    X'04',C'IPSA'                                                    
         DC    X'FF'                                                            
*                                                                               
         DC    AL1(STAGYQ),C'UB'                                                
         DC    X'FF'                                                            
*                                                                               
SUP4LQ   EQU   *-SUP4                                                           
*                                                                               
* LINE 5: IPZS FOR TB                                                           
*                                                                               
SUP5     DC    AL2(SUP5LQ)                                                      
*                                                                               
         DC    X'04',C'IZPS'                                                    
         DC    X'FF'                                                            
*                                                                               
         DC    AL1(STAGYQ),C'TB'                                                
         DC    X'FF'                                                            
*                                                                               
SUP5LQ   EQU   *-SUP5                                                           
*                                                                               
* LINE 6: IPES FOR FM                                                           
*                                                                               
SUP6     DC    AL2(SUP6LQ)                                                      
*                                                                               
         DC    X'04',C'IPES'                                                    
         DC    X'FF'                                                            
*                                                                               
         DC    AL1(STAGYQ),C'FM'                                                
         DC    X'FF'                                                            
*                                                                               
SUP6LQ   EQU   *-SUP6                                                           
*                                                                               
SUPTABX  DC    X'FF'                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
* FAKEIND - ROUTINE TO FILL IN INDEX FIELDS WITH DDWRKIOD DATA                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *             
FAKEIND  NTR1  BASE=*,LABEL=*                                                   
         LAY   R1,WRKEZKEY                                                      
         USING WRKEZKEY,R1                                                      
         LA    R2,EZWKRIND                                                      
         USING UKINDEX,R2                                                       
*                                                                               
         MVC   UKUSRID,WRKEZUID                                                 
         MVC   UKSYSPRG(L'WRKEZSCL),WRKEZSCL                                    
         MVC   UKDAY,WRKEZDAY                                                   
         MVC   UKCLASS,WRKEZMED                                                 
* !!! UKTYPE AND UKATTB SACRIFICED TO FIT IN THE 4-BYTE SEQ NUMBER !!!          
         MVC   UKFILENO(L'WRKEZSQN),WRKEZSQN                                    
         MVC   UKSTAT,WRKEZSTA                                                  
         MVC   UKAGELD,WRKEZBDT                                                 
         MVC   UKUDATA,WRKEZUDT                                                 
*                                                                               
         DROP  R2,R1                                                            
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SETCARDS NTR1  BASE=*,LABEL=*                                                   
         LHI   R0,SORTKLQ                                                       
         LAY   RE,SRTCDLEN                                                      
         EDIT  (R0),(2,0(RE)),ALIGN=LEFT                                        
         LHI   R0,SORTRLQ                                                       
         LAY   RE,RECCDLEN                                                      
         EDIT  (R0),(2,0(RE)),ALIGN=LEFT                                        
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*        CLRALL - CLEAR ALL SAVED LINES                                         
*                                                                               
CLRALL   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,CLRSCH                                                        
         BRAS  RE,CLRSPT                                                        
         BRAS  RE,CLRRCN                                                        
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*        CLRSCH - CLEAR SCHEDULE LINES                                          
*                                                                               
CLRSCH   NTR1  BASE=*,LABEL=*                                                   
         LA    RF,SCHLC                                                         
         LA    R1,SCHLIN1                                                       
         ST    R1,ANXTSCH          SET A(FIRST)                                 
*                                                                               
         MVC   0(L'SCHLIN1,R1),SPACES                                           
         LA    R1,L'SCHLIN1(R1)                                                 
         BCT   RF,*-10                                                          
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*        CLRSPT - CLEAR SPOT LINES                                              
*                                                                               
CLRSPT   NTR1  BASE=*,LABEL=*                                                   
         LA    RF,SPTLC                                                         
         LA    R1,SPTLIN1                                                       
         ST    R1,ANXTSPT          SET A(FIRST)                                 
*                                                                               
         MVC   0(L'SPTLIN1,R1),SPACES                                           
         LA    R1,L'SPTLIN1(R1)                                                 
         BCT   RF,*-10                                                          
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*        CLRRCN - CLEAR RECONCILIATION REMARK LINES                             
*                                                                               
CLRRCN   NTR1  BASE=*,LABEL=*                                                   
         LA    RF,RCNLC                                                         
         LA    R1,RCNLIN1                                                       
         ST    R1,ANXTRCN          SET A(FIRST)                                 
*                                                                               
         MVC   0(L'RCNLIN1,R1),SPACES                                           
         LA    R1,L'RCNLIN1(R1)                                                 
         BCT   RF,*-10                                                          
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
* FILTER ON MARKET GROUP *                                                      
*                                                                               
FTRMGR   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,FMTSTA                                                        
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO3                     
         CLI   8(R1),0             ANY ERROR                                    
         JNE   NEQXIT               NO                                          
*                                                                               
         L     R1,AIO3                                                          
         PACK  DUB,SMKT-STARECD(4,R1)                                           
         CVB   R0,DUB                                                           
*                                                                               
         LA    R3,MKTABLN                                                       
         L     R4,AMKTAB                                                        
*                                                                               
FTRMGR20 OC    0(2,R4),0(R4)                                                    
         JZ    NEQXIT                                                           
         CLM   R0,3,0(R4)                                                       
         JE    EQXIT                                                            
         LA    R4,2(,R4)                                                        
         BCT   R3,FTRMGR20                                                      
         J     NEQXIT                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
*&&DO                                                                           
NEXTLINE LA    RF,SPTLIN1                                                       
         LHI   R0,SPTLC            NUMBER OF SPOT LINES                         
*                                                                               
NEXTLIN2 CLC   0(L'SPTLIN1,RF),SPACES                                           
         JNH   NEXTLIN4                                                         
         LA    RF,L'SPTLIN1(RF)                                                 
         JCT   R0,NEXTLIN2                                                      
*                                                                               
         LTR   RF,RF                                                            
         BR    RE                                                               
*                                                                               
NEXTLIN4 ST    RF,FULL                                                          
         CR    RF,RF                                                            
         BR    RE                                                               
*&&                                                                             
NEXTLINE NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)         DISPLACMENT,LENGTH                           
         LA    R4,SPTLIN1+L'SPTLIN1                                             
         BCTR  R3,0                                                             
         LHI   R0,SPTLC            NUMBER OF SPOT LINES                         
*                                                                               
NEXTLIN2 DS    0H                                                               
         LA    RF,0(R4,R2)                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SPACES                                                   
         BNH   NEXTLIN4                                                         
         LA    R4,L'SPTLIN1(R4)                                                 
         BCT   R0,NEXTLIN2                                                      
*                                                                               
         J     NEQXIT                                                           
*                                                                               
NEXTLIN4 ST    R4,FULL                                                          
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* R1 = A(STRING)                                                                
* R1 HOB - STRING LENGTH                                                        
FINDLEN  LR    RF,R1               A(STRING)                                    
         SRL   R1,24               STRING LENGTH                                
         AR    RF,R1                                                            
         BCTR  RF,0                STRING'S LAST CHARACTER                      
*                                                                               
FINDLEN2 CLI   0(RF),C' '                                                       
         JH    FINDLEN4                                                         
         BCTR  RF,0                                                             
         JCT   R1,FINDLEN2                                                      
*                                                                               
         LTR   RF,RF                                                            
         BR    RE                                                               
*                                                                               
FINDLEN4 CR    RF,RF                                                            
         BR    RE                                                               
*                                                                               
*        TOPCOM - PRINT TOP STANDARD COMMENTS                                   
*                                                                               
TOPCOM   DS    0H                                                               
         CLI   TCOMSW,C'Y'         TEST ALREADY DONE                            
         BER   RE                                                               
*                                                                               
TOPC     NTR1  BASE=*,LABEL=*                                                   
* SAVE BOTTOM COMMENTS                                                          
         LA    R1,SYSD                                                          
         LHI   RF,BCOMSV-SYSD                                                   
         LA    R0,0(R1,RF)                                                      
         LA    RE,EZSBCOM                                                       
         LHI   R1,EZSBDLEN                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   TCOMSW,C'Y'                                                      
         LA    R2,EZSTCOM          FIRST ONE                                    
         LA    R3,EZSTCMN          COUNT                                        
*                                                                               
TC4      DS    0H                                                               
         CLI   0(R2),C' '          FIRST BLANK ONE IS END                       
         BNH   TCX                                                              
         MVC   P1+1(L'EZSTCOM),0(R2)                                            
         BRAS  RE,FIXCOM                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,L'EZSTCOM(R2)                                                 
         BCT   R3,TC4                                                           
*                                                                               
TCX      DS    0H                                                               
         J     EQXIT                                                            
*                                                                               
*                                                                               
*                                                                               
       ++INCLUDE SPGENEZ                                                        
*                                                                               
*                                                                               
*                                                                               
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* EZBLOCK                                                                       
* SPEZFFFD                                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE EZBLOCK                                                        
*                                                                               
EZBLOCKD DSECT                                                                  
         ORG   EZWRKIOB                                                         
       ++INCLUDE DDWRKIOD                                                       
         ORG                                                                    
*                                                                               
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASSB                                                          
       ++INCLUDE SPEZFFFD                                                       
         ORG   CONTAGH                                                          
* SPEZFE6D                                                                      
       ++INCLUDE SPEZFE6D                                                       
*                                                                               
* DDGENTWA                                                                      
       ++INCLUDE DDGENTWA                                                       
*                                                                               
* DMWRKRD                                                                       
       ++INCLUDE DMWRKFD                                                        
*                                                                               
* DMWRKRK                                                                       
       ++INCLUDE DMWRKFK                                                        
*                                                                               
* SPGENSTA                                                                      
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
*                                                                               
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
* SPEZFWORKD                                                                    
       ++INCLUDE SPEZFWORKD                                                     
*                                                                               
* CLIENT HEADER                                                                 
       ++INCLUDE SPGENCLT                                                       
*                                                                               
       ++INCLUDE SPEZDSCTS                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDPERVALD                                                      
*                                                                               
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
RELO     DS    A                                                                
SVRC     DS    A                                                                
TCOMSW   DS    CL1                                                              
IBCSW    DS    CL1                                                              
NEWINV   DS    CL1                                                              
PBCNT    DS    CL1                                                              
PRNTSW   DS    CL1                                                              
RCCNT    DS    XL1                                                              
SVSTA    DS    CL5                                                              
*                                                                               
SVDTES   DS   0XL4                                                              
SVDTESTR DS    XL2                                                              
SVDTEEND DS    XL2                                                              
*                                                                               
SVCONDT  DS    0XL6                CONVERTED DATE RANGE, TYPE 3 BINARY          
SVCONDTS DS    XL3                                                              
SVCONDTE DS    XL3                                                              
*                                                                               
SVBSEQ   DS    XL4                                                              
SVINV    DS    CL10                                                             
SVSRCE   DS    CL4                                                              
         DS    0F                                                               
SAVER0   DS    F                                                                
SAVERE   DS    F                                                                
*                                                                               
NEXTFLAG DS    C                   FLAG FOR SPOT DETAIL PRINTING                
*                                                                               
ALLPTTL  DS    0X                                                               
ANNPTTL  DS    PL8                                                              
RECPTTL  DS    PL8                                                              
SPTSTTL  DS    PL8                                                              
INTTTL   DS    PL8                                                              
ALLPTTN  EQU   (*-ALLPTTL)/8                                                    
ANXTRCN  DS    A                                                                
ANXTSPT  DS    A                                                                
ANXTSCH  DS    A                                                                
ALLPTTLQ EQU   *-ALLPTTL                                                        
*                                                                               
ASTTABC  DS    A                                                                
AMKTAB   DS    A                                                                
AWRKFBUF DS    A                                                                
*                                                                               
SORTENT  DS    0X                                                               
SORTCLT  DS    XL3                 CLIENT CODE                                  
SORTSEQ  DS    XL2                 RECORD  SEQUENCE NUMBER                      
SORTMGR  DS    XL3                 MARKET GROUP (OPTIONAL)                      
SORTMKT  DS    XL2                 MARKET        (OPTIONAL)                     
SORTMONS DS    XL2                 MONTH OF SERVICE BINARY YYMM                 
SORTSTA  DS    CL5                                                              
SORTINV  DS    CL10                                                             
SORTKLQ  EQU   *-SORTENT           SORT KEY LENGTH                              
*                                                                               
SORTSQN  DS    XL4                 BATCH SEQUENCE NUMBER                        
SORTRLQ  EQU   *-SORTENT           SORT RECORD LENGTH                           
*                                                                               
*                                                                               
LASTSEQ  DS    XL4                 LAST BATCH SEQ NUMBER                        
BATSEQ   DS    XL2                 BATCH SEQ NUMBER FOR SORTER                  
LASTCLSQ DS    XL5                                                              
*                                                                               
BYPASS   DS    CL1                 BYPASS THIS INVOICE                          
SVERRNO  DS    XL1                 ERRORS PASSED FROM EZMOD                     
*                                                                               
RQOPTS   DS    0CL(OPTEND-TRCOPT)                                               
TRCOPT   DS    CL1                                                              
SRTOPT   DS    CL1                 Y=SORT INPUT, Z=SORT OUTPUT                  
SRTCLT   DS    CL1                 SORT BY CLIENT OPTION                        
SRTTYP   DS    CL1                 S=STATION, M=MKT, G=MGR                      
SRTOPN   DS    CL1                 SORT IS OPEN SWITCH                          
*                                  SET IN EZMPROC                               
OPTNSW   DS    XL1                                                              
OPTCONV  EQU   X'80'                                                            
OPTDEL   EQU   X'40'                                                            
OPTUNCV  EQU   X'20'                                                            
OPTDONE  EQU   X'10'                                                            
OPTTDYC  EQU   X'08'               OPTION CONVERTED TODAY                       
OPTNOCS  EQU   X'04'               OPTION NOCOST - BLANK ALL RATES              
OPTTDYL  EQU   X'02'               OPTION LOADED TODAY                          
OPTNOBOX EQU   X'01'               OPTION TURN BOXES OFF                        
*                                                                               
OPTODAYC DS    XL3                 CONVERTED TODAY                              
OPTODAYL DS    XL2                 LOADED TODAY                                 
OPTSRCE  DS    CL4                 OPTION SOURCE                                
OPTMOS   DS    CL6                 MONTH OF SERVICE                             
OPTBMOS  DS    XL2                 BINARY MONTH OF SERVICE                      
OPTMKT   DS    XL2                                                              
OPTMEDIA DS    CL1                                                              
OPTMGR   DS    CL1                                                              
OPTMGRP  DS    XL2                                                              
OPTCC    DS    CL3                                                              
OPTNKP   DS    C                   ONLY BATCHES NOT MARKED KEEP                 
OPTEXP   DS    CL3                 RETURN EXPIRED                               
*                                                                               
PGDISP   DS    C                                                                
AGYDISC  DS    C                                                                
EZ2MINUS DS    C                                                                
*                                                                               
OPTEND   EQU   *                                                                
*                                                                               
COUNTERS DS    0H                                                               
SORTCT   DS    PL4                                                              
SORTGET  DS    PL4                                                              
SORTUSED DS    PL4                                                              
BYPASSCT DS    PL4                                                              
BATUSED  DS    PL4                                                              
INVPRT   DS    PL4                                                              
COUNTNQ  EQU   (*-COUNTERS)/4                                                   
*                                                                               
         DS    0D                                                               
*                                  SCHEDULE LINES                               
SCHLC    EQU   5                                                                
SCHLIN1  DS    CL(L'PSCHED)                                                     
         DS    (SCHLC-1)CL(L'SCHLIN1)                                           
SCHLINX  EQU   *                                                                
*                                  SPOT LINES                                   
SPTLC    EQU   5                                                                
SPTLIN1  DS    CL(L'PSPOT)                                                      
         DS    (SPTLC-1)CL(L'SPTLIN1)                                           
SPTLINX  EQU   *                                                                
*                                  RECONCILIATION REMARK LINES                  
RCNLC    EQU   10                                                               
RCNLIN1  DS    CL(L'PRREM)                                                      
         DS    (RCNLC-1)CL(L'RCNLIN1)                                           
RCNLINX  EQU   *                                                                
*                                                                               
         DS    0F                                                               
X        DS    XL64                                                             
*                                                                               
STTABC   DS    0X                                                               
*                                                                               
STTABL   EQU   700                 ENTRY = 7, 5 STATION, 2 MKT                  
STTABENT EQU   7                                                                
         ORG   *+STTABL                                                         
STTABE   EQU   *                                                                
*                                                                               
MKTAB    DS    XL900                                                            
MKTABLN  EQU   (*-MKTAB)/2                                                      
*                                                                               
BCOMSV   DS    CL130               SAVED STANDARD BOTTOM COMMENTS               
         DS    4CL130              5 ALLTOGETHER                                
*                                                                               
WRKFBUFR DS    0D                                                               
         DS    14336X                                                           
WRKFEND  EQU   *                                                                
*                                                                               
*                                                                               
*                                                                               
*                                                                               
PLINED   DSECT                                                                  
*                                                                               
PSCHED   DS    0CL(PSCHEDLQ)                                                    
B1       DS    CL1                                                              
PDAYS    DS    CL7                                                              
B2       DS    CL1                                                              
PTIME    DS    CL11                                                             
B3       DS    CL1                                                              
PRLIN    DS    CL3                                                              
B4       DS    CL1                                                              
PRATE    DS    CL10                                                             
B5       DS    CL1                                                              
PNTMS    DS    CL3                                                              
B6       DS    CL1                                                              
*                                                                               
PSCHEDLQ EQU   *-PDAYS                                                          
B7       DS    CL1                                                              
*                                                                               
PSPOT    DS    0CL(PSPOTLQ)                                                     
PADATE   DS    CL5                                                              
B8       DS    CL1                                                              
PADAY    DS    CL3                                                              
B9       DS    CL1                                                              
PATIME   DS    CL5                                                              
B10      DS    CL1                                                              
PATYPE   DS    CL4                                                              
B11      DS    CL1                                                              
PACLASS  DS    CL3                                                              
B12      DS    CL1                                                              
PAPB     DS    CL7                                                              
B13      DS    CL1                                                              
PAMG     DS    CL5                                                              
B14      DS    CL1                                                              
PAPF     DS    CL14                                                             
B15      DS    CL1                                                              
PARATE   DS    CL10                                                             
*                                                                               
B16      DS    CL1                                                              
B17      DS    CL1                                                              
*                                                                               
PRREM    DS    CL15                                                             
B18      DS    CL1                                                              
PADBCR   DS    CL8                                                              
PSPOTLQ  EQU   *-PADATE                                                         
*                                                                               
B19      DS    CL1                                                              
*                                                                               
* WHY?  ASK BOB GRIFFIN                                                         
         ORG   B11                                                              
PAPGM    DS    0CL44               PGM= PROGRAM DESCRIPTION                     
*                                                                               
         ORG   PAPF+9                                                           
PAINTEG  DS    0CL14               INTEGRATION AMOUNT                           
*                                                                               
*                                                                               
*                                                                               
SUPTABD  DSECT                                                                  
STLEN    DS    XL2                 LENGTH OF ENTRY                              
*                                                                               
STSRCLST DS    0X                  LIST OF SOURCES, X'FF'-TERMINATED            
* SOURCE LIST ENTRY DEFINITION:                                                 
STSRCLEN DS    X                   LENGTH OF SOURCE FOR CLC                     
STSOURCE DS    CL4                 SOURCE NAME                                  
STSRCLQ  EQU   *-STSRCLST                                                       
*                                                                               
STIDLST  DS    0X                  LIST OF IDS, X'FF'-TERMINATED                
* ID LIST ENTRY DEFINITION:                                                     
STTYPE   DS    X                   TYPE FLAG(AGY/ID)                            
STALLQ   EQU   X'01'               ALL                                          
STAGYQ   EQU   X'02'               SPECIFIC AGENCY                              
STIDQ    EQU   X'03'               SPECIFIC USER ID                             
STID     DS    XL2                 BINARY UID OR 2 CHAR AGENCY PWRCODE          
STIDLQ   EQU   *-STIDLST                                                        
*                                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114SPEZF09   11/04/20'                                      
         END                                                                    
