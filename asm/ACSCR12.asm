*          DATA SET ACSCR12    AT LEVEL 094 AS OF 03/18/16                      
*PHASE T60C12A                                                                  
*&&ONLIN SET   Y                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE US VERSION LEVEL 93 AS OF 21/09/15         *         
*                                                                     *         
***********************************************************************         
*                                                                               
* PID  LVl DATE    COMMENTS                                                     
* ---------------------------------                                             
* SMAN 088 10NOV11 PR000122 Estimate Phase 1                                    
* SMAN 089 16DEC11 PR002242 UK/US MERGE                                         
* SMAN 090 09FEB12 BR19163D Minor bug fix to lvl 89                             
* JFOS 091 13DEC12<PR003402>New 'Estimated Time' filter                         
* JFOS 092 05SEP13<DSPCA111>New 'Br'Ocean Invoices Status' filter               
* YNGX 094 17MAR16<PCA2359> Merge US and UK versions                            
*                                                                               
         TITLE 'COLUMN FILTER MAINTAINANCE'                                     
                                                                                
T60C12   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,60C12,RA,R8,RR=RE                                              
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL         4K AREA                                      
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   FVXTRA,SPACES                                                    
         EJECT ,                                                                
         USING RESRECD,R2                                                       
         LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                DELETE COLUMN ELEMENT'S ONLY                 
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                COPY COLUMN ELEMENT'S                        
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
         CLI   APMODE,APMDISK                                                   
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT95              NO                                           
*                                                                               
         CLI   APPFKEY,PFKHLP                                                   
         BNE   *+10                                                             
         XC    ACURDEF,ACURDEF     SET TO BEGINING OF HELP                      
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP                                                    
         MVC   APPARM(1),TWASWPRE          SWAP TO NEW RECORD                   
         MVC   APPARM+1(1),TWASWPAC        SWAP TO NEW ACTION                   
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         CLI   APACTN,ACTCHA                                                    
         BNE   XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
VALKEY   DS    0H                                                               
*                                                                               
         MVI   TUPPROF,0                                                        
         MVI   PTMSTAT,PTMSALL     ALL TIME ARE VALID                           
         MVC   DEFTIMST,SPACES     DEFAULT TIME STATUS                          
         CLI   APREPJCL,REPJCLV    PROD                                         
         BE    *+8                                                              
         CLI   APREPJCL,REPJCL1    PERSON                                       
         BNE   VALKEY08                                                         
*                                                                               
         USING CAPRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   CAPKEY,SPACES                                                    
         MVI   CAPKTYP,CAPKTYPQ    COST ALLOCATION RECORD TYPE                  
         MVI   CAPKSUB,CAPKSUBQ    COST ALLOCATION RECORD SUB TYPE              
         MVC   CAPKCPY,CUABIN      COMPANY LEVEL ONLY                           
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BNE   VALKEY07                                                         
*                                                                               
         USING OPDELD,R3                                                        
         L     R3,AIOAREA2                                                      
         AH    R3,DATADISP                                                      
         SR    RF,RF                                                            
VALKEY04 CLI   OPDEL,0                                                          
         BE    VALKEY07                                                         
         CLI   OPDEL,OPDELQ        X'A4' - OPTIONS DATA ELEMENT                 
         BNE   *+8                                                              
         CLI   OPDNUM,COTUP#       TUP PROFILE NUMBER                           
         BNE   *+14                                                             
         MVC   TUPPROF,OPDDATA     SAVE OFF TUP PROFILE                         
         B     VALKEY06                                                         
*                                                                               
         IC    RF,OPDLN                                                         
         AR    R3,RF                                                            
         B     VALKEY04                                                         
         DROP  R3                                                               
*                                                                               
VALKEY06 CLI   TUPPROF,C' '        Any TUP setting from cost profile            
         BNH   VALKEY07            No - show all time                           
         CLI   TUPPROF,COSAVED                                                  
         BE    VALKEY07                                                         
*                                                                               
         MVI   PTMSTAT,0                                                        
         CLI   TUPPROF,COFUAPR     Fully approved ?                             
         BNE   *+12                                                             
         OI    PTMSTAT,PTMSFAPT                                                 
         MVI   DEFTIMST,C'F'                                                    
         CLI   TUPPROF,COLMAPR     Part approved (line manager) ?               
         BE    *+12                                                             
         CLI   TUPPROF,COCLAPR     Part approved (client)?                      
         BNE   *+14                                                             
         OI    PTMSTAT,PTMSFAPT+PTMSPART                                        
         MVC   DEFTIMST(3),=C'F,P'                                              
         CLI   TUPPROF,COSUBMD     Submitted ?                                  
         BNE   VALKEY07                                                         
         OI    PTMSTAT,PTMSALL-PTMSSAVT                                         
         MVC   DEFTIMST(5),=C'F,P,S'                                            
                                                                                
VALKEY07 CLI   TUPPROF,C' '        Any setting?                                 
         BH    *+12                Yes                                          
         OI    PTMSTAT,PTMSFAPT    No so use the default setting of F           
         MVI   DEFTIMST,C'F'                                                    
         CLI   APREPJCL,REPJCL1    PERSON TYPE                                  
         BNE   VALKEY08            Yes - all time status are valid              
         MVI   PTMSTAT,PTMSALL     ALL TIME ARE VALID                           
*                                                                               
VALKEY08 LA    R2,IOKEY                                                         
         MVI   NEWKEY,NO           RESET TO NO                                  
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
         TM    TWAMODE,TWAMLSM                                                  
         BO    VALKEY10                                                         
         TM    COLCODEH+4,FVITHIS  NEW FORMAT INPUT?                            
         BO    VALKEY10                                                         
         CLC   SAVFORM,COLCODE                                                  
         BE    VALKEY10                                                         
         MVI   CURRCOLN,0                                                       
         MVC   COLCODE,SAVFORM                                                  
         OI    COLCODEH+6,FVOXMT                                                
*                                                                               
VALKEY10 GOTO1 AFVAL,COLNUMH                                                    
         BE    VALKEY12                                                         
         SR    RF,RF                                                            
         IC    RF,CURRCOLN                                                      
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(2),APDUB                                                  
         MVC   COLNUM(1),APWORK+1                                               
         CLI   APWORK,C'0'                                                      
         BE    *+10                                                             
         MVC   COLNUM(2),APWORK                                                 
*                                                                               
VALKEY12 GOTO1 AFVAL,COLNUMH                                                    
         TM    FVIIND,FVINUM       Was it numeric ?                             
         BZ    IVALCOLN            No, invalid column number                    
         L     RF,SCFULL                                                        
         CLC   CURRCOLN,SCFULL+3   This col # with last time                    
         BE    VALKEY14            Same                                         
         OI    INFLAG1,INFDISR     Force to redisplay if col changed            
*                                                                               
VALKEY14 GOTO1 AFVAL,COLCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    COLCODEH+4,FVITHIS  ANY INPUT?                                   
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY            ENTER KEY                                    
         MVC   COLCODE,SAVFORM                                                  
         OI    COLCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         TM    TWAMODE,TWAMLSM                                                  
         BO    VALKEY18                                                         
         CLC   SAVFORM,FVIFLD                                                   
         BE    *+10                                                             
         XC    SAVRECK,SAVRECK                                                  
*                                                                               
VALKEY18 MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY20                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
         B     VALKEY90                                                         
*                                                                               
VALKEY20 MVI   STSEQ,1                                                          
*                                                                               
VALKEY90 BAS   RE,FINDSCRN                                                      
         ST    R2,ASCRTAB                                                       
         BNE   VALKEY99                                                         
*                                                                               
         CLC   APRECKEY(L'RESKEY),SAVRECK                                       
         BE    VALKEY98            VALKEY99                                     
*                                                                               
VALKEY92 GOTO1 =A(SETSCRN),(R2),RR=APRELO                                       
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING RFLELD,R9                                                        
VALREC   L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,COLNMEH                                                    
         BNE   VALREC05                        NAME HAS NOT BEEN INPUT          
         GOTO1 ADDNAME,APPARM,(R2),COLNMEH     GET  FORMAT NAME                 
         BNE   VREC999                         ON   ERROR, EXIT                 
*                                                                               
VALREC05 MVC   RESKEY,APRECKEY                                                  
         GOTO1 AFVAL,COLDATAH                                                   
         MVC   COLDATAH+5(1),FVILEN                                             
         XC    DEFENTRY,DEFENTRY                                                
         XC    SVKYWDD#,SVKYWDD#                                                
         XC    CURAMTFR,CURAMTFR                                                
         MVI   REPMODE,REPCOL                                                   
*                                                                               
         USING DEFTABD,R1                                                       
         GOTO1 VALDEF,COLDATAH                                                  
         BNE   VALREC06                                                         
         ST    R1,DEFENTRY                                                      
         MVC   SVKYWDD#,DEFDDNUM   Save off keyword data dictionary #           
         DROP  R1                                                               
*                                                                               
VALREC06 L     R1,AIOAREA1                                                      
         MVI   APELCODE,RFLELQ     R.L. FILTER DATA                             
         GOTO1 GETEL,(R1)          GET COLUMN DATA                              
         BNE   VREC200                                                          
         LR    R9,R1                                                            
         CLI   RFLTYPE,RFLLDG      LEDGER DATA                                  
         BNE   VREC200                                                          
         XC    LDGLIST,LDGLIST                                                  
         SR    RE,RE               COUNT OF U/L'S                               
         ZIC   R3,RFLLN            LENGTH OF ELEM                               
         AHI   R3,-RFLLNQ          UP TO DATA                                   
         LA    RF,LDGLIST          BUILD LIST                                   
         LA    R1,RFLDATA                                                       
*                                                                               
VREC100  MVC   0(2,RF),0(R1)                                                    
         LA    RE,1(,RE)                                                        
         LA    R1,2(,R1)                                                        
         LA    RF,2(,RF)                                                        
         AHI   R3,-2                                                            
         BE    VREC110                                                          
         CLC   0(1,R1),SCCOMMA     DON'T SAVE COMMAS                            
         BNE   VREC100                                                          
         LA    R1,1(,R1)                                                        
         BCTR  R3,0                                                             
         B     VREC100                                                          
*                                                                               
VREC110  STC   RE,LDGLISTN         SAVE NUMBER OF U/L'S                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  COLUMN #                                                           *         
***********************************************************************         
VREC200  GOTO1 AFVAL,COLNUMH                                                    
         TM    FVIIND,FVINUM       HAS TO BE VALID NUMBER                       
         BZ    IVALNNUM                                                         
         L     RF,SCFULL           AFVAL PASSES BACK VALUE                      
         STC   RF,CURRCOLN                                                      
         BAS   RE,GETCOLS          SHOW VALID COLUMNS                           
         LA    RF,VCOLLIST         VALID COLUMNS LIST                           
*                                                                               
VREC205  CLI   0(RF),0             END OF LIST                                  
         BZ    IVALCOLN            INVALID COLUMN NUMBER                        
         CLC   CURRCOLN,0(RF)                                                   
         BE    VREC215             OK, NEXT PART                                
         LA    RF,1(,RF)                                                        
         B     VREC205                                                          
*                                                                               
*R020    L     R2,AIOAREA1                                                      
*        MVI   APELCODE,RCLELQ                                                  
*        MVC   ELEMSEQ,CURRCOLN    LOOK FOR SPECIFIC COL NUMBER                 
*        GOTO1 GETEL,(R2)                                                       
*        BNE   IVALCOLN                                                         
*                                                                               
VREC215  MVC   SVKEY,IOKEY                                                      
         LA    R9,APELEM                                                        
*                                                                               
         LA    R3,COLTAGH          START OF FIELDS TO VALIDATE                  
VREC220  CLI   0(R3),0             EOS (END OF SCREEN)                          
         BE    VREC900                                                          
         TM    1(R3),FVAXTND                                                    
         BO    VREC224                                                          
*                                                                               
VREC222  SR    RF,RF                                                            
         IC    RF,0(,R3)           GET LENGTH OF FIELD IN RF                    
         AR    R3,RF                                                            
         B     VREC220                                                          
*                                                                               
VREC224  SR    RF,RF                                                            
         IC    RF,0(,R3)           GET LENGTH OF FIELD IN RF                    
         AHI   RF,-8               FOUND FIELD WITH EXTENDED HEADER             
         LR    RE,R3                                                            
         AR    RE,RF               POINT TO EXTENDED HEADER                     
*                                                                               
         USING REPFD,R2                                                         
         L     R2,ASCRTAB          START OF TABLE                               
VREC226  CLI   0(R2),X'FF'         EOT                                          
         BE    VREC222             NOT FOUND                                    
         CLC   REPRFL#,0(RE)       MATCH ON EXTEND FIELD # TO RFLTYPE           
         BE    VREC228                                                          
         LA    R2,REPFLNQ(,R2)                                                  
         B     VREC226                                                          
*                                                                               
VREC228  CLI   REPRFL#,251         SPECIAL CASE                                 
         BE    VREC222             GO TO NEXT FLD W/EXTENDED HEADER             
*                                                                               
         ST    R2,AREPFD           SAVE ADDRESS OF REPFD                        
         BAS   RE,NEXTUNPT         POINT R3 AT UNPROTECTED FIELD                
*                                                                               
         USING ROUTD,R6                                                         
         L     R6,=A(ROUTINES)                                                  
         A     R6,APRELO                                                        
VREC230  CLI   ROUTFLD,X'FF'                                                    
         BE    VREC004X                                                         
         CLC   ROUTFLD,REPFFLDN    MATCH ON FIELD                               
         BE    VREC240                                                          
         LA    R6,ROUTLNQ(,R6)                                                  
         B     VREC230                                                          
*                                                                               
VREC240  DS    0H                                                               
*&&UK                                                                           
         IC    RF,COLDATAH+5                                                    
         LA    R1,L'AC@XDATA                                                    
         LA    RE,AC@XDATA+L'AC@XDATA-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         SHI   RE,1                                                             
         BCT   R1,*-12                                                          
         CR    RF,R1                                                            
         BNE   VREC241                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   VREC241                                                          
         CLC   COLDATA(0),AC@XDATA                                              
*                                                                               
         GOTO1 MAKELIST,APPARM,(REPRFL#,(R3)),AIOAREA1,                X        
               ('MAXPARM',COLBKLNG),(CURRCOLN,APELEM),X'80'                     
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VREC004X            NO   INPUT                                   
         BP    VREC999             BAD  INPUT                                   
*                                                                               
         TM    REPFIND2,REPFXDAT                                                
         BNZ   VREC241A                                                         
         MVC   FVMSGNO,=AL2(ACEINPT)                                            
         B     VREC999                                                          
*&&                                GOOD INPUT                                   
VREC241  GOTO1 MAKELIST,APPARM,(REPRFL#,(R3)),AIOAREA1,                X        
               ('MAXPARM',COLBLOCK),(CURRCOLN,APELEM)                           
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VREC004X            NO   INPUT                                   
         BP    VREC999             BAD  INPUT                                   
*                                                                               
VREC241A CLI   RFLTYPE,RFLDCNTR                                                 
         BL    VREC242                                                          
         CLI   RFLTYPE,RFLDWC                                                   
         BH    VREC242                                                          
         OI    RFLIND,RFLDENO      DENOMINATOR TYPE FILTER                      
*                                                                               
VREC242  DS    0H                                                               
*&&UK                                                                           
         TM    CURRCOPT,RCLEQU     IS IT A CALCULATED COLUMN?                   
         BZ    VREC243                                                          
         TM    REPFIND2,REPFCALC   CALCULATED COLUMN ALLOWED?                   
         BO    VREC243                                                          
         MVC   FVMSGNO,=AL2(ACECFNAL)                                           
         B     VREC999             FILTER NOT ALLOWED FOR THIS COLUMN           
*&&                                                                             
VREC243  TM    RFLIND,RFLXCLD      WAS  EXCLUDE REQUESTED ?                     
         BZ    VREC250             NO,  SKIP                                    
         TM    REPFIND1,REPFXLD    IS   EXCLUDE VALID ?                         
         BO    VREC250             YES, CONTINUE                                
*                                  NO,  FIELD NOT VALID                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VREC999             EXIT                                         
*                                                                               
VREC250  DS    0H                                                               
         LA    RF,VALROUT          VALIDATE ROUTINE                             
         ICM   RF,8,ROUTVAL        ROUTINE #                                    
         BASR  RE,RF                                                            
         BNE   VREC999                                                          
*                                                                               
VREC004X LA    R2,REPFLNQ(,R2)                                                  
         B     VREC222                                                          
*                                                                               
VREC900  CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   VREC999                                                          
         MVC   IOKEY,SVKEY                                                      
         GOTO1 ADDID,APPARM,AIOAREA1                                            
         BNE   VREC999             ON   ERROR, EXIT                             
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA     CHANGE A RECORD?                             
         BO    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VREC999  CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         SR    R1,R1                                                            
         IC    R1,CURRCOLN                                                      
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(2),APDUB                                                  
         LA    RF,1                                                             
         LA    RE,APWORK                                                        
         CLI   APWORK,C'0'                                                      
         BNE   *+8                                                              
         LA    RE,APWORK+1                                                      
         LA    RF,0                                                             
         EXMVC RF,COLNUM,0(RE)     RESTORE OLD NUMBER                           
         B     EXIT                                                             
         DROP  R6,R9                                                            
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE DATA ROUTINES                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ROUTD,R6                                                         
VALROUT  NTR1                                                                   
         MVI   ADD_N_EL,NO                                                      
         CLI   FVILEN,0            DO WE HAVE INPUT?                            
         BE    VALROUT9            NO, LEAVE                                    
         SRL   RF,24               MOVE ROUTINE # TO LOB                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALACT          1 - Account                                      
         B     VALCNT          2 - Contra account                               
         B     VALOFF          3 - Office                                       
         B     VALBUD          4 - Budget                                       
         B     VALTTY          5 - Transaction type                             
         B     VALWCT          6 - Workcode type                                
         B     VALTYT          7 - Type of time                                 
         B     VALROUTX        8 - Cost account                                 
         B     VALTSK          9 - Workcode or task code                        
         B     VALGEN         10 - General account                              
         B     VALUFD         11 - User field                                   
         B     VALSTA         12 - Yes/No/Only input                            
         B     VALFLT         13 - Filters                                      
         B     VALBSR         14 - Billing source                               
         B     VALFCR         15 - Foreign currency                             
         B     VALMTH         16 - Method type                                  
*&&UK*&& B     VALAUTH        17 - VALIDATE AUTH STATUS (UK)                    
*&&US*&& B     VALROUTX       17 - (US)                                         
*&&UK*&& B     VALYN          18 - Yes/No input                                 
*&&US*&& B     VALROUTX       18 - (US)                                         
         B     VALTST         19 - Validate time status                         
*&&US*&& B     VALAPMT        20 - Validate approval method                     
*&&UK*&& B     VALROUTX       20 - (UK)                                         
*&&UK*&& B     VALXDAN        21 - Validate Data name                           
*&&US*&& B     VALROUTX       21 - (US)                                         
*&&UK*&& B     VALXDAV        22 - Validate Data value                          
*&&US*&& B     VALROUTX       22 - (US)                                         
*&&UK*&& B     VALETYPE       23 - Validate Expenditure type                    
*&&US*&& B     VALROUTX       23 - (US)                                         
*&&UK*&& B     VALAMT         24 - Validate Amount                              
*&&US*&& B     VALROUTX       24 - (US)                                         
*&&UK*&& B     VALXDOR        25 - Validate XDATA Origin                        
*&&US*&& B     VALROUTX       25 - (US)                                         
*&&UK*&& B     VALBNVS        26 - Validate Br'O Invoices status                
*&&US*&& B     VALROUTX       26 - (US)                                         
*                                                                               
VALROUTX CLC   FVMSGNO,=AL2(FVFOK)                                              
         BNE   XIT                                                              
         CLI   ADD_N_EL,YES                                                     
         BNE   VALROUT9                                                         
         MVI   ADD_N_EL,NO                                                      
         L     R2,AIOAREA1                                                      
         GOTO1 ADDEL,(R2)                                                       
*                                                                               
VALROUT9 CLC   FVMSGNO,=AL2(FVFOK)                                              
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  GENERAL VALIDATION FOR AN ACCOUNT                                  *         
***********************************************************************         
         SPACE 1                                                                
VALGEN   MVC   APWORK,SPACES                                                    
         MVC   APHALF,=AL2(FVFOK)                                               
         MVI   FLDMAXL,LACCOUNT    MAX  LENGTH OF FIELD                         
*                                                                               
         CLI   ROUTFLD,FLDNCLNT    CLIENT SJ                                    
         BNE   VALGEN10                                                         
         MVC   APWORK(2),=C'SJ'                                                 
         MVC   APHALF,=AL2(ACECLI)                                              
         CLC   APREPUL,=C'1C'      INCOME/COSTING OR P&L                        
         BNE   VALGEN50            NO,   SKIP                                   
         MVC   SAVEUL,ACLEDGER     SAVE  U/L  INFORMATION                       
         LA    R1,=C'SJ'                                                        
         GOTO1 GETLEDG,(R1)        FIND  SIZE OF   LVL  1                       
         MVC   FLDMAXL,ACLEV1      SAVE  LVL  1    SIZE                         
         GOTO1 GETLEDG,SAVEUL      RESET UNIT/LEDGER                            
         B     VALGEN50                                                         
*                                                                               
VALGEN10 CLI   ROUTFLD,FLDNPRSN    PERSON 2P                                    
         BNE   VALGEN12                                                         
         MVC   APWORK(2),=C'2P'                                                 
         MVC   APHALF,=AL2(ACEPRSN)                                             
         B     VALGEN50                                                         
*                                                                               
VALGEN12 CLI   ROUTFLD,FLDNDEPT    DEPARTMENT 2D                                
         BNE   VALGEN14                                                         
         MVC   APWORK(2),=C'2D'                                                 
         MVC   APHALF,=AL2(ACEDEPT)                                             
         B     VALGEN50                                                         
*                                                                               
VALGEN14 CLI   ROUTFLD,FLDNBILL    BILLING 11                                   
         BNE   VALGEN16                                                         
         MVC   APWORK(2),=C'11'                                                 
         MVC   APHALF,=AL2(ACEACCT)                                             
         B     VALGEN50                                                         
*                                                                               
VALGEN16 CLI   ROUTFLD,FLDNRVNU    REVENUE 12                                   
         BNE   VALGEN18                                                         
         MVC   APWORK(2),=C'12'                                                 
         MVC   APHALF,=AL2(ACEACCT)                                             
         B     VALGEN50                                                         
*                                                                               
VALGEN18 CLI   ROUTFLD,FLDNCSTA    COST 1C                                      
         BNE   VALGEN20                                                         
         MVC   APWORK(2),=C'1C'                                                 
         MVC   APHALF,=AL2(ACEACCT)                                             
         B     VALGEN50                                                         
*                                                                               
VALGEN20 CLI   ROUTFLD,FLDN14PT    14 POINTER                                   
         BNE   VALGEN50                                                         
         MVC   APWORK(2),=C'14'                                                 
         MVC   APHALF,=AL2(ACEACCT)                                             
*                                                                               
VALGEN50 SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         L     R3,0(,R1)                                                        
         LA    R4,COLBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALGEN60 DS    0H                                                               
         CLC   0(1,R4),FLDMAXL     IS   THE LENGTH OF ACCOUNT > MAX ?           
         BH    VALGENER            YES, INVALID ACCOUNT                         
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY                                      
         MVC   ACTKUNT(2),APWORK   UNIT/LEDGER                                  
         MVC   ACTKACT,12(R4)      ACCOUNT CODE                                 
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALGEN90                                                         
*                                                                               
         USING REPFD,RF                                                         
         L     RF,AREPFD           ->   REPFD DSECT                             
         TM    REPFIND2,REPFWLD    IS   WILDCARD VALID ?                        
         BZ    VALGENER            NO,  ERROR                                   
         MVI   CHECKUL,C'N'                                                     
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         BAS   RE,WILDCARD                                                      
         BNE   VALGENER                                                         
         DROP  RF                                                               
*                                                                               
VALGEN90 LA    R4,LSCNNTRY(,R4)                                                 
         BCT   R6,VALGEN60                                                      
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*                                                                               
VALGENER MVC   FVXTRA(20),12(R4)                                                
         MVC   FVMSGNO,APHALF                                                   
         B     VALROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  Y/N/O STATUS TYPE, R9=A(APELEM)                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALSTA   LA    RF,COLBLOCK                                                      
         CLC   12(1,RF),APYES      DEFAULT IS YES                               
         BE    VALROUTX                                                         
         MVI   ADD_N_EL,YES                                                     
         MVI   RFLDATA,NO          MAKE IT 'N' FOR NO                           
         CLC   12(1,RF),APNO                                                    
         BE    VALROUTX                                                         
         MVI   RFLDATA,YES         CHANGE C'O' TO C'Y'                          
         CLC   12(1,RF),APONLY                                                  
         BE    VALROUTX                                                         
         MVC   FVXTRA(1),12(RF)                                                 
         B     ERRINPT                                                          
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*  FILTERS                                                            *         
***********************************************************************         
         SPACE 1                                                                
VALFLT   SR    R1,R1               FILTER VALIDATE, LET IT PASS                 
         IC    R1,NPARMS           JUST MAKE SURE FILTERS ARE 1 CHAR            
         LA    RF,COLBLOCK         ACCEPT ALL FILTERS THAT FIT                  
*                                                                               
VALFLT10 CLI   0(RF),1             VERIFY ONE BYTE PER FILTER                   
         BE    VALFLT20                                                         
         MVC   FVXTRA(12),12(RF)                                                
         B     ERRINPT                                                          
*                                                                               
VALFLT20 LA    RF,LSCNNTRY(,RF)                                                 
         BCT   R1,VALFLT10                                                      
*                                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  BILLING SOURCE                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALBSR   DS    0H                                                               
         GOTO1 VALLIST,APPARM,=C'ME',(COLBLOCK,COLBLOCK+12),LITTMED             
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALBSR20            NO   LIST                                    
         BP    VALBSR10            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1   LIST REQUESTED ?                    
         BE    VALBSR50            YES, ADD BILLING SOURCE ELEMENT              
         B     ERR2MANY            NO,  TOO MANY LISTS                          
*                                                                               
VALBSR10 DS    0H                  BAD  LIST                                    
         ZIC   R1,COLBLOCK         SHOW WHICH BILLING SOURCE LIST IS NG         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),COLBLOCK+12                                            
         B     VALROUTX            SHOW BAD LIST                                
*                                                                               
VALBSR20 DS    0H                                                               
         ZIC   R6,NPARMS           GET  NUMBER OF PARAMETERS                    
         LA    R4,COLBLOCK                                                      
*                                                                               
VALBSR30 DS    0H                                                               
         CLI   0(R4),0             IS   LENGTH OF BILLING SRC = 0 ?             
         BE    ERRBSC              YES, INVALID BILLING SOURCE                  
         CLI   0(R4),LACCOUNT      IS   LENGTH OF BILLING SRC > 12 ?            
         BH    ERRBSC              YES, INVALID BILLING SOURCE                  
         LA    R4,LSCNNTRY(,R4)    NO,  GET NEXT BILLING SOURCE                 
         BCT   R6,VALBSR30         TEST NEXT BILLING SOURCE                     
*                                                                               
VALBSR50 DS    0H                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  CURRENCY                                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALFCR   CLI   FVILEN,3                                                         
         BNH   VALFCR10                                                         
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALROUTX                                                         
*                                                                               
VALFCR10 GOTO1 VALFCUR,FVIFLD      MAKE SURE CODE   IS VALID                    
         OC    RFLDATA(3),SPACES   PAD  WITH SPACES IF LEN  <  3                
         MVI   RFLLN,RFLLNQ+3      FORCE THE LENGTH OF DATA TO 3                
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  METHOD TYPE                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING DEFTABD,R6                                                       
         USING CAHRECD,R2                                                       
VALMTH   ICM   R6,15,DEFENTRY                                                   
         BZ    VALMTHNO                                                         
         TM    DEFTYPE,DEFMTHD     IS METHOD TYPE COLUMN?                       
         BZ    VALMTHNO                                                         
         LA    R2,IOKEY                                                         
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVC   CAHKCPY,CUABIN      COMPANY                                      
         CLI   FVILEN,1                                                         
         BL    VALROUTX                                                         
         BH    VALMTH10            SEE IF INPUT NUMBER OF METHOD                
         TM    FVIIND,FVINUM                                                    
         BZ    VALMTH10            NO SO CHECK METHOD NAME                      
         CLI   FVIFLD,C'1'                                                      
         BL    VALMTHNO                                                         
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKMTHD,FVIFLD                                                  
         XC    CAHKOFC,CAHKOFC                                                  
         B     VALMTH20                                                         
*                                                                               
         USING CMTRECD,R2                                                       
VALMTH10 DS    0H                                                               
         CLI   FVILEN,L'CMTKMTHD   MORE THAN 3 CHARACTERS ?                     
         BH    VALMTHNO            YES, INVALID METHOD                          
         MVI   CMTKSUB,CMTKSUBQ    X'02'                                        
         MVC   CMTKMTHD,FVIFLD                                                  
*                                                                               
VALMTH20 GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   VALMTHNO                                                         
*                                                                               
         USING METELD,R1                                                        
         MVC   SVELEM,APELEM                                                    
         XC    APELEM,APELEM                                                    
         L     R1,AIOAREA3                                                      
         MVI   APELCODE,METELQ     X'82'                                        
         GOTO1 GETEL                                                            
         BNE   VALMTHNO                                                         
         MVC   APELEM,SVELEM       RESTORE ELEMENT                              
*                                                                               
         USING RFLELD,R9                                                        
         LA    R9,APELEM                                                        
         MVC   RFLDATA,METNUM                                                   
         MVI   RFLLN,RFLLNQ+1                                                   
         NI    RFLIND,TURNOFF-RFLXCLD                                           
*                                                                               
VALMTH90 MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*                                                                               
VALMTHNO MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     VALROUTX                                                         
         DROP  R1,R2,R6,R9                                                      
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
*  AUTH STATUS TYPE, R9=A(APELEM)                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALAUTH  CLI   FVILEN,L'AC@ATHED                                                
         BH    ERRINPT                                                          
         ZIC   RF,FVXLEN           GET  LENGTH - 1                              
         EXCLC RF,FVIFLD,AC@BOTH   DEFAULT - BOTH?                              
         BE    VALROUTX                                                         
         MVI   BYTE,AUTHQ                                                       
         EXCLC RF,FVIFLD,AC@ATHED  AUTH?                                        
         BE    VALAUTH4                                                         
         MVI   BYTE,UNAUTHQ                                                     
         EXCLC RF,FVIFLD,AC@UATH   UNAUTH?                                      
         BNE   ERRINPT             NO - ERROR                                   
*                                                                               
VALAUTH4 MVI   ADD_N_EL,YES                                                     
         MVC   RFLDATA(L'BYTE),BYTE                                             
         MVI   RFLLN,RFLLNQ+L'BYTE                                              
         B     VALROUTX                                                         
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*  Yes/No,R9=A(APELEM)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALYN    LA    RF,COLBLOCK                                                      
         CLC   12(1,RF),APNO       DEFAULT IS NO                                
         BE    VALROUTX                                                         
                                                                                
         XR    R4,R4                                                            
         IC    R4,COLDATAH+5                                                    
         LA    R1,L'AC@RSEAM                                                    
         LA    RE,AC@RSEAM+L'AC@RSEAM-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         SHI   RE,1                                                             
         BCT   R1,*-12                                                          
         CR    R4,R1                                                            
         BNE   VALYN02                                                          
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         BE    VALYNN                                                           
         CLC   COLDATA(0),AC@RSEAM                                              
                                                                                
VALYN02  IC    R4,COLDATAH+5                                                    
         LA    R1,L'AC@RSEGR                                                    
         LA    RE,AC@RSEGR+L'AC@RSEGR-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         SHI   RE,1                                                             
         BCT   R1,*-12                                                          
         CR    R4,R1                                                            
         BNE   VALYN04                                                          
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         BE    VALYNN                                                           
         CLC   COLDATA(0),AC@RSEGR                                              
                                                                                
VALYN04  MVI   ADD_N_EL,YES                                                     
         MVI   RFLDATA,YES         MAKE IT 'Y' FOR YES                          
         CLC   12(1,RF),APYES                                                   
         BE    VALROUTX                                                         
         MVC   FVXTRA(1),12(RF)                                                 
         B     ERRINPT                                                          
                                                                                
VALYNN   B     ERRICKY             INCOMPATIBLE WITH THIS KEYWORD               
         DROP  R9                                                               
         EJECT ,                                                                
*&&                                                                             
*=====================================================================*         
*  Validate userfield filtering                                       *         
*=====================================================================*         
                                                                                
EL       USING RFLELD,APELEM                                                    
                                                                                
VALUFD   DS    0H                                                               
         ZIC   R0,FVILEN                                                        
         LA    R4,FVIFLD                                                        
         CLI   FVILEN,4            Minimal length                               
         BL    VALUFNO                                                          
         XC    ELEMENT,ELEMENT                                                  
         XC    AUF,AUF                                                          
         MVI   LASTDELM,0          Last delimiter found, none                   
         ZIC   R1,SCCOMMA                                                       
         LA    R1,ELEMENT(R1)                                                   
         MVI   0(R1),X'FF'                                                      
         MVI   ELEMENT+C'&&',X'FF'      Mark for translate                      
         MVI   EL.RFLEL,RFLELQ          Filter element, X'C5'                   
         MVC   EL.RFLSEQ,CURRCOLN                                               
         MVI   EL.RFLTYPE,RFLUF                                                 
         LA    R9,EL.RFLDATA                                                    
         LR    R1,R4               R4 = Start of data                           
         AR    R1,R0               R1 = End of data, R0 = len remaining         
         ST    R1,AEND                                                          
*                                                                               
         USING UFD,R9                                                           
VALUF10  DS    0H                                                               
         BAS   RE,SCANFOR                                                       
         BZ    VALUF90             No data                                      
*                                                                               
         GOTO1 GETSIGN,2(,R4)      Sign value returned in RF                    
         BNZ   VALUF20             Found sign                                   
         ICM   RE,15,AUF           Do we have one with a sign at all            
         BZ    VALUFNO             No so error                                  
         CLI   LASTDELM,C'&&'      Last delimiter must be OR (Comma)            
         BE    VALUFNO             Error must have UF= first                    
*                                                                               
LAST     USING UFD,RE                                                           
         SR    R1,R1                                                            
         NI    LAST.UFD#OF,TURNOFF-UFDON  Turn off to add to                    
         IC    R1,LAST.UFD#OF             Number of OR data fields              
         AHI   R1,1                                                             
         STC   R1,LAST.UFD#OF                                                   
         OI    LAST.UFD#OF,UFDON   Turn on X'80', avoid comma confict           
         ICM   RF,15,ABREAK                                                     
         CLI   0(RF),C'&&'                                                      
         BNE   *+8                                                              
         OI    LAST.UFDCC,UFDAND   AND found                                    
         DROP  LAST                                                             
*                                                                               
         L     RF,SCANFLN          Load length of data                          
         SHI   RF,1                                                             
         BM    VALUFNO             Invalid, no data                             
         EXMVC RF,0(R9),0(R4)       Move in data                                
         LA    RE,1(RF,R9)                                                      
         MVC   0(1,RE),SCCOMMA                                                  
         LA    R9,1(,RE)           Bump bast comma and add 1                    
         B     VALUF30                                                          
*                                                                               
VALUF20  STC   RF,UFDCC            Save sign value                              
         ICM   RF,15,ABREAK                                                     
         BZ    VALUF22                                                          
         CLI   0(RF),C'&&'         AND found ?                                  
         BNE   VALUF22             No                                           
         OI    UFDCC,UFDAND                                                     
*                                                                               
VALUF22  ST    R9,AUF              Save start of user field filter              
         MVI   LASTDELM,0          Reset, new UF= being processed               
         MVC   UFDCODE,0(R4)       User field code                              
         MVI   UFD#OF,UFDON+1      Set to 1 plus bit on                         
         AHI   R4,3                Bump past UF and sign                        
         L     RF,SCANFLN          Length of data                               
         SHI   RF,3                Adjust length                                
         BNP   VALUFNO                                                          
         TM    UFDCC,X'10'         See if sign is Len 1 or 2                    
         BZ    VALUF26             The X'10' bit is on if len 2 only            
         AHI   R4,1                                                             
         BCTR  RF,0                Adjust length on more                        
*                                                                               
VALUF26  SHI   RF,1                                                             
         BM    VALUFNO             Invalid, no data                             
         EXMVC RF,UFDDATA,0(R4)    Move in data                                 
         LA    RE,UFDDATA+1(RF)                                                 
         MVC   0(1,RE),SCCOMMA                                                  
         LA    R9,1(,RE)           Bump bast comma and add 1                    
*                                                                               
VALUF30  ICM   R4,15,ABREAK                                                     
         BZ    VALUF40             Finished                                     
         AHI   R4,1                Bump past delimiter                          
         B     VALUF10                                                          
*                                                                               
VALUF40  LA    RE,EL.RFLELD                                                     
         SR    R9,RE                                                            
         BCTR  R9,0                One less for comma on the end                
         STC   R9,EL.RFLLN         Length of element                            
         MVI   ADD_N_EL,YES                                                     
         B     VALUF90                                                          
*                                                                               
VALUFNO  L     RF,SCANFLN                                                       
         BCTR  RF,0                                                             
         EXMVC RF,FVXTRA,0(R4)                                                  
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
*                                                                               
VALUF90  B     VALROUTX                                                         
         DROP  EL                                                               
         EJECT ,                                                                
*=====================================================================*         
*  Scan of what characters you set in ELEMENT                         *         
*    Set    -       R4       = Start of data                          *         
*                   AEND     = End of data                            *         
*    Return -       R1       = Length of data                         *         
*                   R4       = Start of data  (LTRIM & RTRIM)         *         
*                   ABREAK   = Location of delimiter or zero          *         
*                   SCANFLN  = Full word, length of data              *         
*                   LASTDELM = Previous delimiater scanned            *         
*    CC     -       BZ if length is zero                              *         
*=====================================================================*         
SCANFOR  ST    RE,STRE                                                          
         ST    R2,SVR2                                                          
         L     RF,AEND                                                          
         SR    R1,R1                                                            
         SR    RF,R4                                                            
         BNP   SCANFORX                                                         
         SHI   RF,1                                                             
         EX    RF,SCANDATA                                                      
         BNZ   SCANFOR4            No delimiter found                           
         ST    R1,ABREAK                                                        
         LA    R1,1(,RF)           R1 = Length of data                          
         B     SCANFOR6                                                         
*                                                                               
SCANFOR4 ICM   RF,15,ABREAK        Last comma or c'&'                           
         BZ    *+10                                                             
         MVC   LASTDELM,0(RF)      Save off last delimiter                      
         ST    R1,ABREAK                                                        
         SR    R1,R4               R1 = Length of data                          
*                                                                               
SCANFOR6 BAS   RE,TRIMLEFT                                                      
         BZ    SCANFORX            No data                                      
         BAS   RE,TRIMRGHT                                                      
*                                                                               
SCANFORX ST    R1,SCANFLN                                                       
         LTR   R1,R1                                                            
         L     RE,STRE                                                          
         L     R2,SVR2                                                          
         BR    RE                                                               
                                                                                
SCANDATA TRT   0(0,R4),ELEMENT    Scan for C'&' or C','                         
         EJECT ,                                                                
*=====================================================================*         
*  Find first charcter, remove leading blanks                         *         
*       R1 = Length of data to trim for                               *         
*       R4 = Current location in FVIFLD                               *         
*=====================================================================*         
                                                                                
TRIMLEFT CLI   0(R4),C' '                                                       
         BH    TRIMLXIT                                                         
         LA    R4,1(,R4)                                                        
         BCT   R1,TRIMLEFT                                                      
*                                                                               
TRIMLXIT LTR   R1,R1                                                            
         BR    RE                                                               
                                                                                
*=====================================================================*         
*  Find last charcter, remove trialing blanks                         *         
*       R1 = Length of data, R1 returns new length                    *         
*       R4 = Current location in FVIFLD                               *         
*=====================================================================*         
                                                                                
TRIMRGHT ST    RE,SVRE                                                          
         LA    RE,0(R1,R4)         Point to end of data                         
         BCTR  RE,0                Point to last character of data              
*                                                                               
TRIMR02  CLI   0(RE),C' '          Is it blank                                  
         BH    TRIMRXIT            No                                           
         SHI   RE,1                Bump backwards in data                       
         BCT   R1,TRIMR02                                                       
*                                                                               
TRIMRXIT LTR   R1,R1                                                            
         L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT ,                                                                
*=====================================================================*         
*  Find sign being used                                               *         
*       R1 = location passed of where to examine for sign             *         
*       RF = Equivalent ASM branch code                               *         
*=====================================================================*         
GETSIGN  LA    RF,NEQ              Not equal                                    
         CLC   0(2,R1),=C'<>'                                                   
         BE    GETSIGN9                                                         
*                                                                               
         LA    RF,LTE              Less than or equal                           
         CLC   0(2,R1),=C'<='                                                   
         BE    GETSIGN9                                                         
         CLC   0(2,R1),=C'=<'                                                   
         BE    GETSIGN9                                                         
*                                                                               
         LA    RF,GTE              Greater than or equal                        
         CLC   0(2,R1),=C'>='                                                   
         BE    GETSIGN9                                                         
         CLC   0(2,R1),=C'=>'                                                   
         BE    GETSIGN9                                                         
*                                                                               
         LA    RF,EQ               Equal                                        
         CLI   0(R1),C'='                                                       
         BE    GETSIGN9                                                         
         LA    RF,LT               Less than                                    
         CLI   0(R1),C'<'                                                       
         BE    GETSIGN9                                                         
         LA    RF,GT               Greater than                                 
         CLI   0(R1),C'>'                                                       
         BE    GETSIGN9                                                         
         SR    RF,RF                                                            
*                                                                               
GETSIGN9 LTR   RF,RF                                                            
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE AN ACCOUNT                                                *         
***********************************************************************         
         SPACE 1                                                                
VALACT   DS    0H                                                               
         GOTO1 VALLIST,APPARM,(LDGLISTN,LDGLIST),                      X        
               (COLBLOCK,COLBLOCK+12),1                                         
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALACT05            NO   LIST                                    
         BP    VALACT00            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALACT50            YES, ADD SPECIFIC ACCOUNT ELEMENT            
         B     ERR2MANY            NO,  TOO MANY LISTS                          
*                                                                               
VALACT00 DS    0H                  BAD  LIST                                    
         ZIC   R1,COLBLOCK         SHOW WHICH ACCOUNT LIST IS BAD               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),COLBLOCK+12                                            
         B     VALROUTX            SHOW BAD LIST                                
*                                                                               
VALACT05 DS    0H                  VALIDATE ACCOUNT(S)                          
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,COLBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ACTRECD,R2                                                       
VALACT10 DS    0H                                                               
         CLI   0(R4),LULACNT       IS   THE LENGTH OF ACCOUNT > 14 ?            
         BH    ERRACCT             YES, INVALID ACCOUNT                         
         SR    R3,R3                                                            
         IC    R3,LDGLISTN                                                      
         LA    R9,LDGLIST                                                       
*                                                                               
VALACT20 MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         MVC   ACTKUNT(2),0(R9)                                                 
         MVC   ACTKACT(12),12(R4)  ACCOUNT IS IN COLBLOCK                       
*                                                                               
         MVI   CHECKUL,C'N'        WILDCARD WILL NOT CHECK U/L                  
         CLI   APREPJCL,REPJCLP    FORCE U/L FOR PAY                            
         BE    VALACT21                                                         
         CLI   APREPJCL,REPJCLG    FORCE U/L FOR GENERAL LEDGER                 
         BE    VALACT21                                                         
         CLI   APREPJCL,REPJCLX    FORCE U/L FOR EXP                            
         BNE   VALACT30                                                         
VALACT21 CLI   0(R4),LUNLG         HAS TO HAVE U/L AT MINIMUM                   
         BL    ERRACCT                                                          
*                                                                               
VALACT22 CLC   12(LUNLG,R4),0(R9)  VALID UNIT/LEDGER ?                          
         BE    VALACT25            YES, CONTINUE                                
         LA    R9,2(,R9)                                                        
         BCT   R3,VALACT22                                                      
         B     ERRLDGR                                                          
*                                                                               
VALACT25 MVC   ACTKUNT(14),12(R4)                                               
         LA    R3,1                ONLY CHECK THE ACCOUNT ONCE                  
         MVI   CHECKUL,C'Y'        WILDCARD WILL CHECK U/L                      
         B     VALACT35            VALIDATE THIS RECORD                         
*                                                                               
VALACT30 DS    0H                                                               
         CLI   0(R4),LACCOUNT      IS   THE LENGTH OF ACCOUNT > 12 ?            
         BH    ERRACCT             YES, INVALID ACCOUNT                         
*                                                                               
VALACT35 DS    0H                                                               
         GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALACT40                                                         
*                                                                               
         USING REPFD,RF                                                         
         L     RF,AREPFD           ->   REPFD DSECT                             
         TM    REPFIND2,REPFWLD    IS   WILDCARD VALID ?                        
         BZ    ERRACCT             NO,  ERROR                                   
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         BAS   RE,WILDCARD         OK, IF WILDCARD PRESENT                      
         BE    VALACT40                                                         
         LA    R9,2(,R9)                                                        
         BCT   R3,VALACT20         GO THROUGH ALL VALID U/L'S                   
         B     ERRACCT                                                          
         DROP  RF                  KEEP IT CLEAN                                
*                                                                               
VALACT40 LA    R4,LSCNNTRY(,R4)    NEXT COLBLOCK IN COLBLOCK                    
         BCT   R6,VALACT10                                                      
*                                                                               
VALACT50 DS    0H                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
* VALIDATE CONTRA ACCOUNTS                                            *         
***********************************************************************         
         SPACE 1                                                                
VALCNT   DS    0H                                                               
         MVC   APWORK(2),SPACES                                                 
         MVC   CNTKYWUL,SPACES                                                  
         L     R1,=A(CNTRKYW)      List of keywords + valid ledger              
         A     R1,APRELO                                                        
VALCNT01 OC    0(2,R1),0(R1)       End of table ?                               
         BZ    VALCNT07            Yes                                          
         CLC   SVKYWDD#,0(R1)      Special keywords data dictionary #           
         BE    VALCNT02                                                         
         LA    R1,CNTRKYWQ(,R1)                                                 
         B     VALCNT01                                                         
*                                                                               
VALCNT02 MVC   APWORK,2(R1)        Move in ledger that is valid                 
         MVC   CNTKYWUL,2(R1)      Save off value                               
*                                                                               
VALCNT07 GOTO1 VALLIST,APPARM,APWORK,(COLBLOCK,COLBLOCK+12),1                   
         LTR   RF,RF               Test the return code                         
         BM    VALCNT09            No   list                                    
         BP    VALCNT08            Bad  list                                    
*                                  good list                                    
         CLI   NPARMS,1            ONLY 1   LIST REQUESTED ?                    
         BE    VALCNT60            YES, ADD CONTRA ACCOUNT ELEMENT              
         B     ERR2MANY            NO,  TOO MANY LISTS                          
*                                                                               
VALCNT08 DS    0H                  BAD  LIST                                    
         ZIC   R1,COLBLOCK         SHOW WHICH CONTRA LIST IS BAD                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),COLBLOCK+12                                            
         B     VALROUTX            SHOW BAD LIST                                
*                                                                               
VALCNT09 DS    0H                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,COLBLOCK                                                      
         LA    R2,IOKEY                                                         
         MVC   SVKEY,IOKEY                                                      
*                                                                               
         USING ACTRECD,R2                                                       
VALCNT10 DS    0H                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN      COMPANY CODE                                 
         CLI   0(R4),LULACNT       IS THE LENGTH OF ACCOUNT > 14 ?              
         BH    ERRCNT              YES                                          
         CLI   0(R4),2             IS THE LENGTH OF ACCOUNT <  2 ?              
         BL    ERRCNT              YES                                          
         MVC   ACTKUNT(14),12(R4)  ACCOUNT IS IN COLBLOCK                       
         CLC   CNTKYWUL,SPACES                                                  
         BE    *+10                Ignore checking keywords contra              
         CLC   CNTKYWUL,12(R4)     Keyword only supports this conra             
         BNE   ERRCNT                                                           
*                                                                               
         USING CNTRTBLD,RF         CONTRA ACCOUNT TABLE                         
         CLI   APREPJCL,REPJCLV    IS IT PROD TYPE ?                            
         BE    VALCNT40            YES, JUST READ FOR ACCOUNT                   
         CLI   APREPJCL,REPJCLX    IS IT EXPENSE TYPE ?                         
         BE    VALCNT40                                                         
         CLI   APREPJCL,REPJCLP    IS IT PAYABLES TYPE ?                        
         BE    VALCNT40                                                         
         CLI   APREPJCL,REPJCLB    IS IT CASH TYPE ?                            
         BE    VALCNT40                                                         
         CLI   APREPJCL,REPJCLG    IS IT GENERAL LEDGER TYPE ?                  
         BE    VALCNT40                                                         
*                                                                               
         L     RF,ACCNTTAB         ADDR OF CONTRA TABLE                         
VALCNT20 CLI   0(RF),EOT           END OF CONTRA TABLE ?                        
         BE    ERRCNT              YES, INVALID CONTRA ACCOUNT                  
         CLC   APREPNUM,CNTRRTYP   MATCHING REPORT TYPE ?                       
         BNE   *+10                NO, TRY NEXT ENTRY                           
         CLC   CNTRUNLG,ACTKUNT    MATCHING UNIT/LEDGER ?                       
         BE    VALCNT40            YES, GOOD CONTRA ACCOUNT                     
         LA    RF,CNTRLNQ(,RF)     NEXT TABLE ENTRY                             
         B     VALCNT20                                                         
         DROP  RF                                                               
*                                                                               
         USING REPFD,RF                                                         
VALCNT40 GOTO1 AIO,IORD+IOACCFIL+IO2                                            
         BE    VALCNT50            YES, SKIP                                    
         L     RF,AREPFD           ->   REPFD DSECT                             
         TM    REPFIND2,REPFWLD    IS   WILDCARD VALID ?                        
         BZ    ERRCNT              NO,  ERROR                                   
         MVI   CHECKUL,C'U'        WILCARD WILL CHECK U/L                       
         MVC   FVMSGNO,=AL2(FVFOK) CLEAR ERROR MESSAGE                          
         BAS   RE,WILDCARD         ANY  WILDCARD CHARACTERS ?                   
         BNE   ERRCNT              NO,  INVALID CONTRA ACCOUNT                  
         DROP  RF                                                               
*                                                                               
VALCNT50 DS    0H                                                               
         LA    R4,LSCNNTRY(,R4)    NEXT COLBLOCK IN COLBLOCK                    
         BCT   R6,VALCNT10                                                      
*                                                                               
VALCNT60 DS    0H                  ADD  CONTRA ACCOUNT ELEMENT                  
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE OFFICE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R4                                                        
VALOFF   DS    0H                                                               
         MVI   BYTE,NO             ASSUME INCLUDE                               
         LA    R4,APELEM           ->     ELEMENT                               
         TM    RFLIND,RFLXCLD      EXCLUDE     FILTER ?                         
         BZ    *+8                 NO,    SKIP                                  
         MVI   BYTE,YES            SAY    EXCLUDE                               
         DROP  R4                                                               
*                                                                               
         ZIC   R4,NPARMS           NUMBER OF   OFFICES                          
         GOTO1 VOFFICE,APPARM,('VOFTYPEB',COLBLOCK),(BYTE,(R4))                 
         BE    VALOFF40            GOOD   INPUT                                 
*                                  NO     OFFICES   INCLUDED  ERROR ?           
         CLC   FVMSGNO,=AL2(ACENOOFF)                                           
         BNE   XIT                 NO,    EXIT                                  
         MVC   FVMSGNO,=AL2(FVFOK) YES,   IGNORE    THIS ERROR                  
*                                                                               
VALOFF40 DS    0H                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BUDGET                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BUDRECD,R2                                                       
         USING RFLELD,R9                                                        
VALBUD   DS    0H                                                               
         CLI   NPARMS,1            MORE THAN ONE BUDGET                         
         BH    ERR2MANY            YES, TOO MANY FIELDS                         
         CLI   FVILEN,0            ANY  INPUT ?                                 
         BE    ERRBUDG             NO,  INVALID BUDGET                          
*                                                                               
         MVC   SVKEY,IOKEY                                                      
         MVC   SVELEM,APELEM                                                    
         XC    APELEM,APELEM                                                    
*                                                                               
         LA    R2,IOKEY                                                         
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN                                                   
         MVC   BUDKCOD,FVIFLD                                                   
         L     R2,AIOAREA2                                                      
         GOTO1 AIO,IO2+IOACCFIL+IOHI                                            
         CLC   BUDKCOD,FVIFLD      Did we find this budget code ?               
         BNE   VALBUD10            No                                           
         MVC   APHALF,BUDKNO2      Yes                                          
         B     VALBUD50                                                         
*                                                                               
VALBUD10 TM    FVIIND,FVINUM       Is value typed by user a number ?            
         BZ    ERRBUDG             No, so invalid budget                        
         CLI   FVILEN,LBUDG#       Yes, is the length > 5                       
         BH    ERRBUDG             Yes, invalid budget                          
         ZIC   RF,FVXLEN           convert number                               
         EX    RF,*+8              Pack into APDUB                              
         B     *+10                                                             
         PACK  APDUB,FVIFLD(0)                                                  
*                                                                               
         CVB   RF,APDUB            Convert to binary                            
         C     RF,=A(32767)        Is max value exceeded ?                      
         BH    ERRBUDG             Yes, invalid budget                          
         STH   RF,APHALF           Save budget number                           
         MVC   BUDKNO1,APHALF      Insert into key                              
         L     R2,AIOAREA2         Read for budget                              
         GOTO1 AIO,IO2+IOACCFIL+IOHI                                            
         CLC   BUDKNO1,APHALF      Do we find the one we wanted ?               
         BNE   ERRBUDG             No, invalid budget                           
         SR    RF,RF                                                            
         IC    RF,0(,R3)           Update input field                           
         AHI   RF,-9                                                            
         TM    1(R3),FVAXTND                                                    
         BZ    *+8                                                              
         AHI   RF,-8                                                            
         EXMVC RF,8(R3),BUDKCOD    Move in name instead of number               
         OI    6(R3),FVOXMT        Retransmit field                             
         B     VALBUD50                                                         
*                                                                               
         USING BIVELD,R1                                                        
VALBUD50 DS    0H                                                               
         LR    R1,R2               R1 = Load budget record in AIO2              
         MVI   APELCODE,BIVELQ                                                  
         GOTO1 GETEL                                                            
         BNE   ERRBUDGA            Budget type incompatible w/account           
*                                                                               
VALBUD60 DS    0H                                                               
         CLC   BIVAUNT(2),APREPUL                                               
         BE    VALBUD70                                                         
         GOTO1 NEXTEL                                                           
         BE    VALBUD60                                                         
         B     ERRBUDGA            Budget type incompatible w/account           
*                                                                               
VALBUD70 DS    0H                                                               
         MVC   IOKEY,SVKEY                                                      
         MVC   APELEM,SVELEM                                                    
         MVC   RFLDATA(L'BUDKNO1),APHALF   Store budget number                  
         LA    RF,RFLLNQ+L'BUDKNO1                                              
         STC   RF,RFLLN                    Set length of element                
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         DROP  R1,R2,R9                                                         
         EJECT ,                                                                
***********************************************************************         
*  TRANSACTION TYPE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALTTY   DS    0H                                                               
         LA    R9,APELEM                                                        
         ZIC   R1,NPARMS                                                        
         LA    RF,RFLLNQ(,R1)                                                   
         STC   RF,RFLLN                                                         
         GOTO1 CNVTTYPE,APPARM,(C'N',COLBLOCK),(NPARMS,RFLDATA)                 
         BNE   VALROUTX            ERROR, EXIT                                  
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*  TIME TYPE                                                          *         
***********************************************************************         
         SPACE 1                                                                
VALTYT   CLI   APREPJCL,REPJCLV    PROD                                         
         BE    *+8                                                              
         CLI   APREPJCL,REPJCL1    PERSON                                       
         BNE   VALROUTX                                                         
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R1,COLBLOCK                                                      
*                                                                               
VALTYT10 CLI   0(R1),1                                                          
         BH    ERRTYPE                                                          
         CLI   12(R1),C'B'         CHECK VALID TYPE OF TIME                     
         BE    *+8                                                              
         CLI   12(R1),C'N'                                                      
         BE    *+8                                                              
         CLI   12(R1),C'R'                                                      
         BNE   ERRTYPE                                                          
         LA    R1,LSCNNTRY(,R1)                                                 
         BCT   R6,VALTYT10                                                      
*                                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
*  BRANDOCEAN INVOICE STATUS                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALBNVS  LA    R9,APELEM                                                        
         CLI   APREPJCL,REPJCLV    PROD                                         
         BE    *+8                                                              
         CLI   APREPJCL,REPJCLX    EXPENSES                                     
         BE    *+8                                                              
         CLI   APREPJCL,REPJCLP    CREDITORS                                    
         BNE   VALROUTX                                                         
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R1,COLBLOCK                                                      
         MVI   RFLDATA,0                                                        
*                                                                               
VALBNV10 CLI   0(R1),1                                                          
         BH    ERRTYPE                                                          
         CLC   12(1,R1),AC@SUBM    SUBMITTED                                    
         BNE   VALBNV12                                                         
         TM    RFLDATA,BNVSUBQ                                                  
         BNZ   ERRINPT             DUPLICATE                                    
         OI    RFLDATA,BNVSUBQ                                                  
         B     VALBNV18                                                         
*                                                                               
VALBNV12 CLC   12(1,R1),AC@PAPP    PART-APPROVED                                
         BNE   VALBNV14                                                         
         TM    RFLDATA,BNVPAPQ                                                  
         BNZ   ERRINPT             DUPLICATE                                    
         OI    RFLDATA,BNVPAPQ                                                  
         B     VALBNV18                                                         
*                                                                               
VALBNV14 CLC   12(1,R1),AC@APPR    APPROVED                                     
         BNE   VALBNV16                                                         
         TM    RFLDATA,BNVAPPQ                                                  
         BNZ   ERRINPT             DUPLICATE                                    
         OI    RFLDATA,BNVAPPQ                                                  
         B     VALBNV18                                                         
*                                                                               
VALBNV16 CLC   12(1,R1),AC@REJE    REJECTED                                     
         BNE   ERRINPT                                                          
         TM    RFLDATA,BNVREJQ                                                  
         BNZ   ERRINPT             DUPLICATE                                    
         OI    RFLDATA,BNVREJQ                                                  
*                                                                               
VALBNV18 LA    R1,LSCNNTRY(,R1)                                                 
         BCT   R6,VALBNV10                                                      
         MVI   RFLTYPE,RFLBNVS                                                  
         MVI   RFLLN,RFLLNQ+1                                                   
*                                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
*  TIME STATUS, R9=A(APELEM)                                          *         
***********************************************************************         
         SPACE 1                                                                
VALTST   CLI   APREPJCL,REPJCL1    PERSON                                       
         BE    *+12                                                             
         CLI   APREPJCL,REPJCLV    PROD                                         
         BNE   VALROUTX                                                         
*                                                                               
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R1,COLBLOCK                                                      
*                                                                               
VALTST10 CLI   0(R1),1                                                          
         BH    ERRTSTA                                                          
*                                  CHECK VALID TIME STATUS                      
         USING RFLELD,R9                                                        
         CLI   APREPJCL,REPJCL1    PERSON                                       
         BNE   VALTST18                                                         
         CLI   12(R1),C'R'         REJECT TIME                                  
         BE    VALTST20                                                         
         CLI   12(R1),C'U'         Update time from cost TUP profile            
         BNE   VALTST18                                                         
         CLC   DEFTIMST,SPACES     Any default time status                      
         BNH   VALTST40            No - don't add the element                   
         MVC   RFLDATA(L'DEFTIMST),DEFTIMST                                     
         LHI   RF,RFLLNQ+L'DEFTIMST                                             
         LA    RE,DEFTIMST+L'DEFTIMST-1                                         
VALTST14 CLI   0(RE),C' '                                                       
         BH    *+10                                                             
         BCTR  RE,0                                                             
         BCT   RF,VALTST14                                                      
         STC   RF,RFLLN                                                         
         B     VALTST30                                                         
         DROP  R9                                                               
*                                                                               
VALTST18 CLI   12(R1),C'F'         FULLY APPROVED TIME                          
         BE    VALTST20                                                         
         CLI   12(R1),C'P'         PART APPROVED                                
         BNE   *+16                                                             
         TM    PTMSTAT,PTMSPART                                                 
         BZ    ERRTSTP                                                          
         B     VALTST20                                                         
         CLI   12(R1),C'S'         SUBMITTED TIME                               
         BNE   *+16                                                             
         TM    PTMSTAT,PTMSSUBT                                                 
         BZ    ERRTSTP                                                          
         B     VALTST20                                                         
         CLI   12(R1),C'I'         TIME IN PROCESS                              
         BNE   *+16                                                             
         TM    PTMSTAT,PTMSSAVT                                                 
         BZ    ERRTSTP                                                          
         B     VALTST20                                                         
         B     ERRTSTA             INVALID TIME STATUS                          
*                                                                               
VALTST20 LA    R1,LSCNNTRY(,R1)                                                 
         BCT   R6,VALTST10                                                      
*                                                                               
VALTST30 MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*                                                                               
VALTST40 MVI   ADD_N_EL,NO                                                      
         B     VALROUTX                                                         
         EJECT ,                                                                
*&&US                                                                           
***********************************************************************         
*  APPROVAL METHOD, R9=A(APELEM)                                                
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALAPMT  CLI   FVILEN,L'AC@AAPP                                                 
         BH    ERRINPT                                                          
         ZIC   RF,FVXLEN           GET  LENGTH - 1                              
         MVI   BYTE2,GDAAPPAA                                                   
         EXCLC RF,FVIFLD,AC@AAPP   Auto Approve?                                
         BE    VALAPMT4                                                         
         MVI   BYTE2,GDAAPPMK                                                   
         EXCLC RF,FVIFLD,AC@MAPP   Marker?                                      
         BNE   ERRINPT             NO - ERROR                                   
*                                                                               
VALAPMT4 MVI   ADD_N_EL,YES                                                     
         MVC   RFLDATA(L'BYTE2),BYTE2                                           
         MVI   RFLLN,RFLLNQ+L'BYTE2                                             
         B     VALROUTX                                                         
         DROP  R9                                                               
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
*  WORKCODE TYPE                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALWCT   CLI   APREPJCL,REPJCLV    PRODUCTION ?                                 
         BNE   VALROUTX            NO                                           
         MVC   XWCTYPES,WCTYPES    MAKE LOCAL COPY OF WCTYPES                   
         MVC   FVMSGNO,=AL2(ACEWKCDE)                                           
         ZIC   R1,NPARMS                                                        
         LA    R6,COLBLOCK                                                      
         CLI   0(R6),2             INPUT TOO LONG                               
         BH    VALWC99                                                          
*                                                                               
VALWC10  LA    RF,XWCTYPES         USE  THE LOCAL COPY OF WCTYTPES              
*                                                                               
VALWC20  CLI   0(RF),X'FF'         EOT?                                         
         BE    VALWC99                                                          
         CLC   0(1,RF),12(R6)      MATCHED                                      
         BE    VALWC40                                                          
         LA    RF,2(,RF)           NEXT CODE                                    
         B     VALWC20                                                          
*                                                                               
VALWC40  CLI   1(RF),C'Y'          USED?                                        
         BE    VALWC99                                                          
         MVI   1(RF),C'Y'                                                       
         LA    R6,LSCNNTRY(,R6)                                                 
         BCT   R1,VALWC10                                                       
*                                                                               
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVI   ADD_N_EL,YES                                                     
*                                                                               
VALWC99  CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    VALROUTX                                                         
         MVC   FVXTRA(12),12(R6)                                                
         B     VALROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  WORKCODE / TASK                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALTSK   DS    0H                                                               
         GOTO1 VALLIST,APPARM,SPACES,                                  X        
               (COLBLOCK,COLBLOCK+12),LITTWRK                                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALTSK20            NO   LIST                                    
         BP    VALTSK10            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALTSK50            YES, ADD SPECIFIC WORK CODE LIST             
         B     ERR2MANY            NO,  TOO MANY LISTS                          
*                                                                               
VALTSK10 DS    0H                  BAD  LIST                                    
         ZIC   R1,COLBLOCK         SHOW WHICH WORK CODE LIST IS BAD             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),COLBLOCK+12                                            
         B     VALROUTX            SHOW BAD LIST                                
*                                                                               
VALTSK20 DS    0H                  VALIDATE WORK CODES(S)                       
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,COLBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING WCORECD,R2                                                       
VALTSK30 DS    0H                                                               
         CLI   0(R4),L'WCOKWRK     IS   THE LENGTH > 2 ?                        
         BH    VALTSKER            YES, INVALID WORKCODE                        
         CLC   12(2,R4),=C'99'     SPECIAL CASE                                 
         BE    VALTSK40            OK AS IS                                     
*        CLC   12(2,R4),=C'**'     SPECIAL CASE                                 
*        BE    VALTSK40                                                         
         MVC   WCOKEY,SPACES                                                    
         MVI   WCOKTYP,WCOKTYPQ    X'0A'                                        
         MVC   WCOKCPY,CUABIN      COMPANY                                      
         MVC   WCOKUNT(2),=C'SJ'   UNIT/LEDGER FOR TASK,4/5/95                  
         MVC   WCOKWRK,12(R4)      WORKCODE                                     
         GOTO1 AIO,IORD+IOACCDIR+IO2                                            
         BNE   VALTSKER                                                         
*                                                                               
VALTSK40 DS    0H                                                               
         LA    R4,LSCNNTRY(,R4)                                                 
         BCT   R6,VALTSK30                                                      
*                                                                               
VALTSK50 DS    0H                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*                                                                               
VALTSKER DS    0H                                                               
         MVC   FVMSGNO,=AL2(ACEWKCDE)                                           
         MVC   FVXTRA(20),12(R4)                                                
         B     VALROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  WILDCARD                                                           *         
***********************************************************************         
         SPACE 1                                                                
WILDCARD NTR1                                                                   
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)          LENGTH                                       
         LA    RF,12(,R4)          START OF ACCOUNT                             
*                                                                               
         CLI   CHECKUL,C'U'        CHECK UNIT/LEDGER                            
         BNE   WILD200                                                          
*                                                                               
         CLI   0(R4),2             IF JUST U/L THEN NO GOOD                     
         BE    WILDNO                                                           
*                                                                               
         LA    R2,2                JUST CHECKING U/L                            
WILD100  CLI   0(RF),C'?'          U/L HAS WILDCARD?                            
         BE    WILDNO              WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHAR                                    
         BCTR  R1,0                                                             
         BCT   R2,WILD100                                                       
*                                                                               
WILD200  CLI   0(RF),C'?'          CONTRA U/L HAS WILDCARD?                     
         BE    WILDYES             WILDCARD INVALID                             
         LA    RF,1(,RF)           NEXT CHAR                                    
         BCT   R1,WILD200                                                       
*                                                                               
WILDNO   LTR   RE,RE               WILDCARD INVALID / U/L HAS WILDCARD          
         B     XIT                                                              
*                                                                               
         USING RFLELD,R1                                                        
WILDYES  LA    R1,APELEM                                                        
         OI    RFLIND,RFLWILD      TURN ON THE BIT                              
         CR    RE,RE               WILDCARD IS GOOD                             
         B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
*&&UK                                                                           
*=====================================================================*         
*  Validate Data name                                                 *         
*=====================================================================*         
         SPACE 1                                                                
                                                                                
VALXDAN  DS    0H                                                               
         CLI   COLBKLNG+12,C'*'    DISALLOW +/- LISTS                           
         BNE   VALXN05                                                          
         MVC   FVMSGNO,=AL2(FVFNOTV)                                            
                                                                                
         ZIC   R1,COLBKLNG                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),COLBKLNG+12                                            
         B     VALROUTX            SHOW BAD LIST                                
                                                                                
VALXN05  MVC   BYTE,4(R3)          SAVE FLDIIND INFO                            
         LA    R3,COLBKLNG                                                      
         XR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         XR    R4,R4                                                            
         IC    R4,0(R3)            length of entry                              
         MVC   SVAPKEYH,SPACES                                                  
         LA    RE,SVAPKEYH                                                      
         ST    RE,SVRE                                                          
         AHI   RE,L'SVAPKEYH                                                    
         ST    RE,SVR2                                                          
                                                                                
         USING XDFRECD,R2                                                       
VALXN10  LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   XDFKTYP,XDFKTYPQ                                                 
         MVI   XDFKSUB,XDFKSUBQ                                                 
         MVC   XDFKCPY,CUABIN                                                   
         MVC   SVKEY,IOKEY                                                      
         GOTO1 AIO,IOHI+IOACCDIR+IO2                                            
         BE    VALXN30                                                          
         B     VALXN66                                                          
                                                                                
VALXN20  MVC   IOKEY,SVKEY                                                      
         GOTO1 AIO,IORD+IOACCDIR+IO2                                            
         BNE   VALXN66                                                          
         GOTO1 AIO,IOSQ+IOACCDIR+IO2                                            
         BNE   VALXN66                                                          
                                                                                
VALXN30  CLC   SVKEY(XDFKREM-XDFRECD),IOKEY                                     
         BNE   VALXDERR                                                         
         GOTO1 AIO,IOGET+IOACCMST+IO2                                           
         BNE   VALXN66                                                          
         MVC   SVKEY,IOKEY                                                      
                                                                                
         L     R2,AIOAREA2                                                      
         LA    R1,XDFRFST                                                       
                                                                                
         USING XDFELD,R1                                                        
         XR    RE,RE                                                            
VALXN40  CLI   0(R1),0                                                          
         BE    VALXN20                                                          
         CLI   0(R1),XDFELQ                                                     
         BE    VALXN60                                                          
VALXN50  IC    RE,1(R1)                                                         
         AR    R1,RE                                                            
         B     VALXN40                                                          
                                                                                
VALXN60  XR    RE,RE                                                            
         IC    RE,XDFLN                                                         
         SHI   RE,XDFLN1Q                                                       
         CR    RE,R4                                                            
         BNE   VALXN50                                                          
         XC    APWORK,APWORK                                                    
         SHI   R4,1                                                             
         EXMVC R4,APWORK,XDFNAME                                                
         OC    APWORK,SPACES       (CAPITALIZE)                                 
         EXCLC R4,12(R3),APWORK                                                 
         BE    *+12                                                             
         AHI   R4,1                                                             
         B     VALXN50                                                          
                                                                                
         L     RE,SVRE                                                          
         L     R0,SVR2                                                          
         LR    RF,RE                                                            
         AR    RF,R4                                                            
         CR    RF,R0                                                            
         BL    VALXN62                                                          
         SR    R0,RE                                                            
         BZ    VALXN64                                                          
         LR    R4,R0                                                            
         SHI   R4,1                                                             
VALXN62  EXMVC R4,0(RE),XDFNAME                                                 
         AHI   R4,2                                                             
         AR    RE,R4                                                            
         ST    RE,SVRE                                                          
                                                                                
VALXN64  LA    R3,LSCNNLNG(R3)                                                  
         IC    R4,0(R3)                                                         
         BCT   R6,VALXN10          START OVER                                   
         DROP  R2                                                               
                                                                                
VALXN66  LTR   R6,R6                                                            
         BNZ   VALROUTX                                                         
                                                                                
* CHANGE DEFAULT HEADINGS TOO                                                   
         TM    BYTE,FVITHIS                                                     
         BZ    VALXY                                                            
                                                                                
         L     R6,AIOAREA1                                                      
         AH    R6,DATADISP                                                      
         XC    SVELEM,SVELEM                                                    
         XR    RE,RE                                                            
         USING RCLELD,R6                                                        
VALXN70  CLI   RCLEL,0                                                          
         BE    VALXY                                                            
         CLI   RCLEL,RCLELQ                                                     
         BNE   VALXN80                                                          
         CLC   RCLSEQ,CURRCOLN                                                  
         BE    *+14                                                             
VALXN80  IC    RE,RCLLN                                                         
         AR    R6,RE                                                            
         B     VALXN70                                                          
                                                                                
         IC    RE,RCLLN                                                         
         SHI   RE,1                                                             
         EXMVC RE,SVELEM,RCLELD                                                 
         MVC   BYTE,APELCODE       SAVE APELCODE                                
                                                                                
         MVC   ELEMSEQ,CURRCOLN                                                 
         MVI   APELCODE,RCLELQ                                                  
         L     R2,AIOAREA1                                                      
         GOTO1 DELEL,(R2)                                                       
                                                                                
         LA    R6,SVELEM                                                        
                                                                                
         XC    SVAPKEYH,APKEYHD    SWAP THE FIELDS AROUND                       
         XC    APKEYHD,SVAPKEYH                                                 
         XC    SVAPKEYH,APKEYHD                                                 
                                                                                
         GOTO1 MKHEAD,APPARM,(R6)  MAKE HEADLINES                               
                                                                                
         XC    APKEYHD,SVAPKEYH    SWAP THE FIELDS BACK AROUND                  
         XC    SVAPKEYH,APKEYHD                                                 
         XC    APKEYHD,SVAPKEYH                                                 
                                                                                
         XC    SVELEM,APELEM                                                    
         XC    APELEM,SVELEM                                                    
         XC    SVELEM,APELEM                                                    
                                                                                
         GOTO1 ADDEL,(R2)                                                       
                                                                                
         XC    SVELEM,APELEM                                                    
         XC    APELEM,SVELEM                                                    
         XC    SVELEM,APELEM                                                    
                                                                                
         MVC   APELCODE,BYTE       RESTORE CODE                                 
                                                                                
VALXY    MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
                                                                                
VALXDERR DS    0H                                                               
         MVC   FVMSGNO,=AL2(ACEINXDN)                                           
         B     VALROUTX                                                         
         DROP  R6                                                               
         EJECT ,                                                                
*=====================================================================*         
*  Validate Data value, R9=A(APELEM)                                  *         
*=====================================================================*         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALXDAV  DS    0H                                                               
         L     R2,AIOAREA1                                                      
         AH    R2,DATADISP                                                      
         XR    RE,RE                                                            
ES       USING RFLELD,R2                                                        
VALXDV00 CLI   ES.RFLEL,0                                                       
         BE    VALXDVY                                                          
         CLI   ES.RFLEL,RFLELQ                                                  
         BNE   VALXDV10                                                         
         CLC   ES.RFLSEQ,CURRCOLN                                               
         BNE   VALXDV10                                                         
         CLI   ES.RFLTYPE,RFLXDAOR                                              
         BE    VALXDV20                                                         
VALXDV10 IC    RE,ES.RFLLN                                                      
         AR    R2,RE                                                            
         B     VALXDV00                                                         
*                                                                               
VALXDV20 CLI   ES.RFLDATA,C'E'     XDATA TO COME FROM ESTIMATES?                
         BNE   VALXDVY             NO                                           
         OI    RFLIND,RFLXDEST                                                  
*                                                                               
VALXDVY  MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
         DROP  ES,R9                                                            
         EJECT ,                                                                
*=====================================================================*         
*  Validate Expenditure type                                          *         
*=====================================================================*         
         SPACE 1                                                                
                                                                                
VALETYPE DS    0H                                                               
         GOTO1 VALLIST,APPARM,SPACES,                                  X        
               (COLBLOCK,COLBLOCK+12),LITTWRK                                   
         LTR   RF,RF               TEST THE RETURN CODE                         
         BM    VALETY20            NO   LIST                                    
         BP    VALETY10            BAD  LIST                                    
*                                  GOOD LIST                                    
         CLI   NPARMS,1            ONLY 1 LIST REQUESTED ?                      
         BE    VALETY50            YES, ADD SPECIFIC ETYPE CODE LIST            
         B     ERR2MANY            NO,  TOO MANY LISTS                          
*                                                                               
VALETY10 DS    0H                  BAD  LIST                                    
         ZIC   R1,COLBLOCK         SHOW WHICH ETYPE CODE LIST IS BAD            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVXTRA(0),COLBLOCK+12                                            
         B     VALROUTX            SHOW BAD LIST                                
*                                                                               
VALETY20 DS    0H                  VALIDATE ETYPE CODE(S)                       
         SR    R6,R6                                                            
         IC    R6,NPARMS                                                        
         LA    R4,COLBLOCK                                                      
         LA    R2,IOKEY                                                         
*                                                                               
         USING ETYRECD,R2                                                       
VALETY30 DS    0H                                                               
         CLI   0(R4),L'ETYKCODE    IS   THE LENGTH > 2 ?                        
         BH    VALETYER            YES, INVALID ETYPE CODE                      
         XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ    X'37'                                        
         MVI   ETYKSUB,ETYKSUBQ    X'00'                                        
         MVC   ETYKCPY,CUABIN      COMPANY                                      
         MVC   ETYKCODE,12(R4)     EXPENDITURE TYPE CODE                        
         MVC   SVKEY,IOKEY                                                      
         GOTO1 AIO,IOHI+IOACCDIR+IO2                                            
         BNE   VALETYER                                                         
         CLC   SVKEY(ETYKOFFC-ETYRECD),ETYKEY                                   
         BNE   VALETYER                                                         
*                                                                               
         LA    R4,LSCNNTRY(,R4)                                                 
         BCT   R6,VALETY30                                                      
*                                                                               
VALETY50 DS    0H                                                               
         MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*                                                                               
VALETYER DS    0H                                                               
         MVC   FVMSGNO,=AL2(ACEINETY)                                           
         MVC   FVXTRA(20),12(R4)                                                
         B     VALROUTX                                                         
         DROP  R2                                                               
                                                                                
         EJECT ,                                                                
*=====================================================================*         
*  Validate Amount, R9=A(APELEM)                                      *         
*=====================================================================*         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALAMT   DS    0H                                                               
         MVI   ADD_N_EL,NO                                                      
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    VALAMTX                                                          
*                                                                               
         GOTO1 VCASHVAL,APPARM,(X'80',FVIFLD),(X'80',(RF))                      
         CLI   0(R1),X'FF'                                                      
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(ACEINAMT)                                           
         B     VALAMTX             INVALID AMOUNT                               
                                                                                
         ZAP   RFLAMNT,4(8,R1)     TEST OVERFLOW                                
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(ACEAMTHI)                                           
         B     VALAMTX             AMOUNT TOO HIGH                              
         MVI   RFLLN,RFLAMLNQ                                                   
         NI    RFLIND,TURNOFF-RFLXCLD                                           
*                                                                               
         CLI   RFLTYPE,RFLAMFR     TEST AMOUNT FROM                             
         BNE   *+14                                                             
         MVC   CURAMTFR,RFLAMNT    SAVED AMOUNT FROM                            
         B     VALAMT90                                                         
         CLI   RFLTYPE,RFLAMTO     TEST AMOUNT TO                               
         BNE   VALAMT90                                                         
         OC    CURAMTFR,CURAMTFR                                                
         BZ    VALAMT90                                                         
         CP    CURAMTFR,RFLAMNT                                                 
         BNH   VALAMT90                                                         
         MVC   FVMSGNO,=AL2(ACEAMTLE)                                           
         B     VALAMTX             TO AMOUNT IS LESS THAN FROM AMOUNT           
*                                                                               
VALAMT90 MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*                                                                               
VALAMTX  B     VALROUTX                                                         
         DROP  R9                                                               
         SPACE 1                                                                
***********************************************************************         
*  XDATA Origin (P for Posting, E for Estimate)                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R9                                                        
VALXDOR  LA    RF,COLBKLNG                                                      
         CLC   12(1,RF),AC@PSTG    DEFAULT IS 'P'                               
         BNE   *+14                                                             
         MVC   RFLDATA,AC@PSTG                                                  
         B     VALXOR05                                                         
                                                                                
         CLC   12(1,RF),AC@EST                                                  
         BNE   VALXOR00                                                         
         CLI   APREPJCL,REPJCLV    PROD                                         
         BNE   VALXOR00                                                         
         MVC   RFLDATA,AC@EST      OTHERWISE EXPECT IT TO BE 'E'                
         B     VALXOR05                                                         
                                                                                
VALXOR00 MVC   FVXTRA(1),12(RF)                                                 
         B     ERRINPT                                                          
*                                                                               
* SET RFLXDEST BIT ON FOR RFLXDATN                                              
*                                                                               
VALXOR05 L     R4,AIOAREA1                                                      
         AH    R4,DATADISP                                                      
         XC    SVELEM,SVELEM                                                    
ES       USING RFLELD,R4                                                        
VALXOR10 CLI   ES.RFLEL,0                                                       
         BE    VALXORY                                                          
         CLI   ES.RFLEL,RFLELQ                                                  
         BNE   VALXOR20                                                         
         CLC   ES.RFLSEQ,CURRCOLN                                               
         BNE   VALXOR20                                                         
         CLI   ES.RFLTYPE,RFLXDATN                                              
         BE    VALXOR30                                                         
VALXOR20 XR    RE,RE                                                            
         IC    RE,ES.RFLLN                                                      
         AR    R4,RE                                                            
         B     VALXOR10                                                         
*                                                                               
VALXOR30 IC    RE,ES.RFLLN                                                      
         SHI   RE,1                                                             
         EXMVC RE,SVELEM,ES.RFLELD                                              
         L     R2,AIOAREA1                                                      
         MVC   APHALF(L'RFLSEQ+L'RFLTYPE),ES.RFLSEQ                             
         GOTO1 VHELLO,APPARM,(C'D',=CL8'ACCVBIG'),('RFLELQ',(R2)),     X        
               (2,APHALF)                                                       
         LA    R4,SVELEM                                                        
         CLC   RFLDATA(1),AC@EST                                                
         BNE   *+8                                                              
         OI    ES.RFLIND,RFLXDEST   COLUMN FILTER FOR XDATA FROM ESTS.          
         XC    SVELEM,APELEM                                                    
         XC    APELEM,SVELEM                                                    
         XC    SVELEM,APELEM                                                    
         GOTO1 ADDEL,(R2)                                                       
         XC    SVELEM,APELEM                                                    
         XC    APELEM,SVELEM                                                    
         XC    SVELEM,APELEM                                                    
*                                                                               
VALXORY  MVI   ADD_N_EL,YES                                                     
         B     VALROUTX                                                         
*                                                                               
         DROP  R9,ES                                                            
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
*  DISKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
DISKEY   LA    R2,APRECKEY                                                      
         MVC   COLCODE,RESKFORM                                                 
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
*&&DO                                                                           
DISREC   CLI   APPFKEY,PFKUP                                                    
         BE    DR010                                                            
         CLI   APPFKEY,PFKDOWN                                                  
         BNE   DR012                                                            
*                                                                               
DR010    L     R2,ASCRTAB                                                       
         GOTO1 =A(SETSCRN),(R2),RR=APRELO                                       
*&&                                                                             
DISREC   DS    0H                                                               
DR012    TWAXC COLNMEH,COLOWNH                                                  
         TWAXC COLDATAH,COLTAGH                                                 
*                                                                               
         USING RCLELD,R3                                                        
         L     R2,AIOAREA1         COLUMN ELEMENTS                              
         GOTO1 GETNAME,APPARM,(R2),COLNMEH                                      
         GOTO1 GETPER,APPARM,(R2),COLOWNH                                       
*                                                                               
         GOTO1 AFVAL,COLNUMH                                                    
         TM    FVIIND,FVINUM       HAS TO BE VALID NUMBER                       
         BZ    IVALNNUM                                                         
         L     RF,SCFULL           AFVAL PASSES BACK VALUE                      
         STC   RF,CURRCOLN                                                      
         BAS   RE,GETCOLS          SHOW VALID COLUMNS                           
         LA    RF,VCOLLIST         VALID COLUMNS LIST                           
*                                                                               
DR015    CLI   0(RF),0             END OF LIST                                  
         BZ    IVALCOLN            INVALID COLUMN NUMBER                        
         CLC   CURRCOLN,0(RF)                                                   
         BE    DR020               OK, NEXT PART                                
         LA    RF,1(,RF)                                                        
         B     DR015                                                            
*                                                                               
DR020    L     R2,AIOAREA1                                                      
         MVI   APELCODE,RCLELQ                                                  
         MVC   ELEMSEQ,CURRCOLN    LOOK FOR SPECIFIC COL NUMBER                 
         GOTO1 GETEL,(R2)                                                       
         BNE   IVALCOLN                                                         
         LR    R3,R1               SAVE ELEMENT ADDRESS                         
         SR    R1,R1                                                            
         IC    R1,RCLSEQ           COLUMN NUMBER                                
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  COLNUM(2),APDUB                                                  
         CLI   COLNUM,C'0'                                                      
         BNE   *+14                                                             
         MVC   COLNUM(1),COLNUM+1  LEFT JUSTIFY SINGLE DIGITS                   
         MVI   COLNUM+1,C' '                                                    
         OI    COLNUMH+6,FVOXMT    TRANSMIT                                     
*                                                                               
         MVC   COLDATA,SPACES                                                   
         SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGTH OF DATA FIELD                         
         CLI   RCLDATLN,L'COLDATA                                               
         BNH   *+8                                                              
         LA    RF,L'COLDATA        TOO BIG FOR FIELD MAKE SMALLER               
         STC   RF,COLDATAH+5       STORE INPUT LENGTH                           
         OI    COLDATAH+6,FVOXMT   TRANSMIT                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   COLDATA(0),RCLNDATA COLUMN NAME                                  
*                                                                               
         MVI   REPMODE,REPCOL                                                   
         GOTO1 VALDEF,COLDATAH                                                  
*        BE    DR025                                                            
*                                                                               
         USING DEFTABD,R6                                                       
DR025    MVI   SKIPFLAG,NO                                                      
         LR    R6,R1                                                            
         CLC   =AL2(AC#RSPCT),DEFDDNUM           PCT    KEYWORD                 
         BE    DR028                                                            
         CLC   =AL2(AC#RSPTC),DEFDDNUM           PCTCLI KEYWORD                 
         BE    DR028                                                            
         CLC   =AL2(AC#RSMWR),DEFDDNUM           MWR    KEYWORD                 
         BE    DR028                                                            
         MVI   SKIPFLAG,YES                                                     
*                                                                               
DR028    L     R2,ASCRTAB                                                       
         GOTO1 =A(SETSCRN),(R2),RR=APRELO                                       
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,LASTTWAF                                                    
         BZ    *+6                                                              
         AR    R2,R5                                                            
         TWAXC COLTAGH,(R2)                                                     
*                                                                               
         SR    R1,R1                                                            
         IC    R1,RCLWDTH          COLUMN WIDTH                                 
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  COLWDTH,APDUB                                                    
         CLI   COLWDTH,C'0'                                                     
         BNE   *+8                                                              
         MVI   COLWDTH,C' '                                                     
         OI    COLWDTHH+6,FVOXMT                                                
*                                                                               
         BAS   RE,DISDATE          DISPLAY DATE RANGE                           
         MVI   COLTOTL,C' '        TOTAL ON COLUMN (DEFAULT)                    
         OI    COLTOTLH+6,FVOXMT                                                
         TM    RCLOPT,ACCUM        IS IT AN ACCUMULATED COLUMN?                 
         BO    DR030               YES, GET OUT                                 
         MVC   COLTOTL,APNO        SET OPTION TO NO                             
         TM    RCLOPT2,RCLTOT                                                   
         BZ    *+10                                                             
         MVC   COLTOTL,APYES                                                    
*                                                                               
DR030    MVC   COLPRNT,APYES       PRINT COLUMN (DEFAULT)                       
         TM    RCLOPT,HIDE         HIDDEN COLUMN?                               
         BZ    *+10                                                             
         MVC   COLPRNT,APNO                                                     
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,RCLSORTN         SORT ORDER                                 
         BZ    DR040                                                            
         CVD   R1,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  COLSRTO,APDUB                                                    
         CLI   COLSRTO,C'0'                                                     
         BNE   *+8                                                              
         MVI   COLSRTO,C' '                                                     
         OI    COLSRTOH+6,FVOXMT                                                
*                                                                               
DR040    SR    RF,RF                                                            
         IC    RF,RCLDATLN         LENGHT OF DATA FIELD                         
         LA    RE,RCLNDATA(RF)     POINT TO END OF DATA FIELD                   
         MVC   COLHED1,SPACES                                                   
         OI    COLHED1H+6,FVOXMT                                                
         ICM   RF,1,RCLHD1LN       DO WE HAVE HEADING 1?                        
         BZ    DR045               NO                                           
         LR    R1,RF                                                            
         CLI   RCLHD1LN,L'COLHED1                                               
         BNH   *+8                                                              
         LA    RF,L'COLHED1        TOO BIG FOR FIELD, MAKE SMALLER              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   COLHED1(0),0(RE)                                                 
         AR    RE,R1               POINT TO END OF HEADING 1                    
*                                                                               
DR045    MVC   COLHED2,SPACES                                                   
         OI    COLHED2H+6,FVOXMT                                                
         ICM   RF,1,RCLHD2LN       DO WE HAVE HEADING 2?                        
         BZ    DR090                                                            
         CLI   RCLHD2LN,L'COLHED2                                               
         BNH   *+8                                                              
         LA    RF,L'COLHED2        TOO BIG FOR FIELD, MAKE SMALLER              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   COLHED2(0),0(RE)                                                 
*                                                                               
         USING RFLELD,R1                                                        
DR090    L     R1,AIOAREA1         SEARCH THROUGH PROFILES                      
         MVI   APELCODE,RFLELQ     X'C5'                                        
         MVC   ELEMSEQ,CURRCOLN                                                 
         GOTO1 GETEL                                                            
         BNE   DRXIT                                                            
DR092    ST    R1,ARFLCOL          SAVE A(1ST RFLELQ FOR COLUMN)                
*                                                                               
         LA    R3,COLTAGH                                                       
DR100    CLI   0(R3),0             EOS, END OF SCREEN                           
         BE    DRXIT                                                            
         TM    1(R3),FVAXTND       FIND FIELD WITH EXTENDED HEADER              
         BO    DR103                                                            
*                                                                               
DR102    SR    RF,RF                                                            
         IC    RF,0(,R3)           LENGTH OF FIELD ON SCREEN                    
         AR    R3,RF               POINT TO NEXT FIELD                          
         B     DR100                                                            
*                                                                               
         USING REPFD,R2                                                         
DR103    SR    RF,RF                                                            
         IC    RF,0(,R3)           LENGTH OF FIELD ON SCREEN                    
         AHI   RF,-8                                                            
         LR    RE,R3                                                            
         AR    RE,RF               POINT TO EXTEND FIELD HEADER                 
*                                                                               
         L     R2,ASCRTAB                                                       
DR104    CLI   REPRFL#,X'FF'       EOT?                                         
         BE    DR102               DID NOT FIND ENTRY, SO SKIP                  
         CLC   REPRFL#,0(RE)       MATCH EXTEND FIELD# TO RFLTYPE #             
         BE    DR105                                                            
         LA    R2,REPFLNQ(,R2)                                                  
         B     DR104                                                            
*                                                                               
DR105    CLI   REPRFL#,251         SPECIAL CASE                                 
         BE    DR102               SKIP THIS ONE                                
*                                                                               
         BAS   RE,NEXTUNPT         BUMP UP TO INPUT FIELD                       
*&&UK                                                                           
         TM    CURRCOPT,RCLEQU     CALCULATED COLUMN                            
         BO    DR106               YES, DON'T SET DEFAULT                       
         CLI   REPRFL#,RFLAUTH     AUTH STATUS                                  
         BE    DR105A                                                           
*&&                                                                             
         CLI   REPRFL#,RFLRECON                                                 
         BE    DR105D                                                           
         CLI   REPRFL#,RFLAPPRV                                                 
         BL    DR106                                                            
         CLI   REPRFL#,RFLURGNT                                                 
         BH    DR106                                                            
*&&UK                                                                           
DR105A   XR    RF,RF                                                            
         IC    RF,COLDATAH+5                                                    
         LA    R1,L'AC@XDATA                                                    
         LA    RE,AC@XDATA+L'AC@XDATA-1                                         
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         SHI   RE,1                                                             
         BCT   R1,*-12                                                          
         CR    RF,R1                                                            
         BNE   DR105C                                                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         BNE   DR105C                                                           
         CLC   COLDATA(0),AC@XDATA                                              
         BE    DR105E                                                           
                                                                                
DR105C   CLI   REPRFL#,RFLAUTH     AUTH STATUS                                  
         BNE   *+14                                                             
         MVC   8(L'AC@BOTH,R3),AC@BOTH                                          
         B     *+10                                                             
*&&                                                                             
DR105D   MVC   8(1,R3),APYES       SET DEFAULT VALUE                            
*                                                                               
DR105E   OI    6(R3),FVOXMT                                                     
*                                                                               
DR106    SR    RF,RF                                                            
         L     R1,ARFLCOL                                                       
*                                                                               
DR107    CLI   0(R1),RFLELQ        END OF RFLEL ELEMENTS X'C5'                  
         BNE   DR102               NONE FOUND                                   
         CLI   RFLLN,RFLLNQ                                                     
         BE    DR108               BAD ELEMENT (probably from stereo)           
         CLC   RFLSEQ,CURRCOLN     IS IT COLUMN SPECIFIC?                       
         BNE   DR102               NONE FOUND                                   
         CLC   RFLTYPE,REPRFL#     MATCH RFLTYPE TO TABLE                       
         BE    DR115                                                            
*                                                                               
DR108    IC    RF,1(,R1)           BUMP UP NEXT ELEMENT                         
         AR    R1,RF                                                            
         B     DR107                                                            
*                                                                               
         USING ROUTD,R6                                                         
DR115    L     R6,=A(ROUTINES)     DISPLAY ROUTINE                              
         A     R6,APRELO                                                        
*                                                                               
DR120    CLI   ROUTFLD,X'FF'                                                    
         BE    DR102               NO ROUTINE                                   
         CLC   ROUTFLD,REPFFLDN                                                 
         BE    DR180               FOUND ROUTINE #                              
         LA    R6,ROUTLNQ(,R6)                                                  
         B     DR120                                                            
*                                                                               
DR180    LA    RF,DISROUT          DISPLAY ROUTINE                              
         ICM   RF,8,ROUTDIS        ROUTINE #                                    
         BASR  RE,RF                                                            
         B     DR102               NEXT FIELD                                   
*                                                                               
DRXIT    CLI   APACTN,ACTCHA                                                    
         BE    EXIT                                                             
         LA    RE,COLNUMH          PLACE CURSOR ON COLUMN NUMBER                
         ST    RE,APCURSOR                                                      
         SR    RE,RE               SET CONCODE TO YES                           
         B     EXIT                                                             
         DROP  R1,R2,R3,R6                                                      
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY ROUTINES                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISROUT  NTR1                                                                   
*                                                                               
         OI    6(R3),FVOXMT        TRANSMIT FIELD ON EXIT                       
*                                                                               
         SRL   RF,24               MOVE ROUTINE # TO LOB                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     DISGEN          1 - Display element as is                        
         B     DISBUD          2 - Budget                                       
         B     DISTTY          3 - Transaction type                             
         B     DISSTA          4 - Status                                       
         B     DISMTH          5 - Method type                                  
         B     DISUFD          6 - Userfield                                    
*&&UK*&& B     DISAUTH         7 - DISPLAY AUTH STATUS (UK)                     
*&&US*&& B     DISROUTX        7 - (US)                                         
*&&UK*&& B     DISYN           8 - DISPLAY Yes/No (UK)                          
*&&US*&& B     DISROUTX        8 - (US)                                         
*&&US*&& B     DISAPMT         9 - Display approval method (US)                 
*&&UK*&& B     DISROUTX        9 - (UK)                                         
*&&UK*&& B     DISAMT         10 - DISPLAY AMOUNT (UK)                          
*&&US*&& B     DISROUTX       10 - (US)                                         
*&&UK*&& B     DISBNVS        11 - DISPLAY BR'OCEAN INVOICE STATUS              
*&&US*&& B     DISROUTX       11 - (US)                                         
*                                                                               
DISROUTX B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RFL DATA IN COLFILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DISGEN   SR    RE,RE                                                            
         IC    RE,0(,R3)           LENGTH OF FIELD                              
         AHI   RE,-8                                                            
         TM    1(R3),FVAXTND       EXTENDED FIELD                               
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         LA    R4,8(,R3)                                                        
         CLI   RFLTYPE,RFLTSTA     TIME STATUS FOR BRANDOCEAN (UK)              
         BNE   *+10                                                             
         MVC   0(LENFTSTA,R4),SPACES                                            
*                                                                               
         SR    RF,RF               LENGTH OF DATA                               
         IC    RF,RFLLN                                                         
         AHI   RF,-(RFLLNQ+1)      SUBTRACT UP TO DATA+1                        
         TM    RFLIND,RFLXCLD      EXCLUDE BIT ON?                              
         BZ    DISGEN10                                                         
         MVI   0(R4),C'*'                                                       
         LA    R4,1(,R4)                                                        
         BCTR  RE,0                                                             
*                                                                               
DISGEN10 CR    RF,RE                                                            
         BL    *+6                                                              
         DC    H'00'               DATA DOESN'T FIT IN FIELD?                   
*                                                                               
         EXMVC RF,0(R4),RFLDATA                                                 
         B     DISROUTX                                                         
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RFL TRANSACTION TYPE                                       *         
***********************************************************************         
         SPACE 1                                                                
DISTTY   LR    R0,R1                                                            
         LA    R4,8(,R3)                                                        
         SR    RF,RF                                                            
         IC    RF,0(,R3)                                                        
         AHI   RF,-8                                                            
         TM    1(R3),FVAXTND       EXTENDED FIELD                               
         BZ    *+8                                                              
         AHI   RF,-8                                                            
         GOTO1 CNVTTYPE,APPARM,(C'S',(R0)),((RF),(R4))                          
         LR    R1,R0                                                            
         B     DISROUTX                                                         
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY RFLSTAT                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
         USING STATD,RE                                                         
DISSTA   CLI   RFLDATA,YES                                                      
         BNE   *+10                                                             
         MVC   8(1,R3),APONLY                                                   
         CLI   RFLDATA,NO                                                       
         BNE   *+10                                                             
         MVC   8(1,R3),APNO                                                     
         B     DISROUTX                                                         
         DROP  R1,RE                                                            
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY METHOD                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
         USING CAHRECD,R2                                                       
DISMTH   MVC   8(1,R3),RFLDATA     MOVE IN NUMBER OF METHOD INSTEAD             
         LA    R2,IOKEY                                                         
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ    X'3E'                                        
         MVI   CAHKSUB,CAHKSUBQ    X'01'                                        
         MVC   CAHKCPY,CUABIN      COMPANY                                      
         MVC   CAHKMTHD,RFLDATA    MOVE IN METHOD                               
         XC    CAHKOFC,CAHKOFC                                                  
         GOTO1 AIO,IO3+IOACCFIL+IORD                                            
         BNE   DISROUTX                                                         
*                                                                               
         USING METELD,R1                                                        
DISMTH20 L     R1,AIOAREA3                                                      
         MVI   APELCODE,METELQ     X'82'                                        
         GOTO1 GETEL                                                            
         BNE   DISROUTX                                                         
         MVC   8(3,R3),METCODE     MOVE IN NAME                                 
         B     DISROUTX                                                         
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  Display User field                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DISUFD   LA    R2,8(,R3)           R3 = screen field                            
         LA    R3,RFLDATA          R1 = element                                 
         ZIC   RF,RFLLN                                                         
         AR    R1,RF               Point to end of element                      
         ST    R1,AEND                                                          
         XC    ELEMENT,ELEMENT                                                  
         SR    R1,R1                                                            
         IC    R1,SCCOMMA                                                       
         LA    R1,ELEMENT(R1)                                                   
         MVI   0(R1),X'FF'                                                      
         DROP  R1                                                               
*                                                                               
         USING UFD,R3                                                           
DISUF10  LA    R4,UFDDATA          Point to data                                
         BAS   RE,SCANFOR                                                       
         BZ    DISUF90                                                          
*                                                                               
DISUF12  NI    UFD#OF,TURNOFF-UFDON                                             
         SR    R0,R0                                                            
         IC    R0,UFD#OF                                                        
         MVC   DELIMTER,SCCOMMA                                                 
         TM    UFDCC,UFDAND                                                     
         BZ    *+8                                                              
         MVI   DELIMTER,C'&&'                                                   
         NI    UFDCC,TURNOFF-UFDAND                                             
         MVC   0(2,R2),UFDCODE                                                  
         LA    R2,2(,R2)                                                        
*                                                                               
         LA    R1,#OFSIGNS                                                      
         L     RF,=A(SIGNTAB)                                                   
         A     RF,APRELO                                                        
DISUF15  CLC   UFDCC,0(RF)                                                      
         BE    DISUF16                                                          
         AHI   RF,L'SIGNTAB                                                     
         BCT   R1,DISUF15                                                       
         DC    H'00'               Not valid                                    
*                                                                               
DISUF16  MVC   0(2,R2),2(RF)       Move in sign                                 
         ZIC   R1,1(,RF)           Bump up length of sign                       
         AR    R2,R1                                                            
         LA    RE,UFDDATA                                                       
         L     R5,SCANFLN          Get length of data                           
         LA    R3,UFDDATA                                                       
         DROP  R3                                                               
*                                                                               
DISUF20  CHI   R0,1                                                             
         BE    DISUF25             Finished so move to screen                   
         ICM   R4,15,ABREAK                                                     
         BZ    DISUF25             No more, so move to field                    
         AHI   R4,1                Bump past delimiter                          
         BAS   RE,SCANFOR                                                       
         A     R5,SCANFLN          Add to length                                
         AHI   R5,1                Add 1 for delimiter                          
         BCTR  R0,0                One less to process                          
         B     DISUF20                                                          
*                                                                               
DISUF25  SHI   R5,1                One for EX                                   
         BNM   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         EXMVC R5,0(R2),0(R3)                                                   
         LA    R2,1(R5,R2)         Len of data + EX                             
         ICM   R4,15,ABREAK                                                     
         BZ    DISUF90             No more                                      
         LR    R3,R4                                                            
         AHI   R3,1                Point past delimiter                         
         MVC   0(1,R2),DELIMTER                                                 
         AHI   R2,1                                                             
         B     DISUF10                                                          
*                                                                               
DISUF90  B     DISROUTX                                                         
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
*  DISPLAY AUTH STATUS                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DISAUTH  CLI   RFLDATA,AUTHQ                                                    
         BNE   *+10                                                             
         MVC   8(L'AC@ATHED,R3),AC@ATHED                                        
         CLI   RFLDATA,UNAUTHQ                                                  
         BNE   *+10                                                             
         MVC   8(L'AC@UATH,R3),AC@UATH                                          
         B     DISROUTX                                                         
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY Yes/No                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DISYN    MVC   8(1,R3),APNO                                                     
         CLI   RFLDATA,YES                                                      
         BNE   DISROUTX                                                         
         MVC   8(1,R3),APYES                                                    
         B     DISROUTX                                                         
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY Amount                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R2                                                        
DISAMT   LR    R2,R1                                                            
         CURED RFLAMNT,(13,8(R3)),2,ALIGN=LEFT,MINUS=YES,DMCB=APPARM            
         B     DISROUTX                                                         
         DROP  R2                                                               
         EJECT ,                                                                
*&&                                                                             
*&&US                                                                           
***********************************************************************         
*  DISPLAY APPROVER METHOD                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DISAPMT  CLI   RFLDATA,GDAAPPAA                                                 
         BNE   *+10                                                             
         MVC   8(L'AC@AAPP,R3),AC@AAPP     Auto Approve                         
         CLI   RFLDATA,GDAAPPMK                                                 
         BNE   *+10                                                             
         MVC   8(L'AC@MAPP,R3),AC@MAPP     Marker                               
         B     DISROUTX                                                         
         DROP  R1                                                               
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
*  DISPLAY BUDGET                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
         USING BUDRECD,R2                                                       
DISBUD   MVC   SVKEY,IOKEY         SAVE    THE  KEY                             
         LA    R2,IOKEY            ->      THE  KEY  AREA                       
         XC    BUDKEY,BUDKEY                                                    
         MVI   BUDKTYP,BUDKTYPQ    X'1B'                                        
         MVC   BUDKCPY,CUABIN      COMPANY                                      
         MVC   BUDKNO1,RFLDATA     BUDGET  NUMBER                               
         MVC   APHALF,RFLDATA      SAVE    DESIRED   BUDGET   NUMBER            
         GOTO1 AIO,IO2+IOACCDIR+IOHI                                            
*                                                                               
         CLC   APHALF,BUDKNO1      DID     THE  BUDGET   KEY  CHANGE ?          
         BE    DISBUD10            NO,     CONTINUE                             
*                                                                               
*                                  THIS    COLUMN    BUDGET   MIGHT             
*                                          NOT  EXIST    BECAUSE IT             
*                                          WAS  OBSOLETED     -                 
*                                  DISPLAY THE  BUDGET   AS   A  NUMBER         
*                                          INSTEAD   OF  AS   A                 
*                                          BUDGET    ID  NAME                   
         SR    R1,R1               CLEAR   REGISTER                             
         ICM   R1,3,APHALF         GET     THE  BUDGET   NUMBER                 
         CVD   R1,APDUB            CONVERT IT   TO       PACKED DECIMAL         
         OI    APDUB+7,X'0F'       SETUP   FOR  DISPLAY                         
         UNPK  8(5,R3),APDUB       CONVERT TO   DISPLAY                         
         MVC   13(2,R3),=C' ?'     SAY     SOMETHING IS  WRONG                  
         B     DISBUD20            DISPLAY IT                                   
*                                                                               
DISBUD10 DS    0H                  DISPLAY THE  BUDGET   ID                     
         MVC   8(L'BUDKCOD,R3),BUDKCOD                                          
*                                                                               
DISBUD20 DS    0H                                                               
         MVC   IOKEY,SVKEY         RESTORE THE  KEY                             
         B     DISROUTX RETURN                                                  
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
*  DISPLAY BRANDOCEAN INVOICE STATUS FILTER(S)                        *         
***********************************************************************         
         SPACE 1                                                                
         USING RFLELD,R1                                                        
DISBNVS  DS    0H                                                               
         LA    RF,APDUB                                                         
         MVC   APDUB,SPACES                                                     
         TM    RFLDATA,BNVSUBQ     SUBMITTED                                    
         BZ    DBNVS04                                                          
         MVC   0(1,RF),AC@SUBM                                                  
         MVC   1(1,RF),SCCOMMA                                                  
         AHI   RF,2                                                             
DBNVS04  TM    RFLDATA,BNVPAPQ     PART-APPROVED                                
         BZ    DBNVS06                                                          
         MVC   0(1,RF),AC@PAPP                                                  
         MVC   1(1,RF),SCCOMMA                                                  
         AHI   RF,2                                                             
DBNVS06  TM    RFLDATA,BNVAPPQ     APPROVED                                     
         BZ    DBNVS08                                                          
         MVC   0(1,RF),AC@APPR                                                  
         MVC   1(1,RF),SCCOMMA                                                  
         AHI   RF,2                                                             
DBNVS08  TM    RFLDATA,BNVREJQ     REJECTED                                     
         BZ    DBNVS10                                                          
         MVC   0(1,RF),AC@REJE                                                  
         AHI   RF,2                                                             
DBNVS10  AHI   RF,-1               BACK UP ONE...                               
         MVI   0(RF),C' '          ... AND ANY CLEAR TRAILING COMMA             
         MVC   8(5,R3),APDUB                                                    
         B     DISROUTX                                                         
         DROP  R1                                                               
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
*  GETCOLS - FIND VALID COLUMNS FOR THIS FORMAT                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R1                                                        
GETCOLS  NTR1                                                                   
         MVC   COLVCOL,SPACES                                                   
         XC    VCOLLIST,VCOLLIST                                                
         L     R1,AIOAREA1                                                      
         LA    R2,COLVCOL          FIELD ON SCREEN                              
         LA    R3,VCOLLIST         VALID COLUMNS LIST                           
         LA    R4,COLVCOL+L'COLVCOL-4                                           
         MVI   APELCODE,RCLELQ                                                  
         GOTO1 GETEL                                                            
*                                                                               
GETC10   BNE   GETC30                                                           
*&&UK                                                                           
         TM    RCLOPT4,RCLNACCU    NON ACCUMULATIVE COLFILT?                    
         BO    GETC11                                                           
         TM    RCLOPT,RCLACCM+RCLEQU                                            
         BZ    GETC25                                                           
*&&                                                                             
*&&US                                                                           
         TM    RCLOPT,RCLACCM      HAS TO BE AN ACCUMULATED COLUMN              
         BZ    GETC25                                                           
         TM    RCLOPT,RCLEQU       CANNOT BE A CALCULATED COLUMN                
         BO    GETC25                                                           
*&&                                                                             
GETC11   MVC   0(1,R3),RCLSEQ      SAVE OFF VALID COLUMN NUMBER                 
         LA    R3,1(,R3)                                                        
         CLI   CURRCOLN,0          IS ANY COLUMN # SET?                         
         BNZ   *+10                                                             
         MVC   CURRCOLN,RCLSEQ     SET TO FIRST COLUMN #                        
*&&UK                                                                           
         CLC   CURRCOLN,RCLSEQ                                                  
         BNE   *+10                                                             
         MVC   CURRCOPT,RCLOPT     SET CURRENT RCLOPT                           
*&&                                                                             
         CR    R2,R4               KEEP BUILDING LIST                           
         BNH   GETC12                                                           
         MVI   0(R2),C'>'          MORE BUT CANNOT PRINT                        
         BH    GETC25                                                           
*                                                                               
GETC12   ZIC   RF,RCLSEQ           INSERT COLUMN NUMBER TO OUTPUT FIELD         
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  0(2,R2),APDUB                                                    
         LA    R0,2                                                             
         CLI   0(R2),C'0'          LEFT JUSTIFY COLUMN NUMBER                   
         BNE   GETC15                                                           
         MVC   0(1,R2),1(R2)                                                    
         MVI   1(R2),C' '                                                       
         LA    R0,1                                                             
*                                                                               
GETC15   AR    R2,R0               INSERT A COMMA                               
         MVC   0(1,R2),SCCOMMA                                                  
         LA    R2,1(,R2)                                                        
*                                                                               
GETC25   GOTO1 NEXTEL                                                           
         B     GETC10                                                           
*                                                                               
GETC30   BCTR  R2,0                REMOVE LAST COMMA                            
         MVI   0(R2),C' '                                                       
*                                                                               
GETCOLSX OI    COLVCOLH+6,FVOXMT                                                
         B     XIT                                                              
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISDATE - DISPLAY DATE RANGE                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RCLELD,R9                                                        
DISDATE  NTR1                                                                   
         LR    R9,R3                                                            
         SR    R2,R2                                                            
         MVC   APWORK,SPACES       INITIALIZE WORK AREA                         
         CLI   RCLDATES,0                                                       
         BE    XIT                                                              
*                                                                               
DDATE10  TM    RCLDTEFG,X'FF'-RCLTODTE                                          
         BZ    DDATE30             MUST BE PERIOD RANGE                         
         CLC   RCLSTDT,=XL2'8000'  DELIMETER FOR "PRIOR"                        
         BNE   DDATE21                                                          
         MVC   APWORK(5),AC@PRIOR                                               
         B     DDATE27                                                          
*                                                                               
DDATE21  TM    RCLSTDT,X'80'       NEGATIVE HALF WORD?                          
         BZ    DDATE22             NO                                           
         BCTR  R2,0                                                             
*                                                                               
DDATE22  ICM   R2,3,RCLSTDT                                                     
         TM    RCLDTEFG,RCLPERD+RCLMON                                          
         BNO   DDATE23                                                          
         SR    R2,R2                                                            
         ICM   R2,1,RCLPDN1        GET PERIOD NUMBER                            
         TM    RCLPDYR,X'80'                                                    
         BZ    *+6                                                              
         LNR   R2,R2                                                            
*                                                                               
DDATE23  CVD   R2,APDUB                                                         
         MVC   APWORK(7),=XL7'40202020202060'                                   
         LA    R1,APWORK+5                                                      
         EDMK  APWORK(7),APDUB+5                                                
         CLI   APWORK+5,C' '                                                    
         BNE   *+8                                                              
         MVI   APWORK+5,C'0'       FORCE A ZERO TO PRINT                        
         BCTR  R1,0                                                             
         MVC   0(1,R1),APWORK+6                                                 
         MVI   APWORK+6,C' '                                                    
         TM    RCLDTEFG,RCLPERD+RCLMON                                          
         BNO   DDATE24                                                          
         CLI   RCLPDYR,X'01'       NEXT YEAR?                                   
         BNE   *+8                                                              
         MVI   0(R1),C'+'                                                       
         TM    RCLDTEFG,RCLNROLL   PM1-PM12                                     
         BO    DDATE30                                                          
         SR    R2,R2                                                            
         OC    RCLENDT,RCLENDT                                                  
         BNZ   DDATE27                                                          
         B     DDATE30                                                          
*                                                                               
DDATE24  TM    RCLSTDT,X'80'       NEGATIVE HALF WORD?                          
         BO    DDATE25             YES, SO DISPLAY END DATA                     
         TM    RCLDTEFG,RCLDAY     IS IT DAY?                                   
         BO    DDATE25             YES, SO DISPLAY BOTH PARAMETERS              
         OC    RCLENDT,RCLENDT     ANY END DATE?                                
         BZ    DDATE30             NO SO DON'T WORRY                            
*                                                                               
DDATE25  SR    R2,R2                                                            
         CLC   RCLENDT,=XL2'8000'  DELIMETER FOR "AFTER"                        
         BNE   DDATE26                                                          
         MVC   APWORK+7(1),SCCOMMA                                              
         MVC   APWORK+8(5),AC@AFTER                                             
         B     DDATE30                                                          
*                                                                               
DDATE26  TM    RCLDTEFG,RCLDAY                                                  
         BZ    DDATE30                                                          
*                                                                               
DDATE27  MVC   APWORK+7(1),SCCOMMA                                              
         TM    RCLENDT,X'80'       NEGATIVE HALF WORD?                          
         BZ    DDATE28             NO                                           
         BCTR  R2,0                YES MAKE ALL X'FF'S                          
*                                                                               
DDATE28  ICM   R2,3,RCLENDT                                                     
         CVD   R2,APDUB                                                         
         MVC   APWORK+8(7),=XL7'40202020202060'                                 
         LA    R1,APWORK+13                                                     
         EDMK  APWORK+8(7),APDUB+5                                              
         CLI   APWORK+13,C' '                                                   
         BNE   *+8                                                              
         MVI   APWORK+13,C'0'      FORCE A ZERO TO PRINT                        
         BCTR  R1,0                                                             
         MVC   0(1,R1),APWORK+14                                                
         MVI   APWORK+14,C' '                                                   
*                                                                               
DDATE30  LA    R1,0                R1 = LENGTH OF SQUISHED STRING               
         LA    R2,APWORK           START OF STRING                              
         LA    RE,15                                                            
*                                                                               
DDATE32  CLI   0(R2),X'40'         SQUISH TOGETHER                              
         BNH   DDATE33                                                          
         LA    R1,1(,R1)           NOT BLANK SO BUMP UP STRING LENGTH           
         LA    R2,1(,R2)           BUMP UP IN STRING                            
         BCT   RE,DDATE32                                                       
         B     DDATE34                                                          
*                                                                               
DDATE33  EXMVC RE,0(R2),1(R2)                                                   
         BCT   RE,DDATE32                                                       
*                                                                               
DDATE34  LA    R1,L'COLDRNG-1                                                   
         EXMVC R1,COLDRNG,APWORK                                                
         OI    COLDRNGH+6,FVOXMT   RETRANSMIT                                   
         B     XIT                                                              
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*  FIND SCREEN                                                        *         
***********************************************************************         
         SPACE 1                                                                
FINDSCRN NTR1                                                                   
         L     RF,=A(SCRTABLE)                                                  
         A     RF,APRELO                                                        
*                                                                               
FSCR10   CLI   0(RF),X'FF'                                                      
         BNE   FSCR20                                                           
         MVC   FVMSGNO,=AL2(2090)                                               
         SR    R2,R2                                                            
         B     FSCR90                                                           
*                                                                               
FSCR20   CLC   APREPJCL,0(RF)                                                   
         BE    *+12                                                             
         LA    RF,3(,RF)                                                        
         B     FSCR10                                                           
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,3,1(RF)                                                       
         A     R2,=A(REPFLDS)                                                   
         A     R2,APRELO                                                        
         SR    RE,RE                                                            
*                                                                               
FSCR90   LTR   RE,RE                                                            
         XIT1  REGS=(R2)                                                        
         EJECT ,                                                                
***********************************************************************         
*  NEXTUNPT - FIND NEXT UNPROTECTED FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
NEXTUNPT NTR1                                                                   
*                                                                               
NEXTUN05 ZIC   R1,0(,R3)           LENGTH OF FIELD                              
         AR    R3,R1               BUMP TO NEXT ONE                             
         TM    1(R3),X'20'         PROTECTED?                                   
         BO    NEXTUN05                                                         
         XIT1  REGS=(R3)                                                        
         EJECT ,                                                                
***********************************************************************         
*  INVALID ERRORS TO DISPLAY ON TOP OF SCREEN -                       *         
*          WITHIN AN NTR1 / XIT1 PAIR                                 *         
***********************************************************************         
         SPACE 1                                                                
ERRCNT   MVC   FVMSGNO,=AL2(ACEICNTR)     INVALID CONTRA ACCOUNT                
         MVC   FVXTRA(20),12(R4)                                                
         B     ERRXIT                                                           
*                                                                               
ERRLDGR  MVC   FVMSGNO,=AL2(1450)         VALID LEDGERS ARE                     
         SR    RF,RF                                                            
         IC    RF,LDGLISTN                                                      
         LA    R9,LDGLIST                                                       
         LA    RE,FVXTRA                                                        
*                                                                               
ERRLDG10 MVC   0(2,RE),0(R9)                                                    
         MVC   2(1,RE),SCCOMMA                                                  
         LA    R9,2(,R9)                                                        
         LA    RE,3(,RE)                                                        
         BCT   RF,ERRLDG10                                                      
         BCTR  RE,0                                                             
         MVI   0(RE),C' '                                                       
         B     ERRXIT                                                           
*                                                                               
ERRICKY  MVC   FVMSGNO,=AL2(ACEICWKY)     INCOMPATIBLE WITH KEYWORD             
         B     ERRXIT                                                           
*                                                                               
ERRACCT  MVC   FVMSGNO,=AL2(ACEACTLV)     INVALID ACCOUNT OR LEVEL              
         MVC   FVXTRA(20),12(R4)                                                
         B     ERRXIT                                                           
*                                                                               
ERRTYPE  MVC   FVMSGNO,=AL2(ACEIVTY)       INVALID TYPE                         
         B     ERRXIT                                                           
*                                                                               
ERRTSTA  MVC   FVMSGNO,=AL2(ACEINSTA)      INVALID TIME STATUS                  
         B     ERRXIT                                                           
ERRTSTP  MVC   FVMSGNO,=AL2(ACEINSTP)      INVALID TIME STATUS (prod)           
         B     ERRXIT                                                           
*                                                                               
ERRINPT  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     ERRXIT                                                           
*                                                                               
ERRBSC   MVC   FVMSGNO,=AL2(ACEIBSC)       INVALID BILLING SOURCE               
         MVC   FVXTRA(20),12(R4)                                                
         B     ERRXIT                                                           
*                                                                               
ERRBUDG  MVC   FVMSGNO,=AL2(ACEBUD)        INVALID BUDGET                       
         MVC   FVXTRA(20),FVIFLD                                                
         B     ERRXIT                                                           
*                                                                               
ERRBUDGA MVC   FVMSGNO,=AL2(ACEBUDA)       BUDGET INCOMPATIBLE W/ ACNT          
         MVC   FVXTRA(20),12(R4)                                                
         B     ERRXIT                                                           
*                                                                               
ERR2MANY MVC   FVMSGNO,=AL2(ACE2MANY)      TOO MANY PARAMETERS                  
         B     ERRXIT                                                           
*                                                                               
ERRXIT   CLC   FVMSGNO,=AL2(FVFOK) SET CONCODE                                  
         B     XIT                                                              
         EJECT ,                                                                
***********************************************************************         
*  INVALID ERRORS TO DISPLAY ON TOP OF SCREEN                         *         
***********************************************************************         
         SPACE 1                                                                
IVALEKEY MVI   STSEQ,1                                                          
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         B     IVALEXIT                                                         
*                                                                               
IVALNNUM MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     IVALEXIT                                                         
*                                                                               
IVALCOLN MVC   FVMSGNO,=AL2(ACEIVCN)       INVALID COLUMN NUMBER                
         B     IVALEXIT                                                         
*                                                                               
IVALEXIT DS    0H                                                               
         B     EXIT                                                             
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
WCTYPES  DS    0H                                                               
         DC    C'TN'                                                            
*&&UK*&& DC    C'CN'                                                            
*&&US*&& DC    C'ON'                                                            
*&&US*&& DC    C'PN'                                                            
*&&US*&& DC    C'RN'                                                            
*&&US*&& DC    C'MN'                                                            
         DC    X'FF'                                                            
LWCTYPES EQU   *-WCTYPES                                                        
         EJECT ,                                                                
***********************************************************************         
*  SET DYNAMIC SCREEN                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING REPFD,R2                                                         
         USING TWAELEMD,R4                                                      
SCRPARM  USING TWAPARMD,APPARM                                                  
         SPACE 1                                                                
SETSCRN  NTR1  BASE=*,LABEL=*                                                   
         LTR   R2,R1                                                            
         BZ    SETSXIT                                                          
         XC    APPARM(24),APPARM                                                
         XC    TWAELEM,TWAELEM                                                  
         ST    R5,SCRPARM.TWAPATWA                                              
         LA    R4,TWAELEM                                                       
         ST    R4,SCRPARM.TWAPAFST                                              
         LA    R3,COLTAGH                                                       
         ST    R3,SCRPARM.TWAPAOUT                                              
*                                                                               
SETS0050 CLI   0(R3),0             END OF SCREEN ?                              
         BE    SETS0070                                                         
         TM    1(R3),FVAXTND       EXTEND FIELD HEADER ?                        
         BO    SETS0060                                                         
SETS0052 LR    R2,R1               RE-LOAD R2                                   
         SR    RF,RF                                                            
         IC    RF,0(,R3)                                                        
         AR    R3,RF                                                            
         B     SETS0050                                                         
*                                                                               
SETS0060 CLI   REPRFL#,X'FF'       EOT                                          
         BE    SETS0052            TRY AGAIN                                    
         SR    RF,RF                                                            
         IC    RF,0(,R3)                                                        
         AHI   RF,-8                                                            
         LA    RE,0(RF,R3)         POINT TO EXTENDED HEADER                     
         CLC   REPRFL#,0(RE)       MATCH ON FIELD NUMBER & RFLTYPE              
         BE    SETS0063                                                         
         LA    R2,REPFLNQ(,R2)     NEXT FIELD NEEDED                            
         B     SETS0060            TRY NEXT FIELD                               
*                                                                               
SETS0063 CLI   APPFKEY,PFKUP                                                    
         BNE   SETS0065                                                         
         MVI   APPFKEY,0                                                        
*                                                                               
SETS0064 CR    R1,R2               IF EQUAL THEN AT TOP OF TABLE                
         BE    SETS0070                                                         
         AHI   R2,-REPFLNQ         BACK ONE FIELD                               
         TM    REPFIND2,REPFTOP    START OF NEW LINE                            
         BZ    SETS0064            MUST BE A NEW LINE TYPE                      
         CLI   SKIPFLAG,NO         SET TO SKIP                                  
         BE    SETS0065                                                         
         TM    REPFIND2,REPFSKIP   SKIP THIS LINE WHEN ON                       
         BO    SETS0064                                                         
*                                                                               
SETS0065 CLI   APPFKEY,PFKDOWN                                                  
         BNE   SETS0070                                                         
         MVI   APPFKEY,0                                                        
*                                                                               
SETS0068 LA    R2,REPFLNQ(,R2)     FORWARD ONE FIELD                            
         CLI   REPRFL#,X'FF'       AT BOTTOM NOW ?                              
         BE    SETS0064            YES, BUMP BACK TILL OK                       
         TM    REPFIND2,REPFTOP    START OF NEW LINE                            
         BZ    SETS0068            MUST BE A TOP LINE TYPE                      
         CLI   SKIPFLAG,NO         SET TO SKIP                                  
         BE    SETS0070                                                         
         TM    REPFIND2,REPFSKIP   SKIP THIS LINE WHEN ON                       
         BO    SETS0068                                                         
*                                                                               
SETS0070 CLI   SKIPFLAG,YES        SET TO SKIP                                  
         BNE   SETS0090                                                         
         TM    REPFIND2,REPFSKIP   SKIP THIS LINE WHEN ON                       
         BZ    SETS0090                                                         
         LA    R2,REPFLNQ(,R2)     NEXT FIELD NEEDED                            
         B     SETS0060                                                         
*                                                                               
SETS0090 LA    R3,COLTAGH                                                       
*&&US*&& LA    R0,11               MAX SCREEN LINES                             
*&&UK*&& LA    R0,12               MAX SCREEN LINES                             
         MVI   TWAELCD,1                                                        
         MVI   TWAERLN,2           FIRST PLACE TO INSERT                        
         MVI   LINESTAT,0                                                       
*                                                                               
SETS0200 CLI   REPRFL#,X'FF'       END OF LIST                                  
         BE    SETSPFK             SHOW PFKEYS                                  
         CLI   SKIPFLAG,YES        SET TO SKIP                                  
         BNE   SETS0205                                                         
         TM    REPFIND2,REPFSKIP   SKIP THIS LINE WHEN ON                       
         BZ    SETS0205                                                         
         LA    R2,REPFLNQ(,R2)     NEXT FIELD NEEDED                            
         B     SETS0200                                                         
*                                                                               
         USING FLDDESD,R6                                                       
SETS0205 L     R6,=A(FLDTABLE)                                                  
         A     R6,APRELO                                                        
*                                                                               
SETS0210 CLI   FLDLEN,X'FF'                                                     
         BE    SETSPFK                                                          
         CLC   REPFFLDN,FLDNUM     MATCH ON FIELD                               
         BNE   SETS0220                                                         
         CLI   FLDRTYP,0           DEFAULT                                      
         BE    SETS0300                                                         
         CLC   APREPJCL,FLDRTYP                                                 
         BE    SETS0300                                                         
*                                                                               
SETS0220 ZIC   RF,FLDLEN                                                        
         AR    R6,RF                                                            
         B     SETS0210                                                         
*                                                                               
SETS0300 MVI   TWAECOL,2           FIELD STARTS @ COL 2                         
         TM    LINESTAT,SAMELINE   UNLESS 2ND FIELD                             
         BZ    *+8                                                              
         MVI   TWAECOL,41                                                       
         TM    LINESTAT,NXTAVAIL   USE NEXT AVAILABLE SPACE                     
         BZ    *+8                                                              
         MVI   TWAECOL,0                                                        
         MVI   TWAEATB,X'68'       PROT,LOWER,HIGHLIGHT                         
         MVC   TWAEFLD,REPRFL#                                                  
         XC    TWAEDTA(L'TWAELEM-TWAELLNQ),TWAEDTA                              
         SR    RF,RF                                                            
         IC    RF,FLDLEN                                                        
         AHI   RF,-FLDLNQ                                                       
         STC   RF,TWAEFLN          LENGTH OF DATA FIELD                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TWAEDTA(0),FLDDESC                                               
         LA    RF,TWAELLNQ+1(,RF)                                               
         STC   RF,TWAELLN          LENGTH OF ELEMENT                            
*                                                                               
SETS0318 GOTO1 VTWABLD,APPARM                                                   
         CLI   SCRPARM.TWAPERRS,0                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   FLDULEN,0           ONLY TEXT FIELD ?                            
         BE    SETS0320            NO INPUT FIELD                               
*                                                                               
         MVI   TWAERLN,0           SAME LINE                                    
         NI    LINESTAT,X'FF'-NXTAVAIL                                          
                                                                                
         CLI   REPFFLDN,FLDNFLT1   USE NEXT AVAILABLE SPACE                     
         BL    *+8                                                              
         OI    LINESTAT,NXTAVAIL                                                
                                                                                
*&&UK                                                                           
         CLI   REPFFLDN,FLDNBNVS   BR'O INVOICE STATUS OR HIGHER FLDN           
         BL    *+8                                                              
         OI    LINESTAT,NXTAVAIL                                                
         CLI   REPFFLDN,FLDNSUPE   SUPPRESS UNAP EST                            
         BNE   *+8                                                              
         OI    LINESTAT,NXTAVAIL                                                
*&&                                                                             
         MVC   SCRPARM.TWAPAOUT,SCRPARM.TWAPANXT                                
         MVI   TWAECOL,17          ALL INPUT FILEDS START @ COL 17              
         TM    LINESTAT,SAMELINE                                                
         BZ    *+8                                                              
         MVI   TWAECOL,56          IF ON SAME LINE START @ COL 56               
*&&UK                                                                           
         CLI   REPFFLDN,FLDNTETM                                                
         BNE   *+8                                                              
         MVI   TWAECOL,76                                                       
*&&                                                                             
         TM    LINESTAT,NXTAVAIL                                                
         BZ    *+8                                                              
         MVI   TWAECOL,0                                                        
*                                                                               
         MVI   TWAELCD,1           ELEMENT CODE                                 
         MVI   TWAEATB,X'00'       THE INPUT FIELD                              
         MVI   TWAEFLD,0                                                        
         XC    TWAEDTA(L'TWAELEM-TWAELLNQ),TWAEDTA                              
         MVC   TWAEFLN,FLDULEN     INPUT FIELD LENGTH                           
         MVI   TWAELLN,TWAELLNQ    LENGTH OF ELEM                               
*                                                                               
         GOTO1 VTWABLD,APPARM                                                   
         CLI   SCRPARM.TWAPERRS,0                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,SCRPARM.TWAPAOUT GET DISP OF LAST FIELD                       
         SR    RF,R5               SUBTRACT A(TWA)                              
         STCM  RF,3,LASTTWAF                                                    
*                                                                               
SETS0320 MVI   LINESTAT,0                                                       
         AHI   R0,-1                                                            
         TM    REPFIND1,REPF1LN+REPF2LN                                         
         BNZ   SETS0325                                                         
         OI    LINESTAT,SAMELINE                                                
         AHI   R0,1                ADD BACK, SAME LINE                          
*                                                                               
SETS0325 TM    REPFIND1,X'80'      USE NEXT AVAILABLE SPACE                     
         BZ    *+8                                                              
         OI    LINESTAT,NXTAVAIL                                                
*                                                                               
         MVC   TWAERLN,REPFIND1    NEXT LINE/NEXT AVAIL IS OUTPUT               
         NI    TWAERLN,REPF1LN+REPF2LN                                          
         MVC   SCRPARM.TWAPAOUT,SCRPARM.TWAPANXT                                
         LA    R2,REPFLNQ(,R2)     NEXT FIELD NEEDED                            
         LTR   R0,R0                                                            
         BP    SETS0200    ONLY ALLOW 11 (US) OR 12 (UK) ROWS ON SCREEN         
         EJECT ,                                                                
***********************************************************************         
*  BUILD PFKEY LINES AND END TAB (UK)                                 *         
***********************************************************************         
         SPACE 1                                                                
SETSPFK  MVI   TWAELCD,1           ELEMENT CODE                                 
         MVI   TWAERLN,23          STARTS AT ROW 23                             
         OI    TWAERLN,TWAERLAB                                                 
         MVI   TWAECOL,2           COLUMN 2                                     
         MVI   TWAEATB,X'68'       PROT,LOWER, HIGHLIGHTED                      
         MVI   TWAEFLD,X'FE'       FIELD NUMBER                                 
         XC    TWAEDTA(L'TWAELEM-TWAELLNQ),TWAEDTA                              
         MVC   TWAEDTA(10),=CL10'PFKEY 1'                                       
         MVI   TWAEFLN,78          INPUT FIELD LENGTH                           
         MVI   TWAELLN,TWAELLNQ+10 LENGTH OF ELEM                               
*                                                                               
         GOTO1 VTWABLD,APPARM                                                   
         CLI   SCRPARM.TWAPERRS,0                                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SCRPARM.TWAPAOUT,SCRPARM.TWAPANXT                                
         MVI   TWAELCD,1           ELEMENT CODE                                 
         MVI   TWAERLN,0                                                        
         MVI   TWAECOL,3           COLUMN 2                                     
         MVI   TWAEATB,X'68'       PROT,LOWER, HIGHLIGHTED                      
         MVI   TWAEFLD,X'FE'       FIELD NUMBER                                 
         XC    TWAEDTA(L'TWAELEM-TWAELLNQ),TWAEDTA                              
         MVC   TWAEDTA(10),=CL10'PFKEY 2'                                       
         MVI   TWAEFLN,77          INPUT FIELD LENGTH                           
         MVI   TWAELLN,TWAELLNQ+10 LENGTH OF ELEM                               
*                                                                               
         GOTO1 VTWABLD,APPARM                                                   
         CLI   SCRPARM.TWAPERRS,0                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SCRPARM.TWAPAOUT,SCRPARM.TWAPANXT                                
*                                                                               
SETSXIT  XIT1                                                                   
         DROP  R2,R4                                                            
         DROP  SCRPARM                                                          
         EJECT ,                                                                
***********************************************************************         
*  FIELD LENGTH EQUATES                                               *         
***********************************************************************         
         SPACE 1                                                                
LENFFULL EQU   62                  FULL    LINE                                 
LENFHALF EQU   23                  PARTIAL LINE                                 
LENFFCUR EQU   4                   FOREIGN CURRENCY                             
LENFWCTP EQU   12                  WORK    CODE TYPE                            
LENFFLTR EQU   11                  FILTER  LIST                                 
LENFTIMT EQU   5                   TYPE    OF   TIME                            
LENFONE  EQU   1                   ONE                                          
LENFMTHD EQU   3                   METHOD                                       
LENFAUTH EQU   6                   AUTH STATUS                                  
LENFTSTA EQU   7                   TIME STATUS                                  
LENFAMT  EQU   13                  AMOUNT                                       
LENF3QRT EQU   45                  THREE QUARTERS OF A LINE                     
*                                                                               
LBUDG#   EQU   5                   MAXIMUM LENGTH FOR NUMERIC BUDGET #          
         EJECT ,                                                                
***********************************************************************         
*  TABLE OF SCREEN TO LOAD                                            *         
***********************************************************************         
         SPACE 1                                                                
SCRTABLE DC    AL1(REPJCLV),AL2(REPFLDSV-REPFLDS)                               
         DC    AL1(REPJCLI),AL2(REPFLDSI-REPFLDS)                               
         DC    AL1(REPJCL1),AL2(REPFLDSL-REPFLDS)                               
         DC    AL1(REPJCLP),AL2(REPFLDSP-REPFLDS)                               
         DC    AL1(REPJCLR),AL2(REPFLDSR-REPFLDS)                               
         DC    AL1(REPJCLX),AL2(REPFLDSX-REPFLDS)                               
         DC    AL1(REPJCL2),AL2(REPFLD1C-REPFLDS)                               
         DC    AL1(REPJCLB),AL2(REPFLDSC-REPFLDS)                               
         DC    AL1(REPJCLG),AL2(REPFLDGL-REPFLDS)                               
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  XL1 - LENGTH OF ENTRY                                              *         
*  XL1 - LENGTH OF FIELD - FULL/PARTIAL LINE OR STRING OR             *         
*                          FIELD INCLUDING THE EXCLUDE CHARACTERS     *         
*  XL1 - FIELD NUMBER                                                 *         
*  XL1 - REPORT TYPE VALID FOR                                        *         
*  0C  - FIELD TITLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
FLDTABLE DS    0C                                                               
         SPACE 1                                                                
         DC    AL1(FTBBUDGX-*,LINPBUDG,FLDNBUDG,0)                              
         DCDD  AC#BGT,14                                                        
FTBBUDGX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBFCURX-*,LENFFCUR,FLDNFCUR,0)  CURRENCY                    
         DC    CL14'Currency'                                                   
FTBFCURX EQU   *                                                                
*&&UK                                                                           
         SPACE 1                                                                
         DC    AL1(FTBFAMFX-*,LENFAMT,FLDNAMFR,0)   AMOUNT FROM                 
         DCDD  AC#RSAFR,14                                                      
FTBFAMFX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBFAMTX-*,LENFAMT,FLDNAMTO,0)   AMOUNT T0                   
         DCDD  AC#RSATO,5                                                       
FTBFAMTX EQU   *                                                                
*&&                                                                             
         SPACE 1                                                                
         DC    AL1(FTBACCTX-*,LENFFULL,FLDNACCT,0)  ACCOUNT                     
         DCDD  AC#RSACC,14                                                      
FTBACCTX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBCNT1X-*,LENFFULL,FLDNCNTR,REPJCL1)   COST/NON-CLI         
         DCDD  AC#CSTNC,14                                                      
FTBCNT1X EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBCNT2X-*,LENFFULL,FLDNCNTR,0)  CONTRA ACCOUNT              
         DCDD  AC#RSCOC,14                                                      
FTBCNT2X EQU   *                                                                
         SPACE 1                                                                
*        DC    AL1(FTBSLSCX-*,14,LENFFULL,FLDNSLSC,0)  SALES/CONTRA             
*        DC    CL14'Sales/contra'                                               
*TBSLSCX EQU   *                                                                
         SPACE 1                                                                
*        DC    AL1(FTBCLPRX-*,LENFFULL,FLDNCLPR,0)                              
*        DCDD  AC#CLIP,14                                                       
*TBCLPRX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBCSTAX-*,LENFFULL,FLDNCSTA,0)  COSTING ACCOUNT             
         DCDD  AC#CSTA,14                                                       
FTBCSTAX EQU   *                                                                
         SPACE 1                                                                
*        DC    AL1(FTBCSTNX-*,LENFFULL,FLDNCSTN,0)                              
*        DCDD  AC#CSTNC,14                                                      
*TBCSTNX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBBILLX-*,LENFHALF,FLDNBILL,0)  BILLING                     
         DC    CL14'Billing'                                                    
FTBBILLX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBRVNUX-*,LENFHALF,FLDNRVNU,0)  REVENUE                     
         DC    CL14'Revenue'                                                    
FTBRVNUX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBOF1RX-*,LENFHALF,FLDNOFFC,REPJCL1)  PERSON OFFICE         
         DC    C'Person office'                                                 
FTBOF1RX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBOFFCX-*,LENFHALF,FLDNOFFC,0)  OFFICE                      
         DCDD  AC#OFF,14                                                        
FTBOFFCX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBOFCCX-*,LENFHALF,FLDNOFCC,0)  CLIENT OFFICE               
         DC    C'Client office'                                                 
FTBOFCCX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBTYPEX-*,LINPTTYP,FLDNTRNT,0)  TYPE                        
         DCDD  AC#RSTTY,14                                                      
FTBTYPEX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBWCTPX-*,LENFWCTP,FLDNWCTP,0)  WORK CODE TYPE              
         DCDD  AC#WCTYP,14                                                      
FTBWCTPX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBTIMTX-*,LENFTIMT,FLDNTIMT,0)  TYPE OF TIME                
*&&US                                                                           
         DC    CL14'Type of Time'                                               
*&&                                                                             
*&&UK                                                                           
         DCDD  AC#TYTIM,14                                                      
*&&                                                                             
FTBTIMTX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBBSRCX-*,LENFFULL,FLDNBSRC,0)  BILLING SOURCE              
         DCDD  AC#BLGSR,14                                                      
FTBBSRCX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBCLIX-*,LENFFULL,FLDNCLNT,REPJCL2)  CLIENT                 
         DCDD  AC#CLINT,14                                                      
FTBCLIX  EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBCPJX-*,LENFFULL,FLDNCLNT,REPJCL1)  CLI/PRD/JOB            
         DCDD  AC#CPJ,14                                                        
FTBCPJX  EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBCLNTX-*,LENFFULL,FLDNCLNT,0)  CLI/PRO/JOB(EST)            
         DCDD  AC#CLIPJ,14                                                      
FTBCLNTX EQU   *                                                                
         SPACE 1                                                                
*        DC    AL1(FTBBTYPX-*,LENFFULL,FLDNBILT,0)                              
*        DCDD  AC#BLGTY,14                                                      
*TBBTYPX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBDEPTX-*,LENFFULL,FLDNDEPT,0)  DEPARTMENT                  
         DCDD  AC#DPT,14                                                        
FTBDEPTX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBPRSNX-*,LENFFULL,FLDNPRSN,0)  PERSON                      
         DCDD  AC#PRSN,14                                                       
FTBPRSNX EQU   *                                                                
                                                                                
         DC    AL1(FTB14PTX-*,LENFFULL,FLDN14PT,0)  POINTER                     
         DC    CL3'14'                                                          
         DCDD  AC#PTR,11                                                        
FTB14PTX EQU   *                                                                
                                                                                
         DC    AL1(FTBTSK1X-*,LENFFULL,FLDNTASK,REPJCL1)  TASK                  
         DCDD  AC#TASK,14                                                       
FTBTSK1X EQU   *                                                                
                                                                                
         DC    AL1(FTBTSK2X-*,LENFFULL,FLDNTASK,0)  WORK CODE                   
         DCDD  AC#WC,14                                                         
FTBTSK2X EQU   *                                                                
                                                                                
         DC    AL1(FTBUFX-*,LENFFULL,FLDNUFLD,0)    USER FIELD                  
         DCDD  AC#RSUSF,14                                                      
FTBUFX   EQU   *                                                                
                                                                                
         DC    AL1(FTBFLT1X-*,LENFFLTR,FLDNFLT1,0)  ACC FILTER 1                
         DCDD  AC#FLT1,3                                                        
FTBFLT1X EQU   *                                                                
                                                                                
         DC    AL1(FTBFLT2X-*,LENFFLTR,FLDNFLT2,0)  ACC FILTER 2                
         DCDD  AC#FLT2,3                                                        
FTBFLT2X EQU   *                                                                
                                                                                
         DC    AL1(FTBFLT3X-*,LENFFLTR,FLDNFLT3,0)  ACC FILTER 3                
         DCDD  AC#FLT3,3                                                        
FTBFLT3X EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBFLT4X-*,LENFFLTR,FLDNFLT4,0)  ACC FILTER 4                
         DCDD  AC#FLT4,3                                                        
FTBFLT4X EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBFLT5X-*,LENFFLTR,FLDNFLT5,0)  ACC FILTER 5                
         DCDD  AC#FLT5,3                                                        
FTBFLT5X EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBRCONX-*,1,FLDNRCON,0)   RECONCILED                        
         DCDD  AC#RCND,14                                                       
FTBRCONX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBAPPRX-*,1,FLDNAPPR,0)   APPROVED                          
         DCDD  AC#APRVD,14                                                      
FTBAPPRX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBURGTX-*,1,FLDNURGT,0)   URGENT                            
         DCDD  AC#URG,14                                                        
FTBURGTX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBOFSTX-*,1,FLDNOFST,0)   CONTRA (OFFSET)                   
         DCDD  AC#OFFST,14                                                      
FTBOFSTX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBAOFFX-*,LENFHALF,FLDNAOFF,0)  ANALYSIS OFFICE             
         DCDD  AC#ANOFF,14                                                      
FTBAOFFX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBMTHDX-*,LENFMTHD,FLDNMTHD,0)  METHOD                      
         DCDD  AC#METH,14                                                       
FTBMTHDX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBSPCX-*,0,FLDNSPC,0)                                       
         DC    C' '                                                             
FTBSPCX  EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBMOREX-*,0,FLDNMORE,0)                                     
         DC    C'<More>'                                                        
FTBMOREX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBDPCTX-*,0,FLDNDPCT,0)                                     
         DC    C'PCT Denominator'                                               
FTBDPCTX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBDLINX-*,0,FLDNDLIN,0)                                     
         DC    C'---------------'                                               
FTBDLINX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBAUTHX-*,LENFAUTH,FLDNAUTH,0)  AUTH STATUS                 
         DCDD  AC#RSSTA,12                                                      
FTBAUTHX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBSUPEX-*,1,FLDNSUPE,0)  SUPPRESS UNAPP ESTM                
         DCDD  AC#SUPES,14                                                      
FTBSUPEX EQU   *                                                                
*&&UK                                                                           
         SPACE 1                                                                
         DC    AL1(FTBESTMX-*,1,FLDNTETM,0)  ESTIMATED TIME                     
         DCDD  AC#ESTIM,14                                                      
FTBESTMX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBBNVSX-*,5,FLDNBNVS,0)        BRO INV STATUS               
***      DCDD  AC#BREXP,14       ***TEMP DD ***                                 
         DCDD  AC#BRNVS,14                                                      
FTBBNVSX EQU   *                                                                
*&&                                                                             
         SPACE 1                                                                
         DC    AL1(FTBTSTAX-*,LENFTSTA,FLDNTSTA,0) TIME STATUS                  
         DCDD  AC#TSTAT,14                                                      
FTBTSTAX EQU   *                                                                
*&&US                                                                           
         SPACE 1                                                                
         DC    AL1(FLDNAPMX-*,LENFWCTP,FLDNAPMT,0)                              
         DCDD  AC#RSAP2,14                                                      
FLDNAPMX EQU   *                                                                
*&&                                                                             
*&&UK                                                                           
         SPACE 1                                                                
         DC    AL1(FTBXDANX-*,LENF3QRT,FLDNTXDN,0)  DATA NAME                   
         DCDD  AC#DTANM,14                                                      
FTBXDANX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBXDAVX-*,LENFFULL,FLDNTXDV,0)  DATA VALUE                  
         DCDD  AC#DTAVL,14                                                      
FTBXDAVX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBETYPX-*,LENFHALF,FLDNTETY,0)  EXP. TYPE                   
         DCDD  AC#EXTYP,14                                                      
FTBETYPX EQU   *                                                                
         SPACE 1                                                                
         DC    AL1(FTBXDORX-*,1,FLDNTXDO,0)  XDATA ORIGIN                       
         DCDD  AC#XDAOR,14                                                      
FTBXDORX EQU   *                                                                
         SPACE 1                                                                
*&&                                                                             
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  LIST OF FIELDS TO BUILD FOR REPORTS (PRODUCTION) see REPFD         *         
***********************************************************************         
         SPACE 1                                                                
REPFLDS  DS    0H                                                               
REPFLDSV DS    0H                                                               
*&&US*&& DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLFCUR,0,FLDNFCUR),X'8000'                                  
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLTTYPE,0,FLDNTRNT),X'2100'                                 
         DC    AL1(RFLWCTYP,0,FLDNWCTP),X'0000'                                 
*&&UK*&& DC    AL1(RFLTTIME,0,FLDNTIMT),X'8000'                                 
*&&US*&& DC    AL1(RFLTTIME,0,FLDNTIMT),X'0100'                                 
*&&UK*&& DC    AL1(RFLESTIM,0,FLDNTETM),X'0100'                                 
         DC    AL1(RFLWC,0,FLDNTASK),X'2100'                                    
         DC    AL1(RFLUF,0,FLDNUFLD),X'0100'                                    
*&&UK*&& DC    AL1(RFLAUTH,0,FLDNAUTH),X'0000'                                  
*&&UK*&& DC    AL1(RFLSUPE,0,FLDNSUPE),X'A000'                                  
*&&UK*&& DC    AL1(RFLBNVS,0,FLDNBNVS),X'0100'                                  
         DC    AL1(RFLTSTA,0,FLDNTSTA),X'0000'                                  
*&&UK*&& DC    AL1(RFLETYPE,0,FLDNTETY),X'0100'                                 
*&&UK*&& DC    AL1(RFLXDATN,0,FLDNTXDN),X'8040'                                 
*&&UK*&& DC    AL1(RFLXDAOR,0,FLDNTXDO),X'0140'                                 
*&&UK*&& DC    AL1(RFLXDATV,0,FLDNTXDV),X'0040'                                 
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  INCOME SCREEN FIELDS see REPFD                                     *         
***********************************************************************         
         SPACE 1                                                                
REPFLDSI DS    0H                                                               
         DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLFCUR,0,FLDNFCUR),X'8000'                                  
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLTTYPE,0,FLDNTRNT),X'2100'                                 
         DC    AL1(RFLCLI,0,FLDNCLNT),X'2120'                                   
         DC    AL1(RFLCOST,0,FLDNCSTA),X'2120'                                  
         DC    AL1(RFL11BL,0,FLDNBILL),X'2020'                                  
         DC    AL1(RFL12RV,0,FLDNRVNU),X'2120'                                  
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  RECEIVABLES SCREEN FIELDS see REPFD                                *         
***********************************************************************         
         SPACE 1                                                                
REPFLDSR DS    0H                                                               
         DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLFCUR,0,FLDNFCUR),X'8000'                                  
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLTTYPE,0,FLDNTRNT),X'2100'                                 
         DC    AL1(RFLCLI,0,FLDNCLNT),X'2120'                                   
         DC    AL1(RFLBSR,0,FLDNBSRC),X'2100'                                   
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  EXPENSE SCREEN FIELDS see REPFD                                    *         
***********************************************************************         
         SPACE 1                                                                
REPFLDSX DS    0H                                                               
         DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLFCUR,0,FLDNFCUR),X'8000'                                  
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLTTYPE,0,FLDNTRNT),X'2100'                                 
         DC    AL1(RFLDEPT,0,FLDNDEPT),X'2120'                                  
         DC    AL1(RFLPRSN,0,FLDNPRSN),X'2120'                                  
         DC    AL1(RFLCOST,0,FLDNCSTA),X'2120'                                  
         DC    AL1(RFLAOFF,0,FLDNAOFF),X'A000'                                  
*&&UK*&& DC    AL1(RFLBNVS,0,FLDNBNVS),X'0100'                                  
*&&UK*&& DC    AL1(RFLXDATN,0,FLDNTXDN),X'8040'                                 
*&&UK*&& DC    AL1(RFLXDAOR,0,FLDNTXDO),X'0140'                                 
*&&UK*&& DC    AL1(RFLXDATV,0,FLDNTXDV),X'0040'                                 
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  PAYABLES SCREEN FIELDS see REPFD                                   *         
***********************************************************************         
         SPACE 1                                                                
REPFLDSP DS    0H                                                               
         DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLFCUR,0,FLDNFCUR),X'8000'                                  
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLTTYPE,0,FLDNTRNT),X'2100'                                 
         DC    AL1(RFLCLI,0,FLDNCLNT),X'2120'                                   
         DC    AL1(RFLAPPRV,0,FLDNAPPR),X'0000'                                 
         DC    AL1(RFLOFFST,0,FLDNOFST),X'0100'                                 
         DC    AL1(RFLURGNT,0,FLDNURGT),X'0000'                                 
*&&UK*&& DC    AL1(RFLAUTH,0,FLDNAUTH),X'0100'                                  
*&&US*&& DC    AL1(RFLAPMT,0,FLDNAPMT),X'0000'                                  
*&&UK*&& DC    AL1(RFLETYPE,0,FLDNTETY),X'A000'                                 
*&&UK*&& DC    AL1(RFLBNVS,0,FLDNBNVS),X'0100'                                  
*&&UK*&& DC    AL1(RFLXDATN,0,FLDNTXDN),X'8040'                                 
*&&UK*&& DC    AL1(RFLXDAOR,0,FLDNTXDO),X'0140'                                 
*&&UK*&& DC    AL1(RFLXDATV,0,FLDNTXDV),X'0040'                                 
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  PERSON SCREEN FIELDS see REPFD                                     *         
***********************************************************************         
         SPACE 1                                                                
REPFLDSL EQU   *                                                                
*&&US*&& DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLBUDGT,0,FLDNBUDG),X'8001'                                 
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLCOFF,0,FLDNOFCC),X'2100'                                  
         DC    AL1(RFLTTIME,0,FLDNTIMT),X'0000'                                 
         DC    AL1(RFLMTHD,0,FLDNMTHD),X'0100'                                  
         DC    AL1(RFLCLI,0,FLDNCLNT),X'2120'                                   
         DC    AL1(RFL14DLB,0,FLDN14PT),X'2120'                                 
         DC    AL1(RFLWC,0,FLDNTASK),X'2100'                                    
         DC    AL1(0,0,FLDNSPC),X'0102'                                         
         DC    AL1(0,0,FLDNMORE),X'0102'                                        
         DC    AL1(251,0,FLDNDPCT),X'0103'                                      
         DC    AL1(0,0,FLDNDLIN),X'0102'                                        
         DC    AL1(RFLDCNTR,0,FLDNCNTR),X'2102'                                 
         DC    AL1(RFLDTTME,0,FLDNTIMT),X'2102'                                 
         DC    AL1(RFLDCLI,0,FLDNCLNT),X'2102'                                  
         DC    AL1(RFLDWC,0,FLDNTASK),X'2102'                                   
         DC    AL1(RFLTSTA,0,FLDNTSTA),X'0000'                                  
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  PROFIT AND LOSS SCREEN FIELDS see REPFD                            *         
***********************************************************************         
         SPACE 1                                                                
REPFLD1C EQU   *                                                                
*&&US*&& DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLBUDGT,0,FLDNBUDG),X'8001'                                 
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLMTHD,0,FLDNMTHD),X'0100'                                  
         DC    AL1(RFLCLI,0,FLDNCLNT),X'2120'                                   
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  CASH SCREEN FIELDS see REPFD                                       *         
***********************************************************************         
         SPACE 1                                                                
REPFLDSC EQU   *                                                                
         DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLFCUR,0,FLDNFCUR),X'8000'                                  
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLTTYPE,0,FLDNTRNT),X'2100'                                 
         DC    AL1(RFLRECON,0,FLDNRCON),X'0100'                                 
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  GENERAL LEDGER SCREEN FIELDS see REPFD                             *         
***********************************************************************         
         SPACE 1                                                                
REPFLDGL EQU   *                                                                
         DC    AL1(RFLBUDGT,0,FLDNBUDG),X'0101'                                 
*&&UK*&& DC    AL1(RFLFCUR,0,FLDNFCUR),X'8000'                                  
*&&UK*&& DC    AL1(RFLAMFR,0,FLDNAMFR),X'8080'                                  
*&&UK*&& DC    AL1(RFLAMTO,0,FLDNAMTO),X'0180'                                  
         DC    AL1(RFLACC,0,FLDNACCT),X'2120'                                   
         DC    AL1(RFLCNTR,0,FLDNCNTR),X'2120'                                  
         DC    AL1(RFLFILT1,0,FLDNFLT1),X'A000'                                 
         DC    AL1(RFLFILT2,0,FLDNFLT2),X'A000'                                 
         DC    AL1(RFLFILT3,0,FLDNFLT3),X'A000'                                 
         DC    AL1(RFLFILT4,0,FLDNFLT4),X'A000'                                 
         DC    AL1(RFLFILT5,0,FLDNFLT5),X'2100'                                 
         DC    AL1(RFLOFF,0,FLDNOFFC),X'2000'                                   
         DC    AL1(RFLTTYPE,0,FLDNTRNT),X'2100'                                 
         DC    X'FF'                                                            
         EJECT ,                                                                
SPCKYWS  DC    AL2(AC#RSPCT),AL1(RFLDCNTR,RFLDTTME,RFLDCLI,RFLDWC,0,0)          
         DC    AL2(AC#RSPTC),AL1(RFLDCLI,0,0,0,0,0)                             
*        DC    AL2(AC#A$TOT),AL1(RFLMTHD,0,0,0,0,0)                             
*        DC    AL2(AC#A$SAL),AL1(RFLMTHD,0,0,0,0,0)                             
*        DC    AL2(AC#A$BEN),AL1(RFLMTHD,0,0,0,0,0)                             
*        DC    AL2(AC#A$PEN),AL1(RFLMTHD,0,0,0,0,0)                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*       SPECIAL CONTRA KEYWORDS THAT NEED FURTHER FILTERING           *         
***********************************************************************         
         SPACE 1                                                                
CNTRKYW  DC    AL2(AC#RSGRS),C'11'    BILLING          (INC)                    
CNTRKYWQ EQU   *-CNTRKYW                                                        
         DC    AL2(AC#RSBIL),C'11'    BILLING          (P&L)                    
         DC    AL2(AC#RSREV),C'12'    REVENUE          (P&L)                    
         DC    AL2(AC#RSDXP),C'13'    DIRECT   EXPENSE (P&L)                    
         DC    AL2(AC#RSDLT),C'14'    DIRECT   LABOR   (P&L) (TOTAL)            
         DC    AL2(AC#RSDLS),C'14'    DIRECT   LABOR   (P&L) (SAL)              
         DC    AL2(AC#RSDLB),C'14'    DIRECT   LABOR   (P&L) (BEN)              
         DC    AL2(AC#RSDLP),C'14'    DIRECT   LABOR   (P&L) (PEN)              
         DC    AL2(AC#RSILT),C'15'    INDIRECT LABOR   (P&L) (TOTAL)            
         DC    AL2(AC#RSILS),C'15'    INDIRECT LABOR   (P&L) (SAL)              
         DC    AL2(AC#RSILB),C'15'    INDIRECT LABOR   (P&L) (BEN)              
         DC    AL2(AC#RSILP),C'15'    INDIRECT LABOR   (P&L) (PEN)              
         DC    AL2(AC#RSOVH),C'16'    OVERHEAD         (P&L)                    
         DC    AL2(0)                                                           
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TABLE FOR VALIDATING AND DISPLAYING OF THE DETAIL FIELDS   *         
***********************************************************************         
         SPACE 1                                                                
* SEE ROUTD FOR DSECT                                                           
ROUTINES DS    0CL3                                                             
         DC    AL1(FLDNACCT,ROUTV01,ROUTD01)   VALACT                           
         DC    AL1(FLDNCNTR,ROUTV02,ROUTD01)   VALCNT                           
         DC    AL1(FLDNAOFF,ROUTV03,ROUTD01)   VALOFF                           
         DC    AL1(FLDNOFFC,ROUTV03,ROUTD01)     "                              
         DC    AL1(FLDNOFCC,ROUTV03,ROUTD01)     "                              
         DC    AL1(FLDNBUDG,ROUTV04,ROUTD02)   VALBUD                           
         DC    AL1(FLDNTRNT,ROUTV05,ROUTD03)   VALTTY                           
         DC    AL1(FLDNWCTP,ROUTV06,ROUTD01)   VALWCT                           
         DC    AL1(FLDNTIMT,ROUTV07,ROUTD01)   VALTYT                           
         DC    AL1(FLDNTASK,ROUTV09,ROUTD01)   VALTSK                           
         DC    AL1(FLDNBILL,ROUTV10,ROUTD01)   VALGEN                           
         DC    AL1(FLDNCLNT,ROUTV10,ROUTD01)     "                              
         DC    AL1(FLDNCSTA,ROUTV10,ROUTD01)     "                              
         DC    AL1(FLDNDEPT,ROUTV10,ROUTD01)     "                              
         DC    AL1(FLDNPRSN,ROUTV10,ROUTD01)     "                              
         DC    AL1(FLDNRVNU,ROUTV10,ROUTD01)     "                              
         DC    AL1(FLDN14PT,ROUTV10,ROUTD01)     "                              
         DC    AL1(FLDNUFLD,ROUTV11,ROUTD06)   VALUFD                           
         DC    AL1(FLDNRCON,ROUTV12,ROUTD04)   VALSTA                           
         DC    AL1(FLDNAPPR,ROUTV12,ROUTD04)     "                              
         DC    AL1(FLDNOFST,ROUTV12,ROUTD04)     "                              
         DC    AL1(FLDNURGT,ROUTV12,ROUTD04)     "                              
         DC    AL1(FLDNFLT1,ROUTV13,ROUTD01)   VALFLT                           
         DC    AL1(FLDNFLT2,ROUTV13,ROUTD01)     "                              
         DC    AL1(FLDNFLT3,ROUTV13,ROUTD01)     "                              
         DC    AL1(FLDNFLT4,ROUTV13,ROUTD01)     "                              
         DC    AL1(FLDNFLT5,ROUTV13,ROUTD01)     "                              
         DC    AL1(FLDNBSRC,ROUTV14,ROUTD01)   VALBSR                           
         DC    AL1(FLDNFCUR,ROUTV15,ROUTD01)   VALFCR                           
         DC    AL1(FLDNMTHD,ROUTV16,ROUTD05)   VALMTH                           
         DC    AL1(FLDNAUTH,ROUTV17,ROUTD07)   VALAUTH                          
         DC    AL1(FLDNSUPE,ROUTV18,ROUTD08)   VALYN                            
         DC    AL1(FLDNTSTA,ROUTV19,ROUTD01)   VALTST                           
*&&US*&& DC    AL1(FLDNAPMT,ROUTV20,ROUTD09)   VALAPMT                          
*&&UK                                                                           
         DC    AL1(FLDNTXDN,ROUTV21,ROUTD01)   VALXDAN                          
         DC    AL1(FLDNTXDV,ROUTV22,ROUTD01)   VALXDAV                          
         DC    AL1(FLDNTETY,ROUTV23,ROUTD01)   VALETYPE                         
         DC    AL1(FLDNAMFR,ROUTV24,ROUTD10)   VALAMT                           
         DC    AL1(FLDNAMTO,ROUTV24,ROUTD10)     "                              
         DC    AL1(FLDNTXDO,ROUTV25,ROUTD01)   VALXDOR                          
         DC    AL1(FLDNTETM,ROUTV12,ROUTD04)   VALSTA                           
         DC    AL1(FLDNBNVS,ROUTV26,ROUTD11)   VALBNVS                          
*&&                                                                             
         DC    X'FF'                                                            
         EJECT ,                                                                
SIGNTAB  DS    0CL4                                                             
         DC    AL1(EQ,1),C'= '                                                  
         DC    AL1(LT,1),C'< '                                                  
         DC    AL1(GT,1),C'> '                                                  
         DC    AL1(NEQ,2),C'<>'                                                 
         DC    AL1(LTE,2),C'<='                                                 
         DC    AL1(GTE,2),C'>='                                                 
#OFSIGNS EQU   (*-SIGNTAB)/4                                                    
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
MAXPARM  EQU   33                                                               
AUTHQ    EQU   C'A'                INCLUDE AUTH STATUS                          
UNAUTHQ  EQU   C'U'                INCLUDE UNAUTH STATUS                        
COTUP#   EQU   141                                                              
COSAVED  EQU   C'1'                SAVED                                        
COSUBMD  EQU   C'2'                SUBMITTED                                    
COCLAPR  EQU   C'3'                CLIENT APPROVED                              
COLMAPR  EQU   C'4'                LINE MANAGER APPROVED                        
COFUAPR  EQU   C'5'                FULLY APPROVED                               
*                                                                               
ASCRTAB  DS    A                                                                
ARFLCOL  DS    A                   FIRST RFLEL WITH SPECIFIC COLUMN #           
AREPFD   DS    A                   SAVE AREA FOR ADDRESS OF REPFD               
AEND     DS    A                   End of data                                  
ABREAK   DS    A                   Where found data                             
AUF      DS    A                   Current begining of user field code          
STRE     DS    A                   Store RE                                     
SVR2     DS    A                   Save  R2                                     
SVRE     DS    A                   Save  RE                                     
DEFENTRY DS    A                                                                
SCANFLN  DS    F                   Scan field length                            
ELEMENT  DS    CL(L'APELEM)                                                     
COLBKLNG DS    (MAXPARM)CL(LSCNNLNG) LONGER DATA BLOCK FOR SCANNER              
COLBLOCK DS    (MAXPARM)CL(LSCNNTRY) DATA BLOCK FOR SCANNER                     
SVKEY    DS    CL(L'IOKEY)         SAVE IOKEY                                   
SVELEM   DS    CL(L'APELEM)        SAVE APELEM                                  
SAVEUL   DS    CL2                 SAVE UNIT/LEDGER                             
SVKYWDD# DS    AL2                 Saved data dictionary number                 
CNTKYWUL DS    CL2                 Save off contra keywords unit/ldg            
SKIPFLAG DS    CL1                 USED TO INDICATE SKIP FIELDS                 
CHECKUL  DS    CL1                 WILDCARD TO CHECK U/L                        
FLDMAXL  DS    XL1                 MAXIMUM SIZE OF FIELD                        
LASTDELM DS    CL1                 Last delimiter found                         
DELIMTER DS    CL1                                                              
TUPPROF  DS    CL1                 TUP setting in cost profile record           
DEFTIMST DS    CL(LENFTSTA)        Default time status from TUP profile         
PTMSTAT  DS    XL1                 VALID PRODUCTION TIME STATUS                 
PTMSALL  EQU   X'F8'               ALL TIME                                     
PTMSSAVT EQU   X'80'               SAVED TIME                                   
PTMSSUBT EQU   X'40'               SUBMITTED TIME                               
PTMSREJT EQU   X'20'               REJECTED TIME                                
PTMSPART EQU   X'10'               PART APPROVED                                
PTMSFAPT EQU   X'08'               FULLY APPROVED TIME                          
LDGLISTN DS    XL1                 # OF UNIT/LEDGER IN LIST                     
LDGLIST  DS    CL40                UNIT/LEDGER LIST                             
VCOLLIST DS    CL100               VALID COLUMNS LIST                           
ADD_N_EL DS    XL1                 ADD AN ELEMENT NOW                           
LINESTAT DS    XL1                                                              
SAMELINE EQU   X'80'               FIELD ON SAME LINE                           
NXTAVAIL EQU   X'40'               NEXT AVAILABLE SPACE, NO SET COLUMN          
XWCTYPES DS    CL(LWCTYPES)        LOCAL COPY OF WCTYPES                        
BYTE     DS    CL1                 ONE BYTE WORK AREA                           
BYTE2    DS    CL1                 ONE BYTE WORK AREA                           
SVAPKEYH DS    CL(L'APKEYHD)       SAVED APKEYHD                                
CURRCOPT DS    XL1                 CURRENT RCOPT                                
CURAMTFR DS    PL6                 CURRENT AMOUNT FROM                          
LWSX     DS    0C                                                               
         EJECT ,                                                                
ROUTD    DSECT                                                                  
ROUTFLD  DS    CL1                 FIELD NUMBER                                 
ROUTVAL  DS    AL1                 VALIDATION ROUTINE DISPLACEMENT              
ROUTV01  EQU   1                     ACCOUNT                                    
ROUTV02  EQU   2                     CONTRA                                     
ROUTV03  EQU   3                     OFFICE                                     
ROUTV04  EQU   4                     BUDGET                                     
ROUTV05  EQU   5                     TRANSACTION TYPE                           
ROUTV06  EQU   6                     WORKCODE TYPE                              
ROUTV07  EQU   7                     TYPE OF TIME                               
ROUTV08  EQU   8                     COSTING ACCOUNT                            
ROUTV09  EQU   9                     WORKCODE/TASK                              
ROUTV10  EQU   10                    GENERAL ACCOUNT                            
ROUTV11  EQU   11                    BILLING OR REVENUE                         
ROUTV12  EQU   12                    STATUS - YES/NO/ONLY                       
ROUTV13  EQU   13                    FILTERS                                    
ROUTV14  EQU   14                    BILLING SOURCE                             
ROUTV15  EQU   15                    FOREIGN CURRENCY                           
ROUTV16  EQU   16                    METHOD TYPE                                
ROUTV17  EQU   17                    AUTH STATUS                                
ROUTV18  EQU   18                    YES/NO                                     
ROUTV19  EQU   19                    TIME STATUS                                
ROUTV20  EQU   20                    APPROVAL METHOD (US)                       
ROUTV21  EQU   21                    DATA NAME                                  
ROUTV22  EQU   22                    DATA VALUE                                 
ROUTV23  EQU   23                    EXPENDITURE TYPE                           
ROUTV24  EQU   24                    AMOUNT                                     
ROUTV25  EQU   25                    XDATA ORIGIN                               
ROUTV26  EQU   26                    BR'OCEAN INVOICE STATUS                    
ROUTDIS  DS    AL1                 DISPLAY ROUTINE DISPLACEMENT                 
ROUTD01  EQU   1                     General                                    
ROUTD02  EQU   2                     Budget                                     
ROUTD03  EQU   3                     Transaction type                           
ROUTD04  EQU   4                     Status                                     
ROUTD05  EQU   5                     Method type                                
ROUTD06  EQU   6                     Userfield                                  
ROUTD07  EQU   7                     AUTH STATUS DISPLAY                        
ROUTD08  EQU   8                     YES/NO                                     
ROUTD09  EQU   9                     APPROVAL METH (Auto App or Marker)         
ROUTD10  EQU   10                    AMOUNT                                     
ROUTD11  EQU   11                    BR'OCEAN INVOICE STATUS                    
ROUTLNQ  EQU   *-ROUTD                                                          
                                                                                
UFD      DSECT                                                                  
UFDCODE  DS    CL2                 Userfield code                               
UFD#OF   DS    AL1                 Number of pieces                             
UFDON    EQU   X'80'               Need this to avoid comma conversion          
UFDCC    DS    XL1                 Condition branch code                        
UFDAND   EQU   X'01'               Next part is AND with current                
UFDDATA  DS    0CL1                Userfield data                               
                                                                                
STATD    DSECT                                                                  
STATFLD  DS    CL1                 FIELD NUMBER                                 
STATONLY DS    AL4                 BITS FOR ONLY                                
STATNO   DS    AL4                 BITS FOR NO                                  
STATLNQ  EQU   *-STATD                                                          
                                                                                
FLDDESD  DSECT                                                                  
FLDLEN   DS    AL1                 LEN OF ENTRY                                 
FLDULEN  DS    AL1                 LEN OF UNPROTECTED FIELD DESCRIPTION         
FLDNUM   DS    AL1                 FIELD NUMBER                                 
FLDRTYP  DS    AL1                 REPORT TYPE VALID FOR (0 = DEFAULT)          
FLDLNQ   EQU   *-FLDDESD                                                        
FLDDESC  DS    0CL14                                                            
         EJECT ,                                                                
*ACSCRWRK                                                                       
       ++INCLUDE ACSCRWRK                                                       
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCREAD                                                       
         SPACE 3                                                                
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'094ACSCR12   03/18/16'                                      
         END                                                                    
