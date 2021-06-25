*          DATA SET ACREPM203  AT LEVEL 060 AS OF 01/28/13                      
*PHASE ACM203A,*                                                                
         TITLE 'INITIALIZATION '                                                
***********************************************************************         
* INITIALIZATIN ROUTINES                                              *         
***********************************************************************         
         SPACE 1                                                                
ACM203   CSECT                                                                  
         DC    A(ENTRYTAB-ACM203)                                               
         PRINT NOGEN                                                            
INIT     NMOD1 0,**INIT**,R9,R8,R7                                              
         USING MAND,RA                                                          
         L     RC,BASERC                                                        
         USING ACWORKD,RC                                                       
         TM    RQSW,RQFLG          IS IT FIRST INITIALIZATION                   
         BNO   NXTLDG                                                           
         TITLE 'INITIALIZATION - FIRST LEDGER FOR REQUEST'                      
***********************************************************************         
* FIRST LEDGER FOR THIS REQUEST                                       *         
***********************************************************************         
         SPACE 1                                                                
FSTLDG   MVI   SRTSW,0             SET INIT THE SORT SWITCH                     
         MVI   PCSW,0              CLEAR POSTING CONTROL SWITCH                 
         CLI   SYSEQU,ACMSEUR                                                   
         BNE   *+20                                                             
         CLC   QEND,SPACES                                                      
         BNE   *+10                                                             
         MVC   QEND,QSTART                                                      
         MVC   CSDTE(6),QSTART     GET START DATE - CHARACTER                   
         CLI   CSDTE+4,C' '        NO DAY                                       
         BH    *+10                                                             
         MVC   CSDTE+4(2),=C'01'   MAKE IT THE FIRST                            
         GOTO1 DATCON,DMCB,(0,CSDTE),(1,PSDTE)     AND PACKED                   
         MVC   CEDTE(6),QEND       GET END DATE - CHARACTER                     
         CLC   CEDTE,SPACES        IF END IS BLANK                              
         BNE   *+10                                                             
         MVC   CEDTE,CSDTE         END = START                                  
         CLI   CEDTE+4,C' '                                                     
         BH    *+10                                                             
         MVC   CEDTE+4(2),=C'01'   NO DAY MAKE IT THE FIRST                     
         GOTO1 DATCON,DMCB,(0,CEDTE),(1,PEDTE)     AND PACKED                   
         GOTO1 DATCON,DMCB,(0,CEDTE),(6,MYEDTE)    AND MMM/YY                   
*                                                                               
*        MVC   WORK(6),CSDTE       CONVERT TO DDS FORMAT                        
*        GOTO1 DATCON,DMCB,(0,WORK),(0,CSDTE)                                   
*        MVC   WORK(6),CEDTE                                                    
*        GOTO1 DATCON,DMCB,(0,WORK),(0,CSDTE)                                   
         GOTO1 DATCON,DMCB,(0,CEDTE),(0,CEDTE)                                  
         GOTO1 DATCON,DMCB,(0,CSDTE),(0,CSDTE)                                  
*&&US                                                                           
         TM    RQSW,RQBUD          DO ONLY ONCE                                 
         BO    FSTLDG2                                                          
         OC    QAPPL,QAPPL         CHECK IF ANY OVER-RIDE BUDGETS               
         BZ    FSTLDG2                                                          
         OI    RQSW,RQBUD          SET DONE                                     
         LA    R4,QAPPL                                                         
         LA    R1,L'QAPPL                                                       
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)          GET ADJUSTED BUDGET#                         
         BZ    *+12                                                             
         SHI   RF,48               SUBTRACT X'30' ESCAPE SEQ ADJUSTMENT         
         STC   RF,0(,R4)           SAVE RE-ADJUSTED BUDGET                      
         LA    R4,1(,R4)                                                        
         BCT   R1,*-20                                                          
*&&                                                                             
FSTLDG2  MVI   SRTFLN,16           SET DEFAULT SORT FIELD LENGTH                
         MVC   SRTFLN+1(L'SRTFLN-1),SRTFLN                                      
         XC    LISTUL,LISTUL       RESET LIST LGR                               
         MVC   DUB,=CL8'AC'        LOAD IN USERS PHASE                          
         MVC   DUB+2(2),QPROG                                                   
         CLC   QSRTAREA(2),=C'P='  OPTIONAL PROGCODE FROM REQUEST P=XX          
         BNE   FSTLDG3                                                          
         MVC   DUB+2(2),QSRTAREA+2                                              
         MVC   DUB+6(2),QSRTAREA+4 PHASE + OPTIONAL TEST LETTER                 
*        MVC   DUB+6(1),QSRTAREA+4 OPTIONAL PHASE SUBSCRIPT                     
*                                                                               
FSTLDG3  MVC   DUB+4(2),ALPHAID                                                 
         L     R4,AUSER                                                         
         MVC   SVDUB,DUB                                                        
         GOTO1 LOADER,DMCB,DUB,(R4),(C'M',(R4))                                 
         MVC   MODULE(8),DUB                                                    
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   FSTLDG5             CANT FIND USERS                              
         MVC   DUB+4(2),=C'DD'     SO TRY DEFAULT (DD)                          
         MVI   DUB+7,C' '          REMOVE TEST LETTER                           
         GOTO1 LOADER,DMCB,DUB,(R4),(C'M',(R4))                                 
         MVC   MODULE(8),DUB                                                    
         OC    DMCB+4(4),DMCB+4                                                 
         BZ    INITERR                                                          
         EJECT                                                                  
         USING CPYELD,R6                                                        
FSTLDG5  L     R6,ADCMPEL                                                       
         TM    CPYSTAT5,CPYSNCST   NEW COST                                     
         BNO   *+8                                                              
         OI    RNSW,RNNC           SET NEW METHOD                               
         TM    CPYSTAT7,CPYSTMSY                                                
         BNO   *+8                                                              
         OI    RNSW,RNTMS          TMS IN USE                                   
*                                                                               
         USING TTHRECD,R6                                                       
         LA    R6,DKEY                                                          
         XC    TTHKEY,TTHKEY       LOOK FOR TIME RECORDS                        
         MVI   TTHKTYP,TTHKTYPQ    X'3E'                                        
         MVI   TTHKSUB,TTHKSUBQ    X'0E'                                        
         MVC   TTHKCPY,RCCOMPFL    COMPANY                                      
         GOTO1 ADMGR,HIGH                                                       
         CLC   DKEY(TTHKULA-TTHKEY),DIR                                         
         BNE   *+8                                                              
         OI    RNSW,RNTT           SET TIME TOTALS IN USE                       
*                                                                               
         BAS   RE,FLGS             SET INITIAL FLAGS                            
         BAS   RE,SPEC             SET UP SYSTEM SPECS                          
         BNE   XIT                 END OF LEDGERS                               
         BAS   RE,NAR              BUILD NARRATIVE POOL                         
         BAS   RE,PELV             BUILD TABLE OF PERSONAL AND LEAVE            
         BAS   RE,CNSLT            BUILD LIST CONSULTANT CODES                  
         BAS   RE,GEXE             GENERATE CONDITIONAL EXECUTABLE CODE         
         BAS   RE,SLDGL            HANDLE LEDGER FOR LEVELS                     
         BAS   RE,SETTE            SET T&E LENGTHS                              
         BAS   RE,BUDGT            GET BUDGET CODES                             
         BAS   RE,INSTK            INITIALIZE REPORT STACK                      
         BAS   RE,XFLGS            SET INITIAL EXTRACT FLAGS                    
         BAS   RE,XTRCT            EXTRACT DATA INTO REPORT STACK               
         BAS   RE,BLDM             BUILD MONTH LIST                             
         BAS   RE,DBASE            SET DATE BASE                                
         BAS   RE,SETND            SET DAYS IN VARIOUS CALENDAR LINES           
         BAS   RE,STKUP            SET STACK                                    
         BAS   RE,STSAR            INITIALIZE SORT AND TSAROFF                  
         B     XIT                                                              
         DROP  R6                                                               
         TITLE 'INITIALIZATION - PROCESS SUBSEQUENT LEDGERS'                    
***********************************************************************         
* PROCESS NEXT LEDGER FOR THIS REQUEST                                *         
***********************************************************************         
         SPACE 1                                                                
NXTLDG   BAS   RE,FLGS             SET INITIAL FLAGS                            
         BAS   RE,SPEC             SET NEXT LEDGER                              
         BNE   XIT                                                              
         BAS   RE,SLDGL            HANDLE LEDGER FOR LEVELS                     
         BAS   RE,SETTE            SET T&E LENGTHS                              
         BAS   RE,BUDGT            GET THE BUDGET CODES                         
         TM    RQSW,RQSTK          DON'T ALTER STACK                            
         BO    XIT                                                              
         BAS   RE,INSTK            INITIALIZE REPORT STACK                      
         BAS   RE,XFLGS            SET INITIAL EXTRACT FLAGS                    
         BAS   RE,XTRCT            EXTRACT DATA INTO REPORT STACK               
         BAS   RE,STKUP            FINISH OFF DATA                              
         B     XIT                                                              
         TITLE 'INITIALIZATION - ERROR ROUTINES'                                
***********************************************************************         
* INITIALIZATION - ERROR ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R6                                                      
INITERR  L     R6,VBIGPRNT                                                      
         MVC   XP(28),=C'ACM2 - APG MODULE NOT FOUND.'                          
         MVI   STOPHOOK,YES                                                     
         GOTO1 ACREPORT                                                         
         MVC   XP(41),=C'LOOKING FOR - XXXXXXXX, CHECK FORMAT CODE'             
         MVC   XP+14(8),SVDUB                                                   
         GOTO1 ACREPORT                                                         
         OI    RQSW,RQERR          SET ERROR SWITCH                             
         MVI   STOPHOOK,NO                                                      
         B     XIT                                                              
         DROP  R6                                                               
         TITLE 'INITIALIZATION - SET DEFAULT VALUES'                            
***********************************************************************         
* SET INITIAL SYSTEM FLAGS                                            *         
***********************************************************************         
         SPACE 1                                                                
FLGS     NTR1  ,                                                                
         MVC   READLEDG,QUNIT      REQUEST MAY SPECIFY U/L                      
         CLC   READLEDG,SPACES                                                  
         BNE   *+10                                                             
         MVC   READLEDG,PERSUL                                                  
         MVI   FCPERSEC,NO         OFFICE SECURITY                              
         TM    RQSW,RQSTK          DON'T ALTER STACK OR READ SWITCH             
         BO    *+8                                                              
         MVI   RDSW,RDBKT          DEFAULT IS TO READ BUCKETS                   
         MVI   FORMBASE,NO                                                      
         MVI   ADJHOURS,NO                                                      
         MVI   POSTMOS,NO                                                       
         MVI   BBFSW,NO                                                         
         MVI   BUCKD,NO                                                         
         MVI   RLFND,NO            SET READ LIST NOT FOUND                      
         MVI   STOPHOOK,NO         HOOKS ARE OK                                 
         MVI   CONFIRST,YES                                                     
         MVI   BOXOPT,YES                                                       
         MVI   PRTMOD,YES                                                       
         MVI   FCTR,YES                                                         
*                                                                               
         MVI   RCSUBPRG,0                                                       
         MVI   FORMNUM,0                                                        
         MVI   OVFISCAL,0                                                       
         MVI   LPLSW,0                                                          
         MVI   OPT1,0                                                           
         MVI   HOOKMODE,0                                                       
         MVI   GPRSW,0                                                          
         MVI   DWNOPTN1,0          RESET OPTIONS                                
         MVI   DWNOPTN2,0                                                       
*&&US*&& MVI   SYSWIDTH,164                                                     
*&&UK*&& MVI   SYSWIDTH,132                                                     
         MVI   CURRSIGN,C'+'       CURRENT SIGN                                 
         MVI   HORIZ,X'BF'                                                      
         XC    CONADDR,CONADDR                                                  
         MVC   SYSNAME,SPACES                                                   
         MVC   CONLEDG,SPACES                                                   
         MVC   RLLEDG,SPACES                                                    
         MVC   GPRLDG,SPACES                                                    
         MVC   AHOURS,SPACES                                                    
         ZAP   FOHR,=P'0'          INIT FIXED OVERHEAD RATE                     
         MVC   AFORM,AFRMPOOL                                                   
         MVC   READCO,RCCOMPFL                                                  
         NI    DWNSTAT,DWNINIT     INITIALIZE DOWNLOAD                          
         CLI   QOPT7,YES           DOWNLOADING?                                 
         BNE   *+12                NO                                           
         OI    DWNSTAT,DWNLOAD     SET DOWNLOAD ACTIVE                          
         MVI   BOXOPT,NO           NO BOX                                       
         B     XIT                                                              
         EJECT                                                                  
*                                  INITIALIZE FLAGS BEFORE EXTRACT              
XFLGS    XC    NRSTACK,NRSTACK                                                  
         XC    HIGHCOL,HIGHCOL                                                  
         XC    HIGHROW,HIGHROW                                                  
         L     R2,ACPOOL                                                        
         MVI   0(R2),X'FF'                                                      
         BR    RE                                                               
         TITLE 'INITIALIZATION - INTERPRET SYSTEM SPECS'                        
***********************************************************************         
* INTERPRET SYSTEM SPECS                                              *         
***********************************************************************         
         SPACE 1                                                                
SPEC     NTR1  ,                                                                
         L     R6,AUSER                                                         
         SR    RF,RF                                                            
*                                                                               
SPEC03   CLI   0(R6),EOT                                                        
         BE    SPEC09                                                           
         LA    RE,SPECTAB          MATCH SET TO TABLE                           
*                                                                               
SPEC05   ICM   RF,7,1(RE)          SET ROUTINE                                  
         CLC   0(1,R6),0(RE)                                                    
         BER   RF                  PROCESS SPEC                                 
         LA    RE,L'SPECTAB(,RE)                                                
         CLI   0(RE),EOT                                                        
         BNE   SPEC05                                                           
*                                                                               
SPEC07   SR    RF,RF               BUMP TO NEXT SPEC                            
         IC    RF,1(,R6)                                                        
         AR    R6,RF                                                            
         B     SPEC03                                                           
                                                                                
         USING ACQD,RF                                                          
SPEC09   L     RF,ADQSTACK          TEST ANY OVERHEAD                           
         LA    R0,4                                                             
         LA    RE,ACQTYP1                                                       
         CLI   0(RE),ACQOVHD                                                    
         BE    SPEC11                                                           
         LA    RE,15(,RE)                                                       
         BCT   R0,*-12                                                          
         B     SPEC13                                                           
*                                                                               
SPEC11   PACK  FOHR,1(5,RE)      YES, PACK THE RATE                             
         OI    OPT1,FOH                                                         
*                                                                               
SPEC13   LA    R1,1                                                             
         CLC   READLEDG,SPACES     SEE IF THERE'S ANOTHER SYSTEM                
         BE    SPECX               OR LEDGER TO BE PROCESSED                    
         MVC   RCCOMPFL,READCO                                                  
         MVC   QUNIT(2),READLEDG   UPDATE REQUEST WITH SELECTED U/L             
         SR    R1,R1                                                            
*                                                                               
SPECX    LTR   R1,R1                                                            
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* SPEC CONTROL TABLE                                                  *         
***********************************************************************         
         SPACE 1                                                                
SPECTAB  DS    0F                                                               
         DC    AL1(CMSYSN),AL3(SSYSN)      SYSTEM NAME                          
         DC    AL1(CMCNLG),AL3(SCNLG)      CONLEDG                              
         DC    AL1(CMREAD),AL3(SREAD)      READ                                 
         DC    AL1(CMRLST),AL3(SRLST)      READ LIST                            
         DC    AL1(CMBSIS),AL3(SBSIS)      BASIS                                
         DC    AL1(CMHLOD),AL3(SHLOD)      HOOKLOAD                             
         DC    AL1(CMWDTH),AL3(SWDTH)      WIDTH                                
         DC    AL1(CMNBOX),AL3(SNBOX)      NOBOX                                
         DC    AL1(CMFSCL),AL3(SFSCL)      FISCAL                               
         DC    AL1(CMSRTD),AL3(SPEC07)     SORTD                                
         DC    AL1(CMSHWA),AL3(SSHWA)      SHOWALL                              
         DC    AL1(CMFTDT),AL3(SFTDT)      FORCETD                              
         DC    AL1(CMNMOD),AL3(SNMOD)      PRTNOMOD                             
         DC    AL1(CMMOHK),AL3(SMOHK)      MODEHOOK                             
         DC    AL1(CMGPR),AL3(SGPR)        GPR                                  
         DC    AL1(CMDOWN),AL3(SDOWN)      DOWMLOAD                             
         DC    AL1(CMGENB),AL3(SGENB)      SET FCGENBUK=Y                       
         DC    AL1(CMOFSEC),AL3(SOFSEC)    OFFSEC  N,E,C OR B                   
         DC    AL1(EOT)                                                         
         CNOP  0,4                                                              
         EJECT                                                                  
***********************************************************************         
* SPECIFIC SYSTEM SPECS                                               *         
***********************************************************************         
         SPACE 1                                                                
SSYSN    SR    R1,R1               SYSNAME                                      
         IC    R1,1(,R6)                                                        
         SHI   R1,3                                                             
*MN      EX    R1,*+4                                                           
         MVC   SYSNAME(0),2(R6)                                                 
         EX    R1,*-6                                                           
         CLC   2(7,R6),=C'&&AGENCY'                                             
         BNE   *+8                                                              
         BAS   RE,SYSAGY                                                        
                                                                                
         CLC   2(4,R6),=C'&&IDI'                                                
         BNE   SSYSN5                                                           
         L     R4,ADCOMP                                                        
         AH    R4,DATADISP                                                      
SSYSN2   CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R4),X'20'                                                      
         BE    SSYSN3                                                           
         ZIC   R5,1(,R4)                                                        
         AR    R4,R5                                                            
         B     SSYSN2                                                           
*                                                                               
SSYSN3   ZIC   R5,1(,R4)                                                        
         SHI   R5,3                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   SYSNAME(0),2(R4)                                                 
                                                                                
SSYSN5   GOTO1 CENTER,DMCB,SYSNAME,40                                           
         B     SPEC07                                                           
*                                                                               
SCNLG    MVC   CONLEDG,2(R6)       CONLEDG                                      
         B     SPEC07                                                           
*                                                                               
SREAD    CLC   2(2,R6),SPACES      READ                                         
         BE    *+10                                                             
         MVC   READLEDG,2(R6)                                                   
         NI    RDSW,ALL-(RDDTL+RDBKT) TURNOFF READ DETAIL/BUCKETS               
         CLI   4(R6),C'*'                                                       
         BNE   *+8                                                              
         OI    RDSW,RDDTL+RDBKT    SET FOR READ DETAIL & BUCKETS                
         CLI   4(R6),C'D'                                                       
         BNE   *+8                                                              
         OI    RDSW,RDDTL          DETAIL ONLY                                  
         CLI   4(R6),C'B'                                                       
         BNE   *+8                                                              
         OI    RDSW,RDBKT          OR READ BUCKETS                              
         TM    RQSW,RQFLG          IS IT FIRST LEDGER FOR REQUEST               
         BO    SPEC07              FIRST INITIALIZATION                         
         MVC   READLEDG,SPACES                                                  
         B     SPEC07                                                           
         EJECT                                                                  
***********************************************************************         
* SYSTEM SPECS - READ LIST                                            *         
***********************************************************************         
         SPACE 1                                                                
SRLST    CLI   RLFND,YES           WAS NEW ONE FOUND IN PREV                    
         BE    SPEC07              YES, DON'T USE THIS YET                      
*&&US*&& MVC   RLCO,2(R6)                                                       
         SR    R1,R1                                                            
         IC    R1,1(,R6)           READ LIST                                    
*&&US*&& SHI   R1,3                N'OPERANDS =                                 
*&&UK*&& SHI   R1,2                                                             
         SR    R0,R0               (ELLEN-3)/3                                  
         D     R0,=F'3'                                                         
         LR    R0,R1                                                            
*&&US*&& LA    R1,3(,R6)                                                        
*&&UK*&& LA    R1,2(,R6)                                                        
*                                                                               
SRLST3   OC    LISTUL,LISTUL       FIRST FIRST LIST HANDLING                    
         BZ    SRLST9                                                           
         TM    RQSW,RQFLG          IF FIRST TIME THRU THIS IS                   
         BO    SPEC07              SECOND LIST, IGNORE IT NOW                   
         CLI   RLCO,0                                                           
         BE    *+14                                                             
         CLC   READCO,RLCO                                                      
         BNE   *+14                                                             
         CLC   LISTUL(3),0(R1)     OTHERWISE FIND PREVIOUS                      
         BE    SRLST5                                                           
         LA    R1,3(,R1)                                                        
         BCT   R0,SRLST3                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(,R6)                                                        
         AR    R6,R1                                                            
         CLI   0(R6),5             AT END OF LIST                               
         BE    SRLST               IF ANOTHER CONTINUE SEARCH                   
         DC    H'0'                SOME KINDA LOGIC PROBLEM                     
*                                                                               
SRLST5   LA    R1,3(,R1)                                                        
         BCT   R0,SRLST9                                                        
         SR    R1,R1                                                            
         IC    R1,1(,R6)                                                        
         AR    R6,R1                                                            
         CLI   0(R6),5             AT END OF LIST                               
         BNE   SRLST7              IF ANOTHER START IT OFF                      
*&&US*&& MVC   RLCO,2(R6)                                                       
         LA    R1,3(,R6)                                                        
         B     SRLST9                                                           
*                                                                               
SRLST7   SR    R6,R1                                                            
         MVC   READLEDG,SPACES     NO MORE LEFT                                 
         B     SPEC07                                                           
*                                                                               
SRLST9   MVC   READLEDG,0(R1)      PASS BACK NEXT IN LIST                       
         NI    RDSW,ALL-(RDDTL+RDBKT) TURNOFF READ DETAIL/BUCKETS               
         CLI   2(R1),C'*'                                                       
         BNE   *+8                                                              
         OI    RDSW,RDDTL+RDBKT    SET FOR READ DETAIL & BUCKETS                
         CLI   2(R1),C'D'                                                       
         BNE   *+8                                                              
         OI    RDSW,RDDTL          READ DETAIL ONLY                             
         CLI   2(R1),C'B'                                                       
         BNE   *+8                                                              
         OI    RDSW,RDBKT          OR BUCKETS                                   
         CLI   RLCO,0                                                           
         BE    *+10                                                             
         MVC   READCO,RLCO                                                      
         MVC   LISTUL,0(R1)                                                     
         MVI   RLFND,YES           FOUND READ LIST                              
         B     SPEC07                                                           
         EJECT                                                                  
***********************************************************************         
* SYSTEM SPECS - SUNDRY                                               *         
***********************************************************************         
         SPACE 1                                                                
SBSIS    MVC   FORMBASE,2(R6)      BASIS                                        
         MVC   FORMNUM,3(R6)                                                    
         BAS   RE,QMDATE                                                        
         B     SPEC07                                                           
*                                                                               
SHLOD    TM    RQSW,RQFLG        IS THIS FIRST FOR REQUEST                      
         BNO   SPEC07            NO-ALREADY LOADED                              
         MVC   DUB(8),2(R6)                                                     
         GOTO1 LOADER,DMCB,DUB,0,0                                              
         ICM   RF,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                (DIE IF NOT FOUND)                           
                                                                                
         ST    RF,AHOOKCDE       HOOKLOAD PHASE                                 
         B     SPEC07                                                           
*                                                                               
SWDTH    DS    0H                                                               
         MVC   SYSWIDTH,2(R6)      WIDTH                                        
         B     SPEC07                                                           
*                                                                               
SNBOX    MVI   HORIZ,X'BF'         NOBOX                                        
         MVI   BOXOPT,NO                                                        
         B     SPEC07                                                           
*                                                                               
SFSCL    SR    R1,R1                                                            
         IC    R1,2(,R6)           FISCAL                                       
         CVD   R1,DUB              CHANGE TO COMP REC FORMAT (CHAR)             
         CLI   DUB+6,X'01'         10  OR OVER ADD 1                            
         BNE   *+14                                                             
         AP    DUB,=P'1'                                                        
         B     *+10                                                             
         MVN   DUB+7(1),=X'0F'                                                  
         UNPK  OVFISCAL,DUB+7(1)                                                
         B     SPEC07                                                           
*                                                                               
SSHWA    OI    OPT1,SZD            INCLUDE ZERO DATA LINES                      
         B     SPEC07                                                           
*                                                                               
SFTDT    OI    OPT1,FTD            FORCE TRANSACTION DATE TO QEND               
         B     SPEC07                                                           
*                                                                               
SNMOD    MVI   PRTMOD,NO                                                        
         B     SPEC07                                                           
*                                                                               
SMOHK    MVC   HOOKMODE,2(R6)      HOOK NUMBER FOR MODES                        
         B     SPEC07                                                           
*                                                                               
SGPR     MVC   GPRLDG,3(R6)        GPR LEDGER                                   
         SR    R0,R0                                                            
         IC    R0,2(,R6)           LEVEL  1, 2, 3 OR 4                          
         LA    R1,128              SET LEVEL BIT                                
         STC   R1,GPRSW            LEVEL 1 - X'80'                              
         SRL   R1,1                LEVEL 4 - X'10'                              
         BCT   R0,*-8                                                           
         B     SPEC07                                                           
*                                                                               
SDOWN    MVC   DWNOPTN1,2(R6)      DOWNLOAD OPTIONS                             
         MVC   DWNOPTN2,3(R6)                                                   
         TM    DWNOPTN1,DWNOMICR   MICRO CONTROL                                
         BZ    *+8                                                              
         OI    DWNSTAT,DWNMICRO                                                 
         B     SPEC07                                                           
*                                                                               
SGENB    OI    RDSW,RDGEN          SET FCGENBUK=Y                               
         B     SPEC07                                                           
*                                                                               
SOFSEC   MVC   FCPERSEC,2(R6)      OFFICE SECURITY                              
         B     SPEC07                                                           
         EJECT                                                                  
***********************************************************************         
* GET AGENCY NAME FROM COMPANY RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
SYSAGY   NTR1  ,                                                                
         MVC   SYSNAME,SPACES      GET COMPANY NAME                             
         LA    R6,DKEY                                                          
         MVC   0(42,R6),SPACES                                                  
         MVC   0(1,R6),RCCOMPFL                                                 
         GOTO1 ADMGR,READ                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         USING CPYRECD,R6                                                       
         LA    R6,CPYRFST                                                       
         SR    R1,R1                                                            
*                                                                               
SYSAG1   CLI   0(R6),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R1,1(,R6)                                                        
         CLI   0(R6),X'20'                                                      
         BE    SYSAG2                                                           
         AR    R6,R1                                                            
         B     SYSAG1                                                           
*                                                                               
SYSAG2   SHI   R1,3                                                             
*MN      EX    R1,*+4                                                           
         MVC   SYSNAME(0),2(R6)                                                 
         EX    R1,*-6                                                           
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* QMDATE FILL THE BASE FOR QUESTION MARK KEYS IN THE S/L              *         
***********************************************************************         
         SPACE 1                                                                
QMDATE   NTR1  ,        QUESTION MARK BASE FILLER                               
         MVC   LASTYRQ(6),=C'LYTYNY'                                            
         LA    R3,QSTART                                                        
         CLI   2(R6),C'S'                                                       
         BE    QM2                                                              
         LA    R3,QEND                                                          
         CLI   2(R6),C'E'                                                       
         BNE   XIT                                                              
*                                                                               
QM2      MVC   WORK(2),0(R3)                                                    
         MVC   WORK+2(4),=C'0101'                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-20'                                    
         MVC   LASTYRQ,WORK+6                                                   
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'367'                                    
         MVC   NEXTYRQ,WORK+6                                                   
         MVC   CURRYRQ,WORK                                                     
         B     XIT                                                              
         TITLE 'INITIALIZATION - BUILD A NARRATIVE BLOCK'                       
***********************************************************************         
* BUILD A NARRATIVE BLOCK                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING SCMRECD,R2                                                       
NAR      NTR1  ,                                                                
         MVI   APGNNARL,0          NUMBER OF NARRATIVE LINES                    
         L     R4,ANARBK           A(NARRATIVE BLOCK)                           
         CLC   QCOMMENT,SPACES                                                  
         BE    NARX                                                             
         LA    R2,DKEY                                                          
         XC    0(42,R2),0(R2)                                                   
         MVI   0(R2),X'0C'                                                      
         MVC   1(1,R2),RCCOMPFL                                                 
         MVC   2(6,R2),QCOMMENT                                                 
         GOTO1 ADMGR,READ                                                       
         BNE   NARX                COMMENT NOT FOUND                            
         GOTO1 ADMGR,GETR                                                       
         L     R2,AIO1                                                          
         LA    R2,SCMRFST                                                       
*                                                                               
         USING SCMELD,R2                                                        
         SR    R1,R1                                                            
         SR    R3,R3               COUNT NUMBER IN BLOCK                        
*                                                                               
NAR03    CLI   0(R2),0                                                          
         BNE   NAR05                                                            
         STC   R3,APGNNARL         SAVE NUMBER OF LINES                         
         B     NARX                                                             
*                                                                               
NAR05    CLI   0(R2),X'3E'                                                      
         BE    NAR09                                                            
NAR07    IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     NAR03                                                            
*                                                                               
NAR09    MVI   0(R4),C' '          SET LINE TO SPACES                           
         MVC   0(NARBKLQ-1,R4),1(R4)                                            
         IC    R1,SCMLN                                                         
         SHI   R1,5                                                             
*MN      EX    R1,*+4                                                           
         MVC   0(0,R4),SCMNARR                                                  
         EX    R1,*-6                                                           
         LA    R4,NARBKLQ(,R4)                                                  
         AHI   R3,1                COUNT LINES                                  
         B     NAR07               GET NEXT ELEMENT                             
*                                                                               
NARX     B     XIT                                                              
         DROP  R2                                                               
         TITLE 'INITIALIZATION - BUILD TABLE OF P-TIME AND L-TIME'              
***********************************************************************         
* BUILD TABLE OF P-TIME AND L-TIME                                    *         
***********************************************************************         
         SPACE 1                                                                
PELV     TM    RNSW,RNNC           IS IT NEW COST                               
         BNOR  RE                  NO, DON'T BUILD LIST                         
         NTR1  ,                                                                
         L     R3,APELVT           A(P AND L TIME ACCOUNTS)                     
         MVI   0(R3),X'FF'                                                      
         MVC   DKEY,SPACES         READ 1N RECORDS                              
         LA    R2,DKEY                                                          
         MVC   0(1,R2),RCCOMPFL    COMPANY                                      
         MVC   1(2,R2),NONCUL                                                   
         GOTO1 ADMGR,READ                                                       
         BNE   XIT                                                              
         SR    R5,R5               NUMBER OF ENTRIES = 0                        
*                                                                               
PELV03   GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         SR    R0,R0               GET THE STATUS ELEMENT                       
         LA    R4,ACCRFST-ACCRECD(R6)                                           
PELV05   CLI   0(R4),X'30'                                                      
         BE    PELV07                                                           
         CLI   0(R4),0                                                          
         BE    PELV09                                                           
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     PELV05                                                           
*                                                                               
         USING RSTELD,R4                                                        
PELV07   CLI   RSTCOSTG,C'P'       PERSONAL                                     
         BE    *+12                                                             
         CLI   RSTCOSTG,C'L'       LEAVE                                        
         BNE   PELV09                                                           
         MVC   0(14,R3),1(R6)      ACCOUNT KEY                                  
         MVC   14(1,R3),RSTCOSTG   TYPE (ANALYSIS CODE)                         
         LA    R3,PETLN(R3)                                                     
         MVI   0(R3),X'FF'                                                      
         AHI   R5,1                                                             
         CHI   R5,PETMX                                                         
         BNH   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
PELV09   GOTO1 ADMGR,RSEQ                                                       
         CLC   DIR(3),DKEY                                                      
         BE    PELV03                                                           
         B     XIT                                                              
         DROP  R4                                                               
         TITLE 'INITIALIZATION - BUILD LIST OF 14 CONSULTANT ACCOUNTS'          
***********************************************************************         
* BUILD LIST OF 14 CONSULTANT ACCOUNTS                                *         
***********************************************************************         
         SPACE 1                                                                
CNSLT    TM    RNSW,RNNC           IS IT NEW COST                               
         BNOR  RE                  NO, DON'T BUILD LIST                         
         NTR1  ,                                                                
         LA    R3,CNSLTT           A(CONSULTANT CODE TABLE)                     
         MVI   0(R3),X'FF'                                                      
         MVC   DKEY,SPACES         READ 14 RECORDS                              
         LA    R2,DKEY                                                          
         MVC   0(1,R2),RCCOMPFL    COMPANY                                      
         MVC   1(2,R2),DIRLUL                                                   
         GOTO1 ADMGR,READ                                                       
         BNE   XIT                 NO 14 LEDGER                                 
         SR    R5,R5                                                            
*                                                                               
CNSLT03  GOTO1 ADMGR,GETR                                                       
         SR    R0,R0               GET THE STATUS ELEMENT                       
         L     R6,AIO1                                                          
         LA    R4,ACCRFST-ACCRECD(R6)                                           
CNSLT05  CLI   0(R4),X'30'                                                      
         BE    CNSLT07                                                          
         CLI   0(R4),0                                                          
         BE    CNSLT09                                                          
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     CNSLT05                                                          
*                                                                               
         USING RSTELD,R4                                                        
CNSLT07  CLI   RSTFILT1,C'C'       CONSULTANT                                   
         BNE   CNSLT09                                                          
         MVC   0(1,R3),3(R6)       14 CODE                                      
         LA    R3,1(,R3)                                                        
         MVI   0(R3),X'FF'                                                      
         AHI   R5,1                                                             
         CHI   R5,L'CNSLTT                                                      
         BNH   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
*                                                                               
CNSLT09  GOTO1 ADMGR,RSEQ                                                       
         CLC   DIR(3),DKEY                                                      
         BE    CNSLT03                                                          
         B     XIT                                                              
         DROP  R4                                                               
         TITLE 'INITIALIZATION - GENERATE EXEC. CONDITIONAL STATEMENTS'         
***********************************************************************         
* GENERATE EXECUTABLE CONDITIONAL STATEMENTS                          *         
***********************************************************************         
         SPACE 1                                                                
GEXE     NTR1  ,                                                                
         BAS   RE,GEXL             EXCLUDE UNWANTED LIST STATEMENTS             
         L     RE,ACONADS          CLEAR CONDITION ADDRESS POINTERS             
         LHI   RF,(CATLN*CATMX)                                                 
         XCEFL                                                                  
         L     RE,ACONEXE          CLEAR CONDITION BLOCK                        
         LA    RF,CXTLN                                                         
         XCEFL                                                                  
         L     R3,ACONEXE          R3=CONDITION GENERATION TABLE                
         SR    R5,R5               R5=RELATIVE POSTION FROM CURRENT             
         L     R6,AUSER            R6=USER SPECS                                
         MVI   BYTE,0              CURRENT CONDITIONAL NUMBER                   
         MVI   CONLTYP,0           LIST TYPE                                    
         MVI   GEXSW,0             CONTROL SWITCH                               
         SR    R0,R0                                                            
         MVC   0(L'CONYES,R3),CONYES SET OK RETURN                              
         LA    R3,L'CONYES(,R3)                                                 
         MVC   0(L'CONNO,R3),CONNO   AND NOOT OK                                
         LA    R3,L'CONNO(,R3)                                                  
         LA    R5,L'CONNO+L'CONYES(,R5)                                         
*                                                                               
GEXE03   CLI   0(R6),EOT           END OF SPECS ?                               
         BE    GEXE30                                                           
         CLI   0(R6),50            IS IT AN "IF" STATEMENT                      
         BNE   GEXE07                                                           
         CLC   BYTE,2(R6)          SAME STATEMENT NUMBER                        
         BE    GEXE05              YES, BUILD IF DATA                           
         MVC   BYTE,2(R6)          NEW STATEMENT NUMBER                         
         SR    R1,R1                                                            
         IC    R1,BYTE                                                          
         BCTR  R1,0                LESS ONE                                     
         SLL   R1,2                X 4                                          
         L     RF,ACONADS          A(CONDITIONAL STATEMENTS)                    
         AR    RF,R1                                                            
         OC    0(4,RF),0(RF)       IS STATEMENT ALREADY USED?                   
         BZ    *+12                NO, OK TO USE                                
         MVI   BYTE,0              CLEAR STATEMENT NUMBER                       
         B     GEXE11              SKIP NON-CONSECUTIVE USES                    
         STCM  R3,15,0(RF)         SAVE START OF THIS STATEMENT                 
*                                                                               
GEXE05   MVI   0(R3),CONIF         IF STATEMENT                                 
         B     GEXE20                                                           
*                                                                               
GEXE07   CLI   0(R6),52            IS IT AN "OR" ?                              
         BNE   GEXE09                                                           
         MVI   0(R3),CONOR         SET CODE                                     
         B     GEXE20              BUILD REMAINDER OF DATA                      
*                                                                               
GEXE09   CLI   0(R6),54            IS IT AN "AND" ?                             
         BNE   GEXE11                                                           
         MVI   0(R3),CONAND        SET CODE                                     
         B     GEXE20                                                           
*                                                                               
GEXE11   IC    R0,1(,R6)           GET NEXT STATEMENT                           
         AR    R6,R0                                                            
         B     GEXE03                                                           
*                                                                               
GEXE20   STCM  R6,15,1(R3)         SAVE A(ORIGINAL SPEC)                        
         MVC   5(1,R3),BYTE        SAVE STATEMENT NUMBER                        
         STCM  R5,3,6(R3)          SAVE DISPLACEMENT FROM START                 
         CLC   6(5,R6),=C'\LIST'   LIST CONNECTIVE                              
         BE    GEXE23                                                           
         CLI   6(R6),C'>'          SET CONDITION CODE                           
         BNE   *+8                                                              
         MVI   8(R3),X'20'         GT                                           
         CLI   6(R6),C'<'                                                       
         BNE   *+8                                                              
         MVI   8(R3),X'40'         LT                                           
         CLI   6(R6),C'/'                                                       
         BNE   *+8                                                              
         MVI   8(R3),X'70'         NE                                           
         CLI   6(R6),C'='                                                       
         BNE   *+8                                                              
         MVI   8(R3),X'80'         EQ                                           
         LA    R5,10(,R5)          LENGTH OF IF/AND/OR                          
         LA    R3,10(,R3)                                                       
*                                                                               
GEXE21   IC    R0,1(,R6)           LOOK AT NEXT                                 
         AR    R6,R0                                                            
         CLI   0(R6),52            IS NEXT AN "OR"                              
         BE    GEXE07              YES, PROCESS THE "OR"                        
         MVI   0(R3),CONNOT        UNCONDITIONAL BRANCH                         
         LA    R3,4(,R3)                                                        
         LA    R5,4(,R5)                                                        
         B     GEXE03                                                           
*                                                                               
GEXE23   L     RF,VEXTRAS                                                       
         USING RUNXTRAD,RF                                                      
         L     R2,VLISTREC                                                      
         DROP  RF                                                               
         LA    R2,ACCORFST(R2)     GET THE LIST TYPE ELEMENT                    
         USING LITELD,R2                                                        
*                                                                               
GEXE24   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),X'1E'                                                      
         BE    GEXE25                                                           
         IC    R0,1(,R2)                                                        
         AR    R2,R0                                                            
         B     GEXE24                                                           
*                                                                               
GEXE25   MVC   CONLTYP,LITUSE      I(NCLUDE), E(XCLUDE)                         
         LR    R4,R2                                                            
*                                                                               
         USING LIDELD,R4                                                        
GEXE26   IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    GEXE29                                                           
         CLI   0(R4),X'1F'         GET LIST ELEMENT                             
         BNE   GEXE26                                                           
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LIDLN            ELEMENT LENGTH                               
         SHI   R1,10                                                            
         SR    RE,RE                                                            
         IC    RE,LIDITLN          LENGTH OF ITEM                               
         DR    R0,RE               R1=NUMBER OF ITEMS                           
*                                                                               
GEXE28   CLI   0(R3),0             DON'T ALTER FIRST                            
         BNE   *+8                                                              
         MVI   0(R3),CONOR         LIST IS ALWAYS 'OR"                          
         MVC   1(1,R3),CONLTYP     SET LIST TYPE                                
         STCM  R6,7,2(R3)          SAVE A(ORIGINAL SPEC)                        
         MVC   5(1,R3),BYTE        SAVE STATEMENT NUMBER                        
         STCM  R5,3,6(R3)          SAVE DISPLACEMENT FROM START                 
         MVI   8(R3),X'80'         SET "BE"                                     
         LA    R3,10(,R3)                                                       
         LA    R5,10(,R5)                                                       
         BCT   R1,GEXE28           FOR EACH ITEM ON ELEMENT                     
         B     GEXE26              FOR EACH ELEMENT                             
*                                                                               
GEXE29   CLI   CONLTYP,CONLINC     INCLUDE LIST                                 
         BE    GEXE21              CHECK IF NEXT IS AN "OR"                     
         MVI   0(R3),CONOK         BRANCH OK  - END OF EXCLUDE LIST             
         LA    R3,4(,R3)                                                        
         LA    R5,4(,R5)                                                        
         B     GEXE11              GET NEXT STATEMENT                           
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* SET BRANCH STATEMENTS                                               *         
***********************************************************************         
         SPACE 1                                                                
GEXE30   MVI   0(R3),EOT           SET END INDICATOR                            
         LA    R3,1(,R3)                                                        
         ST    R3,ACONLTL          A(CONDITIONAL LITERALS)                      
*                                                                               
*                                  NOW SET THE BRANCH DISPLACEMENTS             
         L     R3,ACONEXE          R3=CONDITION GENERATION TABLE                
         LA    R3,CONXLNQ(,R3)                                                  
GEXE33   CLI   0(R3),EOT           END OF TABLE                                 
         BE    GEXE40                                                           
         CLI   0(R3),CONNOT        IS IT A BRANCH                               
         BNE   GEXE35                                                           
         MVC   0(2,R3),=X'47F0'    UNCONDITIONAL BRANCH                         
         MVC   2(2,R3),NOTC        SET NOT "OK"                                 
         MVI   ELCODE,CONIF        LOOK FOR NEXT IF                             
         LA    R4,4(,R3)           R4 TO NEXT STATEMENT                         
         BAS   RE,GEXENX           GET NEXT ROUTINE                             
         BNE   *+16                                                             
         STCM  RF,3,2(R3)          SET DISPLACEMENT TO NEXT "IF"                
         NI    2(R3),X'0F'                                                      
         OI    2(R3),CONBSR        BASE REGISTER                                
         LA    R3,4(,R3)                                                        
         B     GEXE33                                                           
*                                                                               
GEXE35   CLI   0(R3),CONOK         OK - END OF EXCLUDE LIST                     
         BNE   GEXE37                                                           
         MVC   0(2,R3),=X'47F0'    UNCONDITIONAL BRANCH                         
         MVC   2(2,R3),YESC        SET  "OK"                                    
         MVI   ELCODE,CONAND       LOOK FOR NEXT "AND"                          
         LA    R4,4(,R3)           R4 TO NEXT STATEMENT                         
         BAS   RE,GEXENX           GET NEXT ROUTINE                             
         BNE   *+16                                                             
         STCM  RF,3,2(R3)          SET DISPLACEMENT TO NEXT "AND"               
         NI    2(R3),X'0F'                                                      
         OI    2(R3),CONBSR        BASE REGISTER                                
         LA    R3,4(,R3)                                                        
         B     GEXE33                                                           
*                                                                               
GEXE37   MVC   BYTE,5(R3)          SAVE STATEMENT NUMBER                        
         MVI   6(R3),X'47'         SET BRANCH                                   
         MVC   7(1,R3),8(R3)       AND CONDITION CODE                           
         MVC   8(2,R3),YESC        SET "OK" EXIT                                
         CLI   1(R3),CONLEXC       IS IT EXCLUDE ITEM?                          
         BNE   GEXE38                                                           
         MVC   8(2,R3),NOTC        SET "NOT OK"                                 
         B     GEXE39                                                           
*                                                                               
GEXE38   MVI   ELCODE,CONAND       LOOK FOR NEXT "AND"                          
         LA    R4,10(,R3)                                                       
         BAS   RE,GEXENX           GET NEXT ROUTINE                             
         BNE   GEXE39              NONE FOUND,                                  
         STCM  RF,3,8(R3)          SET DISPLACEMENT TO NEXT "AND"               
         NI    8(R3),X'0F'                                                      
         OI    8(R3),CONBSR        BASE REGISTER                                
*                                                                               
GEXE39   LA    R3,10(,R3)                                                       
         B     GEXE33                                                           
         EJECT                                                                  
***********************************************************************         
* SET COMPARE INSTRUCTIONS                                            *         
***********************************************************************         
         SPACE 1                                                                
*                                  SET THE "CLC" INSTRUCTIONS                   
GEXE40   L     R3,ACONEXE          R3=CONDITION GENERATION TABLE                
         LA    R3,CONXLNQ(,R3)                                                  
         L     R6,ACONLTL          R6=LITRAL POOL                               
         SR    R5,R5               R5=DISPLACEMENT INTO LITERAL POOL            
*                                                                               
GEXE43   CLI   0(R3),EOT           END OF TABLE                                 
         BE    GEXE90                                                           
         CLI   0(R3),X'47'         IS IT A BRANCH?                              
         BNE   GEXE43A                                                          
         LA    R3,4(,R3)           YES, SKIP IT                                 
         B     GEXE43                                                           
*                                                                               
GEXE43A  CLI   0(R3),X'D5'         ALREADY RESOLVED                             
         BE    GEXE89                                                           
         CLI   1(R3),0             LIST ITEM                                    
         BNE   GEXE60                                                           
         ICM   RF,15,1(R3)         ADDRESS OF ORIGINAL SPEC                     
         MVI   0(R3),X'D5'         "CLC"                                        
         SR    R1,R1                                                            
         IC    R1,5(,RF)           LENGTH                                       
         BCTR  R1,0                                                             
         STC   R1,1(,R3)           SET INSTRUCTION LENGTH                       
         LA    R2,3(,RF)           R2=(FROM,DISP,LEN)                           
         BAS   RE,GEXEFM           GET "FROM" DATA ADDRESS                      
         STCM  R1,3,2(R3)          SET BASE AND DISPLACEMENT FOR ADDR1          
*                                                                               
         CLI   7(RF),0             IS SECOND ADDRESS A CONSTANT                 
         BNE   GEXE45              YES, GO BUILD ADDR2 FROM CONSTANT            
         LA    R2,8(,RF)           R2=SECOND IS SOFT                            
         CLI   0(R2),FRST          "FROM" START DATE OR END DATE                
         BL    GEXE44                                                           
         CLI   0(R2),FRTY          OR TODAY                                     
         BH    GEXE44                                                           
         BAS   RE,GEXEDT           HANDLE &&ST+NNN, &&EN AND &&TY               
         B     GEXE89                                                           
*                                                                               
GEXE44   BAS   RE,GEXEFM           GET "FROM" DATA ADDRESS                      
         STCM  R1,3,4(R3)          SET BASE AND DISPLACEMENT FOR ADDR2          
         B     GEXE89                                                           
*                                                                               
GEXE45   MVC   4(2,R3),=S(SPACES)  SET FOR SPACES                               
         CLC   7(6,RF),=C'SPACES'                                               
         BE    GEXE89                                                           
         STCM  R5,3,4(R3)          SET DISPLACEMENT INTO LITERAL POOL           
         OI    4(R3),CONLTR        SET LITERAL REGISTER                         
         SR    R1,R1                                                            
         IC    R1,5(,RF)           LENGTH COMPARE                               
         BCTR  R1,0                                                             
*MN      EX    R1,*+4                                                           
         MVC   0(0,R6),SPACES      PREFILL WITH SPACES                          
         EX    R1,*-6                                                           
         SR    R2,R2                                                            
         IC    R2,1(,RF)           LENGTH OF STATEMENT                          
         SHI   R2,8                LESS THE CONSTANT OF 7 AND 1 FOR EX          
*MN      EX    R2,*+4                                                           
         MVC   0(0,R6),7(RF)       MOVE DATA TO AREA                            
         EX    R2,*-6                                                           
         LA    R5,1(R1,R5)         BUMP THE DISPLACEMENT                        
         LA    R6,1(R1,R6)         BUMP TO LITERAL POOL AREA                    
         B     GEXE89                                                           
         EJECT                                                                  
***********************************************************************         
* DEAL WITH LIST RECORDS                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RUNXTRAD,RF                                                      
GEXE60   ST    R3,FULL             SAVE A(FIRST STATEMENT IN LIST)              
         L     RF,VEXTRAS                                                       
         L     R4,VLISTREC                                                      
         LA    R4,ACCORFST(R4)     GET THE LIST TYPE ELEMENT                    
         SR    R0,R0                                                            
         DROP  RF                                                               
*                                                                               
GEXE61   IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    GEXE69                                                           
         CLI   0(R4),X'1F'         GET LIST ELEMENT                             
         BNE   GEXE61                                                           
*                                                                               
         USING LIDELD,R4                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LIDLN            ELEMENT LENGTH                               
         SHI   R1,10                                                            
         SR    RE,RE                                                            
         IC    RE,LIDITLN          LENGTH OF ITEM                               
         DR    R0,RE               R0=NUMBER OF ITEMS                           
         LR    R0,R1                                                            
         LA    RF,LIDDACCS         LIST ITEMS                                   
*                                                                               
GEXE65   CLI   1(R3),0             TEST ANOTHER ITEM                            
         BNE   *+6                                                              
         DC    H'0'                EXPECTED ANOTHER STATEMENT                   
         ST    RF,DUB+4            SAVE ADDRESS OF LIST ITEM                    
         ST    R0,DUB              SAVE COUNT                                   
         SR    RF,RF                                                            
         ICM   RF,7,2(R3)          ADDRESS OF ORIGINAL SPEC                     
         MVI   0(R3),X'D5'         "CLC"                                        
         SR    R1,R1                                                            
         IC    R1,5(,RF)           LENGTH                                       
         BCTR  R1,0                                                             
         STC   R1,1(,R3)           SET INSTRUCTION LENGTH                       
         LA    R2,3(,RF)           R2=(FROM,DISP,LEN)                           
         BAS   RE,GEXEFM           GET "FROM" DATA ADDRESS                      
         STCM  R1,3,2(R3)          SET BASE AND DISPLACEMENT FOR ADDR1          
         STCM  R5,3,4(R3)          SET DISPLACEMENT INTO LITERAL POOL           
         OI    4(R3),CONLTR        SET LITERAL REGISTER                         
         L     R0,DUB              RESTORE COUNT                                
         L     RF,DUB+4            AND ITEM ADDRESS                             
         SR    R1,R1                                                            
         IC    R1,LIDITLN          LENGTH OF DATA                               
         BCTR  R1,0                                                             
*MN      EX    R1,*+4                                                           
         MVC   0(0,R6),0(RF)       LIST ITEM TO LITERAL POOL                    
         EX    R1,*-6                                                           
         LA    R5,1(R1,R5)         BUMP THE DISPLACEMENT                        
         LA    R6,1(R1,R6)         BUMP TO LITERAL POOL AREA                    
         LA    RF,1(R1,RF)         NEXT LIST ITEM                               
         LA    R3,10(,R3)          NEXT STATEMENT                               
         BCT   R0,GEXE65                                                        
         B     GEXE61              NEXT LIST ELEMENT                            
*                                                                               
GEXE69   L     R3,FULL             RESTORE A(FIRST STATEMNET)                   
*                                                                               
GEXE89   LA    R3,6(,R3)           BUMP TO NEXT INSTRUCTION                     
         B     GEXE43                                                           
*                                                                               
GEXE90   B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* SEARCH FOR NEXT OR / AND STATEMENT                                  *         
***********************************************************************         
         SPACE 1                                                                
GEXENX   DS    0H                  GET NEXT DISP. (RF=ZERO IF NONE)             
GEXENX1  CLI   0(R4),EOT           END OF TABLE                                 
         BE    GEXNO                                                            
         CLI   0(R4),CONNOT        IS IT A BRANCH - NOT                         
         BE    GEXENX2             R4 TO NEXT STATEMENT                         
         CLI   0(R4),CONOK         IS IT A BRANCH - OK                          
         BNE   GEXENX3             R4 TO NEXT STATEMENT                         
*                                                                               
GEXENX2  LA    R4,4(,R4)                                                        
         B     GEXENX1                                                          
*                                                                               
GEXENX3  CLC   5(1,R4),BYTE        IS IT SAME STATEMENT                         
         BNE   GEXNO               NO, SET NOT FOUND                            
         CLC   ELCODE,0(R4)        IS IT THE STATEMENT REQUESTED                
         BE    GEXENX7             YES, SET RETURN CODES                        
         CLI   ELCODE,CONOR        LOOKING FOR NEXT "OR'                        
         BNE   GEXENX4                                                          
         CLI   0(R4),CONAND        BUT MUST COME BEFORE NEXT "AND"              
         BE    GEXNO               RETURN NOT FOUND                             
         B     GEXENX5                                                          
*                                                                               
GEXENX4  CLI   ELCODE,CONAND       "AND" MUST BE FOUND BEFORE NEXT              
         BNE   GEXENX5                                                          
         CLI   0(R4),CONIF         "IF"                                         
         BE    GEXNO               RETURN NOT FOUND                             
*                                                                               
GEXENX5  LA    R4,10(,R4)          NO, LOOK TO NEXT                             
         B     GEXENX1                                                          
*                                                                               
GEXENX7  SR    RF,RF                                                            
         ICM   RF,3,6(R4)          DISPLACEMENT TO THIS                         
*                                                                               
GEXYES   CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
GEXNO    SR    RF,RF                                                            
         CR    RF,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GENERATE LITERALS FOR SOFT DATES (IE. &&ST+NNN, &&EN AND &&TY)      *         
***********************************************************************         
         SPACE 1                                                                
GEXEDT   LR    R0,RE                                                            
         STCM  R5,3,4(R3)          SET DISPLACEMENT TO DATE IN POOL             
         OI    4(R3),CONLTR        SET REGISTER                                 
         LA    RF,CSDTE            START DATE (CHARACTER - YYMMDD)              
         CLI   0(R2),FRST                                                       
         BE    GEXEDT3                                                          
         LA    RF,CEDTE            END DATE                                     
         CLI   0(R2),FREN                                                       
         BE    GEXEDT3                                                          
         LA    RF,TODAY            MUST BE TODAY                                
*                                                                               
GEXEDT3  SR    R1,R1                                                            
         IC    R1,1(R2)            NUMBER OF DAYS                               
         TM    2(R2),X'10'         PLUS 100                                     
         BNO   *+8                                                              
         AHI   R1,100                                                           
         TM    2(R2),X'20'         PLUS 200                                     
         BNO   *+8                                                              
         AHI   R1,200                                                           
         TM    2(R2),X'01'         ODD IS NEGATIVE                              
         BNO   *+6                                                              
         LNR   R1,R1                                                            
         ST    RF,DMCB             SOURCE DATE                                  
         ST    R6,DMCB+4           RESULT DATE                                  
         ST    R1,DMCB+8           NUMBER OF DAYS                               
         GOTO1 ADDAY,DMCB                                                       
         LA    R5,6(,R5)           BUMP THE DISPLACEMENT                        
         LA    R6,6(,R6)           AND LITERAL POINTER                          
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* GET BASE AND DISPLACEMENT FROM "FROM" LIST                          *         
***********************************************************************         
         SPACE 1                                                                
GEXEFM   LR    R0,RE               R2=A(FROM,DISP)                              
         CLI   0(R2),FRC1          FROM C1, C2, C3, C4                          
         BL    *+16                                                             
         CLI   0(R2),FRC4                                                       
         BH    *+8                                                              
         OI    GEXSW,GEXC1         SET - EXTRACT CONTRA LEVEL SWITCH            
         SR    R1,R1                                                            
         IC    R1,0(,R2)           FROM CODE                                    
         BCTR  R1,0                LESS ONE                                     
         SLL   R1,2                X4                                           
         L     RE,AFROMLST                                                      
         LA    RE,0(R1,RE)         RE=A(BASE/DISP TABLE FOR THIS TYPE)          
         SR    R1,R1                                                            
         ICM   R1,3,0(RE)          LOAD BASE AND DISP FROM TABLE                
         SR    RE,RE                                                            
         IC    RE,1(,R2)           DISPLACEMENT INTO DATA FOR "CLC"             
         AR    R1,RE               RETURN IN R1                                 
         ICM   RE,1,T1DISP                                                      
         BZ    *+6                                                              
         BCTR  RE,0                                                             
         CLI   0(R2),FRT1          IS IT T1                                     
         BNE   *+6                                                              
         AR    R1,RE               ADD EXTRA DISPLACEMENT                       
         CLI   0(R2),FRT2          FROM T2 CODE?                                
         BNE   *+12                                                             
         LH    R1,=S(CURRTE2)      SECOND HALF OF T&E CODE                      
         OI    GEXSW,GEXT2         FIX T2 LENGTH                                
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* REMOVE "LIST" STATEMENTS IF NOT REQUESTING CORRESPONDING LIST       *         
***********************************************************************         
         SPACE 1                                                                
GEXL     L     R6,AUSER            R6=USER SPECS                                
         SR    R1,R1                                                            
*                                                                               
GEXL03   CLI   0(R6),EOT           END OF SPECS                                 
         BER   RE                                                               
         CLI   0(R6),50            "IF"                                         
         BE    GEXL05                                                           
         CLI   0(R6),52            "OR"                                         
         BE    GEXL05                                                           
         CLI   0(R6),54            "AND"                                        
         BNE   GEXL10                                                           
*                                                                               
         USING RUNXTRAD,RF                                                      
GEXL05   CLC   6(5,R6),=C'\LIST'   LIST CONNECTIVE                              
         BNE   GEXL10                                                           
         L     RF,VEXTRAS                                                       
         OC    VLISTREC,VLISTREC   ANY LIST REQUESTED?                          
         BZ    GEXL09              IGNORE STATEMENT                             
         CLI   11(R6),C'-'         FILTER ON THE LIST CODE?                     
         BNE   GEXL10                                                           
         IC    R1,1(,R6)                                                        
         SHI   R1,13               GET LENGTH OF LIST CODE                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   QSELECT+1(0),12(R6)                                              
         BE    GEXL10                                                           
*                                                                               
GEXL09   MVC   3(5,R6),=X'2832017EFF'  REQ+5(1)=X'FF'                           
*                                                                               
GEXL10   IC    R1,1(,R6)                                                        
         AR    R6,R1                                                            
         B     GEXL03                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* EQUATES AND DATA CONSTANTS FOR GEXE                                 *         
***********************************************************************         
         SPACE 1                                                                
CONIF    EQU   1                   IF STATEMENT                                 
CONOR    EQU   2                   OR STATEMENT                                 
CONAND   EQU   3                   AND STATEMENT                                
CONNOT   EQU   4                   BRANCH IF NOT                                
CONOK    EQU   5                   BRANCH OK                                    
CONBSR   EQU   X'10'               BASE REGISTER FOR CONEXE BLOCK               
CONLTR   EQU   X'50'               BASE FOR LITERAL POOL                        
CONLTYP  DS    C                   LIST TYPE I(NCLUDE) E(XCLUDE)                
CONLINC  EQU   C'I'                                                             
CONLEXC  EQU   C'E'                                                             
*                                                                               
*                                                                               
CONYES   DS    0F                  SET YES CONDITION                            
         CR    RB,RB                                                            
         BR    RE                                                               
*                                                                               
CONNO    DS    0F                  SET NO CONDITION                             
         LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
CONFIRST DS    CL1                                                              
CONADDR  DS    F                   A(CURRENT CONDITION)                         
*                                                                               
CONXLNQ  EQU   L'CONYES+L'CONNO                                                 
*                                                                               
YESC     DS    0XL2                BASE DISPLACEMENT TO YES CODE                
         DC    AL1(CONBSR),AL1(0)                                               
NOTC     DS    0XL2                BASE DISPLACEMENT TO NO CODE                 
         DC    AL1(CONBSR),AL1(4)                                               
         CNOP  0,4                                                              
         TITLE 'INITIALIZATION - CONTROL LEDGER SETTINGS'                       
***********************************************************************         
* CONTROL LEDGER SETTINGS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGLD,R5                                                         
SLDGL    NTR1  ,                                                                
         CLI   READLEDG,C'A'       ONLY NEEDED FOR ACCPAK READS                 
         BL    XIT                                                              
         L     R5,ALEVPOOL         LEDGER POOL                                  
         CLI   0(R5),0             HAS TABLE BEEN BUILT                         
         BNE   SLDGL09             YES, GET CURRENT ENTRY                       
         LA    R6,DKEY                                                          
         MVC   0(42,R6),SPACES                                                  
         MVC   0(1,R6),RCCOMPFL    COMPANY                                      
         MVI   1(R6),X'41'                                                      
         LA    R3,LVTMX                                                         
*                                                                               
SLDGL05  SR    R1,R1               READ FOR NEXT LEDGER                         
         IC    R1,DKEY+2           INCREMENT LEDGER                             
         LA    R1,1(,R1)                                                        
         STC   R1,DKEY+2                                                        
         GOTO1 ADMGR,HIGH                                                       
         MVC   DKEY,DIR                                                         
         LA    R6,DIR                                                           
         CLC   0(1,R6),RCCOMPFL    TEST END OF COMAPNY                          
         BNE   SLDGL09             CURRENT ENTRY                                
         CLI   2(R6),C' '          UNIT RECORD                                  
         BE    SLDGL05             SKIP TO NEXT                                 
         CLC   3(12,R6),SPACES     LEDGER RECORD                                
         BNE   SLDGL05             SKIP TO NEXT                                 
         MVC   LDGLUNL,1(R6)       UNIT/LEDGER                                  
         BAS   RE,SLV1             SET LEDGER LEVELS                            
         LA    R5,LDGLLNQ(,R5)                                                  
         BCT   R3,SLDGL05                                                       
         DC    H'0'                LEDGER TABLE FULL                            
*                                                                               
SLDGL09  L     R5,ALEVPOOL         LEDGER POOL                                  
         CLC   LDGLUNL,QUNIT       TEST LEDGER FOR THIS REQUEST                 
         BE    SLDGL11             TABLE IS OK                                  
*                                                                               
         LA    R5,LDGLLNQ(,R5)                                                  
         CLC   LDGLUNL,QUNIT       TEST LEDGER FOR THIS REQUEST                 
         BNE   *-10                                                             
         L     R6,ALEVPOOL                                                      
         XC    0(LDGLLNQ,R5),0(R6) SWITCH THEM                                  
         XC    0(LDGLLNQ,R6),0(R5) SO THAT ENTRY FOR THIS REQUEST               
         XC    0(LDGLLNQ,R5),0(R6) IS ALWAYS FIRST                              
         L     R5,ALEVPOOL                                                      
*                                                                               
SLDGL11  MVC   CURRLEVS,LDGLNUM     SET DATA FOR CURRENT LEDGER                 
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* READ LEDGER AND SET LEVELS                                          *         
*  R5 = LDGLD ENTRY TO BE COMPLETED                                   *         
*  R6 = LEDGER RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGLD,R5                                                         
SLV1     STM   RE,R6,SVRE                                                       
         GOTO1 ADMGR,GETR                                                       
         L     R6,AIO1                                                          
         USING LDGRECD,R6                                                       
         LA    R6,LDGRFST                                                       
         SR    R0,R0                                                            
*                                                                               
SLV3     CLI   0(R6),0             END OF RECORD                                
         BE    SLVX                                                             
         CLI   0(R6),X'14'         LEDGER ELEMENT                               
         BE    SLV7                                                             
         CLI   0(R6),X'16'         LEVELS ELEMENT                               
         BE    SLV11                                                            
*                                                                               
SLV5     IC    R0,1(,R6)                                                        
         AR    R6,R0                                                            
         B     SLV3                                                             
*                                                                               
         USING LDGELD,R6                                                        
SLV7     CLC   LDGLUNL,COSTUL      IS IT CLIENT LEDGER                          
         BNE   SLV5                NO, DON'T SET CLIENT LEVEL                   
         MVI   CLIPOS,3            CLIENT POSITION IN KEY                       
         CLI   LDGCPOS,0                                                        
         BE    SLV5                                                             
         MVC   CLIPOS,LDGCPOS                                                   
         B     SLV5                                                             
*                                                                               
         USING ACLELD,R6                                                        
SLV11    LA    R2,ACLVALS          BUILD UP LEVEL DEFINITIONS                   
         SR    R1,R1               N'LEVELS                                     
         LA    R3,LDGLLEV                                                       
         USING LDGLLEV,R3                                                       
         SR    R4,R4               DISPLACEMENT                                 
         LA    R0,4                MAX 4                                        
*                                                                               
SLV13    CLI   0(R2),0             END OF LEVELS                                
         BE    SLV14                                                            
         MVI   LDGLACC,2           02=ACCOUNT                                   
         STC   R4,LDGLDSP          DISPLACEMENT                                 
         SR    RE,RE                                                            
         IC    RE,0(,R2)           LENGTH TO THIS LEVEL                         
         SR    RE,R4               LESS LENGTH TO PRIOR                         
         BZ    SLV14                                                            
         STC   RE,LDGLLEN          LENGTH OF THIS LEVEL                         
         IC    R4,0(,R2)           TOTAL LENGTH TO THIS LEVEL                   
         LA    R1,1(,R1)           NUMBER OF LEVELS                             
         LA    R2,L'ACLVALS(,R2)   NEXT LEVELS ELEMENT                          
         LA    R3,L'LDGLLEV(,R3)   NEXT LEVEL ENTRY                             
         BCT   R0,SLV13                                                         
*MN                                                                             
SLV14    CLC   LDGLUNL,COSTUL      IS IT CLIENT LEDGER                          
         BNE   SLV15               NO, DON'T SET CLIENT LEVEL                   
         LA    R2,ACLVALS          BUILD UP LEVEL DEFINITIONS                   
         LA    R0,4                                                             
SLV14A   CLC   0(1,R2),CLIPOS                                                   
         BH    SLV14B                                                           
         LA    R2,L'ACLVALS(,R2)   NEXT LEVELS ELEMENT                          
         BCT   R0,SLV14A                                                        
         B     SLV15                                                            
SLV14B   MVC   CLIPOS,0(R2)                                                     
*MN                                                                             
SLV15    STC   R1,LDGLNUM          SET NUMBER OF LEVELS                         
         OI    LDGLNUM,X'F0'       IN CHARACTER                                 
         B     SLV5                                                             
*                                                                               
SLVX     LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         DROP  R3,R5,R6                                                         
         TITLE 'INITIALIZATION - SET T&&E LENGTHS'                              
***********************************************************************         
* SET UP T&E LENGTHS                                                 *          
***********************************************************************         
         SPACE 1                                                                
SETTE    CLI   SYSEQU,ACMSEUR      ONLY FOR UK                                  
         BNER  RE                                                               
         STM   RE,R6,SVRE                                                       
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'A086'                                                 
         MVC   WORK+4(1),RCCOMPFL                                               
         MVC   WORK+12(2),ALPHAID                                               
         MVI   T1DISP,0                                                         
         MVI   T2LEN,0                                                          
         GOTO1 GETPROF,DMCB,WORK,WORK+20,DATAMGR                                
         MVC   T1DISP,WORK+20+1                                                 
         CLI   T1DISP,0                                                         
         BNE   SETTE03                                                          
         MVI   T1DISP,2                                                         
         CLI   WORK+20+3,C'Y'      IF T/E FROM 1ST CHAR OF CONTRA               
         BNE   SETTE03                                                          
         MVI   T1DISP,1            GO BACK ONE                                  
*                                                                               
SETTE03  CLC   QSELECT(4),=C'ALL '                                              
         BE    SETTEX              GET LENGTH FROM FIRST T2 CODE                
         CLI   QSELECT,C'+'                                                     
         BE    SETTEX              AND IF USING A LIST RECORD                   
         CLI   QSELECT,C'-'                                                     
         BE    SETTEX                                                           
         LA    RF,QSELECT+L'QSELECT-1                                           
         LA    RE,L'QSELECT        ELSE USE LENGTH OF SELECT INP.               
         CLI   0(RF),C' '                                                       
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   RE,*-10                                                          
         STC   RE,T2LEN            USE HEREAFTER                                
*                                                                               
SETTEX   LM    RE,R6,SVRE                                                       
         BR    RE                                                               
         TITLE 'INITIALIZATION - SET UP BUDGET TYPES'                           
***********************************************************************         
* SET UP BUDGET TYPES                                                 *         
***********************************************************************         
         SPACE 1                                                                
BUDGT    NTR1  ,                                                                
         L     R6,AUSER                                                         
         XC    BUDGLIST,BUDGLIST                                                
         SR    R0,R0                                                            
*                                                                               
BUDGT3   IC    R0,1(,R6)           LOOK FOR 60 SPECS                            
         AR    R6,R0                                                            
         CLI   0(R6),X'FF'                                                      
         BE    XIT                                                              
         CLI   0(R6),CMBGDT                                                     
         BNE   BUDGT3                                                           
*                                                                               
         SR    R3,R3                                                            
         IC    R3,2(,R6)           BUDGET NO.                                   
         BCTR  R3,0                                                             
         LA    R4,QAPPL(R3)        OVERRIDE BUDGETS CODES                       
         SLL   R3,1                                                             
         LA    R3,BUDGLIST(R3)     KEY INTO BUDGLIST                            
         MVC   0(2,R3),3(R6)                                                    
*&&UK                                                                           
         CLI   0(R4),C' '                                                       
         BE    *+10                                                             
         MVC   1(1,R3),0(R4)                                                    
*&&                                                                             
*&&US                                                                           
         SR    RF,RF                                                            
         CLI   2(R6),L'QAPPL       ONLY OVERRIDE FIRST 12                       
         BH    *+16                                                             
         ICM   RF,1,0(R4)          OVER-RIDE BUDGET#                            
         BZ    *+8                 NO                                           
         STC   RF,1(,R3)                                                        
*&&                                                                             
         SR    R5,R5                                                            
         IC    R5,2(,R6)                                                        
         BCTR  R5,0                                                             
         MHI   R5,20                                                            
         A     R5,ABUDGTIT                                                      
         LA    R4,DKEY                                                          
         XC    0(42,R4),0(R4)                                                   
*                                                                               
         USING BUDKEY,R4                                                        
         MVI   BUDKTYP,X'1B'                                                    
         MVC   1(1,R4),RCCOMPFL                                                 
         MVC   BUDKNO1,0(R3)       USER SELECTED NO.                            
*        MVC   BUDKNO1,3(R6)       USER SELECTED NO.                            
         OC    BUDKNO1,BUDKNO1                                                  
         BZ    BUDGT5                                                           
         GOTO1 ADMGR,HIGH                                                       
         CLC   DIR(5),DKEY                                                      
         BE    BUDGT9                                                           
         B     BUDGT3                                                           
*                                                                               
BUDGT5   MVC   BUDKCOD,5(R6)       USER SELECTED CODE                           
         LA    R1,BUDKCOD                                                       
         LA    R0,9                                                             
*                                                                               
BUDGT7   CLC   0(2,R1),=C'QY'      SUBSTITUTE REQUEST END YEAR                  
         BNE   *+10                           FOR SOFT YEAR                     
         MVC   0(2,R1),QEND                                                     
         CLC   0(2,R1),=C'QM'              OR REQUEST END MONTH                 
         BNE   *+10                           FOR SOFT MONTH                    
         MVC   0(2,R1),QEND+2                                                   
         LA    R1,1(,R1)                                                        
         BCT   R0,BUDGT7                                                        
*                                                                               
         GOTO1 ADMGR,HIGH                                                       
         CLC   DIR(15),DKEY                                                     
         BNE   BUDGT3                                                           
         LA    R4,DIR                                                           
         MVC   0(2,R3),BUDKNO2                                                  
*                                                                               
BUDGT9   GOTO1 ADMGR,GETR                                                       
         L     R4,AIO1                                                          
         LA    R4,BUDRFST                                                       
*                                                                               
BUDGT11  CLI   0(R4),BCNELQ        LOOK FOR A 1B ELEMENT                        
         BNE   BUDGT13                                                          
         USING BCNELD,R4                                                        
         MVC   0(10,R5),BCNCOL1                                                 
         MVC   10(10,R5),BCNCOL2                                                
         B     BUDGT3                                                           
*                                                                               
BUDGT13  IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BNE   BUDGT11                                                          
         B     BUDGT3                                                           
         DROP  R4                                                               
         TITLE 'INITIALIZATION - BUILD REPORT STACK'                            
***********************************************************************         
* INITIALIZE REPORT STACK                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
INSTK    NTR1  ,                                                                
         L     R2,ARSTACK                                                       
         LA    R4,RPTLN            LENGTH OF ONE ENTRY                          
         LA    R5,RPTMX            NUMBER OF ENTRIES                            
         ST    R4,WRSTACK          SAVE WIDTH OF REPORT                         
*                                                                               
INSTK3   MVC   WORK(L'RSRIGHT),RSRIGHT                                          
         LR    RE,R2                                                            
         LA    RF,RPTLN                                                         
         XCEFL                                                                  
         MVC   RSCODE,SPACES                                                    
         MVC   RSNAME,SPACES                                                    
         MVC   RSRIGHT,WORK                                                     
         TM    RQSW,RQFLG                                                       
         BNO   *+10                                                             
         MVC   RSRIGHT,SPACES                                                   
         MVI   RSWLEFT+3,40                                                     
         MVI   RSHEAD1,C' '                                                     
         MVC   RSHEAD1+1(L'RSHEAD1-1),RSHEAD1                                   
         MVC   RSHEAD2,RSHEAD1                                                  
         MVC   RSHEAD3,RSHEAD2                                                  
         MVI   RSCOPY,1                                                         
         MVI   RSSPACE,1                                                        
         MVI   RSAUTOCN,YES                                                     
         MVC   RSROUND,=X'0040'                                                 
         MVC   RSLPROW,=H'1'                                                    
         AR    R2,R4                                                            
         BCT   R5,INSTK3                                                        
         B     XIT                                                              
         DROP  R2                                                               
         TITLE 'INITIALIZATION - EXTRACT REPORT SPECS'                          
***********************************************************************         
* EXTRACT REPORT SPECS                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XTRCT    NTR1  ,                                                                
         L     R6,AUSER                                                         
         SR    RF,RF                                                            
*                                                                               
XTRCT3   CLI   0(R6),EOT           TEST END OF TABLE                            
         BE    XIT                                                              
         LA    R1,XTAB             EXTRACT TABLE                                
*                                                                               
XTRCT5   ICM   RF,7,1(R1)          LOAD ADDRESS OF ROUTINE                      
         CLC   0(1,R1),0(R6)       MATCH CODE                                   
         BER   RF                                                               
         LA    R1,L'XTAB(,R1)      BUMP TO NEXT ENTRY                           
         CLI   0(R1),EOT           END OF TABLE?                                
         BNE   XTRCT5              NO, SO KEEP CHECKING                         
*                                                                               
XTRCT7   SR    RF,RF               NEXT SPEC                                    
         IC    RF,1(,R6)                                                        
         AR    R6,RF                                                            
         B     XTRCT3                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* REPORT SPEC CONTROL TABLE                                           *         
***********************************************************************         
         SPACE 1                                                                
XTAB     DS    0F                                                               
         DC    AL1(CMRPRT),AL3(XRPRT)       REPORT                              
         DC    AL1(CMREPC),AL3(XREPC)       REPCODE                             
         DC    AL1(CMSHWZ),AL3(XSHWZ)       SHOWZERO                            
         DC    AL1(CMREPN),AL3(XREPN)       REPNAME                             
         DC    AL1(CMREPR),AL3(XREPR)       REPRIGHT                            
         DC    AL1(CMCOPS),AL3(XCOPS)       COPIES                              
         DC    AL1(CMNULM),AL3(XNULM)       NULMIDS                             
         DC    AL1(CMSPCG),AL3(XSPCG)       SPACING                             
         DC    AL1(CMRUND),AL3(XRUND)       ROUND                               
         DC    AL1(CMIGNR),AL3(XIGNR)       IGNORE                              
         DC    AL1(CMFLTR),AL3(XFLTR)       FILTER                              
         DC    AL1(CMRVED),AL3(XRVED)       REVEDIT                             
         DC    AL1(CMOPTN),AL3(XOPTN)       OPTION                              
         DC    AL1(CMENCH),AL3(XENCH)       ENCROACH                            
         DC    AL1(CMRCAP),AL3(XRCAP)       RECAP/FDOWN                         
         DC    AL1(CMSRHK),AL3(XSRHK)       SORTHOOK                            
         DC    AL1(CMROWL),AL3(XROWL)       ROWLIST                             
         DC    AL1(CMSROT),AL3(XSROT)       SORTOUT                             
         DC    AL1(CMROW),AL3(XROW)         ROW                                 
         DC    AL1(CMPTHK),AL3(XPTHK)       PUTHOOK                             
         DC    AL1(CMROWN),AL3(XROWN)       ROWNAME                             
         DC    AL1(CMADJH),AL3(XADJH)       ADJHOURS                            
         DC    AL1(CMROWQ),AL3(XROWQ)       ROWEQU                              
         DC    AL1(CMNTOT),AL3(XNTOT)       NOTOT                               
         DC    AL1(CMNRLL),AL3(XNRLL)       NOROLL                              
         DC    AL1(CMSTOP),AL3(XSTOP)       STOP                                
         DC    AL1(CMCHOP),AL3(XCHOP)       CHOP                                
         DC    AL1(CMFOLD),AL3(XFOLD)       FOLD                                
         DC    AL1(CMRNMS),AL3(XRNMS)       ROWNMSQ                             
         DC    AL1(CMCOL),AL3(XCOL)         COL                                 
         DC    AL1(CMCLCP),AL3(XCLCP)       COLCOMP                             
         DC    AL1(CMCOLN),AL3(XCOLN)       COLNAME                             
         DC    AL1(CMCHNK),AL3(XCHNK)       CHUNK                               
         DC    AL1(CMCOLQ),AL3(XCOLQ)       COLEQU                              
         DC    AL1(CMCLCN),AL3(XCLCN)       COLCOMP-                            
         DC    AL1(CMCOLA),AL3(XCOLA)       COLALL                              
         DC    AL1(CMIF),AL3(XIF)           IF                                  
         DC    AL1(CMOR),AL3(XOR)           OR                                  
         DC    AL1(CMRPOL),AL3(XRPOL)       RPOOL                               
         DC    AL1(CMNAUT),AL3(XNAUT)       NOAUTOCN                            
         DC    AL1(CMNSRT),AL3(XNSRT)       NOSRTOT                             
         DC    AL1(CMSCAL),AL3(XSCAL)       SUPERCAL                            
         DC    AL1(CMDO),AL3(XSCAL)         DO                                  
         DC    AL1(CMSCND),AL3(XSCAL)       SCEND                               
         DC    AL1(CMAHRS),AL3(XAHRS)       A-HOURS                             
         DC    AL1(CMWKND),AL3(XWKND)       WEEKEND                             
         DC    AL1(CMBCK$),AL3(XBCK$)       BUCKET$                             
         DC    AL1(CMKCOL),AL3(XKCOL)       KEYCOL,SECCOL,OVRCOL                
         DC    AL1(CMLPOLR),AL3(XLPOLR)     LPOOLROW                            
         DC    AL1(CMPMOS),AL3(XLPMOS)      POSTMOS                             
         DC    AL1(CMPBDT),AL3(XPBDT)       POST BY DATE                        
         DC    AL1(CMNBIN),AL3(XNBIN)       NOBINARY                            
         DC    AL1(CMPDPT),AL3(XPDPT)       POST BY DEPT                        
         DC    X'FF'                                                            
         CNOP  0,4                                                              
         EJECT                                                                  
***********************************************************************         
* EXTRACT - REPORT FIELDS                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XRPRT    SR    R1,R1                                                            
         IC    R1,2(,R6)           REPORT                                       
         C     R1,NRSTACK          SET HIGHEST REPORT                           
         BL    *+8                                                              
         ST    R1,NRSTACK                                                       
         LA    RE,RPTMX            TEST MAXIMUM                                 
         CR    R1,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                TOO MANY REPORTS                             
         BCTR  R1,0                                                             
         M     R0,WRSTACK                                                       
         L     R2,ARSTACK                                                       
         AR    R2,R1                                                            
         B     XTRCT7                                                           
*                                                                               
XREPC    MVC   RSCODE,2(R6)        REPCODE                                      
         B     XTRCT7                                                           
*                                                                               
XREPN    SR    R1,R1                                                            
         IC    R1,1(,R6)           REPNAME                                      
         MVC   RSNAME,SPACES                                                    
         SHI   R1,3                                                             
*MN      EX    R1,*+4                                                           
         MVC   RSNAME(0),2(R6)                                                  
         EX    R1,*-6                                                           
         GOTO1 CENTER,DMCB,RSNAME,40                                            
         B     XTRCT7                                                           
*                                                                               
XREPR    SR    R1,R1                                                            
         IC    R1,1(,R6)           REPRIGHT                                     
         MVC   RSRIGHT,SPACES                                                   
         SHI   R1,3                                                             
         CHI   R1,(L'RSRIGHT-1)                                                 
         BNH   *+8                                                              
         LHI   R1,(L'RSRIGHT-1)                                                 
*MN      EX    R1,*+4                                                           
         MVC   RSRIGHT(0),2(R6)                                                 
         EX    R1,*-6                                                           
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - ROW FIELDS                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XROW     CLI   3(R6),FRTK          ROW DATA - TASK                              
         BNE   *+8                                                              
         OI    PCSW,PCTSK          SET POST BY TASK                             
*                                                                               
         LA    R1,5                                                             
         LA    RE,FLTFORMS                                                      
         CLC   3(1,R6),0(RE)       CHECK FOR F1-F5                              
         BE    XROW03              FOUND A FILTER FORM CODE                     
         LA    RE,1(,RE)           NEXT FROM CODE                               
         BCT   R1,*-14                                                          
         B     *+8                                                              
XROW03   MVI   FCGTFILT,YES        SET MONACC TO GET FILTER NAMES               
*                                                                               
         SR    R1,R1                                                            
         IC    R1,2(,R6)           ROW  LEVEL                                   
         CLI   1(R6),6             TEST ROW CONDITIONAL                         
         BE    *+14                                                             
         CLC   6(2,R6),READLEDG                                                 
         BNE   XTRCT7                                                           
         ST    R1,LASTROWL         SAVE ROW LEVEL                               
         C     R1,RSNROWS                                                       
         BL    *+8                                                              
         ST    R1,RSNROWS          SET NUMBER OF ROWS                           
         C     R1,HIGHROW          SET HIGHEST GLOBAL ROW                       
         BL    *+8                                                              
         ST    R1,HIGHROW                                                       
         BCTR  R1,0                                                             
         MHI   R1,3                                                             
         LA    R1,RSROWS(R1)                                                    
         MVC   0(3,R1),3(R6)                                                    
         BAS   RE,SUBFRM           ANY SUBSTITUTION                             
         LA    R1,3(,R6)                                                        
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - ROW NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XROWN    L     R1,LASTROWL         ROWNAME                                      
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,RSARNAME(R1)                                                  
         ST    R6,0(,R1)           SAVE ADDRESS OF SPEC                         
         CLI   3(R6),1             IF ITS A HEADLINE                            
         BNE   XTRCT7                                                           
         CLI   1(R6),6             IS IT LENGTH 6                               
         BNE   *+12                                                             
         CLI   5(R6),C' '          IS IT JUST A SPACE                           
         BE    XROWN5                                                           
         SR    R1,R1                                                            
         IC    R1,1(,R6)           SEE IF ITS FATTER THAN                       
         SHI   R1,5                PREVIOUS SPECS                               
         SR    R0,R0                                                            
         IC    R0,RSBIGH                                                        
         CR    R1,R0                                                            
         BL    XROWN5                                                           
         STC   R1,RSBIGH                                                        
*                                                                               
XROWN5   CLI   2(R6),3             IF ITS ROWBOTH                               
         BNE   XTRCT7                                                           
         L     R1,LASTROWL                                                      
         BCTR  R1,0                                                             
         MHI   R1,3                                                             
         LA    R1,RSROWS(R1)       POSITION TO COLUMN                           
         CLI   2(R1),0             IF ITS ZERO LENGTH                           
         BNE   *+12                                                             
         LA    R1,DUB              DEFAULT TO 8                                 
         MVI   DUB+2,8                                                          
         CLC   2(1,R1),RSBIGC      SEE IF LENGTH OF CODE > PREVIOUS             
         BL    XTRCT7                                                           
         MVC   RSBIGC,2(R1)                                                     
         B     XTRCT7                                                           
*                                                                               
XRNMS    LA    R0,L'RSNAMSQ        ROWNMSQ ROW NAME SEQUENCE                    
         LA    R1,RSNAMSQ                                                       
         CLI   0(R1),0             FIND NEXT AVAILABLE SLOT                     
         BE    *+16                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
         B     XTRCT7              MAX NUMBER OF ROW NAME SORTS                 
*                                                                               
         L     R3,LASTROWL                                                      
         STC   R3,0(,R1)           SAVE THIS ROW NUMBER                         
         BCTR  R3,0                                                             
         LA    R3,SRTFLN(R3)       R3 = RELATIVE FIELD NUMBER                   
         MVI   0(R3),52            SET SORT FIELD LENGTH                        
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - ROW EQU                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XROWQ    SR    R1,R1                                                            
         IC    R1,2(,R6)           ROW EQU N                                    
         BCTR  R1,0                                                             
         M     R0,WRSTACK                                                       
         L     R3,ARSTACK                                                       
         AR    R3,R1               R3=A(LINE N)                                 
         MVC   RSROWS,RSROWS-RSITEM(R3)                                         
         MVC   RSNROWS,RSNROWS-RSITEM(R3)                                       
         MVC   RSARNAME,RSARNAME-RSITEM(R3)                                     
         MVC   RSBIGH,RSBIGH-RSITEM(R3)                                         
         MVC   RSBIGC,RSBIGC-RSITEM(R3)                                         
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - COL/COLCOMP                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XCOL     BAS   RE,XCOLALL          COL                                          
         MVC   0(9,R3),3(R6)                                                    
*JAN13/04CLI   QOPT7,C'N'          IS SECURITY ACITIVATED ?                     
         CLI   QOPT10,C'N'         IS SECURITY ACITIVATED ?                     
         BNE   XCOLXX              NO, SO COUNTIUE AS USUAL                     
*                                                                               
         LA    RE,M3OVRCOL         OVER-RIDE SECURITY FUNCTIONALITY             
         LA    RF,L'M3OVRCOL                                                    
XCOL10   CLI   0(RE),0                                                          
         BE    XCOL12                                                           
         CLC   0(1,RE),2(R6)       MATCH COLUMN NUMBER                          
         BE    XCOLXX              IGNORE ANY SECURITY                          
         LA    RE,1(,RE)                                                        
         BCT   RF,XCOL10                                                        
*                                                                               
XCOL12   LA    RE,M3SECCOL         FORCE SECURITY FUNCTIONALITY                 
         LA    RF,L'M3SECCOL                                                    
XCOL15   CLI   0(RE),0                                                          
         BE    XCOL20                                                           
         CLC   0(1,RE),2(R6)       MATCH COLUMN NUMBER                          
         BE    XCOL18              IGNORE ANY SECURITY                          
         LA    RE,1(,RE)                                                        
         BCT   RF,XCOL15                                                        
         B     XCOL20                                                           
*                                                                               
XCOL18   MVI   8(R3),1             OVER-RIDE TO HIDE COLUMN                     
*                                                                               
XCOL20   LA    RF,4                                                             
         LR    RE,R3                                                            
XCOL25   CLI   0(RE),9             KEYWORD TYPE                                 
         BE    XCOL28                                                           
         LA    RE,2(,RE)           CHECK NEXT FOR TYPE 9 KEYWORD                
         BCT   RF,XCOL25                                                        
         B     XCOLXX                                                           
*                                                                               
XCOL28   LA    RF,SALKYWDS         LIST OF KEYWORDS                             
         LA    R1,SALKYWQ          NUMBER OF KEYWORDS TO CHECK FOR              
*                                                                               
XCOL30   CLC   1(1,RE),0(RF)                                                    
         BNE   *+8                                                              
         MVI   8(R3),1             OVER-RIDE TO HIDE COLUMN                     
         LA    RF,1(,RF)                                                        
         BCT   R1,XCOL30                                                        
*                                                                               
XCOLXX   BAS   RE,CHKBUD           SEE IF BUDGETS ARE NEEDED                    
         BAS   RE,COLNAM                                                        
         B     XTRCT7                                                           
*                                                                               
XCLCN    DS    0H                  COLCOMP                                      
XCLCP    BAS   RE,XCOLALL          COLCOMP-                                     
         SR    R1,R1                                                            
         IC    R1,1(,R6)                                                        
         SHI   R1,4                                                             
         LA    RE,9(,R3)                                                        
         CLI   0(R6),X'2D'         TEST NEGATIVE FORMULA                        
         BNE   *+8                                                              
         LA    RE,19(,R3)                                                       
*MN      EX    R1,*+4                                                           
         MVC   0(0,RE),3(R6)       MOVE IN FORMULA                              
         EX    R1,*-6                                                           
         BAS   RE,CHKVRT           CHECK FOR VERTICAL PERCENT                   
         B     XTRCT7                                                           
*                                                                               
XCOLALL  SR    R1,R1                                                            
         IC    R1,2(,R6)           POSITION R3 TO COL N                         
         C     R1,RSNCOLS                                                       
         BNH   *+8                                                              
         ST    R1,RSNCOLS          (CHECK N'COLS)                               
         C     R1,HIGHCOL          SET HIGHEST GLOBAL COLUMN                    
         BL    *+8                                                              
         ST    R1,HIGHCOL                                                       
         BCTR  R1,0                                                             
         MHI   R1,29                                                            
         LA    R3,RSCOLS(R1)                                                    
         BR    RE                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - COLNAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
XCOLN    MVC   COLNO,2(R6)         COLNAME                                      
         BAS   RE,COLINF                                                        
         ICM   R5,15,COLWIDE                                                    
         BZ    XTRCT7              NOT FOR W=1 COLS                             
         L     R3,COLAHEAD                                                      
         LA    R4,3(,R6)                                                        
         LA    R0,3                                                             
*                                                                               
XCOLN3   CLC   0(12,R4),SPACES                                                  
         BE    XCOLN7                                                           
         L     R5,COLWIDE                                                       
         CLC   0(12,R4),=CL12'*'   JUST ASTERISK MEANS CLEAR                    
         BNE   XCOLN5                                                           
         BCTR  R5,0                                                             
*MN      EX    R5,*+4                                                           
         MVC   0(0,R3),SPACES                                                   
         EX    R5,*-6                                                           
         B     XCOLN7                                                           
*                                                                               
XCOLN5   MVC   WORK(12),0(R4)                                                   
         GOTO1 CENTER,DMCB,WORK,(R5)                                            
         BCTR  R5,0                                                             
*MN      EX    R5,*+4                                                           
         MVC   0(0,R3),WORK                                                     
         EX    R5,*-6                                                           
*                                                                               
XCOLN7   LA    R4,12(,R4)                                                       
         LA    R3,164(,R3)                                                      
         BCT   R0,XCOLN3                                                        
         B     XTRCT7                                                           
         EJECT                                                                  
***********************************************************************         
* EXTRACT - COLUMN CHUNK                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XCHNK    MVC   COLNO,2(R6)                                                      
XCHNK3   BAS   RE,COLINF                                                        
         L     R3,COLAHEAD         R3=A(FIRST HEADING)                          
         L     R4,COLWIDE                                                       
         TM    DWNSTAT,DWNLOAD     IF DOWN-LOADING CHUNK TO EACH COL            
         BNO   XCHNK5                                                           
         CHI   R4,1                                                             
         BH    XCHNK7                                                           
         B     XCHNK19                                                          
*                                                                               
XCHNK5   MVC   COLNO,3(R6)                                                      
         BAS   RE,COLINF                                                        
         L     R4,COLAHEAD                                                      
         A     R4,COLWIDE                                                       
         SR    R4,R3               R4=WIDTH                                     
*                                                                               
XCHNK7   BCTR  R4,0                CLEAR ANY PREVIOUS NAMES                     
*MN      EX    R4,*+4                                                           
         MVC   0(0,R3),SPACES                                                   
         EX    R4,*-6                                                           
         LA    R4,1(,R4)                                                        
         TM    DWNSTAT,DWNLOAD     IF DOWN-LOADING DON'T CHUNK                  
         BO    XCHNK11                                                          
         MVC   ULN,SPACES          PICK OUT RIGHT&LEFTMOST POS.                 
         LR    R0,R4                                                            
         LA    R5,ULN                                                           
         LA    R1,328(,R3)                                                      
         LA    RF,164(,R3)                                                      
*                                                                               
XCHNK9   CLI   0(R1),X'40'                                                      
         BNE   *+12                                                             
         CLI   0(RF),X'40'                                                      
         BE    *+8                                                              
         MVI   0(R5),C'X'                                                       
         LA    R5,1(,R5)                                                        
         LA    RF,1(,RF)                                                        
         LA    R1,1(,R1)                                                        
         BCT   R0,XCHNK9                                                        
*&&UK                                                                           
         TM    RSOPT2,RSNOCHNK    OPTION TO OMIT MIDLINE IN CHINK               
         BO    XCHNK11                                                          
*&&                                                                             
         GOTO1 UNDERLIN,DMCB,((R4),ULN),(HORIZ,0(R3))                           
*                                                                               
XCHNK11  SR    R1,R1                                                            
         IC    R1,1(,R6)           CENTER CHUNK EXPRESSION IN WORK              
         SHI   R1,5                                                             
         MVC   WORK,SPACES                                                      
*MN      EX    R1,*+4                                                           
         MVC   WORK(0),4(R6)                                                    
         EX    R1,*-6                                                           
         LA    R1,1(,R1)                                                        
         CLC   WORK(5),=C'MONTH'                                                
         BNE   *+14                                                             
*MN      MVC   WORK(6),MONALIST+138                                             
         MVC   WORK(6),MYEDTE                                                   
         LA    R1,1(,R1)                                                        
         CLC   WORK(2),=C'PM'      PERIOD MONTH EXPRESSION?                     
         BE    *+14                                                             
         CLC   WORK(2),=C'RM'      OR ROLLING MONTH                             
         BNE   XCHNK13                                                          
         SR    R5,R5                                                            
         IC    R5,WORK+2           MONTH NUMBER                                 
         TM    WORK+3,X'F0'        OCT NOV OR DEC                               
         BNO   *+12                                                             
         IC    R5,WORK+3           IF SO, TAKE SECOND DIGIT                     
         LA    R5,10(,R5)          AND ADD TEN                                  
         SLL   R5,28               SHUNT OUT ZONE                               
         SRL   R5,27               MULT BY TWO FOR START END PAIR               
         CLI   WORK,C'R'           ROLLING MONTH                                
         BE    *+12                                                             
         LA    R5,PERIOD(R5)                                                    
         B     *+8                                                              
         LA    R5,ROLLING(R5)                                                   
         SR    R0,R0                                                            
         IC    R0,0(,R5)           START MONTH NUMBER                           
         LR    R5,R0                                                            
         BCTR  R5,0                                                             
         MHI   R5,6                                                             
         LA    R5,MONALIST(R5)     AND WE POINT TO THE RIGHT MONTH              
         MVC   WORK(6),0(R5)                                                    
         LA    R1,6                                                             
*                                                                               
XCHNK13  LA    R5,WORK                                                          
XCHNK15  CLI   0(R5),C' '          SUBSTITUTE ZERO FOR SPACE                    
         BNE   *+8                                                              
         MVI   0(R5),0                                                          
         LA    R5,1(,R5)                                                        
         BCT   R1,XCHNK15                                                       
*                                                                               
         GOTO1 CENTER,DMCB,WORK,(R4)                                            
         LA    R5,WORK                                                          
         LR    R1,R3                                                            
         LR    R0,R4                                                            
*                                                                               
XCHNK17  CLI   0(R5),C' '          MOVE EXPRESSION INTO                         
         BE    *+10                CENTER OF HORIZ LINES                        
         MVC   0(1,R1),0(R5)                                                    
         LA    R5,1(,R5)                                                        
         LA    R1,1(,R1)                                                        
         BCT   R0,XCHNK17                                                       
*                                                                               
XCHNK19  CLC   COLNO,3(R6)         LAST COLUMN PROCESSED                        
         BE    XCHNK21                                                          
         SR    R1,R1                                                            
         IC    R1,COLNO            PROCESS NEXT COLUMN                          
         LA    R1,1(,R1)                                                        
         STC   R1,COLNO                                                         
         B     XCHNK3                                                           
*                                                                               
XCHNK21  SR    R1,R1                                                            
         IC    R1,2(,R6)                                                        
         SR    R0,R0                                                            
         IC    R0,3(,R6)                                                        
         SR    R0,R1                                                            
         LA    R1,RSCHUNK(R1)                                                   
*                                                                               
XCHNK23  MVI   0(R1),NO            TURN OFF VERTICAL BOXES                      
         LA    R1,1(,R1)                                                        
         BCT   R0,XCHNK23                                                       
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*  EXTRACT - COL EQU                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XCOLQ    SR    R1,R1                                                            
         IC    R1,2(,R6)           COL EQU N                                    
         BCTR  R1,0                                                             
         M     R0,WRSTACK                                                       
         L     R3,ARSTACK                                                       
         AR    R3,R1               POSITION R3 TO REPORT N                      
         MVC   RSNCOLS,RSNCOLS-RSITEM(R3)                                       
         MVC   RSALLCOL,RSALLCOL-RSITEM(R3)                                     
         MVC   RSCOL1,RSCOL1-RSITEM(R3)                                         
         MVC   RSCOL2,RSCOL2-RSITEM(R3)                                         
         MVC   RSCOL3,RSCOL3-RSITEM(R3)                                         
         MVC   RSCOL4,RSCOL4-RSITEM(R3)                                         
         MVC   RSHEAD1,RSHEAD1-RSITEM(R3)                                       
         MVC   RSHEAD2,RSHEAD2-RSITEM(R3)                                       
         MVC   RSHEAD3,RSHEAD3-RSITEM(R3)                                       
         MVC   RSCHUNK,RSCHUNK-RSITEM(R3)                                       
         MVC   RSFOLD,RSFOLD-RSITEM(R3)                                         
         MVC   RSVERT,RSVERT-RSITEM(R3)                                         
         MVC   RSWLEFT,RSWLEFT-RSITEM(R3)                                       
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - COLALL                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XCOLA    LA    R1,RSALLCOL         COL ALL - ROOM FOR UP TO 3                   
         LA    R0,3                2 BYTE KEYS                                  
*                                                                               
XCOLA3   OC    0(2,R1),0(R1)       LOCATE                                       
         BE    XCOLA5                                                           
         LA    R1,2(,R1)                                                        
         BCT   R0,XCOLA3                                                        
         DC    H'0'                                                             
*                                                                               
XCOLA5   MVC   0(2,R1),2(R6)                                                    
         LA    R3,RSALLCOL                                                      
         BAS   RE,CHKBUD                                                        
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - REVEDIT/NOTOT/NOROLL/STOP                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XNRLL    DS    0H                                                               
XSTOP    DS    0H                                                               
XNTOT    DS    0H                                                               
XNRLL3   CLI   2(R6),64            IS IT NOROLL?                                
         BNE   XSTOP3                                                           
         OI    RSCOPTN,RSCNRANY    TURN ON ANY NOROLL                           
         CLI   1(R6),3             ALL COLUMNS                                  
         BNE   *+8                                                              
         OI    RSCOPTN,RSCNRALL    NOROLL ON ALL COLUMNS                        
         LA    R3,RSCOPTN          COLUMN OPTION FIELD                          
         LA    R0,L'RSCOPTN                                                     
         MVI   BYTE,RSCNROLL       SET TO NOROLL                                
         B     XRVED3                                                           
*                                                                               
XSTOP3   CLI   2(R6),65            IS IT STOP?                                  
         BNE   XNTOT3                                                           
         LA    R3,RSCOPTN          COLUMN OPTION FIELD                          
         LA    R0,L'RSCOPTN                                                     
         MVI   BYTE,RSCSTOP        SET TO STOP                                  
         B     XRVED3                                                           
*                                                                               
XNTOT3   DS    0H                                                               
XRVED    SR    R3,R3               NOTOT AND REVEDIT ARE CELL OPTIONS           
         IC    R3,2(,R6)           ROW(LEVEL) NUMBER                            
         MHI   R3,RSOMX            X NUMBER OF COLUMN OPTION FIELDS             
         LA    R3,RSCELLO(R3)      CELL OPTION BYTE                             
         LA    R0,RSOMX                                                         
         MVI   BYTE,RSCRVEDT       SET REVERSE EDIT                             
         CLI   0(R6),CMRVED        REVERSE EDIT?                                
         BE    XRVED3                                                           
         MVI   BYTE,RSCNTCOL       SET TO NOTOT                                 
         CLI   1(R6),3             ALL COLUMNS                                  
         BNE   *+8                                                              
         OI    0(R3),RSCNTALL      NOTOT ON ALL COLUMNS                         
*                                                                               
XRVED3   CLI   1(R6),3             IS IT FOR ENTIRE ROW                         
         BNE   XRVED5                                                           
         OC    0(1,R3),BYTE        TURN ON BIT FOR EACH COLUMN                  
         LA    R3,1(,R3)                                                        
         BCT   R0,*-10                                                          
         B     XTRCT7                                                           
*                                                                               
XRVED5   SR    R0,R0                                                            
         IC    R0,1(,R6)                                                        
         SHI   R0,3                R0=GET NUMBER OF COLUMNS                     
         LA    R4,3(,R6)           R4=LIST OF COLUMN NUMBERS                    
*                                                                               
XRVED7   SR    RE,RE                                                            
         ICM   RE,1,0(R4)          RE=COLUMN NUMBER                             
         BZ    XRVED9                                                           
         BCTR  RE,0                OFFSET IN ROW                                
         AR    RE,R3               ADD START OF ROW                             
         OC    0(1,RE),BYTE        SET OPTION BIT                               
XRVED9   LA    R4,1(,R4)                                                        
         BCT   R0,XRVED7                                                        
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - IF/OR/RPOOL                                               *         
***********************************************************************         
         SPACE 1                                                                
XIF      DS    0H                                                               
XOR      DS    0H                  SET ADDRESS OF FIRST CONDITIONAL             
         CLI   CONFIRST,YES                                                     
         BNE   XTRCT7                                                           
         ST    R6,CONADDR                                                       
         MVI   CONFIRST,NO                                                      
         B     XTRCT7                                                           
*                                                                               
XRPOL    L     R1,ACPOOL           PUT ENTRY INTO CPOOL                         
         OI    LPLSW,LPLPL         TURN ON PROCESS POOL                         
         LA    R0,CPTMX            MAX IN TABLE                                 
         CLI   0(R1),X'FF'         FIND END OF TABLE                            
         BE    *+14                                                             
         LA    R1,CPOOLNQ(R1)                                                   
         BCT   R0,*-12                                                          
         DC    H'0'                TABLE IS FULL                                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,1(,R6)           ELEMENT LENGTH                               
         SHI   RF,3                                                             
*MN      EX    RF,*+4                                                           
         MVC   CPOOLR-CPOOLD(0,R1),2(R6) REPORT/SIDE/CCONDITION/DATA            
         EX    RF,*-6                                                           
         CLI   3(R6),X'01'                                                      
         BNE   *+8                                                              
         OI    LPLSW,LPLRS         SET RIGHT DATA PRESENT                       
         CLI   3(R6),X'02'                                                      
         BNE   *+8                                                              
         OI    LPLSW,LPLLS         SET LEFT SIDE DATA PRESENT                   
         CLI   3(R6),X'03'                                                      
         BNE   *+8                                                              
         OI    LPLSW,LPLFL         SET FOOTLINE DATA PRESENT                    
         LA    R1,CPOOLNQ(,R1)                                                  
         MVI   0(R1),X'FF'                                                      
         B     XTRCT7                                                           
         EJECT                                                                  
***********************************************************************         
* EXTRACT - IGNORE / FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XIGNR    LA    R1,RSIGNORE         IGNORE                                       
         B     XFLTR+4                                                          
*                                                                               
XFLTR    LA    R1,RSFILTER         FILTER                                       
         LA    R0,6                                                             
         CLI   1(R6),3             MAY BE CONDITIONAL                           
         BE    XFLTR3                                                           
         CLC   3(2,R6),READLEDG    TEST LEDGER MATCH                            
         BNE   XTRCT7                                                           
XFLTR3   CLI   0(R1),0                                                          
         BE    XFLTR5                                                           
         LA    R1,1(,R1)                                                        
         BCT   R0,XFLTR3                                                        
*                                                                               
XFLTR5   MVC   0(1,R1),2(R6)                                                    
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* EXTRACT - SUNDRY                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
XCHOP    MVC   RSWLEFT+3(1),2(R6)  CHOP                                         
         B     XTRCT7                                                           
*                                                                               
XFOLD    MVC   RSFOLD,2(R6)        FOLD                                         
         B     XTRCT7                                                           
*                                                                               
XNAUT    MVI   RSAUTOCN,NO                                                      
         B     XTRCT7                                                           
*                                                                               
XNSRT    MVI   RSSRTOT,NO                                                       
         B     XTRCT7                                                           
*                                                                               
XSCAL    L     R1,AFORM                                                         
         SR    RF,RF                                                            
         IC    RF,1(,R6)                                                        
         SHI   RF,3                                                             
*MN      EX    RF,*+4                                                           
         MVC   0(0,R1),2(R6)                                                    
         EX    RF,*-6                                                           
         AR    R1,RF                                                            
         LA    R1,1(,R1)                                                        
         ST    R1,AFORM                                                         
         MVI   0(R1),X'FF'                                                      
         B     XTRCT7                                                           
*                                                                               
XAHRS    MVI   AHOURS,YES                                                       
         B     XTRCT7                                                           
*                                                                               
XWKND    OI    OPT1,MWR            MAN-WEEK-RATIOS                              
         OI    RDSW,RDMWR          READ DETAIL FOR MWR                          
         MVC   WKEND,2(R6)                                                      
         OI    OPT1,PCT            USE PERCENTAGES                              
         B     XTRCT7                                                           
*                                                                               
XBCK$    MVI   BUCKD,YES                                                        
         B     XTRCT7                                                           
*                                                                               
XKCOL    SR    R1,R1               KEYCOL, SECCOL, OVRCOL                       
         IC    R1,1(,R6)                                                        
         SHI   R1,4                                                             
         XC    M3SECCOL,M3SECCOL                                                
         XC    M3OVRCOL,M3OVRCOL                                                
         LA    RE,RSKEYCOL                                                      
         CLI   2(R6),1             KEYCOL TYPE                                  
         BE    XKCOLA                                                           
         LA    RE,M3SECCOL                                                      
         CLI   2(R6),2             SECCOL TYPE                                  
         BE    XKCOLA                                                           
         LA    RE,M3OVRCOL                                                      
         CLI   2(R6),3             OVRCOL TYPE                                  
         BE    XKCOLA                                                           
         DC    H'00'                                                            
*                                                                               
XKCOLA   DS    0H                                                               
*MN      EX    R1,*+4                                                           
         MVC   0(0,RE),3(R6)                                                    
         EX    R1,*-6                                                           
         B     XTRCT7                                                           
*                                                                               
*&&US                                                                           
XLPOLR   MVC   RSLPROW+1(1),2(R6)                                               
         B     XTRCT7                                                           
*&&                                                                             
*&&UK                                                                           
XLPOLR   B     XTRCT7              RESET OPTION                                 
*&&                                                                             
*                                                                               
XLPMOS   MVI   POSTMOS,YES         POST DETAILS BY BATCH HEADER MOA             
         B     XTRCT7                                                           
*                                                                               
XPBDT    OI    PCSW,PCBD           POST BUCKETS BY DATE                         
         B     XTRCT7                                                           
*                                                                               
XNBIN    DS    0H                  NO BINARY                                    
         B     XTRCT7                                                           
*                                                                               
XPDPT    OI    PCSW,PCDPT          POST 14 CONTRA BY DEPARTMENT                 
         B     XTRCT7                                                           
*                                                                               
XSHWZ    MVI   RSZERO,YES          SHOWZERO                                     
         B     XTRCT7                                                           
*                                                                               
XCOPS    MVC   RSCOPY,2(R6)        COPIES                                       
         B     XTRCT7                                                           
*                                                                               
XNULM    MVC   RSNULM,2(R6)        NULMIDS                                      
         B     XTRCT7                                                           
*                                                                               
XSPCG    MVC   RSSPACE,2(R6)       SPACING                                      
         MVC   RSSKIP,3(R6)        SKIP OPTION                                  
         B     XTRCT7                                                           
*                                                                               
XRUND    MVC   RSROUND,2(R6)       ROUND (POSITIONS)                            
         B     XTRCT7                                                           
*                                                                               
XENCH    SR    R1,R1                                                            
         IC    R1,2(,R6)           ENCROACH OPTION                              
         STH   R1,RSOVLY                                                        
         B     XTRCT7                                                           
*                                                                               
XRCAP    CLI   1(R6),4             CHECK LENGTH OF ELEMENT                      
         BNE   XFDWN                                                            
         MVC   RSRECAP,2(R6)                                                    
         B     XTRCT7                                                           
*                                                                               
XFDWN    MVC   RSDWNRPT,2(R6)      FORCE REPORT TO DOWNLOAD WITH                
         B     XTRCT7                                                           
*                                                                               
XSRHK    MVC   RSHKSRT,0(R6)       ID, HOOK TYPE, HOOK #-FOR SORTHOOK           
         B     XTRCT7                                                           
*                                                                               
XROWL    MVC   RLLEDG,2(R6)                                                     
         B     XTRCT7                                                           
*                                                                               
XSROT    MVC   RSHKOUT,0(R6)       ID, HOOK TYPE, HOOK #-FOR SORTHOOK           
         B     XTRCT7                                                           
*                                                                               
XPTHK    MVC   RSHKPUT,0(R6)       ID,HOOK TYPE,HOOK# FOR PUTHOOK               
         B     XTRCT7                                                           
*                                                                               
XADJH    MVI   ADJHOURS,YES        ADJUST STANDARD HOURS                        
         B     XTRCT7                                                           
         EJECT                                                                  
XOPTN    SR    R1,R1                                                            
         IC    R1,2(,R6)            OPTIONS                                     
         BCTR  R1,0                                                             
         MHI   R1,L'OPTABL                                                      
         LA    R1,OPTABL(R1)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,2(R1)          DISPLACEMENT TO RSTACK BYTE                  
         AR    RF,R2               PLUS STACK ADDRESS                           
         SR    RE,RE                                                            
         IC    RE,1(,R1)           BIT SETTING                                  
*MN      EX    RE,*+4                                                           
         OI    0(RF),0             SET OPTION FLAG                              
         EX    RE,*-6                                                           
         B     XTRCT7                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* SUBSTITUTE FROM CODES                                               *         
***********************************************************************         
         SPACE 1                                                                
SUBFRM   NTR1  ,                                                                
         LR    R2,R1               A(ROW SPEC)                                  
         LA    R3,SUBLIST                                                       
         LA    R4,LEVA                                                          
*                                                                               
SUBFRM3  CLC   0(1,R3),0(R2)                                                    
         BNE   SUBFRM5                                                          
         MVC   0(1,R2),0(R4)       MOVE IN CODE                                 
         SR    R0,R0                                                            
         IC    R0,1(,R2)                                                        
         SR    R1,R1                                                            
         IC    R1,1(,R4)                                                        
         AR    R1,R0               ADD DISPLACEMENT                             
         STC   R1,1(,R2)                                                        
         CLI   2(R2),0                                                          
         BNE   *+10                                                             
         MVC   2(1,R2),2(R4)       USE LENGTH IF NOT SPECIFIED                  
         B     XIT                                                              
*                                                                               
SUBFRM5  LA    R4,3(,R4)                                                        
         LA    R3,1(,R3)                                                        
         CLI   0(R3),EOT           LOOK FOR MATCH                               
         BNE   SUBFRM3                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK FOR PERCENTS, BUDGETS AND/OR BALANCE FORWARD                  *         
***********************************************************************         
         SPACE 1                                                                
CHKBUD   NTR1  ,                                                                
         LA    R0,4                (UP TO 4 KEYS)                               
*                                                                               
CHKBUD3  CLI   0(R3),TYPEQ         TEST TYPE EXPRESSION                         
         BNE   CHKBUD9                                                          
         CLI   1(R3),KWGRS         SEE IF PCT SPECIFIED (3-5)                   
         BL    CHKBUD5                                                          
         CLI   1(R3),KWPCT                                                      
         BH    CHKBUD5                                                          
         OI    OPT1,PCT           USE PERCENTAGES                               
         B     CHKBUD9                                                          
*                                                                               
CHKBUD5  CLI   1(R3),KWBG1         TYPE 31-39                                   
         BL    CHKBUD7                                                          
         CLI   1(R3),KWBG9                                                      
         BH    CHKBUD7                                                          
         OI    RDSW,RDBUD          READ BUDGETS                                 
         B     CHKBUD9                                                          
*                                                                               
CHKBUD7  CLI   1(R3),KWBLF         BALFWD                                       
         BL    CHKBUD9                                                          
         CLI   1(R3),KWBF0         BBF0                                         
         BH    CHKBUD9                                                          
         MVI   BBFSW,YES           SET BBF SWICTH TO GET BBF                    
*                                                                               
CHKBUD9  LA    R3,2(,R3)                                                        
         BCT   R0,CHKBUD3                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CHECK FOR VERTICAL PERCENTS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
CHKVRT   NTR1  ,                                                                
         LA    R0,4                (UP TO 4 OPERANDS)                           
         LA    R3,4(,R6)                                                        
*                                                                               
CHKVRT3  CLI   0(R3),10            VERTICAL PERCENTS ARE 10+N                   
         BL    CHKVRT5             WHERE N IS LEVEL                             
         SR    R1,R1                                                            
         IC    R1,0(,R3)                                                        
         SHI   R1,11                                                            
         LA    R1,RSVERT(R1)       INDEX INTO VERTICAL COL-LIST                 
         MVC   0(1,R1),2(R6)       PUT COLUMN NUMBER HERE                       
         B     XIT                                                              
*                                                                               
CHKVRT5  LA    R3,2(,R3)                                                        
         BCT   R0,CHKVRT3                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DEVELOP COLUMN NAME FROM KEY(S)                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
COLNAM   NTR1  ,                                                                
         CLI   RSAUTOCN,YES                                                     
         BNE   XIT                                                              
         MVC   COLNO,2(R6)                                                      
         BAS   RE,COLINF                                                        
         L     R4,COLAHEAD                                                      
         L     R5,COLWIDE                                                       
         LA    R0,3                                                             
*                                                                               
COLNAM3  OC    0(2,R3),0(R3)                                                    
         BZ    COLNAM9                                                          
         LTR   R5,R5               TEST COLUMN WIDTH (W=1)                      
         BZ    COLNAM9                                                          
         L     R1,ACOLNTAB                                                      
*                                                                               
COLNAM5  CLI   0(R1),X'FF'         LOOK UP TABLE FOR DIRECT TRANSLATE           
         BNE   COLNAM6                                                          
         BAS   RE,PERX                                                          
         B     COLNAM9                                                          
*                                                                               
COLNAM6  CLC   0(2,R3),0(R1)                                                    
         BE    COLNAM7                                                          
         LA    R1,6(,R1)                                                        
         B     COLNAM5                                                          
*                                                                               
COLNAM7  CLC   0(10,R4),SPACES     ONLY REPLACE BLANK HEADING                   
         BNE   COLNAM9                                                          
         MVC   0(4,R4),2(R1)                                                    
         STC   R5,3(,R4)           SET COLUMN WIDTH IN DICT ESCAPE SEQ.         
*                                                                               
COLNAM9  LA    R3,2(,R3)                                                        
         LA    R4,164(,R4)                                                      
         CLC   0(10,R4),SPACES                                                  
         BE    *+8                                                              
         LA    R4,164(,R4)                                                      
         LA    R1,RSHEAD3+164                                                   
         CR    R4,R1                                                            
         BNL   XIT                                                              
         BCT   R0,COLNAM3                                                       
         B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DERIVE COLUMN INFORMATION                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
COLINF   NTR1  ,                                                                
         SR    R0,R0                                                            
         IC    R0,COLNO                                                         
         LA    R3,RSCOLS                                                        
         LA    R4,1                                                             
         LA    R5,RSHEAD1                                                       
*                                                                               
COLINF2  ST    R5,COLAHEAD                                                      
         SR    R1,R1                                                            
         ICM   R1,1,8(R3)                                                       
         BNZ   *+8                                                              
         LA    R1,8                                                             
         CLI   8(R3),1                                                          
         BNE   COLINF3                                                          
         LA    R1,0                                                             
*                                                                               
COLINF3  ST    R1,COLWIDE                                                       
         CR    R0,R4                                                            
         BE    XIT                                                              
         LA    R3,29(,R3)                                                       
         LA    R4,1(,R4)                                                        
         LTR   R1,R1                                                            
         BZ    *+8                                                              
         LA    R5,1(R1,R5)                                                      
         CLI   RSFOLD,0            IF FOLD IS INVOLVED                          
         BE    COLINF2                                                          
         LR    RF,R4                                                            
         SR    RE,RE                                                            
         SR    R1,R1                                                            
         IC    R1,RSFOLD                                                        
         DR    RE,R1               DIVIDE PRESENT COL BY FOLD                   
         CHI   RE,1                IF WE HAVE REACHED START OF NEW FOLD         
         BNE   COLINF2                                                          
         MHI   RF,164                                                           
         LA    R5,RSHEAD1(RF)      POSITION TO NEW HEADLINE                     
         B     COLINF2                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* HANDLE PERIOD EXPRESSIONS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTACKD,R2                                                       
PERX     NTR1  ,                                                                
         LA    R5,NEXTPER                                                       
         CLI   0(R3),11                                                         
         BE    PERX3                                                            
         LA    R5,NEXTYTD                                                       
         CLI   0(R3),12                                                         
         BE    PERX3                                                            
         LA    R5,NEXTROLL                                                      
         CLI   0(R3),13                                                         
         BE    PERX3                                                            
         LA    R5,NEXTFISC                                                      
         CLI   0(R3),14                                                         
         BE    PERX3                                                            
*                                                                               
         L     R5,COLWIDE                                                       
         CLI   0(R3),8                                                          
         BH    PERX7                                                            
         SR    R1,R1                                                            
         IC    R1,0(,R3)           POSITION R5 TO RIGHT YEAR BLOCK              
         BCTR  R1,0                                                             
         MHI   R1,40                                                            
         LA    R5,PERIOD(R1)                                                    
*                                                                               
PERX3    SR    R1,R1                                                            
         IC    R1,1(,R3)           THEN TO START/END MONTH PAIR                 
         SLL   R1,1                                                             
         AR    R5,R1                                                            
         CLI   0(R5),0             (OUT OF PERIOD)                              
         BE    XIT                                                              
         SR    R1,R1                                                            
         IC    R1,0(,R5)           GET START MONTH                              
         BCTR  R1,0                                                             
         MHI   R1,6                                                             
         LA    R1,MONALIST(R1)     DIG OUT MMM/YY                               
         MVC   WORK,SPACES                                                      
         MVC   WORK+1(6),0(R1)                                                  
         CLC   0(1,R5),1(R5)       MONTHS HAVE SAME START/END                   
         BE    PERX5                                                            
         MVC   WORK+1(3),0(R1)     OTHERWISE SHOW MMM-MMM                       
         MVI   WORK+4,C'-'                                                      
         SR    R1,R1                                                            
         IC    R1,1(,R5)           GET END MONTH                                
         BCTR  R1,0                                                             
         MHI   R1,6                                                             
         LA    R1,MONALIST(R1)                                                  
         MVC   WORK+5(3),0(R1)                                                  
*                                                                               
PERX5    L     R5,COLWIDE                                                       
         GOTO1 CENTER,DMCB,WORK,(R5)                                            
         BCTR  R5,0                                                             
*MN      EX    R5,*+4                                                           
         MVC   0(0,R4),WORK                                                     
         EX    R5,*-6                                                           
         B     XIT                                                              
*                                                                               
PERX7    CLI   0(R3),9             HANDLE BUDGET HEADINGS                       
         BNE   XIT                                                              
         CLI   1(R3),31            TYPE 9 (31-39)                               
         BL    XIT                                                              
         CLI   1(R3),41                                                         
         BH    XIT                                                              
         CLI   COLWIDE+3,10        IGNORE IF WIDTH NOT GT 10                    
         BNE   XIT                                                              
         SR    R1,R1                                                            
         IC    R1,1(,R3)           KEY INTO BUDGET TITLES                       
         SHI   R1,31                                                            
         MHI   R1,20                                                            
         L     RF,ABUDGTIT                                                      
         LA    R1,0(RF,R1)                                                      
         MVC   WORK(20),0(R1)                                                   
         GOTO1 ADSQUASH,DMCB,WORK,(R5)                                          
         GOTO1 CENTER,DMCB,WORK,(R5)                                            
         GOTO1 ADSQUASH,DMCB,WORK+10,(R5)                                       
         GOTO1 CENTER,DMCB,WORK+10,(R5)                                         
         BCTR  R5,0                                                             
         EX    R5,PERX9                                                         
         LA    R1,RSHEAD3                                                       
         CR    R4,R1               PROTECT AGAINST PRINTING                     
         BNL   *+8                 BELOW THIRD HEAD LINE                        
         EX    R5,PERX11                                                        
         B     XIT                                                              
*                                                                               
PERX9    MVC   0(0,R4),WORK                                                     
PERX11   MVC   164(0,R4),WORK+10                                                
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LIST OF MONTHS                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R6                                                        
BLDM     NTR1  ,                                                                
         MVC   WORK(2),QSTART      SETUP FOR FISCAL YEAR                        
         MVC   WORK+2(4),=C'0101'  ASSUME JANUARY                               
         L     R6,ADCMPEL                                                       
         LA    R1,SBLIST           GET START OF FISCAL YEAR                     
         LA    R0,12                                                            
         LA    RF,CPYSFST                                                       
         CLI   OVFISCAL,0          TEST FISCAL OVERRIDE                         
         BE    *+8                                                              
         LA    RF,OVFISCAL                                                      
*                                                                               
BLDM3    CLC   0(1,R1),0(RF)       CONVERT  1-9  A-C   TO 01-12                 
         BE    BLDM5                                                            
         LA    R1,3(,R1)                                                        
         BCT   R0,BLDM3                                                         
         B     BLDM7               END OF TABLE - LEAVE IT AS JANUARY           
         DROP  R6                                                               
*                                                                               
BLDM5    MVC   WORK+2(2),1(R1)     SET FISCAL MONTH                             
*                                                                               
BLDM7    GOTO1 DATCON,DMCB,(0,WORK),(0,CFSDTE)      FISCAL START                
         CLC   CEDTE(4),CFSDTE     END DATE BEFORE FISCAL START DATE ?          
         BNL   BLDM8                                                            
         GOTO1 ADDAY,DMCB,(C'Y',CFSDTE),CFSDTE,F'-1'                            
*                                                                               
BLDM8    GOTO1 DATCON,DMCB,(0,CFSDTE),(1,PFSDTE)     (PACKED)                   
         GOTO1 ADDAY,DMCB,(C'M',WORK),(X'80',CFEDTE),F'11'                      
         GOTO1 DATCON,DMCB,(0,CFEDTE),(1,PFEDTE)     (PACKED)                   
*                                                                               
         LA    R4,CFSDTE           FISCAL START                                 
         L     R5,=F'-12'          LAST YEAR                                    
*        CLC   PEDTE(2),PFEDTE     TEST REQUEST END > FISCAL END                
*        BNH   *+12                                                             
*        LA    R4,CEDTE            REQUEST END                                  
*        L     R5,=F'-23'          MINUS 23                                     
         TM    OPT1,ROLL           ROLLING COLUMNS                              
         BNO   *+12                                                             
         LA    R4,CEDTE            REQUEST END                                  
         L     R5,=F'-23'          MINUS 23                                     
*                                                                               
         GOTO1 ADDAY,DMCB,(C'M',(R4)),WORK+6,(R5)                               
         LA    R2,MONLIST          POSITION TO START OF MONTH LIST              
         LA    R4,MONALIST                                                      
         LA    R3,NAC              NUMBER OF MONTHS                             
*                                                                               
BLDM9    GOTO1 DATCON,DMCB,(0,WORK+6),(0,0(R2))    EBCDIC YYMM                  
         GOTO1 DATCON,DMCB,(0,WORK+6),(1,WORK+20)  PACKED YYMM                  
         MVC   4(2,R2),WORK+20                                                  
         GOTO1 DATCON,DMCB,(0,WORK+6),(9,0(R4))    MMM/YY                       
         MVC   WORK(6),WORK+6                                                   
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+6,F'1'                               
         LA    R2,6(,R2)                                                        
         LA    R4,6(,R4)                                                        
         BCT   R3,BLDM9                                                         
*                                                                               
         MVC   PEREXP,SPACES                                                    
         MVC   PEREXP(6),=C'PERIOD'   BUILD THE PERIOD EXPRESSIION              
         MVC   WORK(12),QSTART                                                  
         LA    R0,8                DATE FORMAT MMMDD/YY                         
         LA    R2,PEREXP+7                                                      
*&&US*&& LA    R3,8                LENGTH                                       
*&&UK*&& LA    R3,7                DDMMMYY                                      
         CLI   QSTART+4,C' '       TEST DAY INPUT                               
         BNE   BLDM11                                                           
         CLI   QEND+4,C' '                                                      
         BNE   BLDM11                                                           
         LA    R0,9                DATE FORMAY MMM/YY                           
         LA    R2,PEREXP+8                                                      
*&&US*&& LA    R3,6                LENGTH                                       
*&&UK*&& LA    R3,5                MMMYY                                        
         MVC   WORK+4(2),=C'01'                                                 
         MVC   WORK+10(2),=C'01'                                                
*                                                                               
BLDM11   GOTO1 DATCON,DMCB,(0,WORK),((R0),0(R2))                                
         CLC   QSTART,QEND                                                      
         BE    XIT                                                              
         LA    R2,1(R3,R2)                                                      
         MVI   0(R2),C'-'                                                       
         LA    R2,2(,R2)                                                        
         GOTO1 DATCON,DMCB,(0,WORK+6),((R0),0(R2))                              
         B     XIT                                                              
         TITLE 'INITIALIZATION - SET UP DATE BASE'                              
***********************************************************************         
* SET UP DATE BASE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DBASE    NTR1  ,                                                                
         MVC   WORK(2),PSDTE       PERIOD                                       
         BAS   RE,LKMON                                                         
         MVC   PERIOD(1),WORK      REQUEST START                                
         MVC   WORK(2),PEDTE                                                    
         BAS   RE,LKMON            TO REQUEST END                               
         MVC   PERIOD+1(1),WORK                                                 
         LA    R2,PERIOD                                                        
         LA    R1,LASTPER                                                       
         LA    R3,NEXTPER                                                       
         BAS   RE,DXPND                                                         
*                                                                               
         MVC   WORK(2),PFSDTE      YTD                                          
         BAS   RE,LKMON                                                         
         MVC   YTD(1),WORK         FISCAL START                                 
         MVC   WORK(2),PEDTE                                                    
         BAS   RE,LKMON            TO REQUEST END                               
         MVC   YTD+1(1),WORK                                                    
         LA    R2,YTD                                                           
         LA    R1,LASTYTD                                                       
         LA    R3,NEXTYTD                                                       
         BAS   RE,DXPND                                                         
*                                                                               
         MVC   WORK(2),PEDTE       ROLLING                                      
         BAS   RE,LKMON                                                         
         MVC   ROLLING+1(1),WORK   TO REQUEST END                               
         SR    RF,RF                                                            
         IC    RF,ROLLING+1                                                     
         SHI   RF,11               FOR PRIOR 12 MONTHS                          
         STC   RF,ROLLING                                                       
         LA    R2,ROLLING                                                       
         LA    R1,LASTROLL                                                      
         LA    R3,NEXTROLL                                                      
         BAS   RE,DXPND                                                         
*                                                                               
         MVC   WORK(2),PFSDTE      FISCAL                                       
         BAS   RE,LKMON                                                         
         MVC   FISCAL(1),WORK      FISCAL START                                 
         SR    RF,RF                                                            
         IC    RF,FISCAL                                                        
         AHI   RF,11                                                            
         STC   RF,FISCAL+1         FOR 12 MONTHS                                
         LA    R2,FISCAL                                                        
         LA    R1,LASTFISC                                                      
         LA    R3,NEXTFISC                                                      
         BAS   RE,DXPND                                                         
*                                                                               
         LA    R3,LASTYTD                                                       
         SR    R1,R1                                                            
         IC    R1,1(,R3)           GET END MONTH                                
         SR    RF,RF                                                            
         IC    RF,0(,R3)           GET START MONTH                              
         SR    R1,RF               GET NUMBER OF MONTHS IN-BETWEEN              
         LA    R1,1(,R1)           ADD ONE                                      
         STC   R1,CMONLYR          SAVE CURRENT MONTH LAST YEAR                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* EXPAND START-END FOR MONTH/QUARTER/SIX PHASES                       *         
* NTRY R1 = A(PRIOR YEAR MONTHS LIST)                                 *         
*      R2 = A(CURRENT PERIOD LIST)                                    *         
*      R3 = A(NEXT PERIOD LIST)                                       *         
***********************************************************************         
         SPACE 1                                                                
DXPND    NTR1  ,                                                                
         BAS   RE,DXPND5           FIRST HANDLE FOR THIS YEAR                   
         SR    RF,RF               NOW SET FOR PRIOR YEAR START                 
         IC    RF,0(,R2)                                                        
         SHI   RF,12               BACKUP 12 MONTHS                             
         BP    *+8                                                              
         LA    RF,1                CAN'T BE LESS THAN 1                         
         STC   RF,0(,R1)                                                        
         IC    RF,1(,R2)           SET PRIOR YEAR END                           
         SHI   RF,12               BACKUP 12 MONTHS                             
         BP    *+8                                                              
         LA    RF,1                CAN'T BE LESS THAN 1                         
         STC   RF,1(,R1)                                                        
         ST    R2,SVR2                                                          
         LR    R2,R1                                                            
         BAS   RE,DXPND5           NOW HANDLE PRIOR YEAR                        
         L     R2,SVR2                                                          
*                                                                               
         SR    RF,RF               SET FOR NEXT YEAR START                      
         IC    RF,0(,R2)                                                        
         AHI   RF,12               FORWARD 12 MONTHS                            
         STC   RF,0(,R3)                                                        
         CLI   0(R3),NAC           TEST PAST END                                
         BNH   *+8                                                              
         MVI   0(R3),NAC                                                        
         IC    RF,1(,R2)           SET NEXT YEAR END                            
         AHI   RF,12               FORWARD 12 MONTHS                            
         STC   RF,1(,R3)                                                        
         CLI   1(R3),NAC           TEST PAST END                                
         BNH   *+8                                                              
         MVI   1(R3),NAC                                                        
         ST    R2,SVR2                                                          
         LR    R2,R3                                                            
         BAS   RE,DXPND5           BUILD LIST FOR NEXT YEAR                     
         B     XIT                                                              
*                                                                               
DXPND5   NTR1  ,                                                                
         LA    R3,2(,R2)                                                        
         LA    R4,SBRELS                                                        
         LA    R5,18                                                            
         XC    0(38,R3),0(R3)                                                   
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVC   BYTE,1(R2)          SAVE END NUMBER                              
*                                                                               
DXPND7   IC    R1,0(,R2)           START MON NO                                 
         IC    R0,0(,R4)                 =START RELATIVE                        
         AR    R1,R0                                                            
         CLM   R1,1,BYTE           TEST PAST END                                
         BH    *+8                                                              
         STC   R1,0(,R3)                                                        
         IC    R1,0(,R2)           START MON NO                                 
         IC    R0,1(,R4)                 =END RELATIVE                          
         AR    R1,R0                                                            
         CLM   R1,1,BYTE                                                        
         BH    *+8                                                              
         STC   R1,1(,R3)                                                        
         CLI   1(R3),0             IF END IS NOT IN PERIOD                      
         BNE   DXPND9                                                           
         CLI   0(R3),0             BUT START IS                                 
         BE    DXPND9                                                           
         MVC   1(1,R3),1(R2)       MAKE END THE END OF PERIOD                   
*                                                                               
DXPND9   LA    R3,2(,R3)                                                        
         LA    R4,2(,R4)                                                        
         BCT   R5,DXPND7                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO LOOK UP MONTH                                            *         
***********************************************************************         
         SPACE 1                                                                
LKMON    NTR1  ,                                                                
         LA    R2,MONLIST                                                       
         LA    R3,1                                                             
         LA    R4,NAC                                                           
*                                                                               
LKMON3   CLC   0(4,R2),WORK        WORK CONTAINS YYMM                           
         BE    LKMON5                                                           
         CLC   4(2,R2),WORK                   OR YM (PACKED)                    
         BE    LKMON5                                                           
         LA    R2,6(,R2)                                                        
         LA    R3,1(,R3)                                                        
         BCT   R4,LKMON3                                                        
         DC    H'0'                MONTH PROBLEM                                
*                                                                               
LKMON5   STC   R3,WORK                                                          
         B     XIT                                                              
         TITLE 'INITIALIZATION - COMPLETE STACK LINES'                          
***********************************************************************         
* COMPLETE STACK LINES                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING BIGPRNTD,R6                                                      
         USING RSTACKD,R2                                                       
STKUP    NTR1  ,                                                                
         L     R3,AREPCOPY                                                      
         MVI   0(R3),1                                                          
         MVC   1(RPTMX-1,R3),0(R3)                                              
         TM    DWNSTAT,DWNLOAD     IF DOWN-LOADING DON'T TIDY                   
         BNZ   XIT                                                              
*                                                                               
         L     R6,VBIGPRNT                                                      
         L     R2,ARSTACK                                                       
         L     R0,NRSTACK                                                       
         LA    R1,1                                                             
         STC   R1,SVRP             SAVE REPORT NUMBER                           
*                                                                               
STKUP3   STM   RE,R6,SVRE                                                       
         MVC   0(1,R3),RSCOPY                                                   
         SR    R1,R1               FIGURE OUT EFFECTIVE WIDTH                   
         L     R0,RSNCOLS          OF N COLUMNS                                 
         CLI   RSFOLD,0                                                         
         BE    *+8                                                              
         IC    R0,RSFOLD           OR N FOLD                                    
         LA    R3,RSCOLS                                                        
*                                                                               
STKUP5   SR    R4,R4                                                            
         ICM   R4,1,8(R3)          WIDTH OF THIS COLUMN                         
         BNZ   *+8                                                              
         LA    R4,8                                                             
         CLI   8(R3),1                                                          
         BNE   *+10                                                             
         SR    R4,R4                                                            
         B     *+8                                                              
         LA    R1,1(R1,R4)         PLUS 1                                       
         LA    R3,29(,R3)                                                       
         BCT   R0,STKUP5                                                        
*                                                                               
         OC    RSWLEFT,RSWLEFT     CHOP KEYWORD,WIDTH OF FIRST COLUMN           
         BNZ   *+8                                                              
         MVI   RSWLEFT+3,40        CHOP 40 IS THE DEFAULT                       
         A     R1,RSWLEFT          PLUS WIDTH OF LEFT                           
         SR    R0,R0                                                            
         IC    R0,SYSWIDTH         MAKE SURE IT FITS                            
         SR    R0,R1                                                            
         BNM   STKUP7                                                           
         SR    R0,R0                                                            
         IC    R0,SVRP             MAKE R0 REPORT NUMBER                        
         LR    RF,R1               RF WILL HAVE THE WIDTH                       
         DC    H'0'                                                             
*                                                                               
STKUP7   SRL   R0,1                DISPLACEMENT IS 1/2 DIFF                     
         LTR   R0,R0                                                            
         BNZ   *+8                                                              
         LA    R0,1                AND MUST BE AT LEAST 1                       
         ST    R0,RSDISP                                                        
         LA    R3,RSHEAD1          ALIGN THE 3 HEADLINES                        
         LA    R0,3                                                             
         L     R4,RSDISP                                                        
         A     R4,RSWLEFT          R4=A(DISPLACED HEADING)                      
         LA    R1,164                                                           
         SR    R1,R4                                                            
         SHI   R1,2                R1=WIDTH-1                                   
         LA    R4,1(R3,R4)                                                      
*                                                                               
STKUP9   MVC   XPFOURTH,0(R3)                                                   
         MVC   0(164,R3),XSPACES                                                
*MN      EX    R1,*+4                                                           
         MVC   0(0,R4),XPFOURTH                                                 
         EX    R1,*-6                                                           
         MVC   XPFOURTH,XSPACES                                                 
         LA    R3,164(R3)                                                       
         LA    R4,164(R4)                                                       
         BCT   R0,STKUP9                                                        
*                                                                               
         LA    R3,RSBOXES-1        NOW SET UP BOXES                             
         A     R3,RSDISP                                                        
         MVI   0(R3),C'L'          LEFT                                         
         A     R3,RSWLEFT                                                       
         LA    R3,1(,R3)                                                        
         L     R4,RSNCOLS                                                       
         CLI   RSFOLD,0                                                         
         BE    *+8                                                              
         IC    R4,RSFOLD                                                        
         LA    R5,RSCHUNK                                                       
         LA    R6,RSCOLS                                                        
*                                                                               
STKUP11  CLI   0(R5),NO            UNLESS VERTICAL LINES HAVE BEEN              
         BE    *+8                 SUPPRESSED AS PART OF CHUNK                  
         MVI   0(R3),C'C'          CENTER BEFORE EACH COLUMN                    
         LA    R5,1(,R5)                                                        
         SR    R1,R1                                                            
         ICM   R1,1,8(R6)                                                       
         BNZ   *+8                                                              
         LA    R1,8                                                             
         CLI   8(R6),1                                                          
         BE    STKUP13                                                          
         LA    R3,1(R1,R3)                                                      
*                                                                               
STKUP13  LA    R6,29(,R6)                                                       
         BCT   R4,STKUP11                                                       
         MVI   0(R3),C'R'          RIGHT AFTER LAST                             
*                                                                               
STKUPX   LM    RE,R6,SVRE                                                       
         SR    R1,R1                                                            
         IC    R1,SVRP                                                          
         LA    R1,1(,R1)                                                        
         STC   R1,SVRP                                                          
         A     R2,WRSTACK                                                       
         LA    R3,1(,R3)           REPCOPY                                      
         BCT   R0,STKUP3                                                        
         B     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE SETS UP NUMBER OF DAYS IN EACH MONTH                        *         
***********************************************************************         
         SPACE 1                                                                
SETND    NTR1  ,                                                                
         BAS   RE,MNWK             ESTABLISH MANWEEK CALENDAR                   
         LA    R4,WEEKTAB          TABLE OF ACCUMS LINES                        
         LA    R5,3                                                             
         ICM   R2,15,0(R4)                                                      
         BAS   RE,SETCLR           CLEAR BCW/SWH/HOL                            
         LA    R4,4(,R4)                                                        
         BCT   R5,*-12                                                          
*                                                                               
         LM    R3,R5,WEEKTAB                                                    
         A     R3,AZRO             R3=BROADCAST WEEKS                           
         A     R4,AZRO             R4=STANDARD WORKING HOURS                    
         A     R5,AZRO             R5=HOLIDAYS                                  
         BAS   RE,STDWK            SET HOURS FOR STANDARD WEEK                  
         LA    R2,MONLIST                                                       
         LA    R0,NAC                                                           
*                                                                               
SETND2   BAS   RE,SETHOL                                                        
         CP    0(8,R4),=P'0'       TEST STD HOURS SET FROM RECORD               
         BNE   *+8                 DON'T USE TABLE ROUTINE                      
         BAS   RE,SETSWH           STANDARD WORKING HOURS                       
         BAS   RE,SETBCW           BROADCAST WEEKS                              
         LA    R2,6(,R2)                                                        
         LA    R3,8(,R3)                                                        
         LA    R4,8(,R4)                                                        
         LA    R5,8(,R5)                                                        
         BCT   R0,SETND2                                                        
*                                                                               
         LHI   RE,(LNSWH*WLN)      MOVE LINE 37 (STANDARD WRK HOURS) TO         
         A     RE,AZRO                                                          
         LA    RF,WLN              WIDTH OF LINE                                
         LHI   R2,(LNC30*WLN)      LINE 30                                      
         A     R2,AZRO                                                          
         LR    R3,RF                                                            
         MVCL  R2,RE                                                            
         BAS   RE,SETACT           SET LINE ACTIVE STATUS                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUILD CALENDAR TABLE FOR MANWEEKS WORKED                            *         
***********************************************************************         
         SPACE 1                                                                
         USING MNWKD,R5                                                         
MNWK     NTR1  ,                                                                
         L     R5,AMANWK                                                        
         MVI   0(R5),X'FF'                                                      
         BAS   RE,CLNDR            FIRST LOOK FOR CALENDAR RECORDS              
         CLI   0(R5),X'FF'         TEST TABLE BUILT FROM CALENDAR               
         BNE   MNWK23                                                           
         LHI   R3,(52*3)           DEFAULT NUMBER OF WEEKS                      
         STH   R3,MNWKS                                                         
         BCTR  R3,0                                                             
         MHI   R3,MNWKLNQ                                                       
         AR    R5,R3               R5 TO LAST ENTRY IN TABLE                    
         LH    R0,MNWKS            LOOP FOR 156 WEEKS                           
         MVC   WORK(3),MONALAST    US IS MMM/YY TO MMM/DD/YY                    
         MVI   WORK+3,C'/'                                                      
         MVC   WORK+4(2),=C'31'                                                 
         LA    R6,WORK+4                                                        
         MVI   WORK+6,C'/'                                                      
         MVC   WORK+7(2),MONALAST+4                                             
         CLI   MONALAST+3,C'/'     TEST US                                      
         BE    MNWK03                                                           
*                                                                               
         MVC   WORK(10),SPACES                                                  
         MVC   WORK(2),MONALAST    GER IS MM.YY TO MM/DD/YY                     
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),=C'31'                                                 
         LA    R6,WORK+3                                                        
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),MONALAST+3                                             
         CLI   MONALAST+2,C'.'     TEST GER                                     
         BE    MNWK03                                                           
*                                                                               
         MVC   WORK(10),SPACES                                                  
         MVC   WORK(2),=C'31'      UK IS MMMYY TO DDMMMYY                       
         LA    R6,WORK                                                          
         MVC   WORK+2(3),MONALAST                                               
         MVC   WORK+5(2),MONALAST+3                                             
*                                                                               
MNWK03   GOTO1 DATVAL,DMCB,(0,WORK),WORK+9                                      
         OC    0(4,R1),0(R1)       TEST VALID DATE                              
         BNZ   MNWK07                                                           
*                                                                               
MNWK05   PACK  HALF,0(2,R6)        GET LAST DAY OF MONTH                        
         SP    HALF,=P'1'          SUBTRACT 1 FROM DAY                          
         OI    HALF+1,X'0F'        AND GET NEXT                                 
         UNPK  0(2,R6),HALF(2)                                                  
         B     MNWK03                                                           
*                                                                               
MNWK07   GOTO1 GETDAY,DMCB,WORK+9,WORK+15                                       
*&&US                                                                           
         CLC   WORK+15(3),WKEND    MATCH DAY                                    
         BNE   MNWK05                                                           
*&&                                                                             
*&&UK                                                                           
         CLI   0(R1),6             UK IS SATURDAY                               
         BNE   MNWK05                                                           
*&&                                                                             
*                                                                               
MNWK09   GOTO1 DATCON,DMCB,(0,WORK+9),(1,MNWKEND) GET END AND START             
         GOTO1 ADDAY,DMCB,WORK+9,WORK,F'-6'                                     
         GOTO1 DATCON,DMCB,(0,WORK),(1,MNWKSTR)                                 
         GOTO1 ADDAY,DMCB,WORK,WORK+9,F'-1'                                     
         SHI   R5,MNWKLNQ                                                       
         BCT   R0,MNWK09                                                        
*                                                                               
         LH    R0,MNWKS            ADD MOA TO TABLE ENTRY                       
         L     R5,AMANWK                                                        
MNWK11   MVC   MNWKMOA,MNWKEND     DEFAULT MOA IS END                           
         LA    R5,MNWKLNQ(,R5)                                                  
         BCT   R0,MNWK11                                                        
*                                                                               
MNWK23   LH    R0,MNWKS            ADD RELATIVE MONTH NUMBER                    
         L     R5,AMANWK                                                        
         LA    R3,MONLIST+4        MONTH LIST - PACKED DATES                    
         SR    RF,RF                                                            
*                                                                               
MNWK25   CLC   MNWKMOA,0(R3)       MATCH MOA                                    
         BNH   *+16                                                             
         LA    R3,6(,R3)           NEXT MOA ENTRY                               
         LA    RF,1(,RF)           INCREMENT RELATIVE NUMBER                    
         B     MNWK25                                                           
         STC   RF,MNWKNUM          RELATIVE MONTH NUMBER                        
         LA    R5,MNWKLNQ(,R5)                                                  
         BCT   R0,MNWK25                                                        
         MVI   0(R5),X'FF'         EOT                                          
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* READ CALENDAR RECORD - BUILD WEEK ENDING TABLE                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CASRECD,R6                                                       
CLNDR    NTR1  ,                                                                
         LA    R6,DKEY                                                          
         XC    CASKEY,CASKEY                                                    
         MVI   CASKTYP,CASKTYPQ    X'3E'                                        
         MVI   CASKSUB,CASKSUBQ    X'0B'                                        
         MVC   CASKCPY,RCCOMPFL    COMPANY                                      
         GOTO1 ADMGR,HIGH                                                       
         CLC   DKEY(CASKEMOA-CASKEY),DIR                                        
         BNE   XIT                                                              
*                                                                               
         USING MNWKD,R5                                                         
         L     R5,AMANWK           R6 = A(CALENDAR TABLE)                       
         SR    R3,R3               R3 = NUMBER OF TABLE ENTRIES                 
*                                                                               
CLNDR3   L     R6,AIO1                                                          
         GOTO1 ADMGR,GETR          GET THE CALENDAR RECORD                      
         CLI   CASKOFC,C' '                                                     
         BH    CLNDR15             SKIP OFFICE RECORDS                          
         LA    R4,CASRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING TMPELD,R4                                                        
CLNDR5   CLI   0(R4),TMPELQ        X'88' ELEMENT                                
         BE    CLNDR9                                                           
         CLI   0(R4),0                                                          
         BE    CLNDR15             EOR                                          
CLNDR7   IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     CLNDR5                                                           
*                                                                               
CLNDR9   CLC   TMPMTH,MONLIST+4    TEST BEFORE START                            
         BL    CLNDR7                                                           
         CLC   TMPMTH,MONLAST+4    OR AFTER END                                 
         BH    CLNDR7                                                           
         MVC   MNWKMOA,TMPMTH      MONTH                                        
         MVC   MNWKSTR,TMPSTART    PERIOD START DATE                            
         MVC   MNWKEND,TMPEND      PERIOD END DATE                              
         LA    R5,MNWKLNQ(,R5)     BUMP TO NEXT AVAILABLE ENTRY                 
         LA    R3,1(,R3)           BUMP COUNT                                   
         STH   R3,MNWKS                                                         
         CHI   R3,MWTMX                                                         
         BL    CLNDR7                                                           
         DC    H'0'                CALENDAR TABLE FULL                          
*                                                                               
CLNDR15  GOTO1 ADMGR,RSEQ          GET NEXT CALENDAR RECORD                     
         CLC   DKEY(CASKEMOA-CASKEY),DIR                                        
         BE    CLNDR3                                                           
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
***********************************************************************         
* READ STANDARD HOURS RECORD - ADD HOURS TO TABLE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING STDRECD,R6                                                       
STDWK    NTR1  ,                                                                
         LA    R6,DKEY                                                          
         XC    STDKEY,STDKEY                                                    
         MVI   STDKTYP,STDKTYPQ    X'3E'                                        
         MVI   STDKSUB,STDKSUBQ    X'0D'                                        
         MVC   STDKCPY,RCCOMPFL    COMPANY                                      
         GOTO1 ADMGR,HIGH                                                       
         CLC   DKEY(STDKOFC-STDKEY),DIR                                         
         BNE   XIT                                                              
*                                                                               
STDWK3   L     R6,AIO1                                                          
         GOTO1 ADMGR,GETR          GET THE CALENDAR RECORD                      
         CLI   STDKOFC,C' '                                                     
         BH    STDWK15             SKIP OFFICE RECORDS                          
         LA    R4,STDRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING SHRELD,R4                                                        
STDWK5   CLI   0(R4),SHRELQ        X'89' ELEMENT                                
         BE    STDWK9                                                           
         CLI   0(R4),0                                                          
         BE    STDWK15             EOR                                          
STDWK7   IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     STDWK5                                                           
*                                                                               
         USING MNWKD,R5                                                         
STDWK9   L     R5,AMANWK           R6 = A(CALENDAR TABLE)                       
         LH    R3,MNWKS            R3 = NUMBER OF ENTRIES                       
STDWK11  CLC   SHRSTART,MNWKEND                                                 
         BH    STDWK13                                                          
         CLC   SHREND,MNWKSTR                                                   
         BL    STDWK13                                                          
         ZAP   MNWKSTD,SHRHOURS                                                 
         LHI   RE,(LNSWH*WLN)      RE=STANDARD WORKING HOURS                    
         A     RE,AZRO                                                          
         SR    RF,RF                                                            
         IC    RF,MNWKNUM          MONTH(BUCKET) NUMBER                         
         SLL   RF,3                                                             
         LA    RE,0(RF,RE)                                                      
         AP    0(8,RE),SHRHOURS    ADD STANDARD FOR THIS PERIOD                 
*                                                                               
STDWK13  LA    R5,MNWKLNQ(,R5)     BUMP TO NEXT AVAILABLE ENTRY                 
         BCT   R3,STDWK11                                                       
         B     STDWK7                                                           
*                                                                               
STDWK15  GOTO1 ADMGR,RSEQ          GET NEXT CALENDAR RECORD                     
         CLC   DKEY(STDKOFC-STDKEY),DIR                                         
         BE    STDWK3                                                           
         B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* SET DEFAULT HOLIDAYS                                                *         
*  R5=HOLIDAY ACCUMULATOR LINE                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING HOLTABD,R3                                                       
SETHOL   NTR1  ,                                                                
         LA    R3,HOLLIST                                                       
*                                                                               
SETHOL01 CLC   HOLALPHA,SPACES     NO MATCH ON ALPHA ID?                        
         BE    SETHOL03            TRUE NO MATCH, SO DEFAULT                    
         CLC   HOLALPHA,ALPHAID    MATCH COMPANY ALPHA CODE?                    
         BNE   SETHOL04            NO SO LOOP                                   
*                                                                               
SETHOL03 CLC   HOLYEAR,SPACES      NO MATCH ON YEAR?                            
         BE    SETHOL05            LAST ENTRY, SO DEFAULT                       
         CLC   HOLYEAR,0(R2)       YEAR MATCH                                   
         BE    SETHOL05            YES SO GOT ENTRY                             
*                                                                               
SETHOL04 LA    R3,HOLLNQ(,R3)      BUMP TO NEXT ENTRY                           
         B     SETHOL01            LOOP                                         
*                                                                               
SETHOL05 ZAP   BASRATE,HOLRATE     SAVE RATE FOR FORMULA 3                      
         PACK  DUB,2(2,R2)         GET MONTH FROM DATE LIST                     
         CVB   R1,DUB              CONVERT IT TO BINARY                         
         LA    R1,HOLMONTH-1(R1)   POINT TO MONTH IN TABLE                      
         SR    R0,R0                                                            
         IC    R0,0(,R1)           GET NUMBER OF HOLIDAYS FOR MONTH             
         CVD   R0,DUB              CONVERT TO DECIMAL                           
         ZAP   0(8,R5),DUB         AND SAVE                                     
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SET STANDARD WORKING HOURS                                          *         
*  R4=SWH ACCUMULATOR LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
SETSWH   NTR1  ,                                                                
         MVC   WORK(4),0(R2)       CALENDAR MONTHS                              
         MVC   WORK+4(2),=C'01'                                                 
         B     SETSWH4                                                          
*                                                                               
SETSWH2  GOTO1 ADDAY,DMCB,WORK,WORK+6,1                                         
         CLC   WORK(4),WORK+6                                                   
         BNE   SETSWH8                                                          
         MVC   WORK(6),WORK+6                                                   
*                                                                               
SETSWH4  GOTO1 GETDAY,DMCB,WORK,DUB                                             
         CLI   DUB,C'S'            DONT COUNT SAT/SUN                           
         BE    SETSWH2                                                          
         CLI   FORMNUM,3                                                        
         BNE   *+14                                                             
         AP    0(8,R4),BASRATE                                                  
         B     SETSWH2                                                          
*                                                                               
         CLI   FORMNUM,X'2'        IS IT FORMULA 2                              
         BNE   SETSWH6             NO -USE DEFAULT                              
         AP    0(8,R4),=P'613'     USE 87.58% OF 7 HOURS OR 6.13                
         B     SETSWH2                                                          
*                                                                               
SETSWH6  AP    0(8,R4),=P'632'     90% OF 7 HOURS OR 6.32 (DEFAULT)             
         B     SETSWH2                                                          
*                                                                               
SETSWH8  CLI   FORMNUM,X'2'        IS IT FORMULA 2                              
         BE    XIT                 YES - DON'T SUBTRACT HOLIDAYS                
         ZAP   DUB,0(8,R5)         NUMBER OF HOLIDAYS INTO DUB                  
         CLI   FORMNUM,3                                                        
         BNE   *+14                                                             
         MP    DUB,BASRATE                                                      
         B     *+10                                                             
         MP    DUB,=P'632'         DUB NOW HAS HOLIDAY HOURS                    
         SP    0(8,R4),DUB         LINE 37 HAS WORKDAYS(HOURS) W/O HOLS         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SETS NUMBER OF WEEKS IN BROADCAST MONTHS                            *         
*  R3=BCW ACCUMULATOR LINE                                            *         
***********************************************************************         
         SPACE 1                                                                
SETBCW   NTR1  ,                                                                
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 GETDAY,DMCB,WORK,DUB                                             
         LA    R2,1                                                             
         SR    R0,R0                                                            
         IC    R0,DMCB             1-7                                          
         SR    R2,R0                                                            
         BZ    SETBRD2                                                          
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         MVC   WORK(6),WORK+6                                                   
*                                                                               
SETBRD2  GOTO1 ADDAY,DMCB,WORK,WORK+6,27                                        
         GOTO1 ADDAY,DMCB,WORK+6,WORK,7                                         
         ZAP   0(8,R3),=P'4'                                                    
         CLC   WORK(4),WORK+6      4-WEEK MONTHS                                
         BNE   XIT                                                              
         ZAP   0(8,R3),=P'5'       5-WEEK MONTH                                 
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SORTER AND TSAROFF                                       *         
***********************************************************************         
         SPACE 1                                                                
STSAR    NTR1  ,                                                                
         L     R0,HIGHROW          NUMBER OF ROWS                               
         LA    R1,SRTFLN           SORT FIELD LENGTHS                           
         SR    R2,R2                                                            
         SR    R4,R4                                                            
         IC    R4,0(,R1)           LENGTH OF THIS FIELD                         
         AR    R2,R4               ADD TO TOTAL                                 
         LA    R1,1(,R1)                                                        
         BCT   R0,*-10                                                          
         STH   R2,DSPREP           AND DISPLACEMENT TO REPORT CODE              
         STH   R2,SRTLNGTH         LENGTH OF SORT                               
*                                                                               
         LA    R2,4(,R2)           REPORT/COPY/TYPE/SPARE                       
         STH   R2,DSPNME           DISPLACEMENT TO NAMES                        
         L     R1,HIGHROW          PLUS (ROWSX36) FOR NAMES                     
         MHI   R1,36                                                            
         AR    R2,R1                                                            
         STH   R2,DSPCOL           DISPLACEMENT TO COLUMNS                      
         L     R1,HIGHCOL          NUMBER OF COLUMNS                            
         SLL   R1,3                X 8                                          
         AR    R2,R1                                                            
         STH   R2,RECLNGTH                                                      
*                                                                               
         USING TSARD,R1                                                         
         LA    R1,SORTBLK                                                       
         XC    TSARD(TSARDL),TSARD CLEAR TSAR BLOCK                             
         MVC   TSACOM,ADCOMFAC     ADDR COMFACS                                 
         MVC   TSABUF,ASRTBF       ADDR OF TSAR BUFFER                          
         MVC   TSAREC,=A(SRTLN)    SIZE OF BUFFER                               
         MVC   TSKEYL,SRTLNGTH+1   KEY LENGTH                                   
         MVC   TSRECL,RECLNGTH     RECORD LENGTH                                
         MVI   TSOFFACT,TSAINI     ACTION (INIT) IS HOB OF BUFFER               
         OI    TSIND2,TSI2MANY     USE FULL WORD COUNTERS                       
         GOTO1 TSAROFF                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R1                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              CLEAR SOME CALENDAR LINE                                         
*                                                                               
SETCLR   LR    R0,RE                                                            
         L     RE,AZRO             RE=LINE 0 (ZEROS)                            
         LA    RF,WLN              WIDTH OF LINE                                
         AR    R2,RE                                                            
         LR    R3,RF                                                            
         MVCL  R2,RE               CLEAR ACCUMULATOR LINE                       
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
*              SET ACTIVE STATUS FOR SOME LINES                                 
*                                                                               
SETACT   LA    R1,ACTLST                                                        
SETACT1  SR    R2,R2                                                            
         IC    R2,0(,R1)                                                        
         A     R2,ALNSTL                                                        
         OI    0(R2),LSACT                                                      
         LA    R1,1(,R1)                                                        
         CLI   0(R1),EOT                                                        
         BNE   SETACT1                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* CONSTANTS                                                           *         
***********************************************************************         
         SPACE 1                                                                
ENTRYTAB DC    AL2(INIT-ACM203),AL2(AINIT-MAND)                                 
         DC    X'FF'                                                            
*                                                                               
LISTUL   DC    X'000000'                                                        
WKEND    DC    C'SAT'                   DEFAULT IS SATURDAY                     
RLCO     DC    X'00'                                                            
READCO   DC    X'00'                                                            
*                                                                               
SUBLIST  DC    AL1(FRL1,FRL2,FRL3,FRL4,EOT)                                     
*                                                                               
FLTFORMS DC    AL1(FRF1,FRF2,FRF3,FRF4,FRF5)                                    
*                                                                               
SBLIST   DC    C'101202303404505606'                                            
         DC    C'707808909A10B11C12'                                            
*                                                                               
SBRELS   DC    AL1(0,0,1,1,2,2,3,3,4,4,5,5)            M1-M6                    
         DC    AL1(6,6,7,7,8,8,9,9,10,10,11,11)        M7-M12                   
         DC    AL1(0,2,3,5,6,8,9,11)                   Q1,Q4                    
         DC    AL1(0,5,6,11)                           61,62                    
         DC    X'FF'                                                            
*                                                                               
WEEKTAB  DS    0F                                                               
         DC    AL4(LNBCW*WLN)           BROADCAST WEEKS                         
         DC    AL4(LNSWH*WLN)           STANDARD WORKING HOURS                  
         DC    AL4(LNHOL*WLN)           HOLIDAYS                                
*                                                                               
ACTLST   DC    AL1(LNBCW,LNSWH,LNHOL,LNC30,EOT)                                 
*                                                                               
BASRATE  DC    PL2'0'                                                           
HOLLIST  DC    CL2'DM',CL2'92',PL2'700',AL1(1,1,0,0,1,0,1,0,1,0,1,1)            
         DC    CL2'DM',CL2'91',PL2'700',AL1(1,1,0,0,1,0,1,0,1,0,1,1)            
         DC    CL2'PN',CL2'93',PL2'640',AL1(1,1,1,0,1,0,1,0,1,0,2,3)            
         DC    CL2'PN',CL2'92',PL2'637',AL1(1,1,0,0,1,0,1,0,1,1,2,3)            
         DC    CL2'PN',CL2'91',PL2'613',AL1(1,1,0,0,1,0,1,0,1,1,2,3)            
         DC    CL2'GV',CL2'93',PL2'711',AL1(1,1,0,0,1,0,1,0,1,0,2,1)            
         DC    CL2'GV',CL2'92',PL2'750',AL1(1,1,0,0,1,0,1,0,1,0,2,1)            
         DC    CL2'GV',CL2'91',PL2'711',AL1(1,1,0,0,1,0,1,0,1,0,2,1)            
         DC    CL2'QJ',CL2'93',PL2'728',AL1(1,1,1,0,1,0,1,0,1,0,2,3)            
         DC    CL2'  ',CL2'92',PL2'700',AL1(1,1,0,0,1,0,2,0,1,0,1,1)            
         DC    CL2'  ',CL2'91',PL2'632',AL1(1,1,0,0,1,0,2,0,1,0,1,1)            
         DC    CL2'  ',CL2'  ',PL2'632',AL1(1,1,0,0,1,0,2,0,1,0,1,1)            
         EJECT                                                                  
OPTABL   DS    0F                                                               
         DC    AL1(01),AL1(RSMINTOT),AL2(RSOPT1-RSTACKD)    MINTOTS             
         DC    AL1(02),AL1(RSNOPAGE),AL2(RSOPT1-RSTACKD)    NOPAGE              
         DC    AL1(03),AL1(RSNO100P),AL2(RSOPT1-RSTACKD)    NO100               
         DC    AL1(04),AL1(RSMX100P),AL2(RSOPT1-RSTACKD)    MAX100              
         DC    AL1(05),AL1(RSENDDTE),AL2(RSOPT1-RSTACKD)    ENDDATE             
         DC    AL1(06),AL1(RSWHOLEP),AL2(RSOPT1-RSTACKD)    WHOLEPCT            
         DC    AL1(07),AL1(RSMCLI),AL2(RSOPT3-RSTACKD)      CLIMWR              
         DC    AL1(08),AL1(RSRNDALL),AL2(RSOPT1-RSTACKD)    ROUNDALL            
         DC    AL1(09),AL1(RSBUDCNT),AL2(RSOPT1-RSTACKD)    BUDCENTS            
*&&US*&& DC    AL1(10),AL1(RSPEREND),AL2(RSOPT2-RSTACKD)    PEREND              
*&&UK*&& DC    AL1(10),AL1(RSNOCHNK),AL2(RSOPT2-RSTACKD)    NOCHUNK             
*&&US*&& DC    AL1(11),AL1(RSNOREVL),AL2(RSOPT2-RSTACKD)    NOREVERSAL          
*&&UK*&& DC    AL1(11),AL1(RSUNDRTO),AL2(RSOPT2-RSTACKD)    UNDERTOT            
         DC    AL1(12),AL1(RSNOPERD),AL2(RSOPT2-RSTACKD)    NOPERIOD            
         EJECT                                                                  
SALKYWDS DS    0H                                                               
         DC    AL1(KWSAL,KWTSL,KWASL,KWABN,KWAPN,KWATS)                         
         DC    AL1(KWPS$,KWST$,KWPBR,KWPB$,KWPPR,KWPP$,KWPTR,KWPT$)             
SALKYWQ  EQU   *-SALKYWDS                                                       
***********************************************************************         
* LOCAL STORAGE                                                       *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
RLFND    DS    C        READ LIST FOUND(Y OR N)                                 
ULN      DS    CL100                                                            
ELCODE   DS    XL1                                                              
FORMNUM  DS    CL1                                                              
LASTROWL DS    A        A(LAST ROW NAME)                                        
HORIZ    DS    CL1      SET TO X'BF'                                            
COLNO    DS    CL1      COLUMN NUMBER                                           
COLAHEAD DS    F        COLUMN HEADING TEXT                                     
COLWIDE  DS    F        WIDTH OF COLUMN                                         
*                                                                               
OVFISCAL DS    CL1                                                              
SVDUB    DS    CL8                                                              
AFORM    DS    F                                                                
SVRP     DS    C                                                                
M3SECCOL DS    XL16                                                             
M3OVRCOL DS    XL16                                                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE ACAPGGEND                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'060ACREPM203 01/28/13'                                      
         END                                                                    
