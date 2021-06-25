*          DATA SET ACREPGB02  AT LEVEL 015 AS OF 11/04/20                      
*PHASE ACGB02C                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE ACGLKEY                                                                
ACGB02   TITLE 'General ledger balance reporting'                               
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* RKEJ 014 12DEC19 <SPEC-39587> Fix OOB between UNIT G & UNIT S &     *         
*                               Special records                       *         
* JSAY 014 16NOV18 <SPEC-27345> ADD Peel Date in ACGB to exclude      *         
*                               peeled off S transactions.            *         
* JSAY 014 13JAN20 <SPEC-42341> Updated CPYMOATB for impacted company *         
* ASAX 015 28Oct20 <SPEC-51250> FIX AGB OOB due to peel.              *         
***********************************************************************         
         PRINT NOGEN                                                            
***********************************************************************         
*  QOPT1  = BLANK    -  compares Unit G   vs GLBRECs                            
*        A= ALTRNATE -  compares Unit S   vs Unit G  (only >=GLMOA)             
*        D= DUMPLOAD -  compares Unit S+G vs GLBRECs (only >=GLMOA)             
*                       detailed version of Dump/Load                           
*        G= GLUONLY  -  compares Unit S   vs Unit G GLU postings                
*        B= BALANCE  -  compares Unit S   vs Unit G totals (exclude S9)         
*                                                                               
*  QOPT2 N= Supress office, Default is to report it                             
*                                                                               
*  QOPT3 X= Only works for QOPT1=D, X for extra details                         
*                                                                               
*  QAPPL  = BLANK     -  process for months specified in MOA range              
*         = ##        -  # of months from today and back to process             
*                                                                               
*  SHIP=AHYD in JCL will over-ride the default list of who to send              
*            email error reports to.                                            
***********************************************************************         
         USING GLOBALS,R9                                                       
         USING ACGBD,RA            RA=A(WORK AREA saved area)                   
         USING ACWORKD,RC                                                       
ACGB02   CSECT                                                                  
         NMOD1 0,**GBAL**,R7,CLEAR=Y                                            
         L     RC,0(,R1)                                                        
         LA    RA,SPACEND                                                       
         BASR  R9,0                                                             
         AHI   R9,GLOBALS-*        Resolve GLOBALS addressing                   
                                                                                
         CLI   MODE,RUNFRST                                                     
         JE    RUNF                                                             
         CLI   MODE,PROCRQST                                                    
         JE    PRQS                                                             
         CLI   MODE,REQFRST                                                     
         JE    REQF                                                             
         CLI   MODE,REQLAST                                                     
         JE    REQL                                                             
         CLI   MODE,RUNLAST                                                     
         JE    RUNL                                                             
                                                                                
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         USING COMFACSD,R2                                                      
RUNF     DS    0H                                                               
         L     R2,ADCOMFAC                                                      
         MVI   PKZERO,X'0C'                                                     
         MVI   PKONE,X'1C'                                                      
         MVI   DUMP#,0                                                          
         MVI   FIRSTX,YES          First time in                                
         MVI   SETMOA,NO                                                        
         ZAP   COUNT,PKZERO                                                     
         MVC   HELLO,CHELLO                                                     
         L     RE,=A(IOCPY)                                                     
         ST    RE,AIOCPY                                                        
         L     RE,=A(IOLDG)                                                     
         ST    RE,AIOLDG                                                        
         DROP  R2                                                               
                                                                                
         USING MASTD,R6                                                         
         USING BOXD,R2                                                          
         L     R6,ADMASTC                                                       
         MVC   UPSI,MCUPSI                                                      
         L     R2,MCBXAREA                                                      
         LA    R1,132                                                           
         ST    R1,BOXWIDTH                                                      
                                                                                
         USING LOGOD,R3                                                         
         L     R3,MCVLOGOC                                                      
         CLC   LOGOINFO,SPACES                                                  
         JNH   *+10                                                             
         MVC   MAILCALL,LOGOINFO                                                
         OC    MAILCALL,SPACES                                                  
         DROP  R2,R3,R6            BOXD, LOGOD, MASTD                           
                                                                                
         CLC   MAILCALL+L'MAILCALL-2(2),SPACES                                  
         JE    *+6                                                              
         DC    H'00'               Need at least two spaces for C' :'           
                                                                                
         LA    RF,L'MAILCALL-3                                                  
         LA    RE,MAILCALL(RF)     Point to end                                 
RUNF06   CLI   0(RE),C' '          Find first character on end                  
         JH    RUNF08              Found a character                            
         BCTR  RE,0                Bump backwards in MAILCALL                   
         BRCT  RF,RUNF06           Back to compare                              
         DC    H'00'               Can't happen                                 
                                                                                
RUNF08   CLI   0(RE),C':'          If colon then looking okay                   
         JE    RUNF10                                                           
         AHI   RE,1                Put a space between the emails               
         MVI   0(RE),C':'                                                       
                                                                                
RUNF10   GOTOR DATCON,DMCB,(5,0),ETODAY                                         
         GOTOR DATCON,DMCB,ETODAY,(1,PTODAY)                                    
         L     R0,=A(MAX#OFFS*2*K+16)                                           
         GETMAIN R,LV=(0)          LOW CORE STORAGE                             
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
         MVC   0(16,R1),=CL16'*REC*REC*REC*REC'                                 
         AHI   R1,16                                                            
         ST    R1,AGLRECS          Records                                      
                                                                                
         LHI   R0,2000             # of accounts                                
         MHI   R0,12*5*10          x 12 months x 5 years x # of offices         
                                                                                
         ST    R0,MAX#ACCS                                                      
         MHI   R0,ACBLNQ                                                        
         AHI   R0,16                                                            
         GETMAIN RU,LV=(0),LOC=(ANY,ANY),BNDRY=PAGE                             
         LTR   RF,RF                                                            
         JZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         SAM31                                                                  
         MVC   0(16,R1),=CL16'*BAL*BAL*BAL*BAL'                                 
         AHI   R1,16                                                            
         ST    R1,AGLBAL           Balancing Records                            
         SAM24                                                                  
                                                                                
         MVI   ONEXONLY,NO                                                      
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Modifiy USER ID if multiple request                                           
* If QAPPL is used then calculate # of months back from today                   
***********************************************************************         
         USING MASTD,R6                                                         
         USING CPYELD,R3                                                        
PRQS     DS    0H                                                               
         L     R6,ADMASTC                                                       
         MVC   MCUSERID,SPACES                                                  
         L     R3,ADCOMP                                                        
         LR    R2,R3                                                            
         AH    R3,DATADISP                                                      
         CLI   0(R3),EOR                                                        
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),CPYELQ        X'10'                                        
         JE    *+6                                                              
         DC    H'00'                                                            
         MVC   MCUSERID(L'CPYLOGO),CPYLOGO                                      
                                                                                
         CLC   QAPPL,SPACES                                                     
         JNH   PRQS010                                                          
         CLI   QAPPL,C'0'                                                       
         JNL   *+6                                                              
         DC    H'00'                                                            
         CLI   QAPPL+1,C'0'                                                     
         JNL   *+6                                                              
         DC    H'00'               must be a number >= 01, format ##            
                                                                                
         PACK  DUB,QAPPL(2)                                                     
         CVB   R2,DUB                                                           
         SHI   R2,1                Need to adjust by one                        
         LNR   R2,R2                                                            
         GOTOR ADDAY,DMCB,(C'M',ETODAY),WORK,(R2)                               
                                                                                
         USING ACQD,R4                                                          
         L     R4,ADQSTACK                                                      
         MVC   ACQMOSST,WORK                                                    
         MVC   ACQMOSND,ETODAY                                                  
         DROP  R4                  ACQD                                         
                                                                                
         MVC   QMOSSTRT,WORK       set Q dates too                              
         MVC   QMOSEND,ETODAY                                                   
                                                                                
PRQS010  XC    IOKEY,IOKEY                                                      
         LA    R5,IOKEY                                                         
         USING CT5REC,R5           SYSTEM ACCESS RECORD                         
         MVI   CT5KTYP,CT5KTYPQ    C'5'                                         
         MVC   CT5KALPH,ALPHAID                                                 
         GOTO1 DATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,DAREC                           
         L     R5,DAREC                                                         
         CLC   IOKEY(L'CT5KEY),0(R5)                                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,CT5DATA                                                       
         USING CTAGDD,R5                                                        
         XR    R0,R0                                                            
PRQS020  CLI   CTAGDEL,0           TEST EOR                                     
         JE    PRQSXIT                                                          
         CLI   CTAGDEL,CTAGDELQ    X'B4'                                        
         JE    PRQS030                                                          
         IC    R0,CTAGDLEN                                                      
         AR    R5,R0                                                            
         J     PRQS020                                                          
                                                                                
PRQS030  MVC   AGYOPTS,CTAGOPTS    AGENCY ALPHA ID                              
*                                                                               
PRQSXIT  J     EXIT                                                             
         DROP  R3,R5,R6            CPYELD, CTAGDD,  MASTD                       
***********************************************************************         
* Read through file for company                                                 
***********************************************************************         
REQF     DS    0H                                                               
         MVI   FCRDACC,NO                                                       
         MVI   FCRDTRNS,NO                                                      
         MVC   MOAGLOLD,=X'FFFF'                                                
         CLI   ONEXONLY,YES                                                     
         JNE   REQF002                                                          
         MVI   FORCEHED,YES        Cause heading to print                       
         J     REQFXIT                                                          
                                                                                
REQF002  MVI   ONEXONLY,YES                                                     
         XC    MOASTR,MOASTR                                                    
         MVC   MOAEND,=X'FFFF'                                                  
         CLC   QMOSSTRT,SPACES                                                  
         JNH   REQF010                                                          
         MVC   WORK(4),QMOSSTRT                                                 
         MVC   WORK+4(2),=C'01'                                                 
         GOTOR DATCON,DMCB,WORK,(1,WORK+6)                                      
         MVC   MOASTR,WORK+6                                                    
                                                                                
REQF010  CLC   QMOSEND,SPACES                                                   
         JNH   REQFXIT                                                          
         MVC   WORK(4),QMOSEND                                                  
         MVC   WORK+4(2),=C'01'                                                 
         GOTOR DATCON,DMCB,WORK,(1,WORK+6)                                      
         MVC   MOAEND,WORK+6                                                    
                                                                                
REQFXIT  DS    0H                                                               
         EJECT ,                                                                
***********************************************************************         
* Process each company                                                          
***********************************************************************         
         USING CPYRECD,R2                                                       
CPYL     XC    #OFACCS,#OFACCS     Set to none                                  
         TM    AGYOPTS,CTAGUAT     Is this a UAT agency?                        
         JO    EXIT                Don't bother for UAT files                   
*                                                                               
         L     R2,ADCOMP                                                        
         CLC   RCCOMPFL,0(R2)                                                   
         JE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
CPYL010  CLI   0(R3),EOR                                                        
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R3),CPYELQ        X'10'                                        
         JE    CPYL020                                                          
         LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     CPYL010                                                          
                                                                                
         USING CPYELD,R3                                                        
CPYL020  CLI   CPYLN,CPYLN4Q                                                    
         JL    EXIT                                                             
         MVC   GLMOA,CPYGLMOA                                                   
         OC    GLMOA,GLMOA                                                      
         JZ    EXIT                Don't bother                                 
*                                                                               
         CLI   QOPT1,BALANCE       GLMOA override only valid for                
         JNE   CPYL025              BALANCE reporting                           
         LA    RE,CPYMOATB         Check for MOA override                       
CPYL022  CLI   0(RE),X'FF'                                                      
         JE    CPYL024                                                          
         CLC   ALPHAID,0(RE)                                                    
         JE    *+12                                                             
         LA    RE,L'CPYMOATB(RE)                                                
         J     CPYL022                                                          
         MVC   GLMOA,2(RE)         Set new MOA                                  
*                                                                               
CPYL024  GOTOR GLMOARTN            GET GLMOA FROM ABLELD element                
         CLI   SETMOA,NO                                                        
         BE    CPYL025                                                          
         MVC   WORK(L'GLMOA),GLMOA Save MOA                                     
         MVI   WORK+L'GLMOA,01     Set Start Of Month                           
         GOTO1 DATCON,DMCB,(X'31',WORK),(1,WORK+3),(2,0)                        
         MVC   GLMOA,WORK+3        Set Next Month                               
*                                                                               
CPYL025  MVI   NEWOFF,NO                                                        
         TM    CPYSTAT4,CPYSOFF2                                                
         JZ    *+8                                                              
         MVI   NEWOFF,YES                                                       
*&&US                                                                           
         CLC   ALPHAID,=C'mm'      special code for AVNY                        
         JNE   CPYL030                                                          
         CLC   =X'A705',MOASTR     Were they put on office > YEAR AGO?          
         JL    CPYL030                                                          
         MVI   QOPT2,NO            Force to run w/o office if < YR ago          
         DROP  R3                                                               
                                                                                
CPYL030  MVI   LENCP,6                                                          
*&&                                                                             
*&&UK                                                                           
         USING LDGRECD,R2                                                       
         MVI   LENCP,0                                                          
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   LDGKCPY,RCCOMPFL                                                 
         MVI   LDGKUNT,C'S'                                                     
         MVI   LDGKLDG,C'J'                                                     
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,IOKEY,IOKEY                           
         CLI   8(R1),0                                                          
         JNE   CPYL060                                                          
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,LDGKDA,DAREC,IOWORK                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     R2,DAREC                                                         
         LA    R3,LDGRFST          Point to first element                       
CPYL040  CLI   0(R3),EOR                                                        
         JE    CPYL060                                                          
         CLI   0(R3),ACLELQ        X'16' account lengths                        
         JE    CPYL050                                                          
         LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     CPYL040                                                          
                                                                                
         USING ACLELD,R3                                                        
CPYL050  MVC   LENCP,ACLELLVB                                                   
         DROP  R2,R3                                                            
*&&                                                                             
***********************************************************************         
* Process Special records GLBRECs                                               
***********************************************************************         
         USING GLBRECD,R2                                                       
CPYL060  CLI   QOPT1,BALANCE       Balance is just Unit G vs Unit S             
         JE    CPYL080                                                          
         CLI   QOPT1,ALTRNATE      Alternate checking                           
         JE    CPYL080             Don't process special recs                   
                                                                                
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVI   GLBKTYP,GLBKTYPQ    X'28'                                        
         MVC   GLBKCPY,RCCOMPFL                                                 
         MVC   COMMAND,DMRDHI                                                   
         LA    R2,IOKEY                                                         
CPYL070  GOTOR DATAMGR,DMCB,COMMAND,ACCDIR,IOKEY,IOKEY                          
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         XC    DA,DA               CLEAR SAVED DA#                              
         CLI   GLBKTYP,GLBKTYPQ    X'28', GLBREC?                               
         JNE   CPYL080             No, done                                     
         CLC   GLBKCPY,RCCOMPFL    Right company?                               
         JNE   CPYL080             No, done                                     
         MVC   DA,GLBKDA                                                        
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,GLBKDA,DAREC,IOWORK                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         GOTOR GLBADD,DMCB,(RC)                                                 
         MVC   COMMAND,DMRSEQ                                                   
         J     CPYL070                                                          
                                                                                
***********************************************************************         
* Process Transactions unit G                                                   
***********************************************************************         
         USING ACTRECD,R2                                                       
CPYL080  LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVI   ACTKUNT,C'G'                                                     
         MVC   COMMAND,DMRDHI                                                   
                                                                                
CPYL090  GOTOR DATAMGR,DMCB,COMMAND,ACCDIR,IOKEY,IOKEY                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         XC    DA,DA               CLEAR SAVED DA#                              
         CLC   ACTKCPY,RCCOMPFL                                                 
         JNE   CPYL100             Done                                         
         CLI   ACTKUNT,C'G'                                                     
         JNE   CPYL100             Done                                         
         MVC   DA,ACTKDA                                                        
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,ACTKDA,DAREC,IOWORK                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     R1,DAREC                                                         
         GOTOR TRNGADD                                                          
         MVC   COMMAND,DMRSEQ                                                   
         J     CPYL090                                                          
                                                                                
***********************************************************************         
* Process Transactions unit S                                                   
***********************************************************************         
         USING ACTRECD,R2                                                       
CPYL100  CLI   QOPT1,BLANK                                                      
         JE    CPYLXIT             Don't process unit S                         
         LA    R2,IOKEY                                                         
         MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,RCCOMPFL                                                 
         MVI   ACTKUNT,C'S'                                                     
         MVC   COMMAND,DMRDHI                                                   
                                                                                
CPYL110  GOTOR DATAMGR,DMCB,COMMAND,ACCDIR,IOKEY,IOKEY                          
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLC   ACTKCPY,RCCOMPFL                                                 
         JNE   CPYLXIT             Done                                         
         CLI   ACTKUNT,C'S'                                                     
         JNE   CPYLXIT             Done                                         
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,ACTKDA,DAREC,IOWORK                   
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     R1,DAREC                                                         
         GOTOR TRNSADD                                                          
         MVC   COMMAND,DMRSEQ                                                   
         J     CPYL110                                                          
                                                                                
CPYLXIT  DS    0H                                                               
         J     EXIT                                                             
         DROP  R2                                                               
*                                                                               
CPYMOATB DS    0CL4                                                             
         DC    C'AG',X'B201'                                                    
         DC    C'DN',X'B201'                                                    
         DC    C'I6',X'B305'                                                    
         DC    C'UB',X'B401'                                                    
         DC    C'WD',X'B001'                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* END OF INPUT FILE                                                   *         
***********************************************************************         
REQL     DS    0H                                                               
         ZAP   #OFWRNS,PKZERO                                                   
         ZAP   #OFUPDT,PKZERO           OOB Unit G updated                      
         ZAP   #OFSPUP,PKZERO           OOB Specials updated                    
         GOTOR SHOWBAL,DMCB,(RC)                                                
         CP    #OFWRNS,PKZERO                                                   
         JE    EXIT                None                                         
                                                                                
         USING MASTD,R6                                                         
         L     R6,ADMASTC                                                       
         MVC   P,SPACES                                                         
         MVC   P+1(25),=CL25'Total errors for'                                  
         MVC   P+18(7),MCUSERID                                                 
         OI    #OFWRNS+L'#OFWRNS-1,X'0F'                                        
         EDIT  #OFWRNS,(6,P+30),0,ALIGN=LEFT                                    
         GOTOR SENDMAIL,DMCB,(RC)                                               
         GOTOR ACREPORT                                                         
                                                                                
         MVC   P,SPACES                                                         
         MVC   P+1(25),=CL25'Total G Accounts updated:'                         
         OI    #OFUPDT+L'#OFUPDT-1,X'0F'                                        
         EDIT  #OFUPDT,(6,P+30),0,ALIGN=LEFT                                    
         GOTOR ACREPORT                                                         
                                                                                
         MVC   P,SPACES                                                         
         MVC   P+1(25),=CL25'Total Specials updated:'                           
         OI    #OFSPUP+L'#OFSPUP-1,X'0F'                                        
         EDIT  #OFSPUP,(6,P+30),0,ALIGN=LEFT                                    
         GOTOR ACREPORT                                                         
         J     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* END OF INPUT FILE                                                   *         
***********************************************************************         
RUNL     DS    0H                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Add Unit G balance information to BIN table High core                         
***********************************************************************         
         USING ACBD,ACCBREC                                                     
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
TRNGADD  NTR1                                                                   
         SAM31                                                                  
         LR    R2,R1                                                            
         LA    R3,TRNRFST                                                       
         MVC   ACBKEY,SPACES                                                    
         MVC   ACBDA#,SPACES                                                    
         XC    ACBREF,ACBREF       CLEAR REF DATE                               
         CLI   0(R3),TRNELQ        Is it a tranaction ?                         
         JNE   TRNGADDX                                                         
         CLI   QOPT1,BALANCE                                                    
         JE    TRNGADD1                                                         
         MVC   ACBGACT,TRNKULA     Unit/Ledger/Account (Unit G)                 
         MVC   ACBOFFC,TRNOFFC     Office                                       
         MVC   ACBSACT,SPACES                                                   
         MVC   ACBSACT(2),TRNKULC  Unit/Ledger contra (Unit S)                  
         CLC   TRNKCULC(3),SPACES  Is it production?                            
         JH    *+10                No                                           
         MVC   ACBSACT,TRNKULC     Yes, then media name                         
TRNGADD1 ZAP   ACBSDR,PKZERO                                                    
         ZAP   ACBSCR,PKZERO                                                    
         ZAP   ACBGDR,PKZERO                                                    
         ZAP   ACBGCR,PKZERO                                                    
         CLI   QOPT2,NO                                                         
         JNE   *+10                                                             
         MVC   ACBOFFC,SPACES                                                   
                                                                                
         USING TRSELD,R4                                                        
         LR    R4,R3                                                            
TRNGADD2 CLI   0(R4),EOR                                                        
         JNE   *+6                                                              
         DC    H'00'                                                            
                                                                                
         CLI   0(R4),TRSELQ        Get X'60' status element                     
         JE    TRNGADD3                                                         
         LLC   RF,1(,R4)                                                        
         AR    R4,RF                                                            
         J     TRNGADD2                                                         
*&&DO                                                                           
TRNGADD3 CLI   NEWOFF,YES          1 char, GLBRECs for all of G                 
         JNE   TRNGADD4            Yes one char. office                         
*&&                                                                             
TRNGADD3 CLC   TRSPMOS,GLMOA       No, is it prior to GLMOA?                    
         JL    TRNGADDX            Yes, don't bother then                       
                                                                                
TRNGADD4 CLC   TRSPMOS,MOASTR                                                   
         JL    TRNGADDX            Skip this one                                
         CLC   TRSPMOS,MOAEND                                                   
         JH    TRNGADDX            Skip this one                                
         CLI   QOPT1,BLANK         Unit G vs Specail                            
         JE    TRNGADD5            Yes, so everything                           
         CLI   QOPT1,BALANCE       Balance reporting                            
         JE    TRNGADD5            So everything                                
         CLI   QOPT1,ALTRNATE      Unit G vs Unit S                             
         JNE   TRNGAD4A                                                         
         CLI   TRNTYPE,25                                                       
         JNE   TRNGADDX            Skip direct postings but                     
         J     TRNGADD5            keep A25 & GLU                               
                                                                                
TRNGAD4A CLI   QOPT1,GLUONLY       Unit G vs S, GLU postings only               
         JNE   TRNGAD4B            No                                           
         CLI   TRNTYPE,25          Created via AC25 or $INP 25                  
         JNE   TRNGADDX            Skip direct                                  
         CLC   TRNBREF,SPACES      GLU ?                                        
         JNH   TRNGADDX            Via the A25, so skip                         
         J     TRNGADD5            keep GLU only                                
                                                                                
TRNGAD4B CLI   QOPT1,DUMPLOAD      Unit G & S vs special records                
         JE    *+6                 No                                           
         DC    H'00'               Invalid option                               
         CLI   TRNTYPE,25          Created via AC25 or GLU ?                    
         JNE   TRNGADD5            No, so keep direct postings                  
         CLC   TRNBREF,SPACES      GLU ?                                        
         JNH   TRNGADDX            Via the A25, so skip                         
                                                                                
TRNGADD5 MVC   ACBMOA,TRSPMOS                                                   
         CLC   ACBMOA,MOAGLOLD                                                  
         JNL   *+10                                                             
         MVC   MOAGLOLD,ACBMOA     Save oldest MOA found                        
         CLI   QOPT1,DUMPLOAD      Both G & S & Special records                 
         JE    *+8                                                              
         CLI   QOPT1,GLUONLY       Both G vs S, GLU only                        
         JNE   *+10                                                             
         MVC   ACBSACT,TRNKULC     Yes full unit contra (Unit S)                
                                                                                
         LA    RE,ACBGDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,ACBGCR                                                        
         ZAP   0(L'ACBGCR,RE),TRNAMNT                                           
         DROP  R3,R4               TRNELD, TRSELD                               
                                                                                
TRNGADD8 GOTOR BIN31,DMCB,ACCBREC,AGLBAL,#OFACCS,(1,ACBLNQ),           +        
               L'ACBKEY,MAX#ACCS                                                
         MVC   #OFACCS,8(R1)                                                    
         ICM   R8,15,0(R1)         R8 = A(Record added or found)                
         JNZ   *+6                                                              
         DC    H'00'               Table full                                   
                                                                                
BIN      USING ACBD,R8                                                          
         TM    0(R1),X'80'         Record not found?                            
         JO    TRNGADD9            No record found, so added                    
         AP    BIN.ACBGDR,ACBGDR                                                
         AP    BIN.ACBGCR,ACBGCR                                                
                                                                                
TRNGADD9 CLI   QOPT1,BALANCE       Balance report                               
         JNE   TRNGADDA            Not this option                              
         CLC   ACBMOA,=X'FFFF'                                                  
         JE    TRNGADDX            Done                                         
         MVC   ACBMOA,=X'FFFF'                                                  
         J     TRNGADD8            Add total                                    
                                                                                
TRNGADDA CLI   QOPT1,ALTRNATE      Unit G vs Unit S                             
         JNE   TRNGADDX                                                         
         MVC   ACBREF,TRNKREF      ADD THE INDIVIDUAL TRANSACTIONS              
         MVC   ACBSACT,TRNKULC     Yes full unit contra (Unit S)                
         MVC   ACBDA#,DA           SAVE DISK ADDRESS                            
         GOTOR BIN31,DMCB,ACCBREC,AGLBAL,#OFACCS,(1,ACBLNQ),           +        
               L'ACBKEY,MAX#ACCS                                                
         MVC   #OFACCS,8(R1)                                                    
         ICM   R8,15,0(R1)         R8 = A(Record added or found)                
         JNZ   *+6                                                              
         DC    H'00'               Table full                                   
         TM    0(R1),X'80'         Record not found?                            
         JO    TRNGADDX            No record found, so added                    
         AP    BIN.ACBGDR,ACBGDR                                                
         AP    BIN.ACBGCR,ACBGCR                                                
                                                                                
TRNGADDX J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* Add Unit S balance information to BIN table High core                         
***********************************************************************         
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
         USING GLDELD,R5                                                        
TRNSADD  NTR1                                                                   
         SAM31                                                                  
         LR    R2,R1                                                            
         CLI   TRNKLDG,C'9'                                                     
         JE    TRNSADDX                                                         
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ        Is it a tranaction ?                         
         JNE   TRNSADDX                                                         
         CLC   TRNOFFC,=C'**'                                                   
         JE    TRNSADDX                                                         
         MVC   ACBKEY,SPACES                                                    
         MVC   ACBDA#,SPACES                                                    
         XC    ACBREF,ACBREF       CLEAR REF DATE                               
         ZAP   ACBSDR,PKZERO                                                    
         ZAP   ACBSCR,PKZERO                                                    
         ZAP   ACBGDR,PKZERO                                                    
         ZAP   ACBGCR,PKZERO                                                    
                                                                                
         USING TRSELD,R4                                                        
         LR    R4,R3                                                            
TRNSADD2 CLI   0(R4),EOR                                                        
         JNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),TRSELQ                                                     
         JE    TRNSADD3                                                         
         LLC   RF,1(,R4)                                                        
         AR    R4,RF                                                            
         J     TRNSADD2                                                         
                                                                                
TRNSADD3 DS    0H                                                               
*&&DO*&& CLI   QOPT1,BALANCE       Balance reporting                            
*&&DO*&& JE    TRNSAD3A                                                         
         CLC   TRSPMOS,GLMOA                                                    
         JL    TRNGADDX            Don't bother                                 
                                                                                
TRNSAD3A CLC   TRSPMOS,MOASTR                                                   
         JL    TRNSADDX            Skip this one                                
         CLC   TRSPMOS,MOAEND                                                   
         JH    TRNSADDX            Skip this one                                
         CLI   QOPT1,GLUONLY       Unit S GLU only?                             
         JNE   TRNSAD3C                                                         
         TM    TRSSTAT,TRSSGLIP    GLU                                          
         JO    TRNSAD3D            Only these                                   
         J     TRNSADDX            Skip everything else                         
                                                                                
TRNSAD3C CLI   QOPT1,DUMPLOAD                                                   
         JNE   TRNSAD3D                                                         
         TM    TRSSTAT,TRSSGLIP    GLU                                          
         JO    TRNSADDX            Pick these up via Unit G                     
                                                                                
TRNSAD3D MVC   ACBMOA,TRSPMOS                                                   
         TM    TRSSTAT,TRSSGLUP    Updated to Unit G?                           
         JZ    TRNSADDX            No                                           
         CLI   QOPT1,BALANCE                                                    
         JNE   TRNSAD3T                                                         
         CLC   ACBMOA,MOAGLOLD                                                  
         JL    TRNSADDX            Skip it then won't be there                  
         J     TRNSADD8            Post as is                                   
                                                                                
TRNSAD3T SAM24                                                                  
         GOTOR DATCON,DMCB,(2,TRSUPDT),(1,WORK)                                 
         LR    R5,R3                                                            
         SAM31                                                                  
                                                                                
TRNSADD4 CLI   0(R5),EOR                                                        
         JNE   TRNSAD4A                                                         
         SAM24                                                                  
         LA    RF,TRNRFST-TRNKEY                                                
         GOTOR PRNTBL,DMCB,=CL10'Missing 63',(R2),C'DUMP',(RF),=C'1R', +        
               (C'P',VPRINT)                                                    
         SAM31                                                                  
         J     TRNSADDX                                                         
                                                                                
TRNSAD4A CLI   0(R5),GLDELQ        X'63'                                        
         JNE   TRNSADD5                                                         
         CLC   GLDDATE,WORK        Find matching GLDEL                          
         JE    TRNSADD6                                                         
TRNSADD5 LLC   RF,1(,R5)                                                        
         AR    R5,RF                                                            
         J     TRNSADD4                                                         
                                                                                
TRNSADD6 MVC   ACBGACT,GLDULA      Unit G account                               
         MVC   ACBOFFC,GLDOFFC     Unit G office                                
         MVC   ACBSACT,SPACES                                                   
         MVC   ACBSACT(2),TRNKULA  Unit S unit/ledger                           
         CLC   =C'SJ',TRNKUNT                                                   
         JNE   *+10                                                             
         MVC   ACBSACT,GLDCULA     Media name                                   
                                                                                
         CLI   QOPT2,NO                                                         
         JNE   *+10                                                             
         MVC   ACBOFFC,SPACES                                                   
                                                                                
         CLI   QOPT1,ALTRNATE      Unit S vs Unit G                             
         JE    TRNSADD8                                                         
         CLI   QOPT1,GLUONLY       Unit S vs Unit G, GLU only                   
         JE    TRNSADD8            Yes                                          
         LLC   RF,LENCP                                                         
         AHI   RF,1                Add 2 for U/L less 1 for EX instr.           
         EXMVC RF,ACBSACT,TRNKULA  Move in client/product                       
         CLC   =C'SJ',TRNKUNT                                                   
         JE    *+10                                                             
         MVC   ACBSACT,TRNKULA     Not SJ, so move whole account                
         CLC   =C'SJ',TRNKUNT                                                   
         JNE   TRNSAD6C                                                         
         TM    TRSSTAT,TRSSGLIP    GLU                                          
         JZ    TRNSAD6C            No                                           
         MVC   ACBSACT,GLDCULA                                                  
                                                                                
X        USING GLBRECD,KEY2                                                     
TRNSAD6C CLI   QOPT3,XTRA                                                       
         JNE   TRNSADD7                                                         
         SAM24                                                                  
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,LASTKEY,LASTKEY                        
         GOTOR =V(ACGLKEY),DMCB,(X'80',TRNKEY),GLDEL,KEY1,KEY2,        +        
               (L'SVLDGS,SVLDGS),ADCOMFAC                                       
         GOTOR DATAMGR,DMCB,DMKEY,ACCDIR,WORK,WORK                              
         CLC   LASTKEY,WORK                                                     
         JE    TRNSAD6R                                                         
         GOTOR DATAMGR,DMCB,DMREAD,ACCDIR,LASTKEY,IOKEY                         
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
TRNSAD6R SAM31                                                                  
         MVC   ACBXTRA(1),X.GLBKMEDC                                            
         CLI   TRNKLDG,C'J'                                                     
         JE    *+10                                                             
         MVC   ACBXTRA,X.GLBKSCNT                                               
         DROP  X                                                                
                                                                                
TRNSADD7 LA    RE,ACBGDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,ACBGCR                                                        
         J     TRNSADD9                                                         
                                                                                
TRNSADD8 LA    RE,ACBSDR                                                        
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,ACBSCR                                                        
                                                                                
TRNSADD9 ZAP   0(L'ACBSCR,RE),TRNAMNT                                           
         DROP  R3,R4               TRNELD, TRSELD                               
                                                                                
TRNSADDA GOTOR BIN31,DMCB,ACCBREC,AGLBAL,#OFACCS,(1,ACBLNQ),           +        
               L'ACBKEY,MAX#ACCS                                                
         MVC   #OFACCS,8(R1)                                                    
         ICM   R8,15,0(R1)         R8 = A(Record added or found)                
         JNZ   *+6                                                              
         DC    H'00'               Table full                                   
                                                                                
BIN      USING ACBD,R8                                                          
         TM    0(R1),X'80'         Record not found?                            
         JO    TRNSADDJ            No record found, so added                    
         CLI   QOPT1,DUMPLOAD                                                   
         JNE   TRNSADDE                                                         
         AP    BIN.ACBGDR,ACBGDR                                                
         AP    BIN.ACBGCR,ACBGCR                                                
         J     TRNSADDX                                                         
                                                                                
TRNSADDE AP    BIN.ACBSDR,ACBSDR                                                
         AP    BIN.ACBSCR,ACBSCR                                                
                                                                                
TRNSADDJ CLI   QOPT1,BALANCE       Balance report                               
         JNE   TRNSADDK            Not this option                              
         CLC   ACBMOA,=X'FFFF'                                                  
         JE    TRNSADDX            Done                                         
         MVC   ACBMOA,=X'FFFF'                                                  
         J     TRNSADDA            Add total                                    
                                                                                
                                                                                
TRNSADDK CLI   QOPT1,ALTRNATE      Unit G vs Unit S                             
         JNE   TRNSADDX                                                         
         GOTOR DATCON,DMCB,(1,GLDDATE),(20,WORK)                                
         MVC   ACBREF(6),WORK+2                                                 
         MVC   ACBSACT,GLDCULA                                                  
         GOTOR BIN31,DMCB,ACCBREC,AGLBAL,#OFACCS,(1,ACBLNQ),           +        
               L'ACBKEY,MAX#ACCS                                                
         MVC   #OFACCS,8(R1)                                                    
         ICM   R8,15,0(R1)         R8 = A(Record added or found)                
         JNZ   *+6                                                              
         DC    H'00'               Table full                                   
         TM    0(R1),X'80'         Record not found?                            
         JO    *+2                 No record, we have a PROBLEM!                
         AP    BIN.ACBSDR,ACBSDR                                                
         AP    BIN.ACBSCR,ACBSCR                                                
TRNSADDX J     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* Add GLBRECs balance information to BIN table High core                        
***********************************************************************         
         USING GLBRECD,R2                                                       
GLBADD   DS    0H                                                               
         NMOD1 0,*GLBADD*                                                       
         L     RC,0(R1)                                                         
         SAM31                     Switch to 31 bit mode                        
         MVC   ACBKEY,SPACES                                                    
         MVC   ACBDA#,DA           SAVE DISK ADDRESS                            
         XC    ACBREF,ACBREF       CLEAR REF DATE                               
         L     R2,DAREC                                                         
         MVI   ACBGACT,C'G'                                                     
         MVC   ACBGACT+1(L'GLBKGLA),GLBKGLA                                     
         MVC   ACBOFFC,GLBKGOFF                                                 
         MVC   ACBSACT,SPACES                                                   
         MVC   ACBSACT(2),GLBKSULA Default is Unit/Ledger only                  
         CLC   GLBKGSPC,SPACES     Is it production ?                           
         JNE   *+10                No                                           
         MVC   ACBSACT,GLBKGSJ+1   Yes, SJ - media name                         
         ZAP   ACBGDR,PKZERO                                                    
         ZAP   ACBGCR,PKZERO                                                    
         ZAP   ACBSDR,PKZERO                                                    
         ZAP   ACBSCR,PKZERO                                                    
         CLI   QOPT2,NO                                                         
         JNE   *+10                                                             
         MVC   ACBOFFC,SPACES                                                   
                                                                                
         USING BUKELD,R3                                                        
         LA    R3,GLBRFST                                                       
GLBADD20 CLI   0(R3),EOR                                                        
         JE    GLBADDX             Done                                         
         CLI   0(R3),BUKELQ        X'45'                                        
         JNE   GLBADD60                                                         
         CLC   BUKMOS,MOASTR                                                    
         JL    GLBADD60            Skip this one                                
         CLC   BUKMOS,MOAEND                                                    
         JH    GLBADD60            Skip this one                                
         MVC   ACBMOA,BUKMOS                                                    
         ZAP   ACBSDR,BUKDR                                                     
         ZAP   ACBSCR,BUKCR                                                     
                                                                                
         CLC   BUKMOS,GLMOA        Prior to New GL date                         
         JNL   GLBADD24            Skip, can't do it this way                   
         CLI   NEWOFF,YES                                                       
         JNE   *+6                 Okay if one character                        
         DC    H'00'               Should not have any                          
         CLI   QOPT1,BALANCE                                                    
         JE    GLBADD60            Skip for BALANCE option                      
         CLI   QOPT1,DUMPLOAD                                                   
         JE    GLBADD60            Can't do in this option                      
                                                                                
GLBADD24 CLI   QOPT1,DUMPLOAD      Unit G & S vs Special                        
         JNE   GLBADD40            No, Unit G vs Special                        
         MVC   ACBSACT(2),=C'SJ'                                                
         LLC   RF,LENCP            Length of client product                     
         BCTR  RF,0                                                             
         EXMVC RF,ACBSACT+2,GLBKCP                                              
         CLI   QOPT3,XTRA                                                       
         JNE   *+10                                                             
         MVC   ACBXTRA(1),GLBKMEDC                                              
                                                                                
GLBADD26 CLC   GLBKGSPC,SPACES     Is it production ?                           
         JE    GLBADD40            Yes, so SJ/cli/prd                           
         MVC   ACBSACT,GLBKSULA    No, so unit S full account                   
         CLI   QOPT3,XTRA                                                       
         JNE   *+10                                                             
         MVC   ACBXTRA,GLBKSCNT                                                 
         DROP  R3                                                               
                                                                                
GLBADD40 GOTOR BIN31,DMCB,ACCBREC,AGLBAL,#OFACCS,(1,ACBLNQ),           +        
               L'ACBKEY,MAX#ACCS                                                
         MVC   #OFACCS,8(R1)                                                    
         ICM   R8,15,0(R1)         R8 = A(Record added or found)                
         JNZ   *+6                                                              
         DC    H'00'               Table full                                   
                                                                                
BIN      USING ACBD,R8                                                          
         TM    0(R1),X'80'         Record not found?                            
         JO    GLBADD60            No record found, so added                    
         AP    BIN.ACBSDR,ACBSDR                                                
         AP    BIN.ACBSCR,ACBSCR                                                
                                                                                
GLBADD60 LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     GLBADD20                                                         
                                                                                
GLBADDX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* Get Peel date from S transactions and set it to GLMOA                         
***********************************************************************         
         USING ACTRECD,R2                                                       
GLMOARTN NTR1                                                                   
         SAM31                                                                  
         CLI   QOPT1,BLANK         option provided?                             
         JE    GLMOAXIT            YES, EXIT                                    
*                                                                               
         LA    R2,IOKEY            Set IOKEY to get S Transactions              
         MVC   IOKEY,SPACES                                                     
         MVC   ACTKCPY,RCCOMPFL    Set Company Code                             
         MVI   ACTKUNT,C'S'        set Unit                                     
         MVC   COMMAND,DMRDHI      Set mode TO Read High                        
*                                                                               
GLMOA10  GOTOR DATAMGR,DMCB,COMMAND,ACCDIR,IOKEY,IOKEY                          
         JE    *+6                 found record?                                
         DC    H'00'               no,abort the process                         
*                                                                               
         LA    R2,IOKEY            set IOKEY t o get Master record              
         CLC   ACTKCPY,RCCOMPFL    Change in Company code                       
         JNE   GLMOAXIT            Yes, exit                                    
         CLI   ACTKUNT,C'S'        Change in Unit Code                          
         JNE   GLMOAXIT            Yes, exit                                    
*                                                                               
         GOTOR DATAMGR,DMCB,GETREC,ACCMST,ACTKDA,DAREC,IOWORK                   
         JE    *+6                 Found Master record?                         
         DC    H'00'               No, abort the process                        
*                                                                               
         L     R2,DAREC            Get Master record                            
         LA    R3,ACTRFST          Point to First element of record             
         USING ABLELD,R3                                                        
GLMOA20  CLI   ABLEL,EOR           End of Element?                              
         JE    GLMOA50             Yes, read next record                        
         CLI   ABLEL,ABLELQ        No, Account Balance Element found?           
         JNE   GLMOA40             No, bump to next element                     
         CLI   ABLLN,ABLLN3Q       If no peeled for date is on ABLELD?          
         JL    GLMOA40             Get next element                             
         CLC   ABLPLDT,GLMOA       Yes, compare peel date with GLMOA            
         JNH   GLMOA40             If old, read next element                    
         MVC   GLMOA,ABLPLDT       If latest, set GLMOA to Peel off dt          
         MVI   SETMOA,YES                                                       
*                                                                               
GLMOA40  LLC   R0,ABLLN            Clear off & Get element length               
         AR    R3,R0               Set pointer to next element                  
         J     GLMOA20             Bump to next element                         
*                                                                               
GLMOA50  MVC   COMMAND,DMRSEQ      Set mode to Read Sequential                  
         J     GLMOA10             Read next record                             
*                                                                               
GLMOAXIT XIT1                                                                   
         DROP  R2,R3               Drop R2,R3                                   
         EJECT                                                                  
*=====================================================================*         
* Literals                                                                      
*=====================================================================*         
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* Print out balances                                                            
***********************************************************************         
SHOWBAL  DS    0H                                                               
         NMOD1 0,*SHOBAL*                                                       
         L     RC,0(R1)                                                         
         SAM31                                                                  
         MVI   RCSUBPRG,1          Default headings                             
         CLI   QOPT1,ALTRNATE                                                   
         BNE   *+8                                                              
         MVI   RCSUBPRG,2          Alternate headings                           
         CLI   QOPT1,GLUONLY                                                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,6          GLU transactions only                        
         CLI   QOPT1,BALANCE                                                    
         BNE   *+8                                                              
         MVI   RCSUBPRG,3          Balance headings                             
         CLI   QOPT1,DUMPLOAD                                                   
         BNE   SHOWB080                                                         
         MVI   RCSUBPRG,4          Dump/Load headings                           
         CLI   QOPT3,XTRA                                                       
         JNE   SHOWB080                                                         
         MVI   RCSUBPRG,5          Extra detail headings                        
                                                                                
SHOWB080 L     R8,AGLBAL                                                        
         ICM   R0,15,#OFACCS                                                    
         JZ    SHOWBXIT                                                         
SHOWB090 JAS   R4,SHOWB100             POPULATE ACCOUNT FOR PRINT               
                                                                                
*AH debug for AVNY start                                                        
*        CLI   BIN.ACBGACT+L'ACBGACT-1,C' '                                     
*        JNH   SHOWB180                                                         
*        CLC   BIN.ACBOFFC(1),BIN.ACBGACT+L'ACBGACT-1                           
*        JE    SHOWB180                                                         
*        MVC   P+108(8),=CL8'E-Office'                                          
*AH debug for AVNY end                                                          
                                                                                
SHOWB180 CLC   BIN.ACBMOA,GLMOA                                                 
         JNL   SHOWB182                                                         
         CLI   NEWOFF,YES                                                       
         JE    SHOWB210            Skip                                         
                                                                                
SHOWB182 CP    BIN.ACBSDR,BIN.ACBGDR   Specials vs Unit S                       
         JNE   SHOWB200                                                         
         CP    BIN.ACBSCR,BIN.ACBGCR                                            
         JE    SHOWB210                                                         
                                                                                
SHOWB200 CLC   BIN.ACBMOA,=X'FFFF'     Don't accum if running totals            
         JE    *+10                                                             
         AP    #OFWRNS,=P'1'                                                    
*        SAM24                                                                  
*        GOTOR ACREPORT                                                         
*        SAM31                                                                  
                                                                                
         CLI   QOPT1,ALTRNATE                                                   
         JE    SHOWB204                                                         
         CLI   QOPT1,DUMPLOAD                                                   
         JNE   SHOWB215                                                         
         GOTO1 VHEXOUT,DMCB,BIN.ACBDA#,DAPRNT,L'ACBDA#,(31,0),0                 
         MVC   0(8,R3),DAPRNT                                                   
         SAM24                                                                  
         GOTOR ACREPORT                                                         
         SAM31                                                                  
         CLI   RCWRITE,C'Y'                                                     
         JNE   SHOWB220                                                         
         GOTOR UPDTSS00                Fix Special records if WRITE=Y           
         J     SHOWB220                                                         
*                                      PRINT INVIDUAL TRAN RECS                 
SHOWB204 SAM24                                                                  
         GOTOR ACREPORT                                                         
SHOWB205 SAM31                                                                  
         AHI   R8,ACBLNQ               Next entry                               
         OC    BIN.ACBREF,BIN.ACBREF   REF DATE                                 
         JZ    SHOWB207                                                         
         SHI   R0,1                    ADJUST 1 ENTRY PROCESSED                 
         JAS   R4,SHOWB100             POPULATE ACCOUNT FOR PRINT               
         MVC   0(6,R3),BIN.ACBREF      REF DATE                                 
         GOTO1 VHEXOUT,DMCB,BIN.ACBDA#,DAPRNT,L'ACBDA#,(31,0),0                 
         MVC   9(8,R3),DAPRNT                                                   
         SAM24                                                                  
         GOTOR ACREPORT                                                         
         SAM31                                                                  
         CLI   RCWRITE,C'Y'                                                     
         JNE   SHOWB205                                                         
         GOTOR UPDTSG00                Fix Unit G records if WRITE=Y            
         J     SHOWB205                                                         
                                                                                
SHOWB207 SHI   R8,ACBLNQ               ADJUST BACK 1 ENTRY                      
         MVC   P,SPACES                Print spaces after each block            
         SAM24                                                                  
         GOTOR ACREPORT                                                         
         SAM31                                                                  
         J     SHOWB220                                                         
*                                                                               
SHOWB210 CLI   QOPT7,ALL           Show all                                     
         JNE   SHOWB220            Yes                                          
SHOWB215 SAM24                                                                  
         GOTOR ACREPORT                                                         
                                                                                
SHOWB220 SAM31                                                                  
         AHI   R8,ACBLNQ           Next entry                                   
         LTR   R0,R0                                                            
         JZ    SHOWBXIT                                                         
         OC    BIN.ACBREF,BIN.ACBREF   REF DATE                                 
         JZ    *+12                                                             
         SHI   R0,1                                                             
         J     SHOWB220                                                         
         BRCT  R0,SHOWB090                                                      
         J     SHOWBXIT                EXIT                                     
*--------------------------*                                                    
* POPULATE PRINT AREA RTN  *                                                    
*--------------------------*                                                    
SHOWB100 MVC   P+1(14),BIN.ACBGACT     Unit G account                           
         OC    BIN.ACBSACT,SPACES                                               
         OC    BIN.ACBOFFC,SPACES                                               
         MVC   WORK,BIN.ACBMOA                                                  
         MVI   WORK+2,1                                                         
         CLC   BIN.ACBMOA,=X'FFFF'     Special case                             
         JNE   SHOWB110                                                         
         MVC   P+1(6),=C'Totals'                                                
         MVC   P+16(8),SPACES                                                   
         J     SHOWB120                                                         
                                                                                
SHOWB110 SAM24                                                                  
         GOTOR DATCON,DMCB,(1,WORK),(6,P+16)                                    
         SAM31                                                                  
                                                                                
SHOWB120 MVC   P+23(2),BIN.ACBOFFC     Office code                              
         MVC   P+26(14),BIN.ACBSACT    Unit S account                           
                                                                                
         LA    R3,P+43                                                          
         CLI   QOPT3,XTRA                                                       
         JNE   SHOWB126                                                         
         LA    R3,P+52                                                          
         MVC   P+43(9),BIN.ACBXTRA                                              
                                                                                
SHOWB126 MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),BIN.ACBSDR                                              
         MVC   0(17,R3),WORK+2                                                  
         AHI   R3,18                                                            
                                                                                
         MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),BIN.ACBSCR                                              
         MVC   0(17,R3),WORK+2                                                  
         AHI   R3,18                                                            
                                                                                
         MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),BIN.ACBGDR                                              
         MVC   0(17,R3),WORK+2                                                  
         AHI   R3,18                                                            
                                                                                
         MVC   WORK(19),=X'4040202020202020202020202020214B202060'              
         ED    WORK(19),BIN.ACBGCR                                              
         MVC   0(17,R3),WORK+2                                                  
         AHI   R3,18                                                            
         BR    R4                                                               
*                                                                               
SHOWBXIT SAM24                                                                  
         MVC   P,SPACES                                                         
         GOTOR ACREPORT                                                         
         XIT1                                                                   
         DROP  BIN                                                              
         EJECT ,                                                                
*=====================================================================*         
* Literals                                                                      
*=====================================================================*         
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* Update - Unit S Balance to Unit G                                             
***********************************************************************         
         USING TRNRECD,R2                                                       
         USING TRNELD,R3                                                        
BIN      USING ACBD,R8                                                          
UPDTSG00 NTR1                                                                   
         SAM31                                                                  
         CP    BIN.ACBSDR,BIN.ACBGDR   Unit S vs Unit G                         
         JNE   *+14                                                             
         CP    BIN.ACBSCR,BIN.ACBGCR                                            
         JE    UPDTSG99                                                         
                                                                                
         MVC   DA,BIN.ACBDA#                                                    
         XC    DMCB,DMCB                                                        
         GOTOR DATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,DAREC,IOWORK               
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     R2,DAREC                                                         
         LA    R3,TRNRFST                                                       
         CLI   0(R3),TRNELQ        Is it a tranaction ?                         
         JNE   *+2                 Error during update of Unit G                
         LA    RE,BIN.ACBSDR                                                    
         TM    TRNSTAT,TRNSDR                                                   
         JO    *+8                                                              
         LA    RE,BIN.ACBSCR                                                    
         ZAP   TRNAMNT,0(L'ACBSCR,RE)                                           
                                                                                
         GOTOR DATAMGR,DMCB,(X'80',PUTREC),ACCMST,DA,DAREC,IOWORK               
         AP    #OFUPDT,=P'1'                                                    
                                                                                
UPDTSG99 J     EXIT                                                             
         DROP  R2,R3,BIN                                                        
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
***********************************************************************         
* Update - Unit S Balance to Special records                                    
***********************************************************************         
         USING GLBRECD,R2                                                       
         USING BUKELD,R3                                                        
BIN      USING ACBD,R8                                                          
UPDTSS00 NTR1                                                                   
         SAM31                                                                  
         CP    BIN.ACBSDR,BIN.ACBGDR   Unit S vs Unit G                         
         JNE   *+14                                                             
         CP    BIN.ACBSCR,BIN.ACBGCR                                            
         JE    UPDTSS99                                                         
                                                                                
         MVC   DA,BIN.ACBDA#                                                    
         XC    DMCB,DMCB                                                        
         GOTOR DATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,DAREC,IOWORK               
         JE    *+6                                                              
         DC    H'00'                                                            
                                                                                
         L     R2,DAREC                                                         
         LA    R3,GLBRFST                                                       
UPDTSS10 CLI   0(R3),EOR                                                        
         JE    *+2                 Element not found so error!                  
         CLI   0(R3),BUKELQ        X'45'                                        
         JNE   UPDTSS20                                                         
         CLC   BUKMOS,BIN.ACBMOA                                                
         JNE   UPDTSS20            Skip this one                                
         ZAP   BUKDR,BIN.ACBGDR(L'ACBGDR)                                       
         ZAP   BUKCR,BIN.ACBGCR(L'ACBGCR)                                       
         J     UPDTSS30                                                         
UPDTSS20 LLC   RF,1(,R3)                                                        
         AR    R3,RF                                                            
         J     UPDTSS10                                                         
                                                                                
UPDTSS30 GOTOR DATAMGR,DMCB,(X'80',PUTREC),ACCMST,DA,DAREC,IOWORK               
         AP    #OFSPUP,=P'1'                                                    
                                                                                
UPDTSS99 J     EXIT                                                             
         DROP  R2,R3,BIN                                                        
         LTORG                                                                  
         EJECT ,                                                                
*                                                                               
*=====================================================================*         
* Send warnings to ACC people                                                   
*=====================================================================*         
SENDMAIL DS    0H                                                               
         NMOD1 0,*SNDEML*                                                       
         L     RC,0(R1)                                                         
         CLI   QOPT4,C'N'                                                       
         BE    SENDMXIT                                                         
         MVC   WARNTO(L'MAILCALL),MAILCALL                                      
         MVC   WARNMSG2,P                                                       
         MVC   EMSG,WARNMSG                                                     
         GOTOR DATAMGR,DMCB,=C'OPMSG',(L'EMSG,EMSG)                             
SENDMXIT XIT1                                                                   
                                                                                
         EJECT ,                                                                
         DROP  R9,RA,RB,RC                                                      
***********************************************************************         
* E-mail warning message block                                        *         
***********************************************************************         
WARNMSG  DS    0CL(L'EMSG)         SOME CONSTANT VALUES FOR E-MAIL              
WARNAUTO DC    C'AUTONOTE*'                                                     
WARNTO   DC    CL30' '             THIS IS VAR LEN, COMMA SEPARATED             
WARNMSG1 DC    CL12'ACGB report'                                                
WARNMSG2 DC    CL(L'EMSG-(*-WARNMSG))' '                                        
         EJECT ,                                                                
GLOBALS  DS    0D                                                               
                                                                                
MAXGL    EQU   16                  For 16K block                                
K        EQU   1024                                                             
TURNOFF  EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ALL      EQU   C'A'                                                             
BLANK    EQU   C' '                Oopt1                                        
ALTRNATE EQU   C'A'                Qopt1                                        
DUMPLOAD EQU   C'D'                Qopt1                                        
GLUONLY  EQU   C'G'                Qopt1                                        
BALANCE  EQU   C'B'                Qopt1                                        
XTRA     EQU   C'X'                Qopt3                                        
EOT      EQU   X'FF'                                                            
EOR      EQU   0                                                                
DUMPMAX  DC    P'10'                                                            
DUMPCNT  DC    PL8'0'                                                           
MAXOUT   DC    PL8'0'                                                           
MAXSDMP  DC    PL8'0'                                                           
                                                                                
SETMOA   DC    C'N'                                                             
                                                                                
HEXTAB   DC    C'0123456789ABCDEF'                                              
                                                                                
BIN31    DC    V(BINSRCH)                                                       
ACGLKEY  DC    V(ACGLKEY)                                                       
ACGLPOST DC    V(ACGLPOST)                                                      
ADDBUK   DC    V(ACADDBUK)                                                      
PRNTBL   DC    V(PRNTBL)                                                        
VPRINT   DC    V(PRINT)                                                         
VHEXOUT  DC    V(HEXOUT)                                                        
                                                                                
DAREC    DC    A(IOREC)                                                         
DA       DS    F                                                                
                                                                                
DAPRNT   DS    CL8                                                              
                                                                                
DMIND    DS    X                                                                
COMMAND  DC    C'        '                                                      
GETREC   DC    C'GETREC  '                                                      
PUTREC   DC    C'PUTREC  '                                                      
ADDREC   DC    C'ADDREC  '                                                      
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
CTFILE   DC    C'CTFILE  '                                                      
                                                                                
#OFACCS  DC    A(0)                                                             
MAX#ACCS DC    A(0)                                                             
MAX#OFFS EQU   50                                                               
                                                                                
#OFERRS  DC    PL8'0'                                                           
#OFWRNS  DC    PL8'0'                                                           
#OFUPDT  DC    PL8'0'                                                           
#OFSPUP  DC    PL8'0'                                                           
                                                                                
MAILCALL DC    CL80'JSHA,RGUP,RKEJ                     :'                       
EMSG     DS    CL110               E-mail message to LOTUS NOTES                
         LTORG                                                                  
         EJECT ,                                                                
         DS    0F                                                               
         DC    C'IOIOIOIO'                                                      
IO       DS    XL(4*K)                                                          
         DC    C'DADADADA'                                                      
IOREC    DS    XL(4*K)                                                          
                                                                                
         DC    C'CPY*CPY*'                                                      
IOCPY    DS    XL(4*K)                                                          
         DC    C'LDG*LDG*'                                                      
IOLDG    DS    XL(4*K)                                                          
         DC    C'IOA*IOA*'                                                      
                                                                                
         EJECT                                                                  
***********************************************************************         
* Application working storage                                                   
***********************************************************************         
ACGBD    DSECT                                                                  
MOASTR   DS    XL2                 From QMOSSTRT (packed YYMM)                  
MOAEND   DS    XL2                 From QMOSEND  (packed YYMM)                  
MOAGLOLD DS    XL2                 Oldest MOA from Unit G                       
                                                                                
PKZERO   DS    PL1                                                              
PKONE    DS    PL1                                                              
ONEXONLY DS    CL1                 Yes/No                                       
                                                                                
UPSI     DS    XL1                                                              
AGYOPTS  DS    XL1                 (MCAGCOPT)                                   
                                                                                
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                                                             
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                                                             
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                                                             
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                                                             
LEVLNQ   EQU   *-LEVELS                                                         
                                                                                
LDGTLVS  DS    0XL4                                                             
LDGTLV1L DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LDGTLV2L DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LDGTLV3L DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LDGTLV4L DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
                                                                                
LDGT#LVS DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
                                                                                
AIOCPY   DS    A                                                                
AIOLDG   DS    A                                                                
                                                                                
APARM    DS    A                                                                
SVRE     DS    A                                                                
SVRF     DS    A                                                                
AGLRECS  DS    A                                                                
AGLBAL   DS    A                                                                
HELLO    DS    A                                                                
                                                                                
AMT      DS    PL8                                                              
PTODAY   DS    0PL3                ** TODAY'S DATE (PWOS) **                    
PTODAYYY DS    PL1                 YEAR                                         
PTODAYMM DS    PL1                 MONTH                                        
PTODAYDD DS    PL1                 DAY                                          
                                                                                
ETODAY   DS    0CL6                ** TODAY'S DATE (EBCDIC) **                  
ETODAYYY DS    CL2                 YEAR                                         
ETODAYMM DS    CL2                 MONTH                                        
ETODAYDD DS    CL2                 DAY                                          
GLMOA    DS    XL2                                                              
                                                                                
RECTYPE  DS    X                   EXTRACTED RECORD TYPE                        
COMPANY  DS    X                   Company hex code                             
ACFILE   DS    C                   ACCn FILE                                    
NEWOFF   DS    C                   Yes / No                                     
LENCP    DS    AL1                 Length client product                        
LASTUL   DS    CL2                                                              
ALPHA    DS    CL2                 Company alpha code                           
CLOGO    DS    CL8                 Log on                                       
LASTCPY  DS    XL1                                                              
FIRSTX   DS    CL1                 Yes / No                                     
COUNT    DS    PL3                                                              
DUMP#    DS    XL1                                                              
                                                                                
LASTKEY  DS    XL(ACTRFST-ACTRECD)                                              
IOKEY    DS    XL(ACTRFST-ACTRECD)                                              
IOWORK   DS    XL96                                                             
KEY1     DS    XL(ACTRFST-ACTRECD)                                              
KEY2     DS    XL(ACTRFST-ACTRECD)                                              
                                                                                
SVLDGS   DS    CL48                                                             
ACCBREC  DS    CL(ACBLNQ)                                                       
BINREC   DS    XL(2*K)                                                          
ACGLDEND DS    0X                  End                                          
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* DSECT                                                                         
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
CPYTABD  DSECT                                                                  
CPYCODE  DS    X                   Hex company code                             
CPYMOA   DS    XL2                 New GL moa date                              
CPYTLNQ  EQU   *-CPYTABD                                                        
                                                                                
ACBD     DSECT                                                                  
ACBKEY   DS    0CL47                                                            
ACBGACT  DS    CL14                Unit G Account                               
ACBMOA   DS    XL2                 MOA                                          
ACBOFFC  DS    CL2                 Office code                                  
ACBSACT  DS    CL14                Unit S Account                               
ACBREF   DS    CL6                 GB REF NO.                                   
ACBXTRA  DS    CL9                 Extra info (QOPT3=X and QOPT1=D)             
ACBGDR   DS    PL8                 Debits  for Unit G                           
ACBGCR   DS    PL8                 Credits for Unit G                           
ACBSDR   DS    PL8                 Debits  for Unit S or GLBRECs                
ACBSCR   DS    PL8                 Credits for Unit S or GLBRECs                
ACBDA#   DS    XL4                 DISK ADDRESS - UNIT G ACCOUNTS               
ACBLNQ   EQU   *-ACBD                                                           
                                                                                
ACCMD    DSECT                                                                  
ACCMMSG  DS    CL34                                                             
ACCMBUK  DS    AL4                                                              
ACCMLNQ  EQU   *-ACCMD                                                          
                                                                                
ERRTABD  DSECT                                                                  
ERRTAB#  DS    AL1                                                              
ERRTMSG  DS    CL30                                                             
ERRTLNQ  EQU   *-ERRTABD                                                        
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACQD                                                                          
         PRINT OFF                                                              
       ++INCLUDE ACQD                                                           
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACGLPOSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGLPOSTD                                                      
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015ACREPGB02 11/04/20'                                      
         END                                                                    
