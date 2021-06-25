*          DATA SET CTMAD21    AT LEVEL 023 AS OF 07/08/04                      
*PHASE TA0C21A,*                                                                
         TITLE 'TA0C21 - $MAD PRESTO UPLOAD JOB'                                
TA0C21   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C21,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.  CURRENTLY, NOTHING NEEDS INITIALIZATION.                     
*                                                                               
INIT     NTR1  ,                                                                
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
         GOTO1 SETSYS,DMCB,=C'ACCOUNT',=CL8'ACCDIR',=CL8'ACCMST'                
         BE    INIT05                                                           
* IF THERE WAS AN ERROR, CHECK IF ERROR SWITCHING SYSTEMS                       
* IF ERROR SWITCHING SYSTEMS, CHANGE TO FILE READ-ONLY                          
         CLC   MDACTION,=Y(ERSWITCH)                                            
         BNE   EXIT                                                             
         XC    MDACTION,MDACTION                                                
         B     ERRUPRO                                                          
*                                                                               
INIT05   MVI   LENKEY,L'ACCKEY                                                  
         MVI   DISPDSKA,ACCKDA-ACCRECD                                          
*                                  DON'T ALLOW UPLOAD TO READ ONLY FILE         
         GOTO1 GETFACT,DMCB,(X'80',BYTE),(SYSNUM,F#SEIND)                       
         TM    BYTE,SEIRONLY                                                    
         BO    ERRUPRO                                                          
*                                                                               
*        SET UP GLOBAL VALUES FOR THE COMPANY/UNIT/LEDGER.                      
*                                                                               
         L     RF,AUTL             FIND COMPANY IN UTL                          
         USING UTLD,RF                                                          
         MVC   CUL(1),TAGYB                                                     
         MVC   CTRY,TCTRY                                                       
         DROP  RF                                                               
*                                                                               
         LA    R6,BIGKEY                                                        
         USING CPYRECD,R6                                                       
         MVC   CPYKEY,SPACES                                                    
         MVC   CPYKCPY,CUL         READ COMPANY RECORD                          
         GOTO1 HIGH                                                             
         CLC   CPYKEY(CPYKEND),KEYSAVE                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         LA    R3,CPYELQ                                                        
         GOTO1 GETELEM,DMCB,(R3),0                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CPYELD,R6                                                        
         MVC   CUL+1(2),CPYPROD                                                 
         CLI   CPYLN,CPYLN3Q       TEST ELEMENT LONG ENOUGH                     
         BL    INIT10              FOR EURO CURRENCY SETTINGS                   
*                                                                               
*&&UK                                                                           
         MVC   COMPCUR,CPYCURR                                                  
         MVC   COMPCURS,CPYCURRS                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA9,CPYSTAT9                                                
*&&                                                                             
         DROP  R6                                                               
*                                                                               
INIT10   GOTO1 SETHEIR             EXTRACT ACCOUNT LEDGER HEIRARCHY             
         BNE   ERROBJ                                                           
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  EVERYTHING WILL HAPPEN HERE           
* BECAUSE WE NEED ONLY ONE TRANSACTION TO READ THE TEMPFILE AND WRITE           
* THE JOB AND ESTIMATE RECORDS.                                                 
*                                                                               
PROCSTRT NTR1                                                                   
         GOTO1 WRKLOC              LOCATE RECENTLY UPLOADED TEMP FILE           
         BNE   EXIT                                                             
*                                                                               
         BAS   RE,PROCTMP          PROCESS TEMP FILE OBJECTS                    
*                                                                               
*                                  PUT EOD OBJECT AS SIGN OF SUCCESS            
         GOTO1 PUTITEM,DMCB,A(ITEOD),0                                          
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT DOES NOTHING HERE.                    
*                                                                               
PROCMID  NTR1                                                                   
*                                                                               
PMX      B     XIT                                                              
         SPACE 3                                                                
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE FILLS THE FRAME WITH MAD OBJECTS FROM THE TEMP FILE.             
*                                                                               
PROCTMP  NTR1                                                                   
         MVI   ESTFLAG,C'N'        NO ESTIMATE RECORD YET                       
         MVI   HRSFLAG,C'N'        NO ESTIMATE HOURS RECORD YET                 
*                                                                               
         LA    R4,TMPDATA          R4 = A(MAD OBJECT DATA)                      
*                                                                               
*                                  GET JOB OBJECT FROM TEMP FILE                
         GOTO1 WRKGET,DMCB,TMPHDR                                               
         CLC   TMPHDR(8),=C'*EOFEOF*'  IF END OF TEMP FILE THEN ERROR           
         BE    ERREOT                                                           
*                                  IF NOT JOB OBJECT THEN ERROR                 
         CLC   TMPTYPE,=A(ITPRRTJO)                                             
         BNE   ERRUOBJ                                                          
*                                                                               
         BAS   RE,PROCJOB          PROCESS JOB OBJECT                           
*                                                                               
*                                  GET NEXT OBJECT FROM TEMP FILE               
PT10     LA    RE,TMPAREA                                                       
         LA    RF,L'TMPAREA        PRE-ZERO OBJECT AREA                         
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 WRKGET,DMCB,TMPHDR                                               
         CLC   TMPHDR(8),=C'*EOFEOF*'  IF END OF TEMP FILE THEN ERROR           
         BE    ERREOT                                                           
*                                                                               
         CLC   TMPTYPE,=A(ITEOD)   IF END OF DATA OBJECT THEN DONE              
         BE    PT90                                                             
*                                                                               
         CLC   TMPTYPE,=A(ITPRRTER)                                             
         BNE   PT20                                                             
         BAS   RE,PROCCOL          IF JOB COLUMN THEN CALL PROCCOL              
         B     PT10                                                             
*                                                                               
PT20     CLC   TMPTYPE,=A(ITPRRTCE)                                             
         BNE   PT40                                                             
         BAS   RE,PROCCELL         IF JOB CELL THEN CALL PROCCELL               
         B     PT10                                                             
*                                                                               
PT40     CLC   TMPTYPE,=A(ITPRRTHR)                                             
         BNE   PT10                GET NEXT IF NOT DEFINED                      
         BAS   RE,PROCHOUR         IF HOUR CELL THEN CALL PROCHOUR              
         B     PT10                                                             
*                                                                               
PT90     CLI   ESTFLAG,C'Y'        IF ESTIMATE RECORD TO BE ADDED               
         BNE   PT100                                                            
         BAS   RE,ADDEST           THEN ADD IT                                  
*                                                                               
PT100    CLI   HRSFLAG,C'Y'        IF EST HOURS RECORD TO BE ADDED              
         BNE   PTX                                                              
         BAS   RE,ADDHRS           THEN ADD IT                                  
*                                                                               
PTX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE JOB OBJECT.  IT EXTRACTS THE CLI/PRO/JOB           
* CODES AND VALIDATES THAT THE JOB EXISTS.  FINALLY IT CALLS DELESTS            
* TO DELETE THE EXISTING ESTIMATES FOR TH JOB.                                  
*                                                                               
PROCJOB  NTR1                                                                   
         USING ACTRECD,R2                                                       
         USING PRJOOBJD,R4                                                      
*                                                                               
         MVC   MYCLI,PRJOCLCD      EXTRACT CLI/PRO/JOB FROM MAD OBJECT          
         MVC   MYPRO,PRJOPRCD                                                   
         MVC   MYJOB,PRJOJBCD                                                   
*                                                                               
         MVC   BIGKEY,SPACES       BUILD KEY                                    
         LA    R2,BIGKEY                                                        
         MVC   ACTKCPY(3),CUL      CUL                                          
*                                                                               
         LA    RE,ACTKACT          ADD CLI/PRO/JOB TO KEY                       
         ZIC   RF,LCLI                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),MYCLI                                                    
         LA    RE,1(RE,RF)                                                      
         ZIC   RF,LPRO                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),MYPRO                                                    
         LA    RE,1(RE,RF)                                                      
         ZIC   RF,LJOB                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),MYJOB                                                    
*                                                                               
         GOTO1 HIGH                MAKE SURE VALID JOB                          
         CLC   KEYSAVE(L'ACCKEY),BIGKEY                                         
         BNE   ERRJOKY                                                          
*                                                                               
         BAS   RE,DELESTS          DELETE CURRENT ESTIMATES FOR JOB             
*                                                                               
PJX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE DELETES THE CURRENT SET OF ESTIMATE RECORDS FOR THE              
* JOB BEING UPLOADED.                                                           
*                                                                               
DELESTS  NTR1                                                                   
         MVI   RDUPDATE,C'Y'                                                    
         USING EVERECD,R2                                                       
         LA    R2,BIGKEY           BUILD ESTIMATE START KEY                     
         XC    BIGKEY,BIGKEY                                                    
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
         MVC   EVEKCLI,MYCLI                                                    
         MVC   EVEKPRO,MYPRO                                                    
         MVC   EVEKJOB,MYJOB                                                    
*                                                                               
         GOTO1 HIGH                READ FIRST ESTIMATE                          
*                                                                               
*                                  WHILE NOT END OF ESTIMATES                   
DE10     CLC   BIGKEY(EVEKTYPE-EVEKEY),KEYSAVE                                  
         BNE   DE90                                                             
*                                                                               
         GOTO1 GETREC              GET ESTIMATE RECORD AND DELETE IT            
         L     R3,AIO                                                           
         OI    EVERSTA-EVERECD(R3),X'80'                                        
         GOTO1 PUTREC                                                           
*                                                                               
         OI    EVEKSTA,X'80'       DELETE ESTIMATE KEY                          
         GOTO1 WRITE                                                            
*                                                                               
         GOTO1 SEQ                 GET NEXT KEY AND LOOP BACK                   
         B     DE10                                                             
*                                                                               
DE90     MVI   RDUPDATE,C'N'                                                    
*                                                                               
DEX      B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE JOB COLUMN OBJECT.                                 
*                                                                               
PROCCOL  NTR1                                                                   
         USING EVERECD,R3                                                       
         USING PREROBJD,R4                                                      
*                                                                               
         CLI   ESTFLAG,C'Y'                                                     
         BNE   PC05                                                             
         BAS   RE,ADDEST                                                        
*                                                                               
PC05     CLI   HRSFLAG,C'Y'        ANY PENDING ESTIMATE HOURS?                  
         BNE   PC10                                                             
         BAS   RE,ADDHRS                                                        
*                                                                               
PC10     L     R3,AIO                                                           
         XC    0(EVERFST-EVERECD+1,R3),0(R3)                                    
         MVC   EVERLEN,=Y(EVERFST-EVERECD)                                      
         MVI   EVEKTYP,EVEKTYPQ                                                 
         MVI   EVEKSUB,EVEKSUBQ                                                 
         MVC   EVEKCPY(3),CUL                                                   
         MVC   EVEKCLI,MYCLI                                                    
         MVC   EVEKPRO,MYPRO                                                    
         MVC   EVEKJOB,MYJOB                                                    
         MVC   EVEKTYPE,PRERETYP                                                
         GOTO1 DECIN,DMCB,PREREVER,3                                            
         BNE   ERRESOBJ                                                         
         MVC   EVEKVER,3(R1)                                                    
*                                                                               
         USING EUPELD,R6                                                        
         LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   EUPEL,EUPELQ                                                     
         MVI   EUPLN,EUPLNQ                                                     
*                                                                               
         CLI   PRERPERS,C' '       TEST FOR LAST CHANGE PERSON                  
         BH    PC12                YES                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,EUPADD)                                     
         GOTO1 DATCON,DMCB,(5,0),(1,EUPLAST)                                    
         MVC   EUPERS,=CL8'PRESTO'                                              
         B     PC15                                                             
*                                                                               
PC12     MVC   EUPERS,PRERPERS                                                  
         GOTO1 DATCON,DMCB,(0,PRERDATE),(1,EUPLAST)                             
         MVC   EUPADD,EUPLAST                                                   
*                                                                               
PC15     GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
         USING ENAELD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   ENAEL,ENAELQ                                                     
*                                                                               
         CLC   PRERNAM1,SPACES                                                  
         BE    PC20                                                             
         LA    RF,L'PRERNAM1+ENAME-ENAELD                                       
         STC   RF,ENALN                                                         
         MVI   ENANUM,1                                                         
         MVC   ENAME(L'PRERNAM1),PRERNAM1                                       
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
PC20     CLC   PRERNAM2,SPACES                                                  
         BE    PC30                                                             
         LA    RF,L'PRERNAM2+ENAME-ENAELD                                       
         STC   RF,ENALN                                                         
         MVI   ENANUM,2                                                         
         MVC   ENAME(L'PRERNAM2),PRERNAM2                                       
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
* FOR VERSION 2.5 APPROVAL CAN BE DONE IN PRESTO OR MAINFRAME                   
*                                                                               
PC30     CLC   PRERAPPR,SPACES                                                  
         BE    PC40                                                             
                                                                                
         USING EAPELD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   EAPEL,EAPELQ                                                     
         MVI   EAPLN,EAPLNQ                                                     
         GOTO1 DATCON,DMCB,(5,0),(1,EAPINP)                                     
         MVC   EAPPBY,PRERAPPR                                                  
         GOTO1 DATCON,DMCB,(0,PRERAPDT),(1,EAPDATE)                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
PC40     CLC   PRERPREP,SPACES                                                  
         BE    PC50                                                             
*                                                                               
         USING EPRELD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   EPREL,EPRELQ                                                     
         MVI   EPRLN,EPRLNQ                                                     
         GOTO1 DATCON,DMCB,(5,0),(1,EPRINP)                                     
         MVC   EPRPREP,PRERPREP                                                 
         GOTO1 DATCON,DMCB,(0,PRERPRDT),(1,EPRDATE)                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
PC50     XC    ESTCUR,ESTCUR       CLEAR THIS ESTIMATE'S CURRENCY               
         CLC   PRERCUR,SPACES                                                   
         BE    PC90                                                             
*                                                                               
*&&US*&& B     PC90                DON'T HAVE CURRENCY IN US                    
*&&UK                                                                           
         TM    COMPSTA7,CPYSSCNV   TEST FOR EURO CONVERSIONS                    
         BZ    PC90                NO                                           
*                                                                               
         CLC   PRERCUR,COMPCUR     TEST FOR PRIMARY CURRENCY                    
         BE    PC55                                                             
         CLC   PRERCUR,COMPCURS    TEST FOR SECONDARY CURRENCY                  
         BE    PC55                                                             
         DC    H'0'                BIG TROUBLE IF CAN'T RECOGNIZE IT            
*                                                                               
PC55     MVC   ESTCUR,PRERCUR                                                   
         MVC   EVERCUR,PRERCUR     SET CURRENCY INTO RECORD STATUS              
         USING FFTELD,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'ESTCUR                                 
         MVI   FFTTYPE,FFTTACUR    BUILD ASSOCIATED CURRENCY FREE FORM          
         MVI   FFTDLEN,L'ESTCUR                                                 
         MVC   FFTDATA(L'ESTCUR),ESTCUR                                         
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*&&                                                                             
*                                                                               
PC90     MVI   ESTFLAG,C'Y'        ESTIMATE RECORD TO BE ADDED                  
*                                                                               
PCX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE CELL OBJECT.                                       
*                                                                               
PROCCELL NTR1                                                                   
         USING EDAELD,R6                                                        
         USING PRCEOBJD,R4                                                      
*                                                                               
         CLI   HRSFLAG,C'Y'           ANY ESTIMATE HOURS PENDING?               
         BNE   *+8                    NO                                        
         BAS   RE,ADDHRS              YES, ADD IT                               
*                                                                               
         LA    R6,ELEMENT             BUILD ELEMENT                             
         XC    ELEMENT,ELEMENT                                                  
         MVI   EDAEL,EDAELQ                                                     
         MVI   EDALN,EDALNQ1                                                    
         MVI   EDATYPE,EDATWORK                                                 
         MVC   EDAWORK,PRCEWCOD                                                 
         GOTO1 HEXIN,DMCB,PRCEAMNT,FULL,8                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   EDACOMM,DUB                                                      
         ZAP   EDANCOM,=P'0'          INITIALIZE IN CASE NEEDED                 
*&&UK                                                                           
         CLI   PRCECOVR,C'Y'          IS THERE A COMM OVERRIDE?                 
         BNE   PCL10                  NO                                        
         MVI   EDALN,EDALNQ2          YES, CHANGE LENGTH                        
         OI    EDATYPE,EDATCOHE       SET TYPE                                  
         GOTO1 HEXIN,DMCB,PRCECOMM,FULL,8                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   EDANCOM,DUB            STORE IT IN NON-COMMISSIONABLE            
*                                                                               
*&&                                                                             
*                                                                               
PCL10    CLC   PRCESUFF,=C'00'                                                  
         BE    PCL20                                                            
         MVI   EDALN,EDALNQ3                                                    
         OI    EDATYPE,EDATSUB                                                  
         TM    EDATYPE,EDATCOHE    IS THERE A COMMISSION OVERRIDE?              
         BO    *+10                YES, DON'T CLEAR EDANCOM                     
         ZAP   EDANCOM,=PL6'0'                                                  
         GOTO1 HEXIN,DMCB,PRCESUFF,EDASUBC,2                                    
*                                                                               
PCL20    GOTO1 HEXIN,DMCB,PRCEHOUR,FULL,8                                       
         L     RF,FULL                                                          
         LTR   RF,RF               ANY HOURS?                                   
         BZ    PCL60                                                            
         CVD   RF,DUB                                                           
         ZAP   EDAHOURS,DUB        YES, SAVE THEM AND SET FLAG                  
         OI    EDATYPE,EDATTEST                                                 
         MVI   EDALN,EDALN4Q                                                    
         ZAP   EDABHRS,=P'0'       INITIALIZE IN CASE NEEDED                    
         ZAP   EDARHRS,=P'0'                                                    
         ZAP   EDANHRS,=P'0'                                                    
*                                                                               
PCL30    GOTO1 HEXIN,DMCB,PRCEBHRS,FULL,8                                       
         L     RF,FULL                                                          
         LTR   RF,RF               ANY B HOURS?                                 
         BZ    PCL40                                                            
         CVD   RF,DUB                                                           
         ZAP   EDABHRS,DUB         YES, SAVE THEM AND SET FLAG                  
         OI    EDATYPE,EDATBNR                                                  
         MVI   EDALN,EDALN6Q                                                    
*                                                                               
PCL40    GOTO1 HEXIN,DMCB,PRCENHRS,FULL,8                                       
         L     RF,FULL                                                          
         LTR   RF,RF               ANY N HOURS?                                 
         BZ    PCL50                                                            
         CVD   RF,DUB                                                           
         ZAP   EDANHRS,DUB         YES, SAVE THEM AND SET FLAG                  
         OI    EDATYPE,EDATBNR                                                  
         MVI   EDALN,EDALN6Q                                                    
*                                                                               
PCL50    GOTO1 HEXIN,DMCB,PRCERHRS,FULL,8                                       
         L     RF,FULL                                                          
         LTR   RF,RF               ANY R HOURS?                                 
         BZ    PCL60                                                            
         CVD   RF,DUB                                                           
         ZAP   EDARHRS,DUB         YES, SAVE THEM AND SET FLAG                  
         OI    EDATYPE,EDATBNR                                                  
         MVI   EDALN,EDALN6Q                                                    
*                                                                               
PCL60    GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
PCLX     B     XIT                                                              
         EJECT                                                                  
PROCHOUR NTR1                                                                   
         USING EVERECD,R3                                                       
         USING PRHROBJD,R4                                                      
         MVC   AIO,AADDIO                                                       
         L     R3,AIO                                                           
*                                                                               
         CLI   HRSFLAG,C'Y'          ARE WE BUILDING A RECORD?                  
         BE    PHR02                 YES, KEY IS ALREADY SET                    
*                                                                               
         L     RE,AADDIO             NO, CLEAR AADDIO                           
         LA    RF,2048                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     RE,AADDIO            SET UP THE KEY FROM AIO1                    
         L     RF,AIO1                                                          
         MVC   0(EVERFST-EVEKEY,RE),0(RF)                                       
*                                                                               
         MVC   EVEKWC,PRHRWCOD       PLUG WORKCODE INTO THE KEY                 
         CLC   PRHRSUFF,=C'00'                                                  
         BE    PHR02                                                            
         GOTO1 HEXIN,DMCB,PRHRSUFF,EVEKSWC,2                                    
*                                                                               
         USING EPTELD,R6                                                        
PHR02    LA    R6,ELEMENT            BUILD ESTIMATE PERSON TIME ELEMENT         
         XC    ELEMENT,ELEMENT                                                  
         MVI   EPTEL,EPTELQ                                                     
         MVI   EPTLN,EPTLN2Q                                                    
         MVI   EPTINDS,EPTIHOUR                                                 
         MVC   EPTULA(L'PRHR1RAC),PRHR1RAC                                      
         OC    EPTULA,SPACES                                                    
*                                                                               
         GOTO1 HEXIN,DMCB,PRHRBHRS,FULL,8                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   EPTBHRS,DUB                                                      
*                                                                               
         GOTO1 HEXIN,DMCB,PRHRNHRS,FULL,8                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   EPTNHRS,DUB                                                      
*                                                                               
         GOTO1 HEXIN,DMCB,PRHRRHRS,FULL,8                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   EPTRHRS,DUB                                                      
*                                                                               
         GOTO1 HEXIN,DMCB,PRHRRATE,FULL,8                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   EPTPRTE,DUB                                                      
*                                                                               
         GOTO1 HEXIN,DMCB,PRHRAMNT,FULL,8                                       
         L     RF,FULL                                                          
         CVD   RF,DUB                                                           
         ZAP   EPTAMNT,DUB                                                      
*                                                                               
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         MVC   AIO,AIO1               RESTORE AIO                               
*                                                                               
PHRX     MVI   HRSFLAG,C'Y'        ESTIMATE HOURS RECORD TO BE ADDED            
         B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE ADDS THE ESTIMATE RECORD IN AIO1 TO THE FILE.  FIRST             
* IT MUST CHECK TO SEE IF A DELETED ESTIMATE ALREADY EXISTS AND                 
* REPLACE IT IF SO.                                                             
*                                                                               
ADDEST   NTR1                                                                   
         OI    DMINBTS,X'08'       IF RECORD IS NOT ALREADY THERE               
         MVI   RDUPDATE,C'Y'                                                    
         XC    BIGKEY,BIGKEY                                                    
         L     R3,AIO                                                           
         MVC   BIGKEY(L'EVEKEY),0(R3)                                           
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'EVEKEY),KEYSAVE                                         
         BE    AE10                                                             
*                                                                               
         GOTO1 ADDREC              THEN ADD IT                                  
         B     AE20                                                             
*                                                                               
AE10     MVC   AIO,AIO2            ELSE READ AND THROW AWAY OLD RECORD          
         GOTO1 GETREC                                                           
*                                                                               
*&&DO                                                                           
         GOTO1 GETELEM,DMCB,EAPELQ,0 SEARCH FOR AN APPROVAL ELEM                
         BE    *+6                 FOUND ONE-R6 HAS ADDRESS                     
         SR    R6,R6               ZERO R6 FOR NOT FOUND                        
*                                                                               
         MVC   AIO,AIO1            WRITE NEW RECORD BACK                        
         LTR   R6,R6               TEST FOR APPROVAL ELEMENT                    
         BZ    AE12                NONE                                         
         GOTO1 ADDELEM,DMCB,(R6)   COPY EXISTING APPROVAL ELEMENT               
*&&                                                                             
*                                                                               
AE12     MVC   AIO,AIO2                                                         
         GOTO1 GETELEM,DMCB,EUPELQ,0                                            
         BE    *+14                FOUND AN ELEMENT                             
         MVC   AIO,AIO1                                                         
         B     AE15                                                             
*                                                                               
         USING EUPELD,R6                                                        
         MVC   ADDDATE,EUPADD      SAVE ADD DATE FOR ESTIMATE                   
         MVC   AIO,AIO1                                                         
         GOTO1 GETELEM,DMCB,EUPELQ,0                                            
         BE    *+6                                                              
         DC    H'0'                TROUBLE IF I CAN'T FIND IT                   
         MVC   EUPADD,ADDDATE      PRESERVE ADD DATE                            
*                                                                               
AE15     GOTO1 PUTREC                                                           
*                                  UNDELETE KEY IF MARKED DELETED               
         OC    ESTCUR,ESTCUR       TEST ANY ESTIMATE CURRENCY                   
         BZ    AE17                                                             
*                                                                               
*&&UK                                                                           
         CLC   BIGKEY+EVEKCUR-EVEKEY(L'EVEKCUR),ESTCUR                          
         BE    AE17                NO CHANGE IN CURRENCY                        
*                                                                               
         MVC   BIGKEY+EVEKCUR-EVEKEY(L'EVEKCUR),ESTCUR                          
         NI    BIGKEY+EVEKSTA-EVEKEY,X'7F'                                      
         B     AE18                                                             
*&&                                                                             
*                                                                               
AE17     TM    BIGKEY+EVEKSTA-EVEKEY,X'80'                                      
         BZ    AE20                                                             
*                                                                               
         NI    BIGKEY+EVEKSTA-EVEKEY,X'7F'                                      
*                                                                               
AE18     GOTO1 WRITE                                                            
*                                                                               
AE20     NI    DMINBTS,X'F7'                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVI   ESTFLAG,C'N'                                                     
*                                                                               
AEX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* THIS ROUTINE ADDS THE EST HOURS DATA IN AADDIO TO  THE FILE.  FIRST           
* IT MUST CHECK TO SEE IF A DELETED ESTIMATE ALREADY EXISTS AND                 
* REPLACE IT IF SO.                                                             
*                                                                               
ADDHRS   NTR1                                                                   
         OI    DMINBTS,X'08'       IF RECORD IS NOT ALREADY THERE               
         MVI   RDUPDATE,C'Y'                                                    
         XC    BIGKEY,BIGKEY                                                    
         MVC   AIO,AADDIO                                                       
         L     R3,AIO                                                           
         MVC   BIGKEY(L'EVEKEY),0(R3)                                           
         GOTO1 HIGH                                                             
         CLC   BIGKEY(L'EVEKEY),KEYSAVE                                         
         BE    AH02                                                             
*                                                                               
         GOTO1 ADDREC              THEN ADD IT                                  
         B     AH08                                                             
*                                                                               
AH02     MVC   AIO,AIO2            ELSE READ AND THROW AWAY OLD RECORD          
         GOTO1 GETREC                                                           
         MVC   AIO,AADDIO                                                       
*                                                                               
         GOTO1 PUTREC                                                           
*                                  UNDELETE KEY IF MARKED DELETED               
         OC    ESTCUR,ESTCUR       TEST ANY ESTIMATE CURRENCY                   
         BZ    AH04                                                             
*                                                                               
*&&UK                                                                           
         CLC   BIGKEY+EVEKCUR-EVEKEY(L'EVEKCUR),ESTCUR                          
         BE    AH04                NO CHANGE IN CURRENCY                        
*                                                                               
         MVC   BIGKEY+EVEKCUR-EVEKEY(L'EVEKCUR),ESTCUR                          
         NI    BIGKEY+EVEKSTA-EVEKEY,X'7F'                                      
         B     AH06                                                             
*&&                                                                             
*                                                                               
AH04     TM    BIGKEY+EVEKSTA-EVEKEY,X'80'                                      
         BZ    AH08                                                             
*                                                                               
         NI    BIGKEY+EVEKSTA-EVEKEY,X'7F'                                      
*                                                                               
AH06     GOTO1 WRITE                                                            
*                                                                               
AH08     NI    DMINBTS,X'F7'                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVI   HRSFLAG,C'N'                                                     
         MVC   AIO,AIO1                                                         
*                                                                               
AHX      B     XIT                                                              
         EJECT                                                                  
*                                  UNEXPECTED END OF TEMP FILE                  
ERREOT   MVC   APPLERR,=Y(PREREOT)                                              
         B     ERROBJ                                                           
*                                  INVALID UPLOAD OBJECT                        
ERRUOBJ  MVC   APPLERR,=Y(PRERUOBJ)                                             
         B     ERROBJ                                                           
*                                  INVALID UPLOAD JOB KEY                       
ERRJOKY  MVC   APPLERR,=Y(PRERJOKY)                                             
         B     ERROBJ                                                           
*                                  INVALID ESTIMATE OBJECT                      
ERRESOBJ MVC   APPLERR,=Y(PREREOBJ)                                             
         B     ERROBJ                                                           
*                                  UPLOAD TO READ ONLY FILE                     
ERRUPRO  MVC   APPLERR,=Y(PRERUPRO)                                             
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,A(ITGENERR),4,FULL                                  
         BNE   EXIT                                                             
         B     SETMDLST                                                         
*                                                                               
SETMDLST MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
EXIT     CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   EXIT10                                                           
         GOTO1 WRKSENT             MARK FILE AS SENT                            
*                                                                               
EXIT10   L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 1                                                                
SPACES   DC    CL255' '                                                         
         EJECT                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
CTRY     DS    X                   COUNTRY CODE FROM UTL                        
COMPCUR  DS    CL3                 COMPANY CURRENCY                             
COMPCURS DS    CL3                 COMPANY SECOND CURRENCY                      
COMPSTA7 DS    X                   COMPANY STATUS BYTE 7                        
COMPSTA9 DS    X                   COMPANY STATUS BYTE 9                        
ESTCUR   DS    CL3                 ESTIMATE CURRENCY                            
ESTFLAG  DS    C                   ESTIMATE RECORD TO BE WRITTEN (Y/N)          
HRSFLAG  DS    C                   EST HOURS RECORD TO BE WRITTEN (Y/N)         
MYCLI    DS    CL6                 CLIENT CODE FROM JOB OBJECT                  
MYPRO    DS    CL6                 PRODUCT CODE FROM JOB OBJECT                 
MYJOB    DS    CL6                 JOB CODE FROM JOB OBJECT                     
ADDDATE  DS    PL3                                                              
TMPHDR   DS    F                   LENGTH OF TMPAREA + 4 (FROM WRKGET)          
TMPAREA  DS    0XL2000             AREA TO READ TEMP FILE RECORD                
TMPTYPE  DS    XL4                 MAD OBJECT TYPE                              
TMPDATA  DS    XL1996              MAD OBJECT DATA                              
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
* CTMADPREST                                                                    
       ++INCLUDE CTMADPREST                                                     
         EJECT                                                                  
* ACPRESTOD                                                                     
       ++INCLUDE ACPRESTOD                                                      
         EJECT                                                                  
* ACPRESTOQ                                                                     
       ++INCLUDE ACPRESTOQ                                                      
         EJECT                                                                  
* ACGENFILE                                                                     
* FAUTL                                                                         
* FAFACTS                                                                       
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE FAUTL                                                          
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CTMAD21   07/08/04'                                      
         END                                                                    
