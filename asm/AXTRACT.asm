*          DATA SET AXTRACT    AT LEVEL 098 AS OF 08/02/19                      
*PHASE AXTRACTC                                                                 
*INCLUDE AXROUTS                  ADJUSTED RATE                                 
*INCLUDE AXCNVX                   CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE BUFFERIN                                                               
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE ACRECTYP                                                               
         TITLE 'AXTRACT - EXTRACT TEMPO SYSTEM FILE SQL DATA'                   
***********************************************************************         
* TEMPO SQL SUB SYSTEM EXTRACT CONTROL MODULE                         *         
*                                                                     *         
* CONTROL IS PASSED FROM DXTRACT WITH PARAMETERS:                     *         
* R1=A(EXTRACT CONTROL DATA BLOCK - SEE DSECT DXBLOCKD)               *         
*                                                                     *         
* MODULE ENTERED WITH ONE OF FOLLOWING MODES IN DXMODE:               *         
*   DXOPENQ  - OPEN SYSTEM FILES                                      *         
*   DXCLOSEQ - CLOSE SYSTEM FILES                                     *         
*   DXLOADQ  - EXTRACT FROM FILES IN LOAD MODE                        *         
*   DXUPDTQ  - EXTRACT FROM FILES IN UPDATE MODE                      *         
*                                                                     *         
* FOR DXLOADQ AND DXUPDTQ MODES,                                      *         
* DXBLOCKD CONTAINS DXSTPTR WHICH IS:                                 *         
* A(CURRENT ENTRY IN SUB SYSTEM EXTRACT DRIVER TABLE - SEE DSECT      *         
*                                                      SYSTABD)       *         
*                                                                     *         
* RETURN CC .NE. IF ERROR CONDITION ELSE CC .EQ. FOR OK               *         
*                                                                     *         
* DXUSER = 32 BYTE INPUT CARD FROM USERPARM=                          *         
* DXUSER+00 = C'Y' IF LOCKED ACCOUNTS TO BE DROPPED (UK)              *         
*                                                                     *         
* HISTORY                                                             *         
* -------                                                             *         
* 89      - JSHA SEPT 15/03 - ADDED TYPE CODE 'NOT' PER RAB TO SEND   *         
*           DOWN ALL TABLES EXCEPT TIMESHEET INFO - TAL/TIS           *         
* 92      - ADDED CODE TO LIMIT THE COMPANY ELEMENT MOVE TO ONLY      *         
*           CPYLN2Q                                                   *         
* 93 NSHE 24AUG04 ALTER EXTRACT TO ALLOW FOR DAILY TIME CHANGES       *         
* 94 NSHE 17FEB05 FIX BUG WITH PROCUEND                               *         
* 99 NSHE 11SEP06 ADDITONAL ROUTINE FOR TIMESHEETS                    *         
*101 NSHE 09JAN07 BUILD TSALLOCATIONLINEDAY TABLE FOR BRANDOCEAN      *         
*102 NSHE 20AUG07 IGNORE DRAFT ACCOUNTS                               *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
AXTRACT  CSECT                                                                  
         ENTRY SSB                                                              
         PRINT NOGEN                                                            
         NMOD1 WORKL,**AXTR**,R9                                                
         USING WORKD,RC            RC=A(GLOBAL W/S)                             
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
*                                                                               
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING DXBLOCKD,R7                                                      
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
*                                                                               
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
*                                                                               
         ICM   RF,15,=V(DDSIO)                                                  
         BZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
         USING SXDTABD,R6                                                       
         MVC   VERSION,SXDTVER                                                  
         OC    VERSION,VERSION                                                  
         BNZ   MAIN                                                             
         MVI   VERSION,1                                                        
         B     MAIN                                                             
*                                                                               
YES      SR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
EXIT     XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* MAIN CONTROL CODE                                                   *         
***********************************************************************         
MAIN     BAS   RE,GENINIT          GENERAL INITIALISATION                       
*                                                                               
MOPEN    CLI   DXMODE,DXOPENQ                                                   
         BNE   MCLOSE                                                           
         BAS   RE,PROCOPEN         OPEN SYSTEM FILES                            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MCLOSE   CLI   DXMODE,DXCLOSEQ                                                  
         BNE   MLOAD                                                            
         BAS   RE,PROCCLOS         CLOSE SYSTEM FILES                           
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MLOAD    CLI   DXMODE,DXLOADQ                                                   
         BNE   MUPDT                                                            
         BAS   RE,PROCLOAD         EXTRACT FROM FILES IN LOAD MODE              
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MUPDT    CLI   DXMODE,DXUPDTQ                                                   
         BNE   MUPDTEND                                                         
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MUPDTEND CLI   DXMODE,DXENDQ                                                    
         BNE   MERR                                                             
         BAS   RE,PROCUEND         END CALL FOR UPDATE MODE                     
         BNE   MERR                  EXIT IF ERROR                              
         B     MXIT                                                             
*                                                                               
MERR     MVI   RETCODE,0                                                        
         B     MXIT                                                             
*                                                                               
MXIT     XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
* GENERAL INITIALISATION                                              *         
***********************************************************************         
GENINIT  NTR1                                                                   
         CLI   DXMODE,DXOPENQ                                                   
         BNE   GENINX                                                           
*&&US                                                                           
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         JZ    NO                                                               
         MVC   0(8,RF),DXDDSIO                                                  
*&&                                                                             
         GOTOR VBUFFRIN,DMCB,('BUFFAINI',ALEVBUF),LEVREC,ACOMFACS               
*                                                                               
GENINX   B     MXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                        *         
***********************************************************************         
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
         XC    IOKEY,IOKEY         GET DTF ADDRESS                              
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,DMFAST,ACCDIR,IOKEY,(R2),DMWORK                    
         L     RF,12(R1)                                                        
         LA    RF,0(RF)                                                         
         ST    RF,DTFADDR          OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,ACCOUNT,ACCFILES,IO                         
*&&UK                                                                           
         MVC   IOKEY,SPACES                                                     
         LA    R2,IO                                                            
         MVC   IOKEY+ACTKCPY-ACTRECD(L'ACTKCPY),SXDTAGB                         
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),ACCDIR,IOKEY,(R2),DMWORK            
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                WHERE COMPANY RECORD?                        
*                                                                               
         MVC   ACCADDR,ACTKDA-ACTRECD(R2)                                       
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                WHAT HAPPENED WITH DMGR                      
*                                                                               
         XC    ACCADDR,ACCADDR                                                  
         LA    R2,ACTRFST-ACTRECD(R2)                                           
         XR    RF,RF                                                            
*                                                                               
POPE02   CLI   0(R2),0                                                          
         JNE   *+6                                                              
         DC    H'0'                WHERE CPYEL ON COMPANY RECORD?               
*                                                                               
         CLI   0(R2),CPYELQ        FOUND COMPANY ELEMENT?                       
         BE    *+16                YES                                          
         IC    RF,1(R2)                                                         
         LA    RF,0(R2,RF)                                                      
         B     POPE02                                                           
*                                                                               
         IC    RF,1(R2)                                                         
         CHI   RF,CPYLN2Q          ONLY NEED UPTO HERE                          
         BNH   *+8                                                              
         LA    RF,CPYLN2Q                                                       
         BCTR  RF,0                                                             
         MVC   CMPEL(0),0(R2)                                                   
         EX    RF,*-6                                                           
*&&                                                                             
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE ACCOUNT SYSTEM FILES                      *         
***********************************************************************         
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,ACCOUNT,0,IO                                
         CLI   8(R1),0                                                          
         JE    YES                                                              
         DC    H'0'                ERRORS ARE DEADLY                            
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFILES DC    C'NACCDIR NACCMST NACCARC NACCRCV X'                             
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN LOAD MODE                              *         
***********************************************************************         
PROCLOAD NTR1  ,                                                                
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         J     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS ACCOUNT FILE DATA IN UPDATE MODE READ RECOVERY FILES        *         
***********************************************************************         
PROCUPDT NTR1  ,                                                                
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
         CLI   RFILTY,ACCMSTQ      TEST ACCMST FILE RECORD TYPE                 
         JNE   YES                 ELSE IGNORE RECORD                           
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
         ICM   RF,15,TYPEAUPD      ELSE CALL UPDATE PROCESS ROUTINE             
         JZ    YES                                                              
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   NO                  EXIT ERROR                                   
         J     YES                 EXIT OK                                      
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS END OF UPDATE MODE                                          *         
***********************************************************************         
PROCUEND NTR1                                                                   
         ST    R6,SAVEPTR          SAVE CURRENT POINTER                         
         GOTO1 ASORTEND            COMPLETE PROCESSING FOR SORTED DATA          
         BE    PUPEOK              EXIT OK                                      
*                                                                               
PUPENO   B     NO                                                               
PUPEOK   L     R6,SAVEPTR                                                       
         B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
         USING RECDS,R5                                                         
         USING ACTRECD,RECVHDR+L'RECVHDR                                        
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         GOTO1 VRECTYP,DMCB,(C'D',ACTRECD)                                      
         MVC   RECTYPE,0(R1)       SAVE RECORD TYPE                             
         TM    ACTRSTAT,ACTSDELT   IS THIS RECORD DELETED?                      
         BZ    PKEY10              NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDELT                        
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY02              AND THEREFORE NEVER ON TEMPO DB              
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   PKEY04                                                           
PKEY02   TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDRFT                        
         JNZ   NO                  IGNORE THESE                                 
PKEY04   MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
*              TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD            
*                                                                               
PKEY10   CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   PKEY16                                                           
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDELT                        
         JZ    PKEY20                                                           
         CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY12                                                           
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   PKEY14                                                           
PKEY12   TM    ACTRSTAT,ACTSDRFT                                                
         JNZ   NO                  IGNORE THESE                                 
PKEY14   MVI   DXACTION,C'A'                                                    
         J     YES                                                              
                                                                                
PKEY16   CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY18                                                           
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   YES                                                              
PKEY18   TM    ACTRSTAT,ACTSDRFT                                                
         JNZ   NO                  IGNORE THESE                                 
         J     YES                                                              
                                                                                
PKEY20   CLI   RECTYPE,ACRTACTL    ACCOUNT RECORDS COULD BE DRAFT               
         JE    PKEY22                                                           
         CLI   RECTYPE,ACRTACTH                                                 
         JNE   YES                                                              
PKEY22   TM    ACTRSTAT,ACTSDRFT                                                
         JZ    PKEY24                                                           
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDRFT                        
         JNZ   NO                                                               
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
                                                                                
PKEY24   TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDRFT                        
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
                                                                                
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUFFERIN FILE                                                       *         
***********************************************************************         
LEVBUF   BUFFD TYPE=D,                                                 X        
               KEYLEN=LEVKLNQ,                                         X        
               COMLEN=LEVRLNQ,                                         X        
               BUFFERS=20                                                       
         EJECT                                                                  
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
***********************************************************************         
         DS    0L                                                               
ADDRESS  DC    CL8'EXTERNS'                                                     
         DC    A(COMFACS)                                                       
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
         DC    V(BUFFERIN)                                                      
         DC    V(SORTER)                                                        
         DC    V(ACRECTYP)                                                      
         DC    V(AXCNVX)                                                        
         DC    V(AXDACC)                                                        
         DC    V(AXDTRC)                                                        
         DC    V(AXADRC)           ADJUSTED RATE                                
         DC    V(AXCALC)           CALENDAR                                     
         DC    V(AXCLIC)           CLIENT                                       
         DC    V(AXCPYC)           COMPANY                                      
         DC    A(0)                COST RATE                                    
         DC    V(AXCSAC)           COSTING ACCOUNT                              
         DC    V(AXDEPC)           DEPARTMENT                                   
         DC    V(AXDEGC)           DEPARTMENT GROUP                             
         DC    A(0)                HOURS                                        
         DC    V(AXINAC)           INCOME ACCOUNT                               
         DC    V(AXISAC)           INCOME SUSPENSE ACCOUNT                      
         DC    V(AXJOBC)           JOB                                          
         DC    V(AXLEDC)           LEDGER                                       
         DC    V(AXLESC)           LEDGER STRUCTURE                             
         DC    V(AXLOCC)           LOCALITY                                     
         DC    V(AXLCIC)           LOCALITY INFORMATION                         
         DC    A(0)                MANAGER GROUP                                
         DC    A(0)                MANAGER GROUP MANAGERS                       
         DC    V(AXMEDC)           MEDIA                                        
         DC    V(AXMGRC)           MEDIA GROUP                                  
         DC    V(AXNCTC)           NON-CLIENT TIME                              
         DC    V(AXOFFC)           OFFICE                                       
         DC    V(AXOGRC)           OFFICE GROUP                                 
         DC    V(AXOFLC)           OFFICE LIST                                  
         DC    V(AXOLEC)           OFFICE LIST ENTRY                            
         DC    V(AXPEDC)           PERIOD                                       
         DC    V(AXPERC)           PERSON                                       
         DC    V(AXPEAC)           PERSON ASSIGNMENT                            
         DC    V(AXPROC)           PRODUCT                                      
         DC    V(AXRATC)           RATE                                         
         DC    V(AXSTHC)           STANDARD HOURS                               
         DC    V(AXEDTC)           EDIT HOURS                                   
         DC    V(AXCAPC)           COST ALLOCATION PROFILE                      
         DC    V(AXSBDC)           SUB-DEPARTMENT                               
         DC    V(AXTSKC)           TASK                                         
         DC    V(AXTISC)           TIMESHEET                                    
         DC    V(AXTALC)           TIMESHEET ALLOC/TAX UPDATE                   
         DC    V(AXPHIC)           COST RATE                                    
         DC    V(AXOPTC)           OPTIONS                                      
         DC    V(AXP1RC)           PERSON1R                                     
         DC    V(AXPOFC)           PRODUCTION OFFICE                            
         DC    V(AXTGRC)           WORKCODE GROUP                               
         DC    V(AXDTMC)           DAILY TIME                                   
         DC    V(AXBOSC)           BRAND OCEAN TIME (US 7/09)                   
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(ACCLOAD)                                                       
         DC    A(ACCUPDT)                                                       
         DC    A(DECIOC)                                                        
         DC    A(CHKSEQIO)                                                      
         DC    A(RDLEDG)                                                        
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(RECCMP)                                                        
         DC    A(LEVBUF)                                                        
         DC    A(SORTPUT)                                                       
         DC    A(SORTEND)                                                       
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(LOADADR)          ADJUSTED RATE                                
         DC    A(LOADCAL)          CALENDAR                                     
         DC    A(LOADCLI)          CLIENT                                       
         DC    A(LOADCPY)          COMPANY                                      
         DC    A(0)                COST RATE                                    
         DC    A(LOADCSA)          COSTING ACCOUNT                              
         DC    A(LOADDEP)          DEPARTMENT                                   
         DC    A(LOADDEG)          DEPARTMENT GROUP                             
         DC    A(0)                HOURS                                        
         DC    A(LOADINA)          INCOME ACCOUNT                               
         DC    A(LOADISA)          INCOME SUSPENSE ACCOUNT                      
         DC    A(LOADJOB)          JOB                                          
         DC    A(LOADLED)          LEDGER                                       
         DC    A(LOADLES)          LEDGER STRUCTURE                             
         DC    A(LOADLOC)          LOCALITY                                     
         DC    A(LOADLCI)          LOCALITY INFORMATION                         
         DC    A(0)                MANAGER GROUP                                
         DC    A(0)                MANAGER GROUP MANAGERS                       
         DC    A(LOADMED)          MEDIA                                        
         DC    A(LOADMGR)          MEDIA GROUP                                  
         DC    A(LOADNCT)          NON-CLIENT TIME                              
         DC    A(LOADOFF)          OFFICE                                       
         DC    A(LOADOGR)          OFFICE GROUP                                 
         DC    A(LOADOFL)          OFFICE LIST                                  
         DC    A(LOADOLE)          OFFICE LIST ENTRY                            
         DC    A(LOADPED)          PERIOD                                       
         DC    A(LOADPER)          PERSON                                       
         DC    A(LOADPEA)          PERSON ASSIGNMENT                            
         DC    A(LOADPRO)          PRODUCT                                      
         DC    A(LOADRAT)          RATE                                         
         DC    A(LOADSTH)          STANDARD HOURS                               
         DC    A(LOADEDT)          EDIT HOURS                                   
         DC    A(LOADCAP)          COST ALLOCATION PROFILE                      
         DC    A(LOADSBD)          SUB-DEPARTMENT                               
         DC    A(LOADTSK)          TASK                                         
         DC    A(LOADTIS)          TIMESHEET                                    
         DC    A(LOADTAL)          TIMESHEET ALLOCATION/TAX                     
         DC    A(LOADPHI)          COST RATE                                    
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(LOADOPT)          OPTIONS                                      
         DC    A(LOADP1R)          PERSON1R                                     
         DC    A(LOADPOF)          PRODUCTION OFFICE                            
         DC    A(LOADTGR)          WORKCODE GROUP                               
         DC    A(LOADDTM)          DAILY TIME                                   
         DC    A(LOADBOS)          BrandO Status                                
         DC    CL8'UPDTING'                                                     
         DC    A(UPDTADR)          ADJUSTED RATE                                
         DC    A(UPDTCAL)          CALENDAR                                     
         DC    A(UPDTCLI)          CLIENT                                       
         DC    A(UPDTCPY)          COMPANY                                      
         DC    A(0)                COST RATE                                    
         DC    A(UPDTCSA)          COSTING ACCOUNT                              
         DC    A(UPDTDEP)          DEPARTMENT                                   
         DC    A(UPDTDEG)          DEPARTMENT GROUP                             
         DC    A(0)                HOURS                                        
         DC    A(UPDTINA)          INCOME ACCOUNT                               
         DC    A(UPDTISA)          INCOME SUSPENSE ACCOUNT                      
         DC    A(UPDTJOB)          JOB                                          
         DC    A(UPDTLED)          LEDGER                                       
         DC    A(UPDTLES)          LEDGER STRUCTURE                             
         DC    A(UPDTLOC)          LOCALITY                                     
         DC    A(UPDTLCI)          LOCALITY INFORMATION                         
         DC    A(0)                MANAGER GROUP                                
         DC    A(0)                MANAGER GROUP MANAGERS                       
         DC    A(UPDTMED)          MEDIA                                        
         DC    A(UPDTMGR)          MEDIA GROUP                                  
         DC    A(UPDTNCT)          NON-CLIENT TIME                              
         DC    A(UPDTOFF)          OFFICE                                       
         DC    A(UPDTOGR)          OFFICE GROUP                                 
         DC    A(UPDTOFL)          OFFICE LIST                                  
         DC    A(UPDTOLE)          OFFICE LIST ENTRY                            
         DC    A(UPDTPED)          PERIOD                                       
         DC    A(UPDTPER)          PERSON                                       
         DC    A(UPDTPEA)          PERSON ASSIGNMENT                            
         DC    A(UPDTPRO)          PRODUCT                                      
         DC    A(UPDTRAT)          RATE                                         
         DC    A(UPDTSTH)          STANDARD HOURS                               
         DC    A(UPDTEDT)          EDIT HOURS                                   
         DC    A(UPDTCAP)          COST ALLOCATION PROFILE                      
         DC    A(UPDTSBD)          SUB-DEPARTMENT                               
         DC    A(UPDTTSK)          TASK                                         
         DC    A(UPDTTIS)          TIMESHEET                                    
         DC    A(UPDTTAL)          TIMESHEET ALLOCATION/TAX                     
         DC    A(UPDTPHI)          COST RATE                                    
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(UPDTOPT)          OPTIONS                                      
         DC    A(UPDTP1R)          PERSON1R                                     
         DC    A(UPDTPOF)          PRODUCTION OFFICE                            
         DC    A(UPDTTGR)          WORKCODE GROUP                               
         DC    A(UPDTDTM)          DAILY TIME                                   
         DC    CL8'FILTERS'                                                     
         DC    A(FILTADR)          ADJUSTED RATE                                
         DC    A(FILTCAL)          CALENDAR                                     
         DC    A(FILTCLI)          CLIENT                                       
         DC    A(FILTCPY)          COMPANY                                      
         DC    A(0)                COST RATE                                    
         DC    A(FILTCSA)          COSTING ACCOUNT                              
         DC    A(FILTDEP)          DEPARTMENT                                   
         DC    A(FILTDEG)          DEPARTMENT GROUP                             
         DC    A(0)                HOURS                                        
         DC    A(FILTINA)          INCOME ACCOUNT                               
         DC    A(FILTISA)          INCOME SUSPENSE ACCOUNT                      
         DC    A(FILTJOB)          JOB                                          
         DC    A(FILTLED)          LEDGER                                       
         DC    A(FILTLES)          LEDGER STRUCTURE                             
         DC    A(FILTLOC)          LOCALITY                                     
         DC    A(FILTLCI)          LOCALITY INFORMATION                         
         DC    A(0)                MANAGER GROUP                                
         DC    A(0)                MANAGER GROUP MANAGERS                       
         DC    A(FILTMED)          MEDIA                                        
         DC    A(FILTMGR)          MEDIA GROUP                                  
         DC    A(FILTNCT)          NON-CLIENT TIME                              
         DC    A(FILTOFF)          OFFICE                                       
         DC    A(FILTOGR)          OFFICE GROUP                                 
         DC    A(FILTOFL)          OFFICE LIST                                  
         DC    A(FILTOLE)          OFFICE LIST ENTRY                            
         DC    A(FILTPED)          PERIOD                                       
         DC    A(FILTPER)          PERSON                                       
         DC    A(FILTPEA)          PERSON ASSIGNMENT                            
         DC    A(FILTPRO)          PRODUCT                                      
         DC    A(FILTRAT)          RATE                                         
         DC    A(FILTSTH)          STANDARD HOURS                               
         DC    A(FILTEDT)          EDIT HOURS                                   
         DC    A(FILTCAP)          COST ALLOCATION PROFILE                      
         DC    A(FILTSBD)          SUB-DEPARTMENT                               
         DC    A(FILTTSK)          TASK                                         
         DC    A(FILTTIS)          TIMESHEET                                    
         DC    A(FILTTAU)          TIMESHEET AUDIT - BRANDOCEAN                 
         DC    A(FILTTAL)          TIMESHEET ALLOCATION/TAX                     
         DC    A(FILTPHI)          COST RATE                                    
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(FILTOPT)          OPTIONS                                      
         DC    A(FILTP1R)          PERSON1R                                     
         DC    A(FILTPOF)          PRODUCTION OFFICE                            
         DC    A(FILTTGR)          WORKCODE GROUP                               
         DC    A(FILTDTM)          DAILY TIME                                   
         DC    A(FILTBOS)          BrandO Status (US 7/09)                      
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          GENERAL INITIALISATION                       
         DC    A(INITADR)          ADJUSTED RATE                                
         DC    A(INITCAL)          CALENDAR                                     
         DC    A(INITCLI)          CLIENT                                       
         DC    A(INITCPY)          COMPANY                                      
         DC    A(0)                COST RATE                                    
         DC    A(INITCSA)          COSTING ACCOUNT                              
         DC    A(INITDEP)          DEPARTMENT                                   
         DC    A(INITDEG)          DEPARTMENT GROUP                             
         DC    A(0)                HOURS                                        
         DC    A(INITINA)          INCOME ACCOUNT                               
         DC    A(INITISA)          INCOME SUSPENSE ACCOUNT                      
         DC    A(INITJOB)          JOB                                          
         DC    A(INITLED)          LEDGER                                       
         DC    A(INITLES)          LEDGER STRUCTURE                             
         DC    A(INITLOC)          LOCALITY                                     
         DC    A(INITLCI)          LOCALITY INFORMATION                         
         DC    A(0)                MANAGER GROUP                                
         DC    A(0)                MANAGER GROUP MANAGERS                       
         DC    A(INITMED)          MEDIA                                        
         DC    A(INITMGR)          MEDIA GROUP                                  
         DC    A(INITNCT)          NON-CLIENT TIME                              
         DC    A(INITOFF)          OFFICE                                       
         DC    A(INITOGR)          OFFICE GROUP                                 
         DC    A(INITOFL)          OFFICE LIST                                  
         DC    A(INITOLE)          OFFICE LIST ENTRY                            
         DC    A(INITPED)          PERIOD                                       
         DC    A(INITPER)          PERSON                                       
         DC    A(INITPEA)          PERSON ASSIGNMENT                            
         DC    A(INITPRO)          PRODUCT                                      
         DC    A(INITRAT)          RATE                                         
         DC    A(INITSTH)          STANDARD HOURS                               
         DC    A(INITEDT)          EDIT HOURS                                   
         DC    A(INITCAP)          COST ALLOCATION PROFILE                      
         DC    A(INITSBD)          SUB-DEPARTMENT                               
         DC    A(INITTSK)          TASK                                         
         DC    A(INITTIS)          TIMESHEET                                    
         DC    A(INITTAL)          TIMESHEET ALLOCATION/TAX)                    
         DC    A(INITPHI)          COST RATE                                    
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(INITOPT)          OPTIONS                                      
         DC    A(INITP1R)          PERSON1R                                     
         DC    A(INITPOF)          PRODUCTION OFFICE                            
         DC    A(INITTGR)          WORKCODE GROUP                               
         DC    A(INITDTM)          DAILY TIME                                   
         DC    A(INITBOS)          BrandO STATUS                                
         DC    C'OPEN   '                                                       
         DC    C'DMREAD '                                                       
         DC    C'DMRSEQ '                                                       
         DC    C'DMRDHI '                                                       
         DC    C'DMCLSE '                                                       
         DC    C'DMFAST '                                                       
         DC    C'GETREC '                                                       
         DC    C'RECOVER'                                                       
         DC    C'CONTROL'                                                       
         DC    C'CTFILE '                                                       
         DC    C'ACCDIR '                                                       
         DC    C'ACCMST '                                                       
         DC    C'ACCARC '                                                       
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    CL1'Y'                                                           
         DC    CL1'N'                                                           
         DC    XL1'00'                                                          
         DC    80C' '                                                           
         DC    F'0'                                                             
         DC    (CPYLN2Q)X'00'      COMPANY ELEMENT EXTRACTED                    
*                                                                               
         DS    0D                                                               
         DC    CL16'*SSB**SSB**SSB**'                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSB+(SSOXTND-SSBD)                                               
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
ACCMSTQ  EQU   X'6A'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
FF       EQU   X'FF'                                                            
*                                                                               
***********************************************************************         
* TYPTAB DEFINES PROCESS RECORD TYPES & IS COVERED BY TYPTABD         *         
*                                                                     *         
* CL3    TYPE NAME                                                    *         
* AL1    DEPTH TO READ TO FOR INCREMENT ON RDHI FOR LEDGER LOAD       *         
* AL1    TYPE FLAGS                                                   *         
* AL3    N/D                                                          *         
* AL4    LOAD ROUTINE ADDRESS                                         *         
* AL4    UPDATE ROUTINE ADDRESS                                       *         
***********************************************************************         
TYPTAB   DS    0L                                                               
         DC    CL3'ALL',AL1(00,00,00,00,00),AL4(LOADALL,UPDTALL)                
         DC    CL3'NOT',AL1(00,00,00,00,00),AL4(LOADALL,UPDTALL)                
         DC    CL3'ADR',AL1(00,00,00,00,00),AL4(LOADADR,UPDTADR)                
         DC    CL3'CAL',AL1(00,00,00,00,00),AL4(LOADCAL,UPDTCAL)                
         DC    CL3'CLI',AL1(01,00,00,00,00),AL4(LOADCLI,UPDTCLI)                
         DC    CL3'CPY',AL1(00,00,00,00,00),AL4(LOADCPY,UPDTCPY)                
         DC    CL3'CSA',AL1(01,00,00,00,00),AL4(LOADCSA,UPDTCSA)                
         DC    CL3'DEP',AL1(02,00,00,00,00),AL4(LOADDEP,UPDTDEP)                
         DC    CL3'DEG',AL1(01,00,00,00,00),AL4(LOADDEG,UPDTDEG)                
         DC    CL3'INA',AL1(01,00,00,00,00),AL4(LOADINA,UPDTINA)                
         DC    CL3'ISA',AL1(01,00,00,00,00),AL4(LOADISA,UPDTISA)                
         DC    CL3'JOB',AL1(03,00,00,00,00),AL4(LOADJOB,UPDTJOB)                
         DC    CL3'LED',AL1(00,00,00,00,00),AL4(LOADLED,UPDTLED)                
         DC    CL3'LES',AL1(00,00,00,00,00),AL4(LOADLES,UPDTLES)                
         DC    CL3'LOC',AL1(00,00,00,00,00),AL4(LOADLOC,UPDTLOC)                
         DC    CL3'LCI',AL1(00,00,00,00,00),AL4(LOADLCI,UPDTLCI)                
         DC    CL3'MED',AL1(00,00,00,00,00),AL4(LOADMED,UPDTMED)                
         DC    CL3'MGR',AL1(00,00,00,00,00),AL4(LOADMGR,UPDTMGR)                
         DC    CL3'NCT',AL1(01,00,00,00,00),AL4(LOADNCT,UPDTNCT)                
         DC    CL3'OFF',AL1(01,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'OGR',AL1(00,00,00,00,00),AL4(LOADOGR,UPDTOGR)                
         DC    CL3'OFL',AL1(00,00,00,00,00),AL4(LOADOFL,UPDTOFL)                
         DC    CL3'OLE',AL1(00,00,00,00,00),AL4(LOADOLE,UPDTOLE)                
         DC    CL3'PED',AL1(00,00,00,00,00),AL4(LOADPED,UPDTPED)                
         DC    CL3'PER',AL1(00,00,00,00,00),AL4(LOADPER,UPDTPER)                
         DC    CL3'PEA',AL1(00,00,00,00,00),AL4(LOADPEA,UPDTPEA)                
         DC    CL3'PRO',AL1(02,00,00,00,00),AL4(LOADPRO,UPDTPRO)                
         DC    CL3'RAT',AL1(00,00,00,00,00),AL4(LOADRAT,UPDTRAT)                
         DC    CL3'STH',AL1(03,00,00,00,00),AL4(LOADSTH,UPDTSTH)                
         DC    CL3'EDT',AL1(03,00,00,00,00),AL4(LOADEDT,UPDTEDT)                
         DC    CL3'CAP',AL1(00,00,00,00,00),AL4(LOADCAP,UPDTCAP)                
         DC    CL3'SBD',AL1(03,00,00,00,00),AL4(LOADSBD,UPDTSBD)                
         DC    CL3'TSK',AL1(00,00,00,00,00),AL4(LOADTSK,UPDTTSK)                
         DC    CL3'TAL',AL1(03,00,00,00,00),AL4(LOADTAL,UPDTTAL)                
         DC    CL3'OPT',AL1(00,00,00,00,00),AL4(LOADOPT,UPDTOPT)                
         DC    CL3'P1R',AL1(04,00,00,00,00),AL4(LOADP1R,UPDTP1R)                
         DC    CL3'POF',AL1(00,00,00,00,00),AL4(LOADPOF,UPDTPOF)                
         DC    CL3'TIS',AL1(00,00,00,00,00),AL4(LOADTIS,UPDTTIS)                
         DC    CL3'PHI',AL1(00,00,00,00,00),AL4(LOADPHI,UPDTPHI)                
         DC    CL3'TGR',AL1(00,00,00,00,00),AL4(LOADTGR,UPDTTGR)                
         DC    CL3'DTM',AL1(00,00,00,00,00),AL4(LOADDTM,UPDTDTM)                
         DC    CL3'BOS',AL1(00,00,00,00,00),AL4(LOADBOS,0)                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK SEQUENTIAL IO BROKEN, LAST KEY IN IOKEY                       *         
***********************************************************************         
CHKSEQIO NTR1  BASE=*,LABEL=*                                                   
         L     R2,DTFADDR                                                       
         USING ISDTF,R2                                                         
         L     RF,ISPDKEY                                                       
         LH    RE,ISKEYLN1                                                      
         EX    RE,SEQCLC                                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
SEQCLC   CLC   IOKEY(0),0(RF)                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* GET TYPE TABLE ACTION FROM 3 CHARACTER CODE                         *         
***********************************************************************         
GETTYP   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RF,ATYPTAB                                                       
         USING TYPTABD,RF                                                       
GTYP02   CLI   0(RF),FF            END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME    COMPARE NAME                                 
         BE    GTYP04                                                           
         LA    RF,TYPTABLQ(RF)     GET NEXT ENTRY                               
         B     GTYP02                                                           
*                                                                               
GTYP04   MVC   TYPENAME,TYPNAME    MATCH FOUND - GET TABLE INFORMATION          
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DECREMENT MAXIMUM IO COUNT                                          *         
***********************************************************************         
DECIOC   NTR1  BASE=*,LABEL=*                                                   
         ICM   RF,15,MAXIOS                                                     
         BCTR  RF,0                                                             
         STCM  RF,15,MAXIOS                                                     
         JNZ   YES                                                              
*                                                                               
         LA    R3,DECMSG           OUTPUT IO COUNT EXCEEDED MESSAGE             
         MVC   DECTYPE,TYPENAME                                                 
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
*                                                                               
DECMSGL  DC    AL2(50)                                                          
DECMSG   DC    CL50' '                                                          
         ORG   DECMSG                                                           
         DC    C'IO Count exceeded - Typecode = '                               
DECTYPE  DC    CL3' '                                                           
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       ACCADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
GETIT    NTR1  BASE=*,LABEL=*                                                   
         LA    R0,ACCMST                                                        
*&&UK                                                                           
         TM    TRNKSTAT-TRNRECD(R2),TRNSARCH  IS IT ARCHIVED                    
         BZ    *+8                 NO                                           
         LA    R0,ACCARC           YES                                          
*&&                                                                             
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),(R0),ACCADDR,(R2),DMWORK            
         CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,ACCADDR,GETDA,L'ACCADDR                          
         GOTO1 (RF),(R1),DMCB+8,GETRC,1                                         
*                                                                               
         LA    R3,GETMSG           OUTPUT DISK READ ERROR MESSAGE               
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         J     NO                                                               
*                                                                               
GETMSGL  DC    AL2(50)                                                          
GETMSG   DC    CL50' '                                                          
         ORG   GETMSG                                                           
         DC    C'DMGR GETREC error - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO PERFORM A READHI                                       *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
READHI   NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,ACCDIR,IOKEY,(R2),DMWORK                    
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,IOKEYSAV,RDHKEY,L'IOKEYSAV                       
*                                                                               
         XR    R0,R0                                                            
         WTO   TEXT=((RDHHL,C),(RDH1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
*                                                                               
RDHHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READHI error key hexout follows'                       
*                                                                               
RDH1L    DC    AL2(90)                                                          
RDH1M    DC    CL90' '                                                          
         ORG   RDH1M                                                            
         DC    C'Key='                                                          
RDHKEY   DC    CL84' '                                                          
         ORG   RDH1M+L'RDH1M                                                    
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ LEDGER RECORD TO FIND LENGTHS OF SUB RECORD TYPES              *         
*                                                                     *         
* THIS ROUTINE EXPECTS COMPANY,UNIT,LEDGER TO BE IN THEIR NAMESAKES   *         
* IT RETURNS THE 4 LENGTHS (OR 0) IN L1 TO L4                         *         
***********************************************************************         
RDLEDG   NTR1  BASE=*,LABEL=*                                                   
         CLC   CURRCUL,COMPANY                                                  
         JE    YES                                                              
*                                                                               
         MVC   CURRCUL,COMPANY                                                  
         MVC   LEVREC,SPACES                                                    
         MVC   LEVKCPY,COMPANY                                                  
         MVC   LEVKUL,UNIT                                                      
         MVC   WORK(LEVRLNQ),LEVREC                                             
*                                                                               
         GOTO1 VBUFFRIN,DMCB,('BUFFAGET',ALEVBUF),(0,LEVREC),ACOMFACS           
         MVC   BUFFRET,BUFFERRS-BUFFPARM(R1)                                    
         TM    BUFFRET,BUFFERNF                                                 
         BO    RLED20                                                           
*                                                                               
         MVC   CURRUL,UNIT         UPDATE CURRENT U/L FOR NEXT PASS             
         MVC   LEVELS(L'LEVELS+L'DISPS),LEVRLEVS                                
         CLC   UNIT(L'UNIT+L'LEDGER),=C'SJ'     ARE WE DOING SJ?                
         JNE   YES                                                              
         MVC   SJLEVELS(L'LEVELS+L'DISPS),LEVRLEVS                              
         J     YES                                                              
*                                                                               
RLED20   LA    R2,IOKEY                                                         
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT,UNIT                                                     
         MVC   LDGKLDG,LEDGER                                                   
         MVC   CURRUL,LDGKUNT                                                   
         MVC   CURRCPY,COMPANY                                                  
*                                                                               
         LA    R2,IO                                                            
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),ACCDIR,IOKEY,(R2),DMWORK            
         CLI   8(R1),0                                                          
         JNE   RLEDWTO                                                          
*                                                                               
         MVC   ACCADDR,LDGKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   RLEDWTO                                                          
*                                                                               
         LA    R1,LDGRFST                                                       
         XR    RF,RF                                                            
         DROP  R2                                                               
*                                                                               
RLED30   CLI   0(R1),0             END OF RECORD?                               
         JE    RLEDWTO                                                          
         CLI   0(R1),ACLELQ                                                     
         JE    RLED40                                                           
         IC    RF,1(R1)                                                         
         LA    R1,0(RF,R1)                                                      
         J     RLED30                                                           
*                                                                               
         USING ACLELD,R1                                                        
RLED40   XC    DISPS,DISPS                                                      
         XC    LEVELS,LEVELS                                                    
         LA    RE,ACLVALS                                                       
         IC    RF,ACLLN                                                         
         DROP  R1                                                               
*                                                                               
         SRL   RF,4               WILL GIVE NUMBER OF LEVELS                    
         XR    R2,R2                                                            
         LA    R1,DISPS                                                         
*                                                                               
RLED50   MVC   0(1,R1),0(RE)                                                    
         LA    RE,16(RE)                                                        
         LA    R1,1(R1)                                                         
         BCT   RF,RLED50                                                        
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         LA    R2,DISPS                                                         
         IC    RE,0(R2)                                                         
         LA    R1,LEVELS                                                        
         STC   RE,0(R1)                                                         
         LA    R0,3                                                             
*                                                                               
RLED60   LA    R1,1(R1)                                                         
         IC    RE,0(R2)                                                         
         IC    RF,1(R2)                                                         
         LTR   RF,RF                                                            
         JZ    RLED70                                                           
         SR    RF,RE                                                            
         STC   RF,0(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,RLED60                                                        
*                                                                               
RLED70   CLC   =C'SJ',UNIT                                                      
         JNE   *+10                                                             
         MVC   SJLEVELS(L'LEVELS+L'DISPS),LEVELS                                
*                                                                               
         MVC   LEVREC,SPACES                                                    
         MVC   LEVKCPY,COMPANY                                                  
         MVC   LEVKUL,UNIT                                                      
         MVC   LEVRLEVS(L'LEVRLEVS+L'LEVRDSPS),LEVELS                           
*                                                                               
         GOTO1 VBUFFRIN,DMCB,('BUFFAPUT',ALEVBUF),(0,LEVREC),ACOMFACS           
         MVC   BUFFRET,BUFFERRS-BUFFPARM(R1)                                    
         CLI   BUFFRET,0                                                        
         JE    YES                                                              
         DC    H'0'                                                             
*                                                                               
RLEDWTO  MVC   MSGUNT,UNIT                                                      
         MVC   MSGLDG,LEDGER                                                    
         MVC   MSGCPY,SXDTAGY                                                   
         LA    R3,MSGL                                                          
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         ABEND 922,DUMP                                                         
*                                                                               
MSGL     DC    AL2(70)                                                          
MSG      DC    CL70' '                                                          
         ORG   MSG                                                              
         DC    C'Required ledger - '                                            
MSGUNT   DC    C'?'                                                             
MSGLDG   DC    C'?'                                                             
         DC    C' - Missing/Invalid - AXTRACT Abending (CpyAlph='               
MSGCPY   DC    C'??'                                                            
         DC    C')'                                                             
         ORG   MSG+L'MSG                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMPARE COPY AND CHANGE RECORDS TO SEE IF THEY ARE DIFFERENT        *         
*                                                                     *         
* NTRY:                                                               *         
* EXIT: CC EQ    RECORD TO BE PROCESSED                               *         
*     : CC NE    RECORD NOT TO BE PROCESSED                           *         
***********************************************************************         
RECCMP   NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB          GET CHANGE RECORD ADDRESS                    
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
CHG      USING ACCRECD,R2                                                       
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES                                          
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R4,RECVHDR+L'RECVHDR                                             
CPY      USING ACCRECD,R4                                                       
*                                                                               
         CLC   CHG.ACCRLEN,CPY.ACCRLEN                                          
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
         XR    R3,R3                                                            
         ICM   R3,3,CPY.ACCRLEN                                                 
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
*                                                                               
         LTORG                                                                  
         DROP  CHG,CPY                                                          
         EJECT                                                                  
***********************************************************************         
* SORTPUT STARTS UP THE TIMESHEET ALLOCATION SORT                    *          
*         WRITES A TIMESHEET ALLOCATION RECORD TO SORT                *         
***********************************************************************         
SORTPUT  NTR1  BASE=*,LABEL=*                                                   
         CLI   SORTFLAG,C'Y'                                                    
         JE    SORTP02                                                          
         GOTO1 VSORTER,DMCB,SORTC1,SORTC2                                       
         MVI   SORTFLAG,C'Y'                                                    
SORTP02  GOTO1 VSORTER,DMCB,=C'PUT',(R5)                                        
         J     YES                                                              
*                                                                               
SORTC1   DC    CL80'SORT FIELDS=(9,4,A,17,4,A,29,41,A),FORMAT=BI'               
SORTC2   DC    CL80'RECORD TYPE=V,LENGTH=(2048,,,,)'                            
***********************************************************************         
* SORTEND READS RECOVERY RECORDS FOR TIME RECORDS BACK FROM SORT TO   *         
*        CREATE TIME SHEET ALLOCATION RECORDS                         *         
***********************************************************************         
SORTEND  NTR1  BASE=*,LABEL=*                                                   
         CLI   SORTFLAG,C'Y'       TEST ANYTHING WAS PUT                        
         BNE   SORTENDX            NO, EXIT                                     
         XC    SVPTRPR,SVPTRPR     CLEAR PREVIOUS ADDRESS                       
         XC    SVPTRCU,SVPTRCU     CLEAR CURRENT ADDRESS                        
         XC    SVSENUM,SVSENUM     CLEAR SYSTEM NUMBER                          
         XC    SVAGYB,SVAGYB       CLEAR AGENCY BINARY NUMBER                   
         L     R5,DXARECB          SAVED RECORDS IN DXAXREC (R4)                
         XC    0(4,R5),0(R5)       INDICATE NOTHING SAVED YET                   
         XC    IOKEYSAV,IOKEYSAV                                                
         XC    SAVSIN,SAVSIN                                                    
*                                                                               
SORTENDG GOTO1 VSORTER,DMCB,=C'GET'                                             
         ICM   R2,15,4(R1)                                                      
         JNZ   SORTENDS            PROCESS RECORD                               
         OI    UPDTIND,UPDTEND     SET FINISHED                                 
         J     SORTE06             NO MORE DATA FROM SORT - PUT OUT             
*                                                LAST RECORDS IN BUFFER         
SORTENDS LTR   RE,R2               TEST MORE SORT DATA                          
         BZ    SORTENDX            NO, ALL DONE                                 
         LH    RF,0(,RE)                                                        
         LR    R0,R5                                                            
         LR    R1,RF                                                            
         MVCL  R0,RE               MOVE NEXT REC TO SAVE AREA                   
*                                                                               
         USING AXTALD,R3                                                        
         USING RECDS,R5                                                         
         USING TIMRECD,R2                                                       
         L     R5,DXARECB                                                       
         L     R3,DXAXREC                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
         CLC   SVSENUM,RSYS                                                     
         BNE   *+14                                                             
         CLC   SVAGYB,RAG                                                       
         BE    SORTE03                                                          
         L     R6,DXSTSPTR         R6=A(START OF SYSTEM DRIVER TABLE)           
SORTE00  C     R6,DXSTEPTR         CHECK WE HAVEN'T REACHED THE END             
*&&UK*&& BNH   *+6                                                              
*&&UK*&& DC    H'0'                DIE IF WE HAVE                               
*&&US*&& BH    SORTENDG                                                         
         CLC   RSYS,SXDTSEN        Does SE number match                         
         BNE   SORTE01             NO                                           
         OC    RAG,RAG             Do we have an agency code?                   
         BZ    *+14                Not in the header - Check record.            
         CLC   RAG,SXDTAGB         YES - Does agency code match w/ hdr?         
         BE    SORTE02             YES                                          
         CLC   TIMKCPY,SXDTAGB     YES - Does agency code match w/ rec?         
         BE    SORTE02             YES                                          
SORTE01  LA    R6,SXDTABL(R6)      NO - Move on to next table entry             
         B     SORTE00                                                          
                                                                                
SORTE02  ST    R6,SVPTRCU          SET AS CURRENT ENTRY                         
         OC    SVPTRPR,SVPTRPR                                                  
         BNZ   SORTE02A                                                         
         MVC   SVPTRPR,SVPTRCU                                                  
         ST    R6,DXSTPTR                                                       
         MVC   VERSION,SXDTVER                                                  
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG             GET LEDGER STRUCTURE                         
SORTE02A MVC   SVSENUM,SXDTSEN                                                  
         MVC   SVAGYB,SXDTAGB                                                   
         L     R6,SVPTRPR                                                       
         ST    R6,DXSTPTR                                                       
*                                                                               
SORTE03  MVI   BYTE2,0                                                          
*                                                                               
         CLI   VERSION,5                                                        
         JNE   SORTE13                                                          
*                                                                               
         OC    SAVSIN,SAVSIN       IS IT FIRST TIME IN                          
         JZ    SORTE12             YES                                          
         OC    IOKEYSAV,IOKEYSAV   IS IT FIRST TIME FOR SURE                    
         JZ    SORTE12             YES                                          
         CLC   RSIN+1(3),SAVSIN+1  NO - IS IT THE SAME SEQ AS BEFORE            
         JNE   SORTE06             NO                                           
         CLC   TIMKEY(L'TIMKEY-L'TIMKSBR),IOKEYSAV YES - IS IT SAME KEY         
         JNE   SORTE06             NO                                           
         CLI   RPRG,C'A'           YES ARE WE ADDING                            
         JE    SORTE04             NO - MUST BE DELETING                        
         TM    UPDTIND,UPDTDEL     HAVE WE DONE A DELETE BEFORE                 
         JZ    SORTE14             NO - ADD TO BUFFER                           
         MVI   BYTE2,1             SET THIS IS ADDING TO EXISTING BUFF          
         J     SORTE14             ADD TO EXISTING DELETE BUFFER                
*                                                                               
SORTE04  TM    UPDTIND,UPDTADD     HAVE WE DONE AN ADD BEFORE                   
         JZ    SORTE14             NO - ADD TO BUFFER                           
         MVI   BYTE2,1             SET THIS IS ADDING TO EXISTING BUFF          
         J     SORTE14             ADD TO EXISTING BUFFER                       
*                                                                               
SORTE06  TM    UPDTIND,UPDTDEL     DID WE HAVE ANY RECORDS TO DELETE            
         JZ    SORTE08             NO - CHECK WHETHER WE HAVE ANY ADDS          
         MVI   FLAG,1              YES - SET DELETE FOR FLAG                    
         NI    UPDTIND,X'FF'-UPDTDEL REMOVE INDICATOR                           
         J     SORTE20                                                          
SORTE08  TM    UPDTIND,UPDTADD     DID WE HAVE ANY RECORDS TO ADD               
         BZ    SORTE10             NO - CHECK WHETHER WE'VE REACHED END         
         MVI   FLAG,0              YES - SET ADD FOR FLAG                       
         NI    UPDTIND,X'FF'-UPDTADD  REMOVE INDICATOR                          
         J     SORTE20                                                          
*                                                                               
SORTE10  TM    UPDTIND,UPDTEND     HAVE WE REACH END OF SORT                    
         JNZ   SORTENDX            YES                                          
*                                                                               
SORTE12  MVC   SAVSIN,RSIN         NO - SAVE SIN AND KEY                        
         MVC   IOKEYSAV,TIMKEY                                                  
SORTE13  MVC   SVPTRPR,SVPTRCU                                                  
         L     R6,SVPTRCU                                                       
         ST    R6,DXSTPTR                                                       
         MVC   VERSION,SXDTVER                                                  
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG             GET LEDGER STRUCTURE                         
*                                                                               
SORTE14  GOTO1 AINITTAL            INITIALISE TAL RECORD                        
         MVC   AXTALACT,RPRG                                                    
                                                                                
         MVI   FLAG,0              SET ADD FOR FLAG                             
         CLI   RPRG,C'A'           ADD?                                         
         JE    SORTE16             NO - NEED FOR DELETE BELOW                   
         MVI   FLAG,1              SET DELETE FOR FLAG                          
*                                                                               
SORTE16  GOTO1 VAXTALC,DMCB,DXAXREC,(BYTE2,(R2)),(1,0),(FLAG,(R6)),LEVEX        
               LS                                                               
         CLI   VERSION,5                                                        
         JNE   SORTE20                                                          
         CLI   RPRG,C'A'           ARE WE ADDING                                
         JE    SORTE18                                                          
         OI    UPDTIND,UPDTDEL     NO MUST BE DELETE - SET INDICATOR            
         J     SORTENDG                                                         
SORTE18  OI    UPDTIND,UPDTADD     RECORDS ADDED TO ADD BUFFER                  
         J     SORTENDG                                                         
*                                                                               
SORTE20  GOTO1 VAXTALC,DMCB,DXAXREC,(BYTE2,(R2)),(2,0),(FLAG,(R6)),LEVEX        
               LS                                                               
         CLI   8(R1),FF                                                         
         JE    SORTE26             GET NEXT RECORD FROM SORT                    
         CLI   DXWRITE,C'Y'                                                     
         JNE   SORTE24             DO NOT WRITE THIS THIS RECORD                
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    SORTE22                                                          
*                                                                               
         LA    R0,=C'TAL'                                                       
         CLI   DMCB+8,X'EE'        TAX RECORDS IF EE PASSED BACK                
         JNE   *+8                                                              
         LA    R0,=C'TAT'                                                       
         CLI   DMCB+8,X'DD'        DAY RECORDS IF DD PASSED BACK                
         JNE   *+8                                                              
         LA    R0,=C'TAD'                                                       
         CLI   DMCB+8,X'CC'        BRANDO LINE RECS IF CC PASSED BACK           
         JNE   SORTE21                                                          
         LA    R0,=C'BOL'                                                       
         CLI   VERSION,5                                                        
         JL    SORTE20                                                          
SORTE21  GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,(R0)),VERSION            
*                                                                               
         L     RF,DXASQLB                                                       
SORTE22  GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
SORTE24  GOTO1 ADECIOC                                                          
         JE    SORTE20                                                          
         J     NO                                                               
         DROP  R3                                                               
*                                                                               
         USING AXBOSD,R3                                                        
SORTE26  MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG             GET LEDGER STRUCTURE                         
*                                                                               
         CLI   VERSION,5                                                        
         JL    SORTE28                                                          
*                                                                               
         GOTO1 AFILTBOS            FILTER RECORD                                
         JNE   SORTE28             NOT VALID                                    
*                                                                               
         GOTO1 AINITBOS            INITIALISE EXTRACT BUFFER                    
         MVC   AXBOSACT,RPRG                                                    
*                                                                               
         GOTO1 VAXBOSC,DMCB,DXAXREC,(R2),0,(R6),LEVELS                          
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    SORTE28                                                          
         TM    DMCB+8,X'80'                                                     
         JO    SORTE28                                                          
*                                                                               
         L     RE,DXAXREC                                                       
         CLC   SAVBOSC,0(RE)       Same as previous?                            
         JE    SORTE28                                                          
         MVC   SAVBOSC,0(RE)                                                    
         LA    R0,=C'BOS'                                                       
         GOTO1 VAXCNVX,DMCB,(C'U',DXAXREC),(1,DXASQLB),(0,(R0)),VERSION         
*                                                                               
         L     RF,DXASQLB                                                       
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
SORTE28  CLI   VERSION,5                                                        
         JNE   SORTENDG                                                         
         J     SORTE06                                                          
*                                                                               
SORTENDX GOTO1 VSORTER,DMCB,=C'END'                                             
         MVI   SORTFLAG,C'N'                                                    
         J     YES                                                              
         DROP  R3                                                               
*                                                                               
SAVBOSC  DS    XL(AXBOSDL)                                                      
***********************************************************************         
* LOAD BrandO Status Record                                           *         
***********************************************************************         
LOADBOS  NTR1  BASE=*,LABEL=*                                                   
         CLI   VERSION,5                                                        
         JL    YES                                                              
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST BOS RECORD             
         USING TSWRECD,R2                                                       
         XC    IOKEYSAV,IOKEYSAV   Clear Saved area for Key                     
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ    X'3E'                                        
         MVI   TSWKSUB,TSWKSUBQ    X'0F'                                        
         MVC   TSWKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LBOS02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TSWKEY(TSWKPER-TSWKEY),IOKEY                                     
         JNE   YES                                                              
*                                                                               
         CLC   IOKEYSAV(TSWKODS-TSWKEY),TSWKEY  Same as previous                
         JE    LBOS10                                                           
         MVC   IOKEYSAV,TSWKEY     Save key                                     
*                                                                               
         MVC   ACCADDR,TSWKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
         GOTO1 AFILTBOS            FILTER ROUTINE?                              
         JNE   YES                 NOT VALID                                    
*                                                                               
         GOTO1 AINITBOS            INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 VAXBOSC,DMCB,DXAXREC,(R2),0,(R6),LEVELS                          
         TM    DMCB+8,X'80'                                                     
         JO    YES                 ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    YES                 DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 CONTROLLER REQUESTS NO WRITE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LBOS04              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',DXAXREC),(1,DXASQLB),(0,TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LBOS04   GOTO1 DXPUT,DMCB,(RF),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
LBOS10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LBOS02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER BrandO Status at R2                                          *         
***********************************************************************         
FILTBOS  NTR1  BASE=*,LABEL=*                                                   
         USING TIMRECD,R2                                                       
         CLC   TIMKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   TIMKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   TIMKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
*                                                                               
         LA    RF,TIMRFST                                                       
         XR    R0,R0               R0=A(TIMEL)                                  
         XR    R1,R1               R1=A(PIDEL)                                  
         XR    RE,RE                                                            
*                                                                               
FBOS10   CLI   0(RF),0             END OF RECORD                                
         JE    FBOS20                                                           
         CLI   0(RF),TIMELQ        TIMESHEET ELEMENT                            
         JNE   *+6                                                              
         LR    R0,RF               R1=A(TIMEL)                                  
         CLI   0(RF),PIDELQ        PIDEL?                                       
         JNE   *+6                                                              
         LR    R1,RF               R1=A(PIDEL)                                  
*                                                                               
         ICM   RE,1,1(RF)          BUMP TO NEXT                                 
         JZ    FBOS20                                                           
         AR    RF,RE                                                            
         J     FBOS10                                                           
*                                                                               
FBOS20   LTR   R0,R0               NO TIMEL                                     
         JZ    NO                                                               
         LTR   R1,R1               NO PIDEL                                     
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE TIME SHEET RECORD                                        *         
***********************************************************************         
INITBOS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXBOSDL          R1=L'BOS RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD ALL RECORD DATA                                                *         
***********************************************************************         
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD10   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLI   3(R3),5                                                          
         BNE   LOAD12                                                           
         CLC   VERSION,3(R3)                                                    
         BE    LOAD14                                                           
         B     LOAD30                                                           
*                                                                               
LOAD12   CLC   VERSION,3(R3)                                                    
         BL    LOAD30                                                           
LOAD14   MVC   TYPECODE,0(R3)                                                   
*                                                                               
         CLC   SXDTTYP,=C'NOT'     DO WE WANT TO RUN W/O T/S INFO?              
         JNE   LOAD20                                                           
         CLC   TYPECODE,=C'TAL'    IF T/S ALLOC-SKIP                            
         JE    LOAD30                                                           
         CLC   TYPECODE,=C'TIS'    IF TEMPO XREF-SKIP                           
         JE    LOAD30                                                           
*                                                                               
LOAD20   GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD30   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD10                                                           
*                                                                               
LOADTAB  DS    0XL8                                                             
         DC    CL3'CPY',AL1(0),AL4(LOADCPY) Company                             
         DC    CL3'LED',AL1(0),AL4(LOADLED) Ledger                              
         DC    CL3'LES',AL1(0),AL4(LOADLES) Ledger Structure                    
         DC    CL3'OGR',AL1(0),AL4(LOADOGR) Office Group                        
         DC    CL3'DEG',AL1(0),AL4(LOADDEG) Department Group                    
         DC    CL3'OFF',AL1(0),AL4(LOADOFF) Office                              
         DC    CL3'DEP',AL1(0),AL4(LOADDEP) Department                          
         DC    CL3'SBD',AL1(0),AL4(LOADSBD) Sub-Department                      
         DC    CL3'P1R',AL1(0),AL4(LOADP1R) Person 1R                           
         DC    CL3'PER',AL1(0),AL4(LOADPER) Person                              
         DC    CL3'PEA',AL1(0),AL4(LOADPEA) Person Assignment                   
         DC    CL3'OFL',AL1(0),AL4(LOADOFL) Office List                         
         DC    CL3'OLE',AL1(0),AL4(LOADOLE) Office List Entry                   
         DC    CL3'PED',AL1(0),AL4(LOADPED) Period                              
         DC    CL3'LOC',AL1(0),AL4(LOADLOC) Locality                            
         DC    CL3'LCI',AL1(0),AL4(LOADLCI) Locality Information                
         DC    CL3'MGR',AL1(0),AL4(LOADMGR) Media Group                         
         DC    CL3'MED',AL1(0),AL4(LOADMED) Media                               
         DC    CL3'POF',AL1(0),AL4(LOADPOF) Production Office                   
         DC    CL3'CLI',AL1(0),AL4(LOADCLI) Client                              
         DC    CL3'PRO',AL1(0),AL4(LOADPRO) Product                             
         DC    CL3'JOB',AL1(0),AL4(LOADJOB) Job                                 
         DC    CL3'TSK',AL1(0),AL4(LOADTSK) Task                                
         DC    CL3'TGR',AL1(2),AL4(LOADTGR) Taskcode Group                      
         DC    CL3'NCT',AL1(0),AL4(LOADNCT) Non-Client Time                     
         DC    CL3'INA',AL1(0),AL4(LOADINA) Income Account                      
         DC    CL3'ISA',AL1(0),AL4(LOADISA) Income Suspense Account             
         DC    CL3'ADR',AL1(0),AL4(LOADADR) Adjusted Rate                       
         DC    CL3'CSA',AL1(0),AL4(LOADCSA) Costing Account                     
         DC    CL3'EDT',AL1(0),AL4(LOADEDT) Edit Hours                          
         DC    CL3'RAT',AL1(0),AL4(LOADRAT) Rate                                
         DC    CL3'STH',AL1(0),AL4(LOADSTH) Standard Hours                      
         DC    CL3'OPT',AL1(0),AL4(LOADOPT) Option                              
         DC    CL3'TIS',AL1(0),AL4(LOADTIS) Timesheet                           
         DC    CL3'TAL',AL1(0),AL4(LOADTAL) Timesheet Allocation (+TAT)         
         DC    CL3'CAP',AL1(0),AL4(LOADCAP) Cost Alloc Profile                  
*        DC    CL3'CAL',AL1(0),AL4(LOADCAL) Calendar (18NOV94 COMMENT)          
         DC    CL3'DTM',AL1(5),AL4(LOADDTM) Daily Time Record                   
         DC    CL3'BOS',AL1(0),AL4(LOADBOS) BrandO Status Record                
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,UPDTTAB                                                       
UPDT10   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLI   3(R3),5                                                          
         BNE   UPDT12                                                           
         CLC   VERSION,3(R3)                                                    
         BE    UPDT14                                                           
         B     UPDT30                                                           
UPDT12   CLC   VERSION,3(R3)                                                    
         BL    UPDT30                                                           
UPDT14   MVC   TYPECODE,0(R3)                                                   
*                                                                               
         CLC   SXDTTYP,=C'NOT'     DO WE WANT TO RUN W/O T/S INFO?              
         JNE   UPDT20                                                           
         CLC   TYPECODE,=C'TAL'    IF T/S ALLOC-SKIP                            
         JE    UPDT30                                                           
         CLC   TYPECODE,=C'TIS'    IF TEMPO XREF-SKIP                           
         JE    UPDT30                                                           
*                                                                               
UPDT20   GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT30   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT10                                                           
*                                                                               
UPDTTAB  DS    0XL8                                                             
         DC    CL3'CPY',AL1(0),AL4(UPDTCPY) Company                             
         DC    CL3'LED',AL1(0),AL4(UPDTLED) Ledger                              
         DC    CL3'LES',AL1(0),AL4(UPDTLES) Ledger Structure                    
         DC    CL3'OGR',AL1(0),AL4(UPDTOGR) Office Group                        
         DC    CL3'DEG',AL1(0),AL4(UPDTDEG) Department Group                    
         DC    CL3'OFF',AL1(0),AL4(UPDTOFF) Office                              
         DC    CL3'DEP',AL1(0),AL4(UPDTDEP) Department                          
         DC    CL3'SBD',AL1(0),AL4(UPDTSBD) Sub-Department                      
         DC    CL3'P1R',AL1(0),AL4(UPDTP1R) Person 1R                           
         DC    CL3'PER',AL1(0),AL4(UPDTPER) Person                              
         DC    CL3'PEA',AL1(0),AL4(UPDTPEA) Person Assignment                   
         DC    CL3'OFL',AL1(0),AL4(UPDTOFL) Office List                         
         DC    CL3'OLE',AL1(0),AL4(UPDTOLE) Office List Entry                   
         DC    CL3'PED',AL1(0),AL4(UPDTPED) Period                              
         DC    CL3'LOC',AL1(0),AL4(UPDTLOC) Locality                            
         DC    CL3'LCI',AL1(0),AL4(UPDTLCI) Locality Information                
         DC    CL3'MGR',AL1(0),AL4(UPDTMGR) Media Group                         
         DC    CL3'MED',AL1(0),AL4(UPDTMED) Media                               
         DC    CL3'POF',AL1(0),AL4(UPDTPOF) Production Office                   
         DC    CL3'CLI',AL1(0),AL4(UPDTCLI) Client                              
         DC    CL3'PRO',AL1(0),AL4(UPDTPRO) Product                             
         DC    CL3'JOB',AL1(0),AL4(UPDTJOB) Job                                 
         DC    CL3'TSK',AL1(0),AL4(UPDTTSK) Task                                
         DC    CL3'TGR',AL1(2),AL4(UPDTTGR) Taskcode Group                      
         DC    CL3'NCT',AL1(0),AL4(UPDTNCT) Non-Client Time                     
         DC    CL3'INA',AL1(0),AL4(UPDTINA) Income Account                      
         DC    CL3'ISA',AL1(0),AL4(UPDTISA) Income Suspense Account             
         DC    CL3'ADR',AL1(0),AL4(UPDTADR) Adjusted Rate                       
         DC    CL3'CSA',AL1(0),AL4(UPDTCSA) Costing Account                     
         DC    CL3'EDT',AL1(0),AL4(UPDTEDT) Edit Hours                          
         DC    CL3'RAT',AL1(0),AL4(UPDTRAT) Rate                                
         DC    CL3'STH',AL1(0),AL4(UPDTSTH) Standard Hours                      
         DC    CL3'OPT',AL1(0),AL4(UPDTOPT) Option                              
         DC    CL3'TIS',AL1(0),AL4(UPDTTIS) Timesheet                           
         DC    CL3'TAL',AL1(0),AL4(UPDTTAL) Timesheet Allocation (+TAT)         
         DC    CL3'CAP',AL1(0),AL4(UPDTCAP) Cost Alloc Profile                  
*        DC    CL3'CAL',AL1(0),AL4(UPDTCAL) Calendar (18NOV94 COMMENT)          
         DC    CL3'DTM',AL1(5),AL4(UPDTDTM) Daily Time Record                   
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
INITALL  NTR1  BASE=*,LABEL=*                                                   
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R3,DXAXREC          R3=A(EXTRACT RECORD AREA)                    
         USING DXHDRD,R3                                                        
*                                                                               
         LA    RF,DXHDRHL                                                       
         SLL   RF,16                                                            
         ST    RF,DXHDRLEN         SET MINIMUM RECORD LEN IN HDR                
         MVI   DXHDRRTY-1,MXTRTQ                                                
         MVC   DXHDRRTY,DXACTION                                                
         MVI   DXHDRCDT-1,MXTRTQ                                                
         CLI   DXMODE,DXLOADQ      LOAD MODE?                                   
         JE    IALL02              YES                                          
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(X'20',DXHDRCDT+2)                        
         MVC   DXHDRCDT(2),DXCENT                                               
         MVI   DXHDRCTI-1,MXTRTQ                                                
         ICM   RF,15,RTIME          FORMAT DATE & TIME FROM RECOVERY            
         TM    RTIME,X'80'                                                      
         JNO   *+12                                                             
         SLL   RF,1                                                             
         SRL   RF,5                                                             
         XC    DUB,DUB                                                          
         STCM  RF,15,DUB+4                                                      
         OI    DUB+7,X'0C'                                                      
         UNPK  DXHDRCTI(6),DUB+4(4)                                             
         OI    DXHDRCTI+5,X'F0'                                                 
         J     YES                                                              
*                                  HERE IF LOAD MODE                            
IALL02   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
         MVI   DXHDRCTI-1,MXTRTQ                                                
         MVC   DXHDRCTI,DXTIMEN                                                 
         CLI   DXDAFORM,C'Y'                                                    
         JNE   YES                                                              
         MVI   DXHDRCDT+00,C''''                                                
         MVC   DXHDRCDT+01(6),DXDATEN+2                                         
         MVC   DXHDRCDT+07(2),=C'  '                                            
         MVC   DXHDRCDT+09(2),DXTIMEN                                           
         MVI   DXHDRCDT+11,C':'                                                 
         MVC   DXHDRCDT+12(2),DXTIMEN+2                                         
         MVI   DXHDRCDT+14,C''''                                                
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R3,R5                                                            
***********************************************************************         
* LOAD CALENDAR RECORDS                                               *         
***********************************************************************         
LOADCAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CAL RECORD             
         USING CASRECD,R2                                                       
         XC    CASKEY,CASKEY                                                    
         MVC   CASKCPY,COMPANY                                                  
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCAL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   CASKEY(CASKEMOA-CASKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         MVC   ACCADDR,CASKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXCALC,AINITCAL,AFILTCAL                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCAL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE CALENDAR RECORD DATA                                         *         
***********************************************************************         
UPDTCAL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CASRECD,R2                                                       
*                                                                               
         GOTO1 AFILTCAL                                                         
         JNE   YES                                                              
         GOTO1 AINITCAL                                                         
         GOTO1 AACCUPDT,DMCB,VAXCALC,=C'CAL'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER CAL RECORD AT R2                                             *         
***********************************************************************         
FILTCAL  NTR1  BASE=*,LABEL=*                                                   
         USING CASRECD,R2                                                       
         CLC   CASKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   CASKTYP,CASKTYPQ    X'3E'                                        
         JNE   NO                                                               
         CLI   CASKSUB,CASKSUBQ    X'0B'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE CALENDAR RECORD                                          *         
***********************************************************************         
INITCAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCALDL          R1=L'CAL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD ADJUSTED RATE RECORD                                           *         
***********************************************************************         
LOADADR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ADR RECORD             
         USING PAJRECD,R2                                                       
         XC    PAJKEY,PAJKEY                                                    
         MVC   PAJKCPY,COMPANY                                                  
         MVI   PAJKTYP,PAJKTYPQ    X'19' RECORD                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             ERROR ON READ HIGH                           
         JNE   NO                                                               
*                                                                               
LADR02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   PAJKEY(PAJKOFF-PAJKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         MVC   ACCADDR,PAJKDA                                                   
         GOTO1 AFILTADR            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LADR08                                                           
*                                                                               
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON GETREC                              
*                                                                               
         GOTO1 AINITADR            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXADRC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LADR04   GOTO1 VAXADRC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   8(R1),FF                                                         
         JE    LADR08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LADR08              DO NOT WRITE RECORDS TO FILE                 
*                                                                               
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    LADR06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
LADR06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             PUT RECORD & DECREMENT IO COUNT              
         JNE   YES                                                              
         J     LADR04                                                           
*                                                                               
LADR08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LADR10                                                           
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LADR10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LADR02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE ADJUSTED RATE RECORD                                         *         
***********************************************************************         
UPDTADR  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PAJRECD,R2                                                       
         GOTO1 AFILTADR            FILTER IT                                    
         JNE   YES                 NO MATCH                                     
*                                                                               
         L     R3,DXAXREC                                                       
         USING AXADRD,R3                                                        
         GOTO1 AINITADR                                                         
         CLI   AXADRACT,C'C'       CHANGE?                                      
         JNE   UADR04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXADRC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
         MVI   AXADRACT,C'K'                                                    
         CLI   DXWRITE,C'Y'        WRITE THIS RECORD?                           
         JNE   UADR04              NO                                           
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UADR02              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(C'K',TYPECODE),   *        
               VERSION             CONVERT TO SQL FORMAT                        
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UADR02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
         MVI   AXADRACT,C'A'       NOW ADD COPY RECORD DETAILS                  
         DROP  R3                                                               
*                                                                               
UADR04   GOTO1 VAXADRC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
UADR06   GOTO1 VAXADRC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF            FINISHED?                                    
         JE    YES                                                              
         CLI   DXWRITE,C'Y'                                                     
         JNE   UADR06              DO NOT WRITE RECORDS                         
         L     RF,DXAXREC          RF=A(UNCONVERTED RECORD)                     
         CLI   SXDTPLFM,0                                                       
         JE    UADR08              DO NOT CONVERT RECORDS                       
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
*                                                                               
UADR08   GOTO1 DXPUT,DMCB,(RF),(R7) PUT UNCONVERTED REC TO EXTRACT              
         J     UADR06                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER ADR RECORD AT R2                                             *         
***********************************************************************         
FILTADR  NTR1  BASE=*,LABEL=*                                                   
         USING PAJRECD,R2                                                       
         CLC   PAJKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   PAJKTYP,PAJKTYPQ    X'19'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE ADJUSTED RATE RECORD                                     *         
***********************************************************************         
INITADR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXADRDL          R1=L'ADR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT RECORD                                                  *         
***********************************************************************         
LOADCLI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG             GET LEDGER STRUCTURE                         
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CLI RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI             INITIAL READ HIGH                            
         JNE   NO                                                               
*                                                                               
LCLI02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF C/U/L CHANGES                    
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXCLIC,AINITCLI,AFILTCLI                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLI02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE CLIENT RECORD DATA                                           *         
***********************************************************************         
UPDTCLI  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG                                                          
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTCLI            CLIENT RECORD?                               
         JNE   YES                 NO                                           
*                                                                               
         GOTO1 AINITCLI                                                         
         GOTO1 AACCUPDT,DMCB,VAXCLIC,=C'CLI'                                    
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER CLI RECORD AT R2                                             *         
***********************************************************************         
FILTCLI  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT,SPACES      MUST HAVE ACCOUNT CODE                       
         JNH   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
*                                                                               
         GOTO1 ARDLEDG             GET LEDGER STRUCTURE                         
*                                                                               
         LA    RF,L'ACTKACT                                                     
         XR    RE,RE                                                            
         IC    RE,L1               LENGTH OF CLIENT CODE                        
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(DATA AFTER CLIENT CODE)                 
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FCLICLC                                                       
         JNE   NO                                                               
*                                                                               
         CLI   DXMODE,DXLOADQ      LOAD?                                        
         JNE   YES                 NO                                           
         TM    ACTKSTAT,ACTSDRFT   CLIENT DRAFT?                                
         JNZ   NO                  YES                                          
*&&US*&& J     YES                                                              
*&&UK                                                                           
         CLI   DXUSER,C'Y'         STRIP LOCKED CLIENTS                         
         JNE   YES                 NO                                           
         TM    ACTKSTAT,ACTSLOCK   CLIENT LOCKED?                               
         JNZ   NO                  YES                                          
         J     YES                                                              
*&&                                                                             
FCLICLC  CLC   0(0,RE),SPACES      THIS MUST BE EMPTY                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE CLIENT RECORD                                            *         
***********************************************************************         
INITCLI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCLIDL          R1=L'CLI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD COMPANY RECORD                                                 *         
***********************************************************************         
LOADCPY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C' '                                                        
         MVI   LEDGER,C' '                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CPY RECORD             
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES       SPACE FILL THE KEY                           
         MVC   ACTKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCPY02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKCPY,COMPANY     ALL DONE IF COMPANY CHANGES                  
         JNE   YES                                                              
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AFILTCPY            FILTER ROUTINE?                              
         JNE   YES                 NOT VALID                                    
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITCPY            INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 VAXCPYC,DMCB,DXAXREC,(R2),0,(R6),LEVELS                          
         TM    DMCB+8,X'80'                                                     
         JO    YES                 ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    YES                 DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 CONTROLLER REQUESTS NO WRITE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LCPY04              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',DXAXREC),(1,DXASQLB),(0,TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LCPY04   GOTO1 DXPUT,DMCB,(RF),(R7) UNCONVERTED RECORD TO EXTRACT               
         J     YES                 DECREMENT IO COUNT                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE COMPANY RECORD DATA                                          *         
***********************************************************************         
UPDTCPY  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
*                                                                               
         GOTO1 AFILTCPY                                                         
         JNE   YES                                                              
         GOTO1 AINITCPY                                                         
         GOTO1 AACCUPDT,DMCB,VAXCPYC,=C'CPY'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER CPY RECORD AT R2                                             *         
***********************************************************************         
FILTCPY  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKULA(L'ACTKULA),SPACES                                        
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE COMPANY RECORD                                           *         
***********************************************************************         
INITCPY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCPYDL          R1=L'CPY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD COSTING ACCOUNT RECORD                                         *         
***********************************************************************         
LOADCSA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'C'                                                      
         XC    DISPS,DISPS         FIDDLE FOR RDHIS                             
         XC    CURRUL,CURRUL                                                    
         MVI   D1,L'ACTKACT                                                     
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CSA RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCSA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXCSAC,AINITCSA,AFILTCSA                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCSA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE COSTING ACCOUNT RECORD DATA                                  *         
***********************************************************************         
UPDTCSA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
*                                                                               
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'C'                                                      
         GOTO1 AFILTCSA                                                         
         JNE   YES                                                              
         GOTO1 AINITCSA                                                         
         GOTO1 AACCUPDT,DMCB,VAXCSAC,=C'CSA'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER CSA RECORD AT R2                                             *         
***********************************************************************         
FILTCSA  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
         CLC   ACTKACT,SPACES      THIS MUST NOT BE EMPTY                       
         JE    NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
*                                                                               
         TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE COSTING ACCOUNT RECORD                                   *         
***********************************************************************         
INITCSA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCSADL          R1=L'CSA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD DEPARTMENT RECORD                                              *         
***********************************************************************         
LOADDEP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST DEP RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDEP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXDEPC,AINITDEP,AFILTDEP                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDEP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE DEPARTMENT RECORD DATA                                       *         
***********************************************************************         
UPDTDEP  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTDEP                                                         
         JNE   YES                                                              
         GOTO1 AINITDEP                                                         
         GOTO1 AACCUPDT,DMCB,VAXDEPC,=C'DEP'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER DEP RECORD AT R2                                             *         
***********************************************************************         
FILTDEP  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
         GOTO1 ARDLEDG                                                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,L2             LENGTH OF DEPARTMENT CODE                    
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE A LENGTH OR DIE                    
*                                                                               
         XR    RE,RE                                                            
         IC    RE,L1               LENGTH OF CLIENT CODE                        
         LA    RE,ACTKACT(RE)      DISP TO START OF DEPARTMENT                  
         BCTR  RF,0                                                             
         EX    RF,FDEPCLC                                                       
         JE    NO                                                               
         XR    RE,RE                                                            
         IC    RE,L1               LENGTH OF CLIENT CODE                        
         LA    RE,1(RF,RE)         LENGTH CLIENT+DEPT CODES                     
         LA    RF,L'ACTKACT        LENGTH OF WHOLE THING                        
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(DATA AFTER DEPT CODE)                   
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FDEPCLC                                                       
         JNE   NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
*                                                                               
         TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
FDEPCLC  CLC   0(0,RE),SPACES      NEED A DEPARTMENT                            
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE DEPARTMENT RECORD                                        *         
***********************************************************************         
INITDEP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXDEPDL          R1=L'DEP RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD DEPARTMENT GROUP RECORD                                        *         
***********************************************************************         
LOADDEG  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'4'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST DEG RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDEG02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXDEGC,AINITDEG,AFILTDEG                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDEG02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE DEPT GROUP RECORD DATA                                       *         
***********************************************************************         
UPDTDEG  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'4'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTDEG                                                         
         JNE   YES                                                              
         GOTO1 AINITDEG                                                         
         GOTO1 AACCUPDT,DMCB,VAXDEGC,=C'DEG'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER DEG RECORD AT R2                                             *         
***********************************************************************         
FILTDEG  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT(1),SPACES   NEED DEPT GROUP                              
         JE    NO                                                               
         CLC   ACTKACT+1(ACTKSTA-ACTKACT-1),SPACES                              
         JNE   NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
*                                                                               
         TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE DEPARTMENT GROUP RECORD                                  *         
***********************************************************************         
INITDEG  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXDEGDL          R1=L'DEG RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD INCOME ACCOUNT RECORD                                          *         
***********************************************************************         
LOADINA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'I'                                                      
         XC    CURRUL,CURRUL                                                    
         XC    DISPS,DISPS         FIDDLE FOR RDHIS                             
         MVI   D1,L'ACTKACT                                                     
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST INA RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LINA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXINAC,AINITINA,AFILTINA                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LINA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE INCOME ACCOUNT RECORD DATA                                   *         
***********************************************************************         
UPDTINA  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'I'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTINA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITINA                                                         
         GOTO1 AACCUPDT,DMCB,VAXINAC,=C'INA'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER INA RECORD AT R2                                             *         
***********************************************************************         
FILTINA  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT,SPACES      THIS MUST NOT BE EMPTY                       
         JE    NO                                                               
*                                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY RECORDS                  
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
         TM    0(R1),ACTSABLP      NEEDS BALANCE ELEMENT                        
         JO    FINA02                                                           
         J     NO                                                               
*                                                                               
FINA02   TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE INCOME ACCOUNT RECORD                                    *         
***********************************************************************         
INITINA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXINADL          R1=L'INA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD INCOME SUSPENSE ACCOUNT RECORD                                 *         
***********************************************************************         
LOADISA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'K'                                                      
         XC    DISPS,DISPS         FIDDLE FOR RDHIS                             
         XC    CURRUL,CURRUL                                                    
         MVI   D1,L'ACTKACT                                                     
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ISA RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LISA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXISAC,AINITISA,AFILTISA                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LISA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE INCOME SUSPENSE ACCOUNT RECORD DATA                          *         
***********************************************************************         
UPDTISA  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'K'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTISA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITISA                                                         
         GOTO1 AACCUPDT,DMCB,VAXISAC,=C'ISA'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER ISA RECORD AT R2                                             *         
***********************************************************************         
FILTISA  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT,SPACES      THIS MUST NOT BE EMPTY                       
         JE    NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
         TM    0(R1),ACTSABLP      NEEDS BALANCE ELEMENT                        
         JO    FISA02                                                           
         J     NO                                                               
*                                                                               
FISA02   TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE INCOME SUSPENSE ACCOUNT                                  *         
***********************************************************************         
INITISA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXISADL          R1=L'ISA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD JOB RECORD                                                     *         
***********************************************************************         
LOADJOB  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST JOB RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LJOB02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXJOBC,AINITJOB,AFILTJOB                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LJOB02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* UPDATE JOB RECORD DATA                                              *         
***********************************************************************         
UPDTJOB  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTJOB                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITJOB                                                         
         GOTO1 AACCUPDT,DMCB,VAXJOBC,=C'JOB'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
***********************************************************************         
* FILTER JOB RECORD AT R2                                             *         
***********************************************************************         
FILTJOB  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT,SPACES      MUST HAVE SOMETHING IN IT                    
         JE    NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
*                                                                               
         LA    RF,L'ACTKACT                                                     
         XR    RE,RE                                                            
         IC    RE,L1               LENGTH OF CLIENT CODE                        
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(DATA AFTER CLIENT CODE)                 
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FJOBCLC                                                       
         JNE   FJOB02                                                           
         MVC   CLISTAT,ACTKSTAT                                                 
         J     NO                                                               
*                                                                               
FJOB02   XR    RE,RE                                                            
         IC    RE,L1                                                            
         XR    RF,RF                                                            
         IC    RF,L2                                                            
         AR    RE,RF                                                            
         LA    RF,L'ACTKACT                                                     
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(DATA AFTER PRODUCT CODE)                
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FJOBCLC          THIS MUST BE EMPTY FOR A PRODUCT             
         JNE   FJOB04                                                           
         MVC   PROSTAT,ACTKSTAT                                                 
         J     NO                                                               
*                                                                               
FJOB04   LA    RE,L'ACTKACT                                                     
         XR    RF,RF                                                            
         IC    RF,L3                                                            
         SR    RE,RF                                                            
         LA    RE,ACTKACT(RE)      DISPLACEMENT TO JOB CODE                     
         BCTR  RF,0                LEN-1 OF THE JOB CODE                        
         EX    RF,FJOBCLC          MUST BE A JOB CODE PRESENT                   
         JE    NO                                                               
         CLI   DXMODE,DXLOADQ      LOAD                                         
         JNE   YES                                                              
         TM    CLISTAT,ACTSDRFT    TEST CLIENT DRAFT                            
         JNZ   NO                                                               
         TM    PROSTAT,ACTSDRFT    TEST PRODUCT DRAFT                           
         JNZ   NO                                                               
         TM    ACTKSTAT,ACTSDRFT   TEST JOB DRAFT                               
         JNZ   NO                                                               
*&&US*&& J     YES                                                              
*&&UK                                                                           
         CLI   DXUSER,C'Y'         STRIP LOCKED                                 
         JNE   YES                                                              
         TM    CLISTAT,ACTSLOCK    TEST CLIENT LOCKED                           
         JNZ   NO                                                               
         TM    PROSTAT,ACTSLOCK    TEST PRODUCT LOCKED                          
         JNZ   NO                                                               
         TM    ACTKSTAT,ACTSLOCK   TEST JOB LOCKED                              
         JNZ   NO                                                               
         J     YES                                                              
*&&                                                                             
*                                                                               
FJOBCLC  CLC   0(0,RE),SPACES                                                   
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE JOB RECORD                                               *         
***********************************************************************         
INITJOB  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXJOBDL          R1=L'JOB RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LEDGER RECORD                                                  *         
***********************************************************************         
LOADLED  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   BYTE,1                                                           
         LA    RF,1                                                             
*                                                                               
LLED02   SLL   RF,2                                                             
         B     *+0(RF)                                                          
         J     LLED04                                                           
         J     LLED06                                                           
         J     LLED08                                                           
         J     LLED10                                                           
         J     LLED12                                                           
         J     LLED14                                                           
         J     YES                                                              
*                                                                               
LLED04   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         J     LLED16                                                           
*                                                                               
LLED06   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'K'                                                      
         J     LLED16                                                           
*                                                                               
LLED08   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'I'                                                      
         J     LLED16                                                           
*                                                                               
LLED10   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'C'                                                      
         J     LLED16                                                           
*                                                                               
LLED12   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'N'                                                      
         J     LLED16                                                           
*                                                                               
LLED14   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         J     LLED16                                                           
*                                                                               
LLED16   LA    R2,IOKEY            SET KEY TO READ FIRST LED RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),ACCDIR,IOKEY,(R2),DMWORK            
*                                                                               
         TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         TM    DMCB+8,X'10'        CONTINUE IF NOT FOUND                        
         JO    LLED20                                                           
         CLI   DMCB+8,0            DIE IF DISK ERROR                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXLEDC,AINITLED,AFILTLED                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JZ    YES                                                              
*                                                                               
LLED20   XR    RF,RF                                                            
         IC    RF,BYTE                                                          
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         J     LLED02                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE LEDGER RECORD DATA                                           *         
***********************************************************************         
UPDTLED  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING LDGRECD,R2                                                       
         CLC   LDGKCPY,COMPANY     COMPANY OK?                                  
         JNE   YES                                                              
*                                                                               
         LA    RF,ULEDLST                                                       
ULED02   CLI   0(RF),0             END OF LIST                                  
         JE    YES                                                              
         CLC   LDGKUNT(2),0(RF)                                                 
         JE    ULED04                                                           
         LA    RF,2(RF)                                                         
         J     ULED02                                                           
*                                                                               
ULEDLST  DC    CL2'SJ'                                                          
         DC    CL2'SK'                                                          
         DC    CL2'SI'                                                          
         DC    CL2'1C'                                                          
         DC    CL2'1N'                                                          
         DC    CL2'1R'                                                          
         DC    X'00'                                                            
*                                                                               
ULED04   MVC   UNIT,0(RF)                                                       
         MVC   LEDGER,1(RF)                                                     
         GOTO1 AFILTLED                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITLED                                                         
         GOTO1 AACCUPDT,DMCB,VAXLEDC,=C'LED'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER LED RECORD AT R2                                             *         
***********************************************************************         
FILTLED  NTR1  BASE=*,LABEL=*                                                   
         USING LDGRECD,R2                                                       
         LA    RE,L'LDGKEY                                                      
         LA    RF,LDGKEND                                                       
         SR    RE,RF                                                            
         BCTR  RE,0                LEN-1 OF THE STUFF AT THE END                
         EX    RE,FLEDCLC                                                       
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
FLEDCLC  CLC   LDGKEY+LDGKEND(0),SPACES MUST BE NOTHING ELSE BUT U/L            
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE LEDGER RECORD                                            *         
***********************************************************************         
INITLED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLEDDL          R1=L'LED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LEDGER STRUCTURE RECORD                                        *         
***********************************************************************         
LOADLES  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   BYTE,1                                                           
         LA    RF,1                                                             
*                                                                               
LLES02   SLL   RF,2                                                             
         B     *+0(RF)                                                          
         J     LLES04                                                           
         J     LLES06                                                           
         J     LLES08                                                           
         J     LLES10                                                           
         J     LLES12                                                           
         J     LLES14                                                           
         J     YES                                                              
*                                                                               
LLES04   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         B     LLES16                                                           
*                                                                               
LLES06   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'K'                                                      
         J     LLES16                                                           
*                                                                               
LLES08   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'I'                                                      
         J     LLES16                                                           
*                                                                               
LLES10   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'C'                                                      
         J     LLES16                                                           
*                                                                               
LLES12   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'N'                                                      
         J     LLES16                                                           
*                                                                               
LLES14   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         J     LLES16                                                           
*                                                                               
LLES16   LA    R2,IOKEY            SET KEY TO READ FIRST LES RECORD             
         USING ACTRECD,R2                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,(X'00',DMREAD),ACCDIR,IOKEY,(R2),DMWORK            
*                                                                               
LLES18   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         TM    DMCB+8,X'10'        CONTINUE IF NOT FOUND                        
         JO    LLES24                                                           
         CLI   DMCB+8,0            DIE IF DISK ERROR                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AFILTLES            FILTER RECORD, GET NEXT IF NOT VALID         
         BNE   LLES24                                                           
*                                                                               
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AINITLES            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXLESC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                                                               
LLES20   GOTO1 VAXLESC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   DMCB+8,FF                                                        
         BE    LLES24              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         BNE   LLES20              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         BE    LLES22              DO NOT CONVERT RECORDS                       
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',DXAXREC),(1,DXASQLB),(0,TYPENAME),   *        
               VERSION                                                          
         L     RF,DXASQLB                                                       
*                                                                               
LLES22   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LLES20                                                           
         J     YES                                                              
*                                                                               
LLES24   OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JZ    YES                                                              
         XR    RF,RF                                                            
         ZIC   RF,BYTE                                                          
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         J     LLES02                                                           
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE LEDGER STRUCTURE RECORD DATA                                 *         
***********************************************************************         
UPDTLES  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING LDGRECD,R2                                                       
         CLC   LDGKCPY,COMPANY     COMPANY OK?                                  
         JNE   YES                                                              
*                                                                               
         LA    RF,ULESLST                                                       
ULES02   CLI   0(RF),0             END OF LIST                                  
         JE    YES                                                              
         CLC   LDGKUNT(2),0(RF)                                                 
         JE    ULES04                                                           
         LA    RF,2(RF)                                                         
         J     ULES02                                                           
*                                                                               
ULESLST  DC    CL2'SJ'                                                          
         DC    CL2'SK'                                                          
         DC    CL2'SI'                                                          
         DC    CL2'1C'                                                          
         DC    CL2'1N'                                                          
         DC    CL2'1R'                                                          
         DC    X'00'                                                            
*                                                                               
ULES04   MVC   UNIT,0(RF)                                                       
         MVC   LEDGER,1(RF)                                                     
         GOTO1 AFILTLES                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITLES                                                         
         L     R3,DXAXREC          EXTRACT BUILT HERE                           
         USING AXLESD,R3                                                        
         CLI   AXLESACT,C'C'       CHANGE?                                      
         JNE   ULES08              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXLESC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
         MVI   AXLESACT,C'K'                                                    
         CLI   DXWRITE,C'Y'        WRITE THIS RECORD?                           
         JNE   ULES08              NO                                           
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ULES06              NO CONVERSION                                
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(C'K',TYPECODE),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
ULES06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
*                                                                               
         MVI   AXLESACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
ULES08   GOTO1 VAXLESC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
ULES10   GOTO1 VAXLESC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                                                              
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   ULES14              NO RECORD WRITE                              
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ULES12              NO EXTRACT CONVERSION                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
ULES12   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
ULES14   GOTO1 ADECIOC                                                          
         JE    ULES10                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER LES RECORD AT R2                                             *         
***********************************************************************         
FILTLES  NTR1  BASE=*,LABEL=*                                                   
         USING LDGRECD,R2                                                       
         CLC   LDGKEY+LDGKEND(L'LDGKEY-LDGKEND),SPACES                          
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE LEDGER STRUCTURE RECORD                                  *         
***********************************************************************         
INITLES  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLESDL          R1=L'LES RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LOCALITY RECORD                                                *         
***********************************************************************         
LOADLOC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST LOC RECORD             
         USING SUTRECD,R2                                                       
         XC    SUTKEY,SUTKEY                                                    
         MVC   SUTKCPY,COMPANY                                                  
         MVI   SUTKTYP,SUTKTYPQ                                                 
         MVI   SUTKSUB,SUTKSUBQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LLOC02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   SUTKEY(SUTKLOC-SUTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,SUTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXLOCC,AINITLOC,AFILTLOC                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LLOC02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE LOCALITY RECORD DATA                                         *         
***********************************************************************         
UPDTLOC  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING SUTRECD,R2                                                       
*                                                                               
         GOTO1 AFILTLOC            FILTER LOCALITY RECORD                       
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITLOC                                                         
         GOTO1 AACCUPDT,DMCB,VAXLOCC,=C'LOC'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER LOC RECORD AT R2                                             *         
***********************************************************************         
FILTLOC  NTR1  BASE=*,LABEL=*                                                   
         USING SUTRECD,R2                                                       
         CLC   SUTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   SUTKTYP,SUTKTYPQ    X'2D'                                        
         JNE   NO                                                               
         CLI   SUTKSUB,SUTKSUBQ    X'01'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE LOCALITY RECORD                                          *         
***********************************************************************         
INITLOC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLOCDL          R1=L'LOC RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LOCALITY INFORMATION RECORD                                    *         
***********************************************************************         
LOADLCI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST LCI RECORD             
         USING SUTRECD,R2                                                       
         XC    SUTKEY,SUTKEY                                                    
         MVC   SUTKCPY,COMPANY                                                  
         MVI   SUTKTYP,SUTKTYPQ                                                 
         MVI   SUTKSUB,SUTKSUBQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LLCI02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   SUTKEY(SUTKLOC-SUTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTLCI            FILTER RECORD                                
         JNE   LLCI08                                                           
*                                                                               
         MVC   ACCADDR,SUTKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
*                                                                               
         GOTO1 AINITLCI            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXLCIC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LLCI04   GOTO1 VAXLCIC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   8(R1),FF                                                         
         JE    LLCI08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LLCI08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LLCI06                                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LLCI06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LLCI04                                                           
         J     YES                                                              
*                                                                               
LLCI08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LLCI10                                                           
*                                                                               
         GOTO1 AREADHI                                                          
         JE    LLCI10                                                           
         DC    H'0'                                                             
*                                                                               
LLCI10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LLCI02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE LOCALITY INFORMATION RECORD                                  *         
***********************************************************************         
UPDTLCI  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING SUTRECD,R2                                                       
         GOTO1 AFILTLCI                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITLCI                                                         
*                                                                               
         L     R3,DXAXREC                                                       
         USING AXLCID,R3                                                        
         CLI   AXLCIACT,C'C'       CHANGE?                                      
         JNE   ULCI04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXLCIC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
*                                                                               
         MVI   AXLCIACT,C'K'                                                    
         CLI   DXWRITE,C'Y'        WRITE THIS RECORD?                           
         JNE   ULCI04              NO                                           
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ULCI02                                                           
*                                  CONVERT TO SQL FORMAT                        
         GOTO1 VAXCNVX,DMCB,(C'U',DXAXREC),(1,DXASQLB),(C'K',TYPECODE),*        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
ULCI02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         MVI   AXLCIACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
ULCI04   GOTO1 VAXLCIC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
ULCI06   GOTO1 VAXLCIC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   DMCB+8,FF                                                        
         JE    YES                 NO MORE RECORDS TO WRITE                     
         CLI   DXWRITE,C'Y'                                                     
         JNE   ULCI10              DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ULCI08              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
ULCI08   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
ULCI10   GOTO1 ADECIOC                                                          
         JE    ULCI06                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER LCI RECORD AT R2                                             *         
***********************************************************************         
FILTLCI  NTR1  BASE=*,LABEL=*                                                   
         USING SUTRECD,R2                                                       
         CLC   SUTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   SUTKTYP,SUTKTYPQ    X'2D'                                        
         JNE   NO                                                               
         CLI   SUTKSUB,SUTKSUBQ    X'01'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE LOCALITY INFORMATION RECORD                              *         
***********************************************************************         
INITLCI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLCIDL          R1=L'LCI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD MEDIA RECORD                                                   *         
***********************************************************************         
LOADMED  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST MED RECORD             
         USING PMDRECD,R2                                                       
         XC    PMDKEY,PMDKEY                                                    
         MVC   PMDKCPY,COMPANY                                                  
         MVI   PMDKTYP,PMDKTYPQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMED02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   PMDKEY(PMDKMED-PMDKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,PMDKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXMEDC,AINITMED,AFILTMED                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMED02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE MEDIA RECORD DATA                                            *         
***********************************************************************         
UPDTMED  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PMDRECD,R2                                                       
         GOTO1 AFILTMED                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITMED                                                         
         GOTO1 AACCUPDT,DMCB,VAXMEDC,=C'MED'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER MED RECORD AT R2                                             *         
***********************************************************************         
FILTMED  NTR1  BASE=*,LABEL=*                                                   
         USING PMDRECD,R2                                                       
         CLC   PMDKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   PMDKTYP,PMDKTYPQ    X'09'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE MEDIA RECORD                                             *         
***********************************************************************         
INITMED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXMEDDL          R1=L'MED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD MEDIA GROUP RECORD                                             *         
***********************************************************************         
LOADMGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST MGR RECORD             
         USING MGRRECD,R2                                                       
         XC    MGRKEY,MGRKEY                                                    
         MVI   MGRKTYP,MGRKTYPQ                                                 
         MVI   MGRKSUB,MGRKSUBQ                                                 
         MVC   MGRKCPY,COMPANY                                                  
         MVC   MGRKUNT,UNIT                                                     
         MVC   MGRKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMGR02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   MGRKEY(MGRKCODE-MGRKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,MGRKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXMGRC,AINITMGR,AFILTMGR                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMGR02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE MEDIA GROUP RECORD DATA                                      *         
***********************************************************************         
UPDTMGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING MGRRECD,R2                                                       
         GOTO1 AFILTMGR            FILTER RECORD                                
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITMGR            INITIALISE EXTRACT BUFFER                    
         GOTO1 AACCUPDT,DMCB,VAXMGRC,=C'MGR'                                    
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER MGR RECORD AT R2                                             *         
***********************************************************************         
FILTMGR  NTR1  BASE=*,LABEL=*                                                   
         USING MGRRECD,R2                                                       
         CLI   MGRKTYP,MGRKTYPQ    X'2C'                                        
         JNE   NO                                                               
         CLI   MGRKSUB,MGRKSUBQ    X'06'                                        
         JNE   NO                                                               
         CLC   MGRKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   MGRKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   MGRKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE MEDIA GROUP RECORD                                       *         
***********************************************************************         
INITMGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXMGRDL          R1=L'MGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD NON - CLIENT TIME RECORD                                       *         
***********************************************************************         
LOADNCT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'N'                                                      
         XC    DISPS,DISPS         FIDDLE FOR RDHIS                             
         XC    CURRUL,CURRUL                                                    
         MVI   D1,L'ACTKACT                                                     
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST NCT RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LNCT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXNCTC,AINITNCT,AFILTNCT                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LNCT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE NON - CLIENT TIME RECORD DATA                                *         
***********************************************************************         
UPDTNCT  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'N'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTNCT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 ARDLEDG                                                          
         GOTO1 AINITNCT                                                         
         GOTO1 AACCUPDT,DMCB,VAXNCTC,=C'NCT'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER NCT RECORD AT R2                                             *         
***********************************************************************         
FILTNCT  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT,SPACES      MUST HAVE A ACCOUNT CODE IN IT               
         JE    NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
*                                                                               
         TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE NON - CLIENT TIME ACCOUNT RECORD                         *         
***********************************************************************         
INITNCT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXNCTDL          R1=L'NCT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE RECORD                                                  *         
***********************************************************************         
LOADOFF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST OFF RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOFF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXOFFC,AINITOFF,AFILTOFF                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE RECORD DATA                                           *         
***********************************************************************         
UPDTOFF  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTOFF                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOFF                                                         
         GOTO1 AACCUPDT,DMCB,VAXOFFC,=C'OFF'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER OFF RECORD AT R2                                             *         
***********************************************************************         
FILTOFF  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
*                                                                               
         GOTO1 ARDLEDG             PULL OUT LEDGER STRUCTURE                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,L1               L' OFFICE CODE                               
         BCTR  RF,0                                                             
         EX    RF,FOFFCLC                                                       
         JE    NO                                                               
         LA    RF,1(RF)                                                         
         LA    RE,L'ACTKACT                                                     
         SR    RE,RF                                                            
         LA    RF,ACTKACT(RF)      DISP TO FIRST NON-OFFICE                     
         BCTR  RE,0                                                             
         EX    RE,FOFFCLC1                                                      
         JNE   NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
*                                                                               
         TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
FOFFCLC  CLC   ACTKACT(0),SPACES   OFFICE CODE MUST BE NON-ZERO                 
FOFFCLC1 CLC   0(0,RF),SPACES      MUST HAVE ONLY OFFICE CODE                   
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE OFFICE RECORD                                            *         
***********************************************************************         
INITOFF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOFFDL          R1=L'OFF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE GROUP RECORD                                            *         
***********************************************************************         
LOADOGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST OGR RECORD             
         USING OGRRECD,R2                                                       
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKGRPQ                                                 
         MVC   OGRKCPY,COMPANY                                                  
         MVC   OGRKUNT,UNIT                                                     
         MVC   OGRKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOGR02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   OGRKEY(OGRKCODE-OGRKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,OGRKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXOGRC,AINITOGR,AFILTOGR                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOGR02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE GROUP RECORD DATA                                     *         
***********************************************************************         
UPDTOGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING OGRRECD,R2                                                       
         GOTO1 AFILTOGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOGR                                                         
         GOTO1 AACCUPDT,DMCB,VAXOGRC,=C'OGR'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER OGR RECORD AT R2                                             *         
***********************************************************************         
FILTOGR  NTR1  BASE=*,LABEL=*                                                   
         USING OGRRECD,R2                                                       
         CLI   OGRKTYP,OGRKTYPQ    X'2C'                                        
         JNE   NO                                                               
         CLI   OGRKSUB,OGRKGRPQ    X'02'                                        
         JNE   NO                                                               
         CLC   OGRKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   OGRKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   OGRKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE OFFICE GROUP RECORD                                      *         
***********************************************************************         
INITOGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOGRDL          R1=L'OGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE LIST RECORD                                             *         
***********************************************************************         
LOADOFL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST OFL RECORD             
         USING OFFRECD,R2                                                       
         XC    OFFKEY,OFFKEY                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOFL02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   OFFKEY(OFFKCPY+1-OFFKEY),IOKEY                                   
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,OFFKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXOFLC,AINITOFL,AFILTOFL                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE LIST RECORD DATA                                      *         
***********************************************************************         
UPDTOFL  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING OFFRECD,R2                                                       
         GOTO1 AFILTOFL            FILTER OFFICE LIST                           
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOFL                                                         
         GOTO1 AACCUPDT,DMCB,VAXOFLC,=C'OFL'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER OFL RECORD AT R2                                             *         
***********************************************************************         
FILTOFL  NTR1  BASE=*,LABEL=*                                                   
         USING OFFRECD,R2                                                       
         CLI   OFFKTYP,OFFKTYPQ    X'01'                                        
         JNE   NO                                                               
         CLC   OFFKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
*                                                                               
         LA    R1,OFFRSTAT                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JNE   *+8                                                              
         LA    R1,OFFKSTAT                                                      
         TM    0(R1),OFFSLIST      MUST BE OFFICE LIST TYPE                     
         JO    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE OFFICE LIST RECORD                                       *         
***********************************************************************         
INITOFL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOFLDL          R1=L'OFL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCTION OFFICE RECORD                                       *         
***********************************************************************         
LOADPOF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST POF RECORD             
         USING OGRRECD,R2                                                       
         XC    OGRKEY,OGRKEY                                                    
         MVI   OGRKTYP,OGRKTYPQ                                                 
         MVI   OGRKSUB,OGRKOFFQ                                                 
         MVC   OGRKCPY,COMPANY                                                  
         MVC   OGRKUNT,UNIT                                                     
         MVC   OGRKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPOF02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   OGRKEY(OGRKOFC-OGRKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF U/L CHANGES                      
*                                                                               
         MVC   ACCADDR,OGRKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXPOFC,AINITPOF,AFILTPOF                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPOF02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCTION OFFICE RECORD DATA                                *         
***********************************************************************         
UPDTPOF  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING OGRRECD,R2                                                       
         GOTO1 AFILTPOF                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPOF                                                         
         GOTO1 AACCUPDT,DMCB,VAXPOFC,=C'POF'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER POF RECORD AT R2                                             *         
***********************************************************************         
FILTPOF  NTR1  BASE=*,LABEL=*                                                   
         USING OGRRECD,R2                                                       
         CLI   OGRKTYP,OGRKTYPQ    X'2C'                                        
         JNE   NO                                                               
         CLI   OGRKSUB,OGRKOFFQ    X'04'                                        
         JNE   NO                                                               
         CLC   OGRKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   OGRKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   OGRKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE PRODUCTION OFFICE                                        *         
***********************************************************************         
INITPOF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPOFDL          R1=L'POF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE LIST ENTRY RECORD                                       *         
***********************************************************************         
LOADOLE  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST OLE RECORD             
         USING OFFRECD,R2                                                       
         XC    OFFKEY,OFFKEY                                                    
         MVC   OFFKCPY,COMPANY                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOLE02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   OFFKEY(OFFKCPY+1-OFFKEY),IOKEY                                   
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTOLE            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LOLE08                                                           
*                                                                               
         MVC   ACCADDR,OFFKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
         GOTO1 AINITOLE            INITIALISE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VAXOLEC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LOLE04   GOTO1 VAXOLEC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   DMCB+8,FF                                                        
         JE    LOLE08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LOLE08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LOLE06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LOLE06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LOLE04                                                           
         J     YES                                                              
*                                                                               
LOLE08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LOLE10                                                           
         GOTO1 AREADHI                                                          
         JE    LOLE10                                                           
         DC    H'0'                                                             
*                                                                               
LOLE10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOLE02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE LIST ENTRY RECORD                                     *         
***********************************************************************         
UPDTOLE  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING OFFRECD,R2                                                       
         GOTO1 AFILTOLE                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOLE                                                         
         L     R3,DXAXREC                                                       
         USING AXOLED,R3                                                        
         CLI   AXOLEACT,C'C'       CHANGE?                                      
         BNE   UOLE04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXOLEC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
         MVI   AXOLEACT,C'K'                                                    
         CLI   DXWRITE,C'Y'        WRITE THIS RECORD?                           
         BNE   UOLE04              NO                                           
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         BE    UOLE02              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(C'K',TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UOLE02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         MVI   AXOLEACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
UOLE04   GOTO1 VAXOLEC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
UOLE06   GOTO1 VAXOLEC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                 NO MORE RECORDS                              
         CLI   DXWRITE,C'Y'                                                     
         JNE   UOLE10              DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UOLE08              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UOLE08   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UOLE10   GOTO1 ADECIOC                                                          
         JE    UOLE06                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER OLE RECORD AT R2                                             *         
***********************************************************************         
FILTOLE  NTR1  BASE=*,LABEL=*                                                   
         USING OFFRECD,R2                                                       
         CLI   OFFKTYP,OFFKTYPQ    X'01'                                        
         JNE   NO                                                               
         CLC   OFFKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         LA    R1,OFFRSTAT                                                      
         CLI   DXMODE,DXLOADQ                                                   
         JNE   *+8                                                              
         LA    R1,OFFKSTAT                                                      
         TM    0(R1),OFFSLIST      MUST BE OFFICE LIST TYPE                     
         JO    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE OFFICE LIST ENTRY RECORD                                 *         
***********************************************************************         
INITOLE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOLEDL          R1=L'OLE RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OPTION RECORD                                                  *         
***********************************************************************         
LOADOPT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST OPT RECORD             
         USING POPRECD,R2                                                       
         XC    POPKEY,POPKEY                                                    
         MVC   POPKCPY,COMPANY                                                  
         MVI   POPKTYP,POPKTYPQ                                                 
         MVI   POPKSUB,POPKSUBQ                                                 
         MVC   POPKUNT,UNIT                                                     
         MVC   POPKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOPT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   POPKEY(POPKOFG-POPKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,POPKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXOPTC,AINITOPT,AFILTOPT                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOPT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE OPTION RECORD                                                *         
***********************************************************************         
UPDTOPT  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING POPRECD,R2                                                       
         GOTO1 AFILTOPT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOPT                                                         
         GOTO1 AACCUPDT,DMCB,VAXOPTC,=C'OPT'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER OPT RECORD AT R2                                             *         
***********************************************************************         
FILTOPT  NTR1  BASE=*,LABEL=*                                                   
         USING POPRECD,R2                                                       
         CLC   POPKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   POPKTYP,POPKTYPQ                                                 
         JNE   NO                                                               
         CLI   POPKSUB,POPKSUBQ                                                 
         JNE   NO                                                               
         CLC   POPKUNT,UNIT                                                     
         JNE   NO                                                               
         CLC   POPKLDG,LEDGER                                                   
         JNE   NO                                                               
         CLI   VERSION,1           VERSION 1 DOES NOT WANT THESE                
         JH    YES                                                              
         OC    POPKWGR,POPKWGR                                                  
         JNZ   NO                                                               
         OC    POPKWRK,POPKWRK                                                  
         JNZ   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE OPTION RECORD                                            *         
***********************************************************************         
INITOPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOPTDL          R1=L'OPT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PERIOD RECORD                                                  *         
***********************************************************************         
LOADPED  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PED RECORD             
         USING CASRECD,R2                                                       
         XC    CASKEY,CASKEY                                                    
         MVC   CASKCPY,COMPANY                                                  
         MVI   CASKTYP,CASKTYPQ                                                 
         MVI   CASKSUB,CASKSUBQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPED02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   CASKEY(CASKEMOA-CASKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         GOTO1 AFILTPED                                                         
         JNE   LPED08                                                           
*                                                                               
         MVC   ACCADDR,CASKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
*                                                                               
         GOTO1 AINITPED            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXPEDC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LPED04   GOTO1 VAXPEDC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   8(R1),FF                                                         
         JE    LPED08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LPED08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LPED06              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',DXAXREC),(1,DXASQLB),(0,TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LPED06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LPED04                                                           
         J     YES                                                              
*                                                                               
LPED08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LPED10                                                           
         GOTO1 AREADHI                                                          
         JE    LPED10                                                           
         DC    H'0'                                                             
*                                                                               
LPED10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPED02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERIOD RECORD                                                *         
***********************************************************************         
UPDTPED  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CASRECD,R2                                                       
         GOTO1 AFILTPED                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPED                                                         
         L     R3,DXAXREC                                                       
         USING AXPEDD,R3                                                        
         CLI   AXPEDACT,C'C'       CHANGE?                                      
         JNE   UPED04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXPEDC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
         MVI   AXPEDACT,C'K'                                                    
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPED04              DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPED02              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',DXAXREC),(1,DXASQLB),(C'K',=C'PED'), *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UPED02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         MVI   AXPEDACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
UPED04   GOTO1 VAXPEDC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
UPED06   GOTO1 VAXPEDC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                 FINISHED                                     
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPED10              DO NOT WRITE                                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPED08              DO NOT CONVERT                               
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UPED08   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UPED10   GOTO1 ADECIOC                                                          
         JE    UPED06                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER PED RECORD AT R2                                             *         
***********************************************************************         
FILTPED  NTR1  BASE=*,LABEL=*                                                   
         USING CASRECD,R2                                                       
         CLC   CASKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   CASKTYP,CASKTYPQ    X'3E'                                        
         JNE   NO                                                               
         CLI   CASKSUB,CASKSUBQ    X'0B'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE PERIOD RECORD                                            *         
***********************************************************************         
INITPED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPEDDL          R1=L'PED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON RECORD                                                  *         
***********************************************************************         
LOADPER  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PER RECORD             
         USING PERRECD,R2                                                       
         XC    PERKEY,PERKEY                                                    
         MVC   PERKCPY,COMPANY                                                  
         MVI   PERKTYP,PERKTYPQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPER02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   PERKEY(PERKCODE-PERKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,PERKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXPERC,AINITPER,AFILTPER                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPER02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON RECORD DATA                                           *         
***********************************************************************         
UPDTPER  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PERRECD,R2                                                       
         GOTO1 AFILTPER                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPER                                                         
         GOTO1 AACCUPDT,DMCB,VAXPERC,=C'PER'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER PER RECORD AT R2                                             *         
***********************************************************************         
FILTPER  NTR1  BASE=*,LABEL=*                                                   
         USING PERRECD,R2                                                       
         CLC   PERKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   PERKTYP,PERKTYPQ    X'09'                                        
         JNE   NO                                                               
         CLC   PERKCODE,SPACES     MUST HAVE A PERSON CODE                      
         JE    NO                                                               
         CLC   PERKCODE+L'PERKCODE(32),SPACES                                   
         JNE   NO                  THIS SHOULD BE SPACES                        
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE PERSON RECORD                                            *         
***********************************************************************         
INITPER  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPERDL          R1=L'PER RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON ASSIGNMENT                                              *         
***********************************************************************         
LOADPEA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PEA RECORD             
         USING PERRECD,R2                                                       
         XC    PERKEY,PERKEY                                                    
         MVC   PERKCPY,COMPANY                                                  
         MVI   PERKTYP,PERKTYPQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPEA02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   PERKEY(PERKCODE-PERKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTPEA            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LPEA08                                                           
*                                                                               
         MVC   ACCADDR,PERKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
*                                                                               
         GOTO1 AINITPEA            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXPEAC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LPEA04   GOTO1 VAXPEAC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   DMCB+8,FF                                                        
         JE    LPEA08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LPEA08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LPEA06                                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',DXAXREC),(1,DXASQLB),(0,TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LPEA06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LPEA04                                                           
         J     YES                                                              
*                                                                               
LPEA08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LPEA10                                                           
         GOTO1 AREADHI                                                          
         JE    LPEA10                                                           
         DC    H'0'                                                             
*                                                                               
LPEA10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPEA02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON ASSIGNMENT RECORD DATA                                *         
***********************************************************************         
UPDTPEA  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PERRECD,R2                                                       
         GOTO1 AFILTPEA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPEA                                                         
         L     R3,DXAXREC                                                       
         USING AXPEAD,R3                                                        
         CLI   AXPEAACT,C'C'       CHANGE?                                      
         JNE   UPEA04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXPEAC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
         MVI   AXPEAACT,C'K'                                                    
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPEA04              DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPEA02              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(C'K',TYPECODE),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UPEA02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         MVI   AXPEAACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
UPEA04   GOTO1 VAXPEAC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
UPEA06   GOTO1 VAXPEAC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                 NO MORE RECORDS TO WRITE                     
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPEA10              DO NOT WRITE                                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPEA08              DO NOT CONVERT                               
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UPEA08   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UPEA10   GOTO1 ADECIOC                                                          
         JE    UPEA06                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER PEA RECORD AT R2                                             *         
***********************************************************************         
FILTPEA  NTR1  BASE=*,LABEL=*                                                   
         USING PERRECD,R2                                                       
         CLC   PERKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   PERKTYP,PERKTYPQ    X'09'                                        
         JNE   NO                                                               
         CLC   PERKCODE,SPACES     MUST HAVE A PERSON CODE                      
         JE    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE PERSON ASSIGNMENT RECORD                                 *         
***********************************************************************         
INITPEA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPEADL          R1=L'PEA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT RECORD                                                 *         
***********************************************************************         
LOADPRO  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PRO RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRO02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXPROC,AINITPRO,AFILTPRO                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRO02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT RECORD DATA                                          *         
***********************************************************************         
UPDTPRO  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTPRO                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPRO                                                         
         GOTO1 AACCUPDT,DMCB,VAXPROC,=C'PRO'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER PRO RECORD AT R2                                             *         
***********************************************************************         
FILTPRO  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT,SPACES      MUST HAVE SOMETHING IN IT                    
         JNH   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
*                                                                               
         GOTO1 ARDLEDG                                                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,L1               L' CLIENT CODE                               
         LA    RE,ACTKACT(RE)      DISPLACEMENT TO PRO CODE                     
         XR    RF,RF                                                            
         IC    RF,L2               L'PRODUCT CODE                               
         BCTR  RF,0                LEN-1 OF THE PRO CODE                        
         EX    RF,FPROCLC                                                       
         JNE   FPRO02                                                           
         MVC   CLISTAT,ACTKSTAT                                                 
         J     NO                                                               
*                                                                               
FPRO02   LA    RE,1(RE,RF)         A(WHAT COMES AFTER CLI/PRO)                  
         IC    RF,L3               LENGTH OF JOB CODE                           
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FPROCLC          MUST BE EMPTY                                
         JNE   NO                                                               
         CLI   DXMODE,DXLOADQ      LOAD?                                        
         JNE   YES                 NO                                           
         TM    CLISTAT,ACTSDRFT    CLIENT DRAFT                                 
         JNZ   NO                                                               
         TM    ACTKSTAT,ACTSDRFT   PRODUCT DRAFT                                
         JNZ   NO                                                               
*&&US*&& J     YES                                                              
*&&UK                                                                           
         CLI   DXUSER,C'Y'         FILTER OUT LOCKED CLI/PRO                    
         JNE   YES                 NO                                           
         TM    CLISTAT,ACTSLOCK    CLIENT LOCKED                                
         JNZ   NO                                                               
         TM    ACTKSTAT,ACTSLOCK   PRODUCT LOCKED                               
         JNZ   NO                                                               
         J     YES                                                              
*&&                                                                             
FPROCLC  CLC   0(0,RE),SPACES      MUST BE A PRO CODE PRESENT                   
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALISE PRODUCT RECORD                                           *         
***********************************************************************         
INITPRO  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPRODL          R1=L'PRO RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD COST ALLOCATION PROFILE RECORD                                 *         
***********************************************************************         
LOADCAP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C' '                                                        
         MVI   LEDGER,C' '                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CAP RECORD             
         USING CAPRECD,R2                                                       
         XC    CAPKEY,CAPKEY                                                    
         MVI   CAPKTYP,CAPKTYPQ                                                 
         MVI   CAPKSUB,CAPKSUBQ                                                 
         MVC   CAPKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCAP02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   CAPKEY(CAPKMTHD-CAPKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         MVC   ACCADDR,CAPKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXCAPC,AINITCAP,AFILTCAP                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCAP02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE COST ALLOCATION PROFILE RECORD                               *         
***********************************************************************         
UPDTCAP  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING CAPRECD,R2                                                       
*                                                                               
         GOTO1 AFILTCAP                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCAP                                                         
         GOTO1 AACCUPDT,DMCB,VAXCAPC,=C'CAP'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER CAP RECORD AT R2                                             *         
***********************************************************************         
FILTCAP  NTR1  BASE=*,LABEL=*                                                   
         USING CAPRECD,R2                                                       
         CLI   CAPKTYP,CAPKTYPQ                                                 
         JNE   NO                                                               
         CLI   CAPKSUB,CAPKSUBQ                                                 
         JNE   NO                                                               
         CLC   CAPKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   CAPKMTHD,C' '                                                    
         JH    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE COST ALLOCATION PROFILE                                  *         
***********************************************************************         
INITCAP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCAPDL          R1=L'CAP RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD RATE RECORD                                                    *         
***********************************************************************         
LOADRAT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RAT RECORD             
         USING PCRRECD,R2                                                       
         XC    PCRKEY,PCRKEY                                                    
         MVC   PCRKCPY,COMPANY                                                  
         MVI   PCRKTYP,PCRKTYPQ    X'2A' RECORD                                 
*&&UK                                                                           
         CLI   SXDTCTRY,CTRYGERQ                                                
         JE    *+16                                                             
         TM    CMPEL+(CPYSTAT7-CPYELD),CPYSTMSY                                 
         JZ    *+8                                                              
         MVI   PCRKTYP,PCRKTMSQ    X'29' RECORD                                 
*&&                                                                             
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LRAT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
*&&US                                                                           
         CLC   PCRKEY(PCRKDEP-PCRKEY),IOKEY                                     
*&&                                                                             
*&&UK                                                                           
         CLC   PCRKEY(PCRKPER-PCRKEY),IOKEY                                     
*&&                                                                             
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         GOTO1 AFILTRAT            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LRAT08                                                           
*                                                                               
         MVC   ACCADDR,PCRKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
         GOTO1 AINITRAT            INITIALISE EXTRACT BUFFER                    
*                                                                               
         GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS,SJLEVELS             
*                                  GET NEXT UNCOMMITTED RECORD                  
LRAT04   GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS,SJLEVELS             
         CLI   DMCB+8,FF                                                        
         JE    LRAT08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LRAT08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LRAT06                                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LRAT06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LRAT04                                                           
         J     YES                                                              
*                                                                               
LRAT08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LRAT10                                                           
         GOTO1 AREADHI                                                          
         JE    LRAT10                                                           
         DC    H'0'                                                             
*                                                                               
LRAT10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LRAT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE RATE RECORD                                                            
***********************************************************************         
UPDTRAT  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG                                                          
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PCRRECD,R2                                                       
         GOTO1 AFILTRAT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITRAT                                                         
         L     R3,DXAXREC                                                       
         USING AXRATD,R3                                                        
         CLI   AXRATACT,C'C'       CHANGE?                                      
         JNE   URAT04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
*                                              BUILD KILL RECORD                
         GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(0,0),(R6),LEVELS,SJLEVELS             
         MVI   AXRATACT,C'K'                                                    
         CLI   DXWRITE,C'Y'        WRITE THIS RECORD?                           
         JNE   URAT04              NO                                           
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    URAT02              DO NOT CONVERT                               
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(C'K',TYPECODE),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
URAT02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         MVI   AXRATACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
*                                              BUILD RECORDS                    
URAT04   GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS,SJLEVELS             
*                                              GET NEXT RECORD                  
URAT06   GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS                      
         CLI   8(R1),FF                                                         
         JE    YES                                                              
         CLI   DXWRITE,C'Y'                                                     
         JNE   URAT10                                                           
         CLI   SXDTPLFM,0                                                       
         JE    URAT08                                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',DXAXREC),(1,DXASQLB),(0,TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
URAT08   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
URAT10   GOTO1 ADECIOC                                                          
         JE    URAT06                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER RAT RECORD AT R2                                             *         
***********************************************************************         
FILTRAT  NTR1  BASE=*,LABEL=*                                                   
         USING PCRRECD,R2                                                       
         CLC   PCRKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    RF,PCRKTYPQ         X'2A' RECORD                                 
*&&UK                                                                           
         CLI   SXDTCTRY,CTRYGERQ                                                
         JE    FRAT02                                                           
         TM    CMPEL+(CPYSTAT7-CPYELD),CPYSTMSY                                 
         JZ    FRAT02                                                           
         LA    RF,PCRKTMSQ         X'29' RECORD                                 
*&&                                                                             
FRAT02   CLM   RF,1,PCRKTYP        TEST SAME TYPE                               
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE RATE RECORD                                              *         
***********************************************************************         
INITRAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXRATDL          R1=L'RAT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD STANDARD HOURS RECORD                                          *         
***********************************************************************         
LOADSTH  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST STH RECORD             
         USING STDRECD,R2                                                       
         XC    STDKEY,STDKEY                                                    
         MVC   STDKCPY,COMPANY                                                  
         MVI   STDKTYP,STDKTYPQ    3E                                           
         MVI   STDKSUB,STDKSUBQ    0D                                           
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTH02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   STDKEY(STDKOFC-STDKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         GOTO1 AFILTSTH            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LSTH08                                                           
*                                                                               
         MVC   ACCADDR,STDKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
         GOTO1 AINITSTH            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXSTHC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LSTH04   GOTO1 VAXSTHC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   DMCB+8,FF                                                        
         JE    LSTH08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LSTH08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LSTH06                                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LSTH06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LSTH04                                                           
         J     YES                                                              
*                                                                               
LSTH08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LSTH10                                                           
*                                                                               
         GOTO1 AREADHI                                                          
         JE    LSTH10                                                           
         DC    H'0'                                                             
*                                                                               
LSTH10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTH02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE STANDARD HOURS RECORD                                        *         
***********************************************************************         
UPDTSTH  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING STDRECD,R2                                                       
         GOTO1 AFILTSTH                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSTH                                                         
         L     R3,DXAXREC                                                       
         USING AXSTHD,R3                                                        
         CLI   AXSTHACT,C'C'       CHANGE?                                      
         JNE   USTH04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXSTHC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
         MVI   AXSTHACT,C'K'                                                    
         CLI   DXWRITE,C'Y'        WRITE THIS RECORD?                           
         JNE   USTH04              NO                                           
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    USTH02              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',DXAXREC),(1,DXASQLB),(C'K',=C'STH'), *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
USTH02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         MVI   AXSTHACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
USTH04   GOTO1 VAXSTHC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
USTH06   GOTO1 VAXSTHC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                 NO MORE RECORDS TO CHANGE                    
         CLI   DXWRITE,C'Y'                                                     
         BNE   USTH10              DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         BE    USTH08              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
USTH08   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
USTH10   GOTO1 ADECIOC                                                          
         JE    USTH06                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER STH RECORD AT R2                                             *         
***********************************************************************         
FILTSTH  NTR1  BASE=*,LABEL=*                                                   
         USING STDRECD,R2                                                       
         CLC   STDKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   STDKTYP,STDKTYPQ    X'3E'                                        
         JNE   NO                                                               
         CLI   STDKSUB,STDKSUBQ    X'0B'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE STANDARD HOURS RECORD                                    *         
***********************************************************************         
INITSTH  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXSTHDL          R1=L'STH RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD EDIT HOURS RECORD                                              *         
***********************************************************************         
LOADEDT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST EDT RECORD             
         USING EDTRECD,R2                                                       
         XC    EDTKEY,EDTKEY                                                    
         MVC   EDTKCPY,COMPANY                                                  
         MVI   EDTKTYP,EDTKTYPQ    3E                                           
         MVI   EDTKSUB,EDTKSUBQ    10                                           
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEDT02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   EDTKEY(EDTKOFC-EDTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         GOTO1 AFILTEDT            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LEDT08                                                           
*                                                                               
         MVC   ACCADDR,EDTKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
*                                                                               
         GOTO1 AINITEDT            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXEDTC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LEDT04   GOTO1 VAXEDTC,DMCB,DXAXREC,(R2),(2,0),(R6)                             
         CLI   DMCB+8,FF                                                        
         JE    LEDT08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LEDT08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LEDT06                                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LEDT06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LEDT04                                                           
         J     YES                                                              
*                                                                               
LEDT08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LEDT10                                                           
         GOTO1 AREADHI                                                          
         JE    LEDT10                                                           
         DC    H'0'                                                             
*                                                                               
LEDT10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEDT02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE EDIT HOURS RECORD                                            *         
***********************************************************************         
UPDTEDT  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING EDTRECD,R2                                                       
         GOTO1 AFILTEDT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITEDT                                                         
         L     R3,DXAXREC                                                       
         USING AXEDTD,R3                                                        
         CLI   AXEDTACT,C'A'       SKIP ALL ADDS - WILL BE HANDLED              
         JE    YES                      WITH COPY/CHANGE SEQ                    
*        CLI   AXEDTACT,C'C'       CHANGE?                                      
*        BNE   UEDT04              NO - NO TABLES TO CLEAR THEN                 
*                                                                               
         GOTO1 VAXEDTC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
         MVI   AXEDTACT,C'K'                                                    
         CLI   DXWRITE,C'Y'                                                     
         BNE   UEDT04              DO NOT WRITE THIS RECORD                     
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         BE    UEDT02              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(C'K',TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UEDT02   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         GOTO1 AINITEDT                                                         
         MVI   AXEDTACT,C'A'                                                    
         DROP  R3                                                               
*                                                                               
UEDT04   GOTO1 VAXEDTC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
UEDT06   GOTO1 VAXEDTC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                 NO MORE RECORDS                              
         CLI   DXWRITE,C'Y'                                                     
         JNE   UEDT10              DO NOT WRITE                                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UEDT08              DO NOT CONVERT                               
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UEDT08   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UEDT10   GOTO1 ADECIOC                                                          
         JE    UEDT06                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER EDT RECORD AT R2                                             *         
***********************************************************************         
FILTEDT  NTR1  BASE=*,LABEL=*                                                   
         USING EDTRECD,R2                                                       
         CLC   EDTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   EDTKTYP,EDTKTYPQ    X'3E'                                        
         JNE   NO                                                               
         CLI   EDTKSUB,EDTKSUBQ    X'10'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE EDIT HOURS RECORD                                        *         
***********************************************************************         
INITEDT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXEDTDL          R1=L'EDT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD SUB - DEPARTMENT RECORD                                        *         
***********************************************************************         
LOADSBD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST SBD RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSBD02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXSBDC,AINITSBD,AFILTSBD                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSBD02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE SUB - DEPARTMENT RECORD DATA                                 *         
***********************************************************************         
UPDTSBD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTSBD                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSBD                                                         
         GOTO1 AACCUPDT,DMCB,VAXSBDC,=C'SBD'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER SBD RECORD AT R2                                             *         
***********************************************************************         
FILTSBD  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKACT+L'ACTKACT(ACTKSTA-ACTKACT-L'ACTKACT),SPACES              
         JNE   NO                                                               
         GOTO1 ARDLEDG                                                          
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,L1               L'OFFICE CODE                                
         IC    RF,L2               L'DEPARTMENT CODE                            
         LA    RF,0(RE,RF)                                                      
         IC    RE,L3                                                            
         LA    RF,ACTKACT(RF)                                                   
         BCTR  RE,0                                                             
         EX    RE,FSBDCLC                                                       
         JE    NO                                                               
*                                                                               
         CLI   L4,0                LEVEL BELOW SUB-DEPT?                        
         JE    YES                 NO                                           
*                                                                               
         LA    RF,1(RE,RF)         A(START OF SUB LEVEL)                        
         IC    RE,L4                                                            
         BCTR  RE,0                LEN-1 OF REMAINDER                           
         EX    RE,FSBDCLC                                                       
         JNE   NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
*                                                                               
         TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
FSBDCLC  CLC   0(0,RF),SPACES      SUB DEPARTMENT MUST CONTAIN DATA             
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE SUB - DEPARTMENT RECORD                                  *         
***********************************************************************         
INITSBD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXSBDDL          R1=L'SBD RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON1R RECORD                                                *         
***********************************************************************         
LOADP1R  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST P1R RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LP1R02   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                                                              
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXP1RC,AINITP1R,AFILTP1R                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LP1R02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON 1R RECORD DATA                                        *         
***********************************************************************         
UPDTP1R  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTP1R                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITP1R                                                         
         GOTO1 AACCUPDT,DMCB,VAXP1RC,=C'P1R'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER P1R RECORD AT R2                                             *         
***********************************************************************         
FILTP1R  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),SPACES                          
         JNE   NO                                                               
*                                                                               
         GOTO1 ARDLEDG                                                          
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RE,L1               L'OFFICE CODE                                
         IC    RF,L2               L'DEPARTMENT CODE                            
         AR    RF,RE                                                            
         IC    RE,L3               L'SUB-DEPT CODE                              
         AR    RF,RE                                                            
         LA    RF,ACTKACT(RF)                                                   
*                                                                               
         IC    RE,L4               L'PERSON CODE                                
         BCTR  RE,0                                                             
         EX    RE,FP1RCLC                                                       
         JNH   NO                                                               
         LA    R1,ACTRSTAT                                                      
         CLI   DXMODE,DXLOADQ      LOAD USES DIRECTORY KEY                      
         BNE   *+8                                                              
         LA    R1,ACTKSTAT                                                      
*                                                                               
         TM    0(R1),ACTSDRFT      EXCLUDE DRAFT ACCOUNTS                       
         JZ    YES                                                              
         J     NO                                                               
*                                                                               
FP1RCLC  CLC   0(0,RF),SPACES      PERSON CODE MUST CONTAIN DATA                
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE PERSON1R RECORD                                          *         
***********************************************************************         
INITP1R  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXP1RDL          R1=L'P1R RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD TASK RECORD                                                    *         
***********************************************************************         
LOADTSK  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST TSK RECORD             
         USING WCORECD,R2                                                       
         XC    WCOKEY,WCOKEY                                                    
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,COMPANY                                                  
         MVC   WCOKUNT,UNIT                                                     
         MVC   WCOKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTSK02   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   WCOKEY(WCOKWRK-WCOKEY),IOKEY                                     
         JNE   YES                                                              
*                                                                               
         MVC   ACCADDR,WCOKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXTSKC,AINITTSK,AFILTTSK                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTSK02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TASK  RECORD DATA                                            *         
***********************************************************************         
UPDTTSK  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING WCORECD,R2                                                       
         GOTO1 AFILTTSK                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITTSK                                                         
         GOTO1 AACCUPDT,DMCB,VAXTSKC,=C'TSK'                                    
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TSK RECORD AT R2                                             *         
***********************************************************************         
FILTTSK  NTR1  BASE=*,LABEL=*                                                   
         USING WCORECD,R2                                                       
         CLC   WCOKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   WCOKTYP,WCOKTYPQ    X'09'                                        
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE TASK RECORD                                              *         
***********************************************************************         
INITTSK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTSKDL          R1=L'TSK RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD TIMESHEET RECORDS                                              *         
***********************************************************************         
LOADTIS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
         LA    R2,IOKEY            SET KEY TO READ FIRST TAL RECORD             
         USING TSXRECD,R2                                                       
         XC    TSXKEY,TSXKEY                                                    
         MVC   TSXKCPY,COMPANY                                                  
         MVI   TSXKTYP,TSXKTYPQ                                                 
         MVI   TSXKSUB,TSXKSUBQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   LTIS04                                                           
*                                                                               
LTIS02   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    LTIS04                                                           
         CLC   TSXKEY(TSXKPER-TSXKEY),IOKEY                                     
         JNE   LTIS04              ALL DONE IF COMPANY CHANGES                  
*                                                                               
         MVC   ACCADDR,TSXKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXTISC,AINITTIS,AFILTTIS                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTIS02                                                           
         J     YES                                                              
*                                                                               
LTIS04   LA    R2,IOKEY                                                         
         USING AUDRECD,R2                                                       
         XC    AUDKEY,AUDKEY                                                    
         MVC   AUDKCPY,COMPANY                                                  
         MVI   AUDKTYP,AUDKTYPQ                                                 
         MVI   AUDKSUB,AUDKSUBQ                                                 
         MVI   AUDKAUDT,AUDKTIME                                                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTIS06   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   AUDKEY(AUDKPIDB-AUDKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         MVC   ACCADDR,AUDKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXTISC,AINITTIS,AFILTTAU                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTIS06                                                           
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TIMESHEET RECORDS                                            *         
***********************************************************************         
UPDTTIS  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING TSXRECD,R2                                                       
         GOTO1 AFILTTIS                                                         
         JE    UTIS02                                                           
         GOTO1 AFILTTAU                                                         
         JNE   YES                                                              
*                                                                               
UTIS02   GOTO1 AINITTIS                                                         
         GOTO1 AACCUPDT,DMCB,VAXTISC,=C'TIS'                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TIS RECORD AT R2                                             *         
***********************************************************************         
FILTTIS  NTR1  BASE=*,LABEL=*                                                   
         USING TSXRECD,R2                                                       
         CLI   TSXKTYP,TSXKTYPQ    TYPE                                         
         JNE   NO                                                               
         CLI   TSXKSUB,TSXKSUBQ    SUB ID                                       
         JNE   NO                                                               
         CLC   TSXKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
*&&US                                                                           
         CLC   =C'*B',SXDTAGY      HARD CODE - OK TO DOWNLOAD T/S'S             
         JE    YES                 FOR DDSB                                     
*                                                                               
         CLC   DXFDATEP,SPACES     TEST FROM DATE                               
         JNH   FTIS10                                                           
         CLC   TSXKEND,DXFDATEP                                                 
         JL    NO                                                               
*                                                                               
FTIS10   CLC   DXTDATEP,SPACES     TEST TO DATE                                 
         JNH   FTIS20                                                           
         CLC   TSXKEND,DXTDATEP                                                 
         JH    NO                                                               
*                                                                               
*&&                                                                             
FTIS20   CLI   VERSION,1           OR FOR VERSION 2 OR HIGHER                   
         JH    YES                                                              
         CLI   DXMODE,DXLOADQ      LOAD HAS DIRECTORY RECORD                    
         JNE   FTIS30                                                           
         TM    TSXKSTA1,TSXTEMPO                                                
         JO    NO                                                               
         J     YES                                                              
*                                                                               
FTIS30   TM    TSXRSTA1,TSXTEMPO   UPDATE HAS FILE RECORD                       
         JO    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* FILTER TIMESHEET AUDIT RECORDS - BRANDOCEAN TIMESHEETS              *         
***********************************************************************         
FILTTAU  NTR1  BASE=*,LABEL=*                                                   
         USING AUDRECD,R2                                                       
         CLI   AUDKTYP,AUDKTYPQ    TYPE                                         
         JNE   NO                                                               
         CLI   AUDKSUB,AUDKSUBQ    SUB ID                                       
         JNE   NO                                                               
         CLC   AUDKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   AUDKAUDT,AUDKTIME   TIME                                         
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE TIME SHEET RECORD                                        *         
***********************************************************************         
INITTIS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTISDL          R1=L'TIS RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD TIMESHEET ALLOCATION / TAX RECORDS                             *         
***********************************************************************         
LOADTAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG             GET LEDGER STRUCTURE                         
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST TAL RECORD             
         USING TIMRECD,R2                                                       
         XC    TIMKEY,TIMKEY                                                    
         MVC   TIMKCPY,COMPANY                                                  
         MVC   TIMKUNT,UNIT                                                     
         MVC   TIMKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
         MVI   BYTE2,0                                                          
         MVI   FLAG,0                                                           
*                                                                               
LTAL02   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    YES                                                              
LTAL02A  CLC   TIMKEY(TIMKACT-TIMKEY),IOKEY                                     
         JNE   YES                                                              
*                                                                               
         MVC   ACCADDR,TIMKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                  ERROR ON READ                                
*                                                                               
         GOTO1 AFILTTAL            FILTER RECORD                                
         JNE   LTAL09              NOT VALID                                    
         MVC   IOKEYSAV,TIMKEY     SAVE CURRENT KEY                             
*                                                                               
         GOTO1 AINITTAL            INITIALISE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
LTAL03   GOTO1 VAXTALC,DMCB,DXAXREC,(BYTE2,(R2)),(1,0),(0,(R6)),LEVELS          
         CLI   VERSION,5                                                        
         BNE   LTAL04                                                           
         MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ             DID WE BREAK SEQUENCE??                      
         BE    LTAL03A             NO                                           
         GOTO1 AREADHI                                                          
         JE    LTAL03A                                                          
         DC    H'0'                                                             
LTAL03A  GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JZ    YES                                                              
         MVC   FLAG,8(R1)                                                       
         TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    LTAL03B                                                          
         CLC   TIMKEY(L'TIMKEY-L'TIMKSBR),IOKEYSAV                              
         BNE   LTAL03B                                                          
         MVC   ACCADDR,TIMKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
         MVI   BYTE2,1                                                          
         B     LTAL03                                                           
*                                                                               
LTAL03B  MVI   BYTE2,0                                                          
*                                  GET NEXT UNCOMMITTED RECORD                  
LTAL04   GOTO1 VAXTALC,DMCB,DXAXREC,(BYTE2,(R2)),(2,0),(0,(R6)),LEVELS          
         CLI   8(R1),FF                                                         
         JE    LTAL08                                                           
         CLI   DXWRITE,C'Y'                                                     
         JNE   LTAL08              DO NOT WRITE RECORDS                         
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LTAL06              DO NOT CONVERT RECORDS                       
*                                                                               
         LA    R0,=C'TAL'                                                       
         CLI   DMCB+8,X'EE'        TAX RECORDS IF EE PASSED BACK                
         JNE   *+8                                                              
         LA    R0,=C'TAT'                                                       
         CLI   DMCB+8,X'DD'        DAY RECORDS IF DD PASSED BACK                
         JNE   *+8                                                              
         LA    R0,=C'TAD'                                                       
         CLI   DMCB+8,X'CC'        BRANDO LINE RECS IF CC PASSED BACK           
         JNE   *+8                                                              
         LA    R0,=C'BOL'                                                       
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,(R0)),VERSION            
*                                                                               
         L     RF,DXASQLB                                                       
LTAL06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LTAL04                                                           
         J     YES                                                              
*                                                                               
LTAL08   TM    FLAG,X'80'                                                       
         JO    YES                                                              
         CLI   VERSION,5                                                        
         BE    LTAL02A                                                          
LTAL09   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ             DID WE BREAK SEQUENCE??                      
         BE    LTAL10              NO                                           
         GOTO1 AREADHI                                                          
         JE    LTAL10                                                           
         DC    H'0'                                                             
*                                                                               
LTAL10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTAL02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TIMESHEET ALLOCATION / TAX RECORDS                           *         
***********************************************************************         
UPDTTAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING TIMRECD,R2                                                       
         GOTO1 AFILTTAL            NOT A VALID TIMESHEET RECORD                 
         JNE   YES                                                              
*                                                                               
         CLI   DXACTION,C'A'       ADD?                                         
         JE    UTAL18              YES- NO TABLES TO CLEAR THEN                 
*                                                                               
         L     R1,SEQNO                                                         
         AHI   R1,1                                                             
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         ST    R1,RVCHR            SET SEQUENCE NUMBER                          
         ST    R1,SEQNO            SAVE SEQUENCE NUMBER                         
         MVI   RPRG,C'D'           MARK RECS FOR DELETION ON READ BACK          
         GOTO1 ASORTPUT                                                         
*                                                                               
UTAL18   L     R5,DXARECB          NOW ADD LINES FROM CHANGE RECORD             
         LA    R2,RECVHDR+L'RECVHDR                                             
         TM    TIMRSTA,X'80'       IS THIS RECORD DELETED?                      
         JO    YES                 YES - DO NOT RE-ADD                          
         L     R1,SEQNO            INCREMENT SEQUENCE NUMBER                    
         AHI   R1,1                                                             
         ST    R1,RVCHR            SET SEQUENCE NUMBER                          
         ST    R1,SEQNO            SAVE SEQUENCE NUMBER                         
         MVI   RPRG,C'A'           MARK RECS FOR ADD ON READ BACK               
         GOTO1 ASORTPUT                                                         
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER TAL RECORD AT R2                                             *         
***********************************************************************         
FILTTAL  NTR1  BASE=*,LABEL=*                                                   
         USING TIMRECD,R2                                                       
         CLC   TIMKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
*                                                                               
         CLC   DXFDATEP,SPACES     TEST FROM DATE                               
         JNH   FTAL10                                                           
         CLC   TIMKPEDT,DXFDATEP                                                
         JL    NO                                                               
*                                                                               
FTAL10   CLC   DXTDATEP,SPACES     TEST TO DATE                                 
         JNH   FTAL20                                                           
         CLC   TIMKPEDT,DXTDATEP                                                
         JH    NO                                                               
*                                                                               
FTAL20   LA    RF,TIMRFST                                                       
         XR    R0,R0               R0=A(TIMEL)                                  
         XR    R1,R1               R1=A(PIDEL)                                  
         XR    RE,RE                                                            
*                                                                               
FTAL30   CLI   0(RF),0             END OF RECORD                                
         JE    FTAL40                                                           
         CLI   0(RF),TIMELQ        TIMESHEET ELEMENT                            
         JNE   *+6                                                              
         LR    R0,RF               R1=A(TIMEL)                                  
         CLI   0(RF),PIDELQ        PIDEL?                                       
         JNE   *+6                                                              
         LR    R1,RF               R1=A(PIDEL)                                  
         CLI   0(RF),TRNELQ        TRNEL?                                       
         JE    NO                  Draft type 34 - Skip                         
*                                                                               
         ICM   RE,1,1(RF)          BUMP TO NEXT                                 
         JZ    FTAL40                                                           
         AR    RF,RE                                                            
         J     FTAL30                                                           
*                                                                               
FTAL40   LTR   R0,R0               NO TIMEL                                     
         JZ    FTAL50              Check if deleted timrec                      
         LTR   R1,R1               NO PIDEL                                     
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
FTAL50   CLC   TIMKREF,=C'*TIME*'  Is this a time record?                       
         JNE   NO                                                               
         TM    TIMRSTAT,TIMSDELT   Was it deleted?                              
         JO    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE TIME SHEET ALLOCATION AND TAX RECORDS                    *         
***********************************************************************         
INITTAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTALDL          R1=L'TAL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PAYROLL HISTORY RECORDS                                        *         
***********************************************************************         
LOADPHI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PHI RECORD             
         USING PHIRECD,R2                                                       
         XC    PHIKEY,PHIKEY                                                    
         MVI   PHIKTYP,PHIKTYPQ                                                 
         MVI   PHIKSUB,PHIKSUBQ                                                 
         MVC   PHIKCPY,COMPANY                                                  
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPHI02   TM    DMCB+8,X'80'                                                     
         JO    YES                 FINISHED ON EOF                              
         CLC   PHIKEY(PHIKOFC-PHIKEY),IOKEY                                     
         JNE   YES                 NO MORE PHI RECORDS FOR COMPANY              
*                                                                               
         MVC   ACCADDR,PHIKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JE    *+6                                                              
         DC    H'0'                IO ERROR ON GETREC COMMAND                   
*                                                                               
         GOTO1 AFILTPHI            FILTER PHI RECORD                            
         JNE   LPHI08                                                           
*                                                                               
         GOTO1 AINITPHI            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXPHIC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                                                               
LPHI04   GOTO1 VAXPHIC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF            MORE RECORDS TO OUTPUT?                      
         JE    LPHI08              NO                                           
         CLI   DXWRITE,C'Y'        WRITING THESE RECORDS TO FILE?               
         JNE   LPHI08              NO                                           
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0          CONVERTING TO SPECIFIC PLATFORM?             
         JE    LPHI06              NO                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LPHI06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LPHI04                                                           
         J     NO                                                               
*                                                                               
LPHI08   MVC   IOKEY(L'PHIKEY),0(R2)                                            
         GOTO1 ACHKSEQ             DID WE BREAK READ SEQUENCE?                  
         JE    LPHI10              NO                                           
*                                                                               
         GOTO1 AREADHI                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LPHI10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPHI02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE PAYROLL HISTORY RECORDS                                      *         
***********************************************************************         
UPDTPHI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING PHIRECD,R2                                                       
         GOTO1 AFILTPHI                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPHI            INITIALISE PHI RECORD                        
         L     R3,DXAXREC                                                       
         USING AXPHID,R3                                                        
         CLI   AXPHIACT,C'A'       ADD?                                         
         JE    UPHI08              YES- NO TABLES TO CLEAR THEN                 
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R2,RECVHDR+L'RECVHDR                                             
         MVI   AXPHIACT,C'D'       SET ACTION TO DELETE                         
*                                                                               
         GOTO1 VAXPHIC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD COPY RECORDS          
*                                                                               
UPHI02   GOTO1 VAXPHIC,DMCB,DXAXREC,(R2),(2,0),(R6) GET COPY RECORDS            
         CLI   8(R1),FF                                                         
         JE    UPHI08              DONE WITH COPY RECORD                        
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPHI06              DO NOT WRITE THIS RECORD                     
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPHI04              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UPHI04   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UPHI06   GOTO1 ADECIOC                                                          
         JE    UPHI02                                                           
         J     NO                                                               
*                                                                               
UPHI08   L     R5,DXARECB          GET BACK ADDRESS OF CHANGE RECORD            
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 AINITPHI                                                         
         MVI   AXPHIACT,C'A'       SET ACTION TO ADD                            
         DROP  R3                                                               
*                                                                               
         GOTO1 VAXPHIC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS               
*                                                                               
UPHI10   GOTO1 VAXPHIC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                 NO MORE RECORDS                              
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPHI14              DO NOT WRITE THIS RECORD                     
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPHI12              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UPHI12   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UPHI14   GOTO1 ADECIOC                                                          
         JE    UPHI10                                                           
         JNE   NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER PHI RECORD AT R2                                             *         
***********************************************************************         
FILTPHI  NTR1  BASE=*,LABEL=*                                                   
         USING PHIRECD,R2                                                       
         CLI   PHIKTYP,PHIKTYPQ    PAYROLL HISTORY                              
         JNE   NO                                                               
         CLI   PHIKSUB,PHIKSUBQ    PAYROLL HISTORY                              
         JNE   NO                                                               
         CLC   PHIKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
*                                                                               
         USING PDEELD,RF                                                        
         LA    RF,PHIRFST                                                       
         XR    RE,RE                                                            
*                                                                               
FPHI02   CLI   0(RF),0             END OF RECORD                                
         JE    NO                                                               
         CLI   PDEEL,PDEELQ        PDEEL?                                       
         JNE   FPHI04                                                           
         TM    PDESTAT2,PDESHRTE   HOURLY RATE?                                 
         JO    YES                 YES                                          
*                                                                               
FPHI04   IC    RE,1(RF)            BUMP TO NEXT                                 
         AR    RF,RE               ..                                           
         B     FPHI02              AND TRY AGAIN.                               
*                                                                               
         LTORG                                                                  
         DROP  R2,RF                                                            
***********************************************************************         
* INITIALISE PAYROLL HISTORY RECORD                                   *         
***********************************************************************         
INITPHI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPHIDL          R1=L'PHI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD DAILY TIME RECORDS                                             *         
***********************************************************************         
LOADDTM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST P1R RECORD             
         USING ACTRECD,R2                                                       
         XC    ACTKEY,ACTKEY                                                    
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT,UNIT                                                     
         MVC   ACTKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDTM02   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                                                              
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JE    *+6                                                              
         DC    H'0'                IO ERROR ON GETREC COMMAND                   
*                                                                               
         GOTO1 AFILTDTM                                                         
         JNE   LDTM08                                                           
*                                                                               
         GOTO1 AINITDTM            INITIALISE EXTRACT BUFFER                    
         GOTO1 VAXDTMC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS                      
*                                                                               
LDTM04   GOTO1 VAXDTMC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS                      
         CLI   8(R1),FF            MORE RECORDS TO OUTPUT?                      
         JE    LDTM08              NO                                           
         CLI   DXWRITE,C'Y'        WRITING THESE RECORDS TO FILE?               
         JNE   LDTM08              NO                                           
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0          CONVERTING TO SPECIFIC PLATFORM?             
         JE    LDTM06              NO                                           
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
LDTM06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LDTM04                                                           
         J     NO                                                               
*                                                                               
LDTM08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ             DID WE BREAK READ SEQUENCE?                  
         JE    LDTM10              NO                                           
*                                                                               
         GOTO1 AREADHI                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LDTM10   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDTM02                                                           
         J     YES                                                              
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE DAILY TIME RECORD DATA                                       *         
***********************************************************************         
UPDTDTM  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
         GOTO1 AFILTDTM                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITDTM            INITIALISE RECORD                            
         L     R3,DXAXREC                                                       
         USING AXDTMD,R3                                                        
         CLI   AXDTMACT,C'A'       ADD?                                         
         JE    UDTM08              YES- NO TABLES TO CLEAR THEN                 
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R2,RECVHDR+L'RECVHDR                                             
         MVI   AXDTMACT,C'D'       SET ACTION TO DELETE                         
*                                                                               
         GOTO1 VAXDTMC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS  BUILD RECS          
*                                                                               
UDTM02   GOTO1 VAXDTMC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS  GET RECS            
         CLI   8(R1),FF                                                         
         JE    UDTM08              DONE WITH COPY RECORD                        
         CLI   DXWRITE,C'Y'                                                     
         JNE   UDTM06              DO NOT WRITE THIS RECORD                     
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UDTM04              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UDTM04   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UDTM06   GOTO1 ADECIOC                                                          
         JE    UDTM02                                                           
         J     NO                                                               
*                                                                               
UDTM08   L     R5,DXARECB          GET BACK ADDRESS OF CHANGE RECORD            
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 AINITDTM                                                         
         MVI   AXDTMACT,C'A'       SET ACTION TO ADD                            
         DROP  R3                                                               
*                                                                               
         GOTO1 VAXDTMC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS  BUILD RECS          
*                                                                               
UDTM10   GOTO1 VAXDTMC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS  GET RECS            
         CLI   8(R1),FF                                                         
         JE    YES                 NO MORE RECORDS                              
         CLI   DXWRITE,C'Y'                                                     
         JNE   UDTM14              DO NOT WRITE THIS RECORD                     
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UDTM12              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,TYPENAME),      *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
UDTM12   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UDTM14   GOTO1 ADECIOC                                                          
         JE    UDTM10                                                           
         JNE   NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER DAILY TIME AT R2                                             *         
***********************************************************************         
FILTDTM  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   ACTKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         CLC   ACTKEY+ACTKEND(L'ACTKEY-ACTKEND),SPACES                          
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE DAILY TIME RECORD                                        *         
***********************************************************************         
INITDTM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXDTMDL          R1=L'DTM RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE                  *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
***********************************************************************         
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA02                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA06              NOT VALID - GET NEXT                         
*                                                                               
ALOA02   GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
ALOA03   GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),LEVELS                             
         TM    DMCB+8,X'80'                                                     
         JO    ALOA06              ERROR - NO WRITE                             
         CLI   DMCB+8,FF                                                        
         JE    ALOA06              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA06              CONTROLLER REQUESTS NO WRITE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ALOA04              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',DXAXREC),(1,DXASQLB),(0,TYPENAME),   *        
               VERSION                                                          
*                                                                               
         L     RF,DXASQLB                                                       
ALOA04   GOTO1 DXPUT,DMCB,(RF),(R7) UNCONVERTED RECORD TO EXTRACT               
*                                                                               
ALOA06   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         MVC   IOKEY(L'ACTKEY),0(R2) READ NEXT RECORD - SEQUENTIAL              
         CLI   TYPEDEEP,0          LEDGER TYPE RECORDS?                         
         JNE   ALOA08              YES, NEED TO READ HIGH                       
*                                                                               
         GOTO1 ACHKSEQ             SEE IF READ SEQUENCE BROKEN                  
         JE    ALOA10              NO                                           
         GOTO1 AREADHI                                                          
         JE    ALOA10                                                           
         DC    H'0'                                                             
*                                                                               
ALOA08   LA    RF,IOKEY            READ NEXT RECORD - HIGH                      
         USING ACTRECD,RF                                                       
         XR    RE,RE                                                            
         IC    RE,TYPEDEEP         LEVEL TO FILTER AT                           
         BCTR  RE,0                                                             
         LA    RE,DISPS(RE)        DISP TO START OF NEXT IN R1 HERE             
         XR    R1,R1                                                            
         IC    R1,0(RE)            WE WANT LAST OF THIS LEVEL                   
         BCTR  R1,0                - SO SUBTRACT 1...                           
         LA    R1,ACTKACT(R1)      R1=A(BYTE TO INCREMENT BY 1)                 
         XR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,1(RE)            INCREMENT IT                                 
         STC   RE,0(R1)                                                         
         DROP  RF                                                               
*                                                                               
         GOTO1 AREADHI                                                          
         JE    YES                                                              
         J     NO                                                               
*                                                                               
ALOA10   GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN UPDATE MODE                *         
* R2 = A(ACCOUNT RECORD BUFFER)                                       *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
ACCUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),LEVELS                             
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPDL04              DO NOT CONVERT RECORD                        
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   UPDL02                                                           
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(COPYBUFL)                                                  
         XR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,32-8                                                          
         MVCL  R0,RE               SET RECORD TO ALL SPACES                     
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6),LEVELS  BUILD COPY REC            
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
         LH    R1,=AL2(AXGENCCD-AXGEND) DISP TO COMPANY                         
         AR    R0,R1               BUMP TO COMPANY CODE IN BOTH RECS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
         SH    RF,=AL2(L'AXGENDX)  DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    YES                 ARE THE SAME THEN SKIP                       
*                                                                               
UPDL02   GOTO1 VAXCNVX,DMCB,(C'U',DXAXREC),(1,DXASQLB),(R4),VERSION             
*                                                                               
         L     RF,DXASQLB                                                       
UPDL04   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LOAD WORKCODE GROUP RECORD                                          *         
***********************************************************************         
LOADTGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST MGR RECORD             
         USING WGRRECD,R2                                                       
         XC    WGRKEY,WGRKEY                                                    
         MVI   WGRKTYP,WGRKTYPQ                                                 
         MVI   WGRKSUB,WGRKSUBQ                                                 
         MVC   WGRKCPY,COMPANY                                                  
         MVC   WGRKUNT,UNIT                                                     
         MVC   WGRKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTGR02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   WGRKEY(WGRKCODE-WGRKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,WGRKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXTGRC,AINITTGR,AFILTTGR                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTGR02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE WORKCODE GROUP RECORD                                        *         
***********************************************************************         
UPDTTGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING WGRRECD,R2                                                       
         GOTO1 AFILTTGR            FILTER RECORD                                
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITTGR            INITIALISE EXTRACT BUFFER                    
         GOTO1 AACCUPDT,DMCB,VAXTGRC,=C'TGR'                                    
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER WORKCODE GROUP RECORD AT R2                                  *         
***********************************************************************         
FILTTGR  NTR1  BASE=*,LABEL=*                                                   
         USING WGRRECD,R2                                                       
         CLI   WGRKTYP,WGRKTYPQ    X'2C'                                        
         JNE   NO                                                               
         CLI   WGRKSUB,WGRKSUBQ    X'08'                                        
         JNE   NO                                                               
         CLC   WGRKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   WGRKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   WGRKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
***********************************************************************         
* INITIALISE WORKCODE GROUP RECORD                                    *         
***********************************************************************         
INITTGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTGRDL          R1=L'TGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED      *         
***********************************************************************         
COPYBUFF DS    8096C                                                            
COPYBUFL EQU   *-COPYBUFF                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GLOBAL WORKING STORAGE                               *         
***********************************************************************         
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
DMWORK   DS    12D                                                              
SAVEPTR  DS    F                   SAVED ADDRESS OF SYSTEM DRIVER TAB           
SVPTRPR  DS    F                   SAVED ADDRESS OF PREVIOUS ENTRY              
SVPTRCU  DS    F                   SAVED ADDRESS OF CURRENT ENTRY               
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
ACCADDR  DS    XL4                                                              
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
FLAG     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
SAVSIN   DS    XL4                                                              
SVSENUM  DS    XL1                                                              
SVAGYB   DS    XL1                                                              
*                                                                               
COMPANY  DS    XL1                                                              
UNIT     DS    CL1                                                              
LEDGER   DS    CL1                                                              
VERSION  DS    XL1                                                              
*                                                                               
CLISTAT  DS    XL1                 ACTKSTAT FROM CLIENT                         
PROSTAT  DS    XL1                 ACTKSTAT FROM PRODUCT                        
*                                                                               
CURRCUL  DS    0XL3                CURRENT COMPANY UNIT LEDGER                  
CURRCPY  DS    X                   CURRENT COMPANY FOR LEVELS                   
CURRUL   DS    XL2                 CURRENT UNIT/LEDGER FOR LEVELS               
*                                                                               
BUFFRET  DS    XL1                                                              
*                                                                               
LEVREC   DS    0XL11               WORK AREA FOR BUFFERIN                       
LEVKCPY  DS    XL1                   KEY - COMPANY                              
LEVKUL   DS    CL2                       - UNIT/LEDGER                          
LEVKLNQ  EQU   *-LEVREC            KEY LENGTH                                   
LEVRLEVS DS    0CL4                                                             
LEVRL1   DS    XL1                                                              
LEVRL2   DS    XL1                                                              
LEVRL3   DS    XL1                                                              
LEVRL4   DS    XL1                                                              
LEVRDSPS DS    0CL4                                                             
LEVRD1   DS    XL1                                                              
LEVRD2   DS    XL1                                                              
LEVRD3   DS    XL1                                                              
LEVRD4   DS    XL1                                                              
LEVRLNQ  EQU   *-LEVREC            RECORD LENGTH                                
*                                                                               
LEVELS   DS    0CL4                                                             
L1       DS    XL1                                                              
L2       DS    XL1                                                              
L3       DS    XL1                                                              
L4       DS    XL1                                                              
DISPS    DS    0CL4                                                             
D1       DS    XL1                                                              
D2       DS    XL1                                                              
D3       DS    XL1                                                              
D4       DS    XL1                                                              
*                                                                               
SJLEVELS DS    0CL4                PRODUCTION LEDGER                            
SJL1     DS    XL1                                                              
SJL2     DS    XL1                                                              
SJL3     DS    XL1                                                              
SJL4     DS    XL1                                                              
SJDISPS  DS    0CL4                                                             
SJD1     DS    XL1                                                              
SJD2     DS    XL1                                                              
SJD3     DS    XL1                                                              
SJD4     DS    XL1                                                              
*                                                                               
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPEDEEP DS    XL1                                                              
TYPEFLAG DS    XL1                                                              
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
*                                                                               
WORK     DS    XL80                                                             
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TYPTAB TABLE                                         *         
***********************************************************************         
TYPTABD  DSECT                                                                  
TYPNAME  DS    XL3                 3 CHAR MNEMONIC FOR RECORD TYPE              
TYPLDEEP DS    XL1                 DEPTH INTO LEDGER FOR COMPARE (LOAD)         
TYPFLAG  DS    XL1                 FLAGS                                        
         DS    XL3                 N/D                                          
TYPLOAD  DS    XL4                 A(LOAD ROUTINE)                              
TYPUPDT  DS    XL4                 A(UPDATE ROUTINE)                            
TYPTABLQ EQU   *-TYPTABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER COMMON ADDRESSES                                     *         
***********************************************************************         
ADDRESSD DSECT                                                                  
COMMON   DS    CL8                                                              
ACOMFACS DS    V                                                                
VDATAMGR DS    V                                                                
VDMOD000 DS    V                                                                
VDADDS   DS    V                                                                
VLOGIO   DS    V                                                                
VDATCON  DS    V                                                                
VBUFFRIN DS    V                                                                
VSORTER  DS    V                                                                
VRECTYP  DS    V                                                                
VAXCNVX  DS    V                                                                
VAXDACC  DS    V                                                                
VAXDTRC  DS    V                                                                
VAXADRC  DS    V                   ADJUSTED RATE                                
VAXCALC  DS    V                   CALENDAR                                     
VAXCLIC  DS    V                   CLIENT                                       
VAXCPYC  DS    V                   COMPANY                                      
         DS    V                                                                
VAXCSAC  DS    V                   COSTING ACCOUNT                              
VAXDEPC  DS    V                   DEPARTMENT                                   
VAXDEGC  DS    V                   DEPARTMENT GROUP                             
VAXHRSC  DS    V                   HOURS                                        
VAXINAC  DS    V                   INCOME ACCOUNT                               
VAXISAC  DS    V                   INCOME SUSPENSE ACCOUNT                      
VAXJOBC  DS    V                   JOB                                          
VAXLEDC  DS    V                   LEDGER                                       
VAXLESC  DS    V                   LEDGER STRUCTURE                             
VAXLOCC  DS    V                   LOCALITY                                     
VAXLCIC  DS    V                   LOCALITY INFORMATION                         
VAXMNGC  DS    V                   MANAGER GROUP                                
VAXMGMC  DS    V                   MANAGER GROUP MANAGERS                       
VAXMEDC  DS    V                   MEDIA                                        
VAXMGRC  DS    V                   MEDIA GROUP                                  
VAXNCTC  DS    V                   NON-CLIENT TIME                              
VAXOFFC  DS    V                   OFFICE                                       
VAXOGRC  DS    V                   OFFICE GROUP                                 
VAXOFLC  DS    V                   OFFICE LIST                                  
VAXOLEC  DS    V                   OFFICE LIST ENTRY                            
VAXPEDC  DS    V                   PERIOD                                       
VAXPERC  DS    V                   PERSON                                       
VAXPEAC  DS    V                   PERSON ASSIGNMENT                            
VAXPROC  DS    V                   PRODUCT                                      
VAXRATC  DS    V                   RATE                                         
VAXSTHC  DS    V                   STANDARD HOURS                               
VAXEDTC  DS    V                   EDIT HOURS                                   
VAXCAPC  DS    V                   COST ALLOCATION PROFILE                      
VAXSBDC  DS    V                   SUB-DEPARTMENT                               
VAXTSKC  DS    V                   TASK                                         
VAXTISC  DS    V                   TIMESHEET                                    
VAXTALC  DS    V                   TIMESHEET ALLOCATION/TAX UPDATE              
VAXPHIC  DS    V                   COST RATE                                    
VAXOPTC  DS    V                   OPTIONS                                      
VAXP1RC  DS    V                   PERSON1R                                     
VAXPOFC  DS    V                   PRODUCTION OFFICE                            
VAXTGRC  DS    V                   WORKCODE GROUP                               
VAXDTMC  DS    V                   DAILY TIME                                   
VAXBOSC  DS    V                   BRAND OCEAN TIME (7/09)                      
         DS    CL8                 COMMON INTERNAL ROUTINES                     
AACCLOAD DS    A                                                                
AACCUPDT DS    A                                                                
ADECIOC  DS    A                                                                
ACHKSEQ  DS    A                                                                
ARDLEDG  DS    A                                                                
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                                                                
ARECCMP  DS    A                                                                
ALEVBUF  DS    A                                                                
ASORTPUT DS    A                                                                
ASORTEND DS    A                                                                
         DS    CL8                 LOAD ROUTINES                                
ALOADADR DS    A                   ADJUSTED RATE                                
ALOADCAL DS    A                   CALENDAR                                     
ALOADCLI DS    A                   CLIENT                                       
ALOADCPY DS    A                   COMPANY                                      
         DS    A                                                                
ALOADCSA DS    A                   COSTING ACCOUNT                              
ALOADDEP DS    A                   DEPARTMENT                                   
ALOADDEG DS    A                   DEPARTMENT GROUP                             
ALOADHRS DS    A                   HOURS                                        
ALOADINA DS    A                   INCOME ACCOUNT                               
ALOADISA DS    A                   INCOME SUSPENSE ACCOUNT                      
ALOADJOB DS    A                   JOB                                          
ALOADLED DS    A                   LEDGER                                       
ALOADLES DS    A                   LEDGER STRUCTURE                             
ALOADLOC DS    A                   LOCALITY                                     
ALOADLCI DS    A                   LOCALITY INFORMATION                         
ALOADMNG DS    A                   MANAGER GROUP                                
ALOADMGM DS    A                   MANAGER GROUP MANAGERS                       
ALOADMED DS    A                   MEDIA                                        
ALOADMGR DS    A                   MEDIA GROUP                                  
ALOADNCT DS    A                   NON-CLIENT TIME                              
ALOADOFF DS    A                   OFFICE                                       
ALOADOGR DS    A                   OFFICE GROUP                                 
ALOADOFL DS    A                   OFFICE LIST                                  
ALOADOLE DS    A                   OFFICE LIST ENTRY                            
ALOADPED DS    A                   PERIOD                                       
ALOADPER DS    A                   PERSON                                       
ALOADPEA DS    A                   PERSON ASSIGNMENT                            
ALOADPRO DS    A                   PRODUCT                                      
ALOADRAT DS    A                   RATE                                         
ALOADSTH DS    A                   STANDARD HOURS                               
ALOADEDT DS    A                   EDIT HOURS                                   
ALOADCAP DS    A                   COST ALLOCATION PROFILE                      
ALOADSBD DS    A                   SUB-DEPARTMENT                               
ALOADTSK DS    A                   TASK                                         
ALOADTIS DS    A                   TIMESHEET                                    
ALOADTAL DS    A                   TIMESHEET ALLOCATION/TAX                     
ALOADPHI DS    A                   COST RATE                                    
         DS    A                                                                
         DS    A                                                                
ALOADOPT DS    A                   OPTIONS                                      
ALOADP1R DS    A                   PERSON1R                                     
ALOADPOF DS    A                   PRODUCTION OFFICE                            
ALOADTGR DS    A                   WORKCODE GROUP                               
ALOADDTM DS    A                   DAILY TIME                                   
ALOADBOS DS    A                   BrandO Status                                
         DS    CL8                 UPDATE ROUTINES                              
AUPDTADR DS    A                   ADJUSTED RATE                                
AUPDTCAL DS    A                   CALENDAR                                     
AUPDTCLI DS    A                   CLIENT                                       
AUPDTCPY DS    A                   COMPANY                                      
         DS    A                                                                
AUPDTCSA DS    A                   COSTING ACCOUNT                              
AUPDTDEP DS    A                   DEPARTMENT                                   
AUPDTDEG DS    A                   DEPARTMENT GROUP                             
AUPDTHRS DS    A                   HOURS                                        
AUPDTINA DS    A                   INCOME ACCOUNT                               
AUPDTISA DS    A                   INCOME SUSPENSE ACCOUNT                      
AUPDTJOB DS    A                   JOB                                          
AUPDTLED DS    A                   LEDGER                                       
AUPDTLES DS    A                   LEDGER STRUCTURE                             
AUPDTLOC DS    A                   LOCALITY                                     
AUPDTLCI DS    A                   LOCALITY INFORMATION                         
AUPDTMNG DS    A                   MANAGER GROUP                                
AUPDTMGM DS    A                   MANAGER GROUP MANAGERS                       
AUPDTMED DS    A                   MEDIA                                        
AUPDTMGR DS    A                   MEDIA GROUP                                  
AUPDTNCT DS    A                   NON-CLIENT TIME                              
AUPDTOFF DS    A                   OFFICE                                       
AUPDTOGR DS    A                   OFFICE GROUP                                 
AUPDTOFL DS    A                   OFFICE LIST                                  
AUPDTOLE DS    A                   OFFICE LIST ENTRY                            
AUPDTPED DS    A                   PERIOD                                       
AUPDTPER DS    A                   PERSON                                       
AUPDTPEA DS    A                   PERSON ASSIGNMENT                            
AUPDTPRO DS    A                   PRODUCT                                      
AUPDTRAT DS    A                   RATE                                         
AUPDTSTH DS    A                   STANDARD HOURS                               
AUPDTEDT DS    A                   EDIT HOURS                                   
AUPDTCAP DS    A                   COST ALLOCATION PROFILE                      
AUPDTSBD DS    A                   SUB-DEPARTMENT                               
AUPDTTSK DS    A                   TASK                                         
AUPDTTIS DS    A                   TIMESHEET                                    
AUPDTTAL DS    A                   TIMESHEET ALLOCATION/TAX                     
AUPDTPHI DS    A                   COST RATE                                    
AUPDTDAC DS    A                                                                
AUPDTDTR DS    A                                                                
AUPDTOPT DS    A                   OPTIONS                                      
AUPDTP1R DS    A                   PERSON1R                                     
AUPDTPOF DS    A                   PRODUCTION OFFICE                            
AUPDTTGR DS    A                   WORKCODE GROUP                               
AUPDTDTM DS    A                   DAILY TIME                                   
         DS    CL8                 FILTER ROUTINES                              
AFILTADR DS    A                   ADJUSTED RATE                                
AFILTCAL DS    A                   CALENDAR                                     
AFILTCLI DS    A                   CLIENT                                       
AFILTCPY DS    A                   COMPANY                                      
         DS    A                                                                
AFILTCSA DS    A                   COSTING ACCOUNT                              
AFILTDEP DS    A                   DEPARTMENT                                   
AFILTDEG DS    A                   DEPARTMENT GROUP                             
AFILTHRS DS    A                   HOURS                                        
AFILTINA DS    A                   INCOME ACCOUNT                               
AFILTISA DS    A                   INCOME SUSPENSE ACCOUNT                      
AFILTJOB DS    A                   JOB                                          
AFILTLED DS    A                   LEDGER                                       
AFILTLES DS    A                   LEDGER STRUCTURE                             
AFILTLOC DS    A                   LOCALITY                                     
AFILTLCI DS    A                   LOCALITY INFORMATION                         
AFILTMNG DS    A                   MANAGER GROUP                                
AFILTMGM DS    A                   MANAGER GROUP MANAGERS                       
AFILTMED DS    A                   MEDIA                                        
AFILTMGR DS    A                   MEDIA GROUP                                  
AFILTNCT DS    A                   NON-CLIENT TIME                              
AFILTOFF DS    A                   OFFICE                                       
AFILTOGR DS    A                   OFFICE GROUP                                 
AFILTOFL DS    A                   OFFICE LIST                                  
AFILTOLE DS    A                   OFFICE LIST ENTRY                            
AFILTPED DS    A                   PERIOD                                       
AFILTPER DS    A                   PERSON                                       
AFILTPEA DS    A                   PERSON ASSIGNMENT                            
AFILTPRO DS    A                   PRODUCT                                      
AFILTRAT DS    A                   RATE                                         
AFILTSTH DS    A                   STANDARD HOURS                               
AFILTEDT DS    A                   EDIT HOURS                                   
AFILTCAP DS    A                   COST ALLOCATION PROFILE                      
AFILTSBD DS    A                   SUB-DEPARTMENT                               
AFILTTSK DS    A                   TASK                                         
AFILTTIS DS    A                   TIMESHEET                                    
AFILTTAU DS    A                   TIMESHEET - BRANDOCEAN                       
AFILTTAL DS    A                   TIMESHEET ALLOCATION/TAX                     
AFILTPHI DS    A                   COST RATE                                    
         DS    A                                                                
         DS    A                                                                
AFILTOPT DS    A                   OPTIONS                                      
AFILTP1R DS    A                   PERSON1R                                     
AFILTPOF DS    A                   PRODUCTION OFFICE                            
AFILTTGR DS    A                   WORKCODE GROUP                               
AFILTDTM DS    A                   DAILY TIME                                   
AFILTBOS DS    A                   BRAND OCEAN STATUS                           
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   GENERAL INITIALISATION                       
AINITADR DS    A                   ADJUSTED RATE                                
AINITCAL DS    A                   CALENDAR                                     
AINITCLI DS    A                   CLIENT                                       
AINITCPY DS    A                   COMPANY                                      
         DS    A                                                                
AINITCSA DS    A                   COSTING ACCOUNT                              
AINITDEP DS    A                   DEPARTMENT                                   
AINITDEG DS    A                   DEPARTMENT GROUP                             
AINITHRS DS    A                   HOURS                                        
AINITINA DS    A                   INCOME ACCOUNT                               
AINITISA DS    A                   INCOME SUSPENSE ACCOUNT                      
AINITJOB DS    A                   JOB                                          
AINITLED DS    A                   LEDGER                                       
AINITLES DS    A                   LEDGER STRUCTURE                             
AINITLOC DS    A                   LOCALITY                                     
AINITLCI DS    A                   LOCALITY INFORMATION                         
AINITMNG DS    A                   MANAGER GROUP                                
AINITMGM DS    A                   MANAGER GROUP MANAGERS                       
AINITMED DS    A                   MEDIA                                        
AINITMGR DS    A                   MEDIA GROUP                                  
AINITNCT DS    A                   NON-CLIENT TIME                              
AINITOFF DS    A                   OFFICE                                       
AINITOGR DS    A                   OFFICE GROUP                                 
AINITOFL DS    A                   OFFICE LIST                                  
AINITOLE DS    A                   OFFICE LIST ENTRY                            
AINITPED DS    A                   PERIOD                                       
AINITPER DS    A                   PERSON                                       
AINITPEA DS    A                   PERSON ASSIGNMENT                            
AINITPRO DS    A                   PRODUCT                                      
AINITRAT DS    A                   RATE                                         
AINITSTH DS    A                   STANDARD HOURS                               
AINITEDT DS    A                   EDIT HOURS                                   
AINITCAP DS    A                   COST ALLOCATION PROFILE                      
AINITSBD DS    A                   SUB-DEPARTMENT                               
AINITTSK DS    A                   TASK                                         
AINITTIS DS    A                   TIMESHEET                                    
AINITTAL DS    A                   TIMESHEET ALLOCATION/TAX                     
AINITPHI DS    A                   COST RATE                                    
AINITDAC DS    A                                                                
AINITDTR DS    A                                                                
AINITOPT DS    A                   OPTIONS                                      
AINITP1R DS    A                   PERSON1R                                     
AINITPOF DS    A                   PRODUCTION OFFICE                            
AINITTGR DS    A                   WORKCODE GROUP                               
AINITDTM DS    A                   DAILY TIME                                   
AINITBOS DS    V                   BRAND OCEAN STATUS                           
DMOPEN   DS    CL7                                                              
DMREAD   DS    CL7                                                              
DMRSEQ   DS    CL7                                                              
DMRDHI   DS    CL7                                                              
DMCLSE   DS    CL7                                                              
DMFAST   DS    CL7                                                              
GETREC   DS    CL7                                                              
DMRFIL   DS    CL7                                                              
CONTROL  DS    CL7                                                              
CTFILE   DS    CL7                                                              
ACCDIR   DS    CL7                                                              
ACCMST   DS    CL7                                                              
ACCARC   DS    CL7                                                              
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ACOPYBUF DS    A                                                                
ACTIVITY DS    CL1                                                              
SORTFLAG DS    CL1                                                              
UPDTIND  DS    XL1                                                              
UPDTADD  EQU   X'80'               RECORDS IN ADD BUFFER                        
UPDTDEL  EQU   X'40'               RECORDS IN DELETE BUFFER                     
UPDTEND  EQU   X'20'               NO MORE RECORDS FROM SORT                    
SPACES   DS    CL80                                                             
SEQNO    DS    F                                                                
CMPEL    DS    CL(CPYLN2Q)                                                      
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
* SSBOFF                                                                        
SSBD     DSECT                                                                  
       ++INCLUDE FASSBOFF                                                       
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
* AXRECD                                                                        
         PRINT OFF                                                              
       ++INCLUDE AXRECD                                                         
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACRECEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACRECEQUS                                                      
         PRINT ON                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
* DDBUFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'098AXTRACT   08/02/19'                                      
         END                                                                    
