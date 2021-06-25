*          DATA SET JSHTRACT   AT LEVEL 087 AS OF 03/19/03                      
*PHASE AXTRACTA                                                                 
*INCLUDE AXROUTS                  ADJUSTED RATE                                 
*INCLUDE AXCNVX                   CONVERSION ROUTINES FOR ALL ABOVE             
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
         SPACE 1                                                                
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
***********************************************************************         
         EJECT                                                                  
AXTRACT  CSECT                                                                  
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
         SPACE 1                                                                
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
         BNE   MERR                                                             
         BAS   RE,PROCUPDT         EXTRACT FROM FILES IN UPDATE MODE            
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
         SPACE 1                                                                
GENINIT  BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                        *         
***********************************************************************         
         SPACE 1                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
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
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   CMPEL(0),0(R2)                                                   
*&&                                                                             
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE ACCOUNT SYSTEM FILES                      *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         L     RF,TYPEAUPD         ELSE CALL UPDATE PROCESS ROUTINE             
         GOTO1 (RF),DMCB,(RC)                                                   
         JNE   NO                  EXIT ERROR                                   
         J     YES                 EXIT OK                                      
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PROCESS KEY VALUES IN RECOVERY RECORD                               *         
* NTRY: R5 = A(RECOVERY RECORD)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RECDS,R5                                                         
         USING ACTRECD,RECVHDR+L'RECVHDR                                        
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         TM    ACTRSTAT,ACTSDELT   IS THIS RECORD DELETED?                      
         BZ    PKEY02              NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDELT                        
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
*              TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD            
*                                                                               
PKEY02   CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+ACTRSTAT-ACTRECD+4(R4),ACTSDELT                        
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0L                                                               
ADDRESS  DC    CL8'EXTERNS'                                                     
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         DC    A(FILTTAL)          TIMESHEET ALLOCATION/TAX                     
         DC    A(FILTPHI)          COST RATE                                    
         DC    A(0)                                                             
         DC    A(0)                                                             
         DC    A(FILTOPT)          OPTIONS                                      
         DC    A(FILTP1R)          PERSON1R                                     
         DC    A(FILTPOF)          PRODUCTION OFFICE                            
         DC    A(FILTTGR)          WORKCODE GROUP                               
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         DC    80C' '                                                           
         DC    (CPYLN2Q)X'00'      COMPANY ELEMENT EXTRACTED                    
*                                                                               
*                                                                               
         LTORG                                                                  
ACCMSTQ  EQU   X'6A'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
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
         SPACE 1                                                                
TYPTAB   DS    0L                                                               
         DC    CL3'ALL',AL1(00,00,00,00,00),AL4(LOADALL,UPDTALL)                
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
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* CHECK SEQUENTIAL IO BROKEN, LAST KEY IN IOKEY                       *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
***********************************************************************         
* CALL DMGR TO PERFORM A READHI                                       *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
RDLEDG   NTR1  BASE=*,LABEL=*                                                   
         CLC   CURRCPY,COMPANY                                                  
         JNE   RLED01                                                           
         CLC   UNIT,CURRUL                                                      
         JNE   RLED01                                                           
         CLC   LEDGER,CURRUL+1                                                  
         JE    YES                                                              
*                                                                               
RLED01   LA    R2,IOKEY                                                         
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
RLED02   CLI   0(R1),0             END OF RECORD?                               
         JE    RLEDWTO                                                          
         CLI   0(R1),ACLELQ                                                     
         JE    RLED04                                                           
         IC    RF,1(R1)                                                         
         LA    R1,0(RF,R1)                                                      
         J     RLED02                                                           
*                                                                               
         USING ACLELD,R1                                                        
RLED04   XC    DISPS,DISPS                                                      
         XC    LEVELS,LEVELS                                                    
         LA    RE,ACLVALS                                                       
         IC    RF,ACLLN                                                         
         DROP  R1                                                               
*                                                                               
         SRL   RF,4               WILL GIVE NUMBER OF LEVELS                    
         XR    R2,R2                                                            
         LA    R1,DISPS                                                         
*                                                                               
RLED06   MVC   0(1,R1),0(RE)                                                    
         LA    RE,16(RE)                                                        
         LA    R1,1(R1)                                                         
         BCT   RF,RLED06                                                        
*                                                                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         LA    R2,DISPS                                                         
         IC    RE,0(R2)                                                         
         LA    R1,LEVELS                                                        
         STC   RE,0(R1)                                                         
         LA    R0,3                                                             
*                                                                               
RLED08   LA    R1,1(R1)                                                         
         IC    RE,0(R2)                                                         
         IC    RF,1(R2)                                                         
         LTR   RF,RF                                                            
         JZ    YES                                                              
         SR    RF,RE                                                            
         STC   RF,0(R1)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,RLED08                                                        
         J     YES                                                              
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
         SPACE 1                                                                
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
* LOAD ALL RECORD DATA                                                *         
***********************************************************************         
         SPACE 1                                                                
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOAD04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD04   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD02                                                           
*                                                                               
LOADTAB  DS    0XL8                                                             
         DC    CL3'CPY',AL1(0),AL4(LOADCPY) COMPANY                             
         DC    CL3'LED',AL1(0),AL4(LOADLED) LEDGER                              
         DC    CL3'LES',AL1(0),AL4(LOADLES) LEDGER STRUCTURE                    
         DC    CL3'OGR',AL1(0),AL4(LOADOGR) OFFICE GROUP                        
         DC    CL3'DEG',AL1(0),AL4(LOADDEG) DEPARTMENT GROUP                    
         DC    CL3'OFF',AL1(0),AL4(LOADOFF) OFFICE                              
         DC    CL3'DEP',AL1(0),AL4(LOADDEP) DEPARTMENT                          
         DC    CL3'SBD',AL1(0),AL4(LOADSBD) SUB-DEPARTMENT                      
         DC    CL3'P1R',AL1(0),AL4(LOADP1R) PERSON 1R                           
         DC    CL3'PER',AL1(0),AL4(LOADPER) PERSON                              
         DC    CL3'PEA',AL1(0),AL4(LOADPEA) PERSON ASSIGNMENT                   
         DC    CL3'OFL',AL1(0),AL4(LOADOFL) OFFICE LIST                         
         DC    CL3'OLE',AL1(0),AL4(LOADOLE) OFFICE LIST ENTRY                   
         DC    CL3'PED',AL1(0),AL4(LOADPED) PERIOD                              
         DC    CL3'LOC',AL1(0),AL4(LOADLOC) LOCALITY                            
         DC    CL3'LCI',AL1(0),AL4(LOADLCI) LOCALITY INFORMATION                
         DC    CL3'MGR',AL1(0),AL4(LOADMGR) MEDIA GROUP                         
         DC    CL3'MED',AL1(0),AL4(LOADMED) MEDIA                               
         DC    CL3'POF',AL1(0),AL4(LOADPOF) PRODUCTION OFFICE                   
         DC    CL3'CLI',AL1(0),AL4(LOADCLI) CLIENT                              
         DC    CL3'PRO',AL1(0),AL4(LOADPRO) PRODUCT                             
         DC    CL3'JOB',AL1(0),AL4(LOADJOB) JOB                                 
         DC    CL3'TSK',AL1(0),AL4(LOADTSK) TASK                                
         DC    CL3'TGR',AL1(2),AL4(LOADTGR) TASKCODE GROUP                      
         DC    CL3'NCT',AL1(0),AL4(LOADNCT) NON-CLIENT TIME                     
         DC    CL3'INA',AL1(0),AL4(LOADINA) INCOME ACCOUNT                      
         DC    CL3'ISA',AL1(0),AL4(LOADISA) INCOME SUSPENSE ACCOUNT             
         DC    CL3'ADR',AL1(0),AL4(LOADADR) ADJUSTED RATE                       
         DC    CL3'CSA',AL1(0),AL4(LOADCSA) COSTING ACCOUNT                     
         DC    CL3'EDT',AL1(0),AL4(LOADEDT) EDIT HOURS                          
         DC    CL3'RAT',AL1(0),AL4(LOADRAT) RATE                                
         DC    CL3'STH',AL1(0),AL4(LOADSTH) STANDARD HOURS                      
         DC    CL3'OPT',AL1(0),AL4(LOADOPT) OPTION                              
         DC    CL3'TIS',AL1(0),AL4(LOADTIS) TIMESHEET                           
         DC    CL3'TAL',AL1(0),AL4(LOADTAL) TIMESHEET ALLOCATION (+TAT)         
         DC    CL3'CAP',AL1(0),AL4(LOADCAP) COST ALLOC PROFILE                  
*        DC    CL3'CAL',AL1(0),AL4(LOADCAL) CALENDAR (18NOV94 COMMENT)          
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
UPDT02   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDT04                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT04   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT02                                                           
*                                                                               
UPDTTAB  DS    0XL8                                                             
         DC    CL3'CPY',AL1(0),AL4(UPDTCPY) COMPANY                             
         DC    CL3'LED',AL1(0),AL4(UPDTLED) LEDGER                              
         DC    CL3'LES',AL1(0),AL4(UPDTLES) LEDGER STRUCTURE                    
         DC    CL3'OGR',AL1(0),AL4(UPDTOGR) OFFICE GROUP                        
         DC    CL3'DEG',AL1(0),AL4(UPDTDEG) DEPARTMENT GROUP                    
         DC    CL3'OFF',AL1(0),AL4(UPDTOFF) OFFICE                              
         DC    CL3'DEP',AL1(0),AL4(UPDTDEP) DEPARTMENT                          
         DC    CL3'SBD',AL1(0),AL4(UPDTSBD) SUB-DEPARTMENT                      
         DC    CL3'P1R',AL1(0),AL4(UPDTP1R) PERSON 1R                           
         DC    CL3'PER',AL1(0),AL4(UPDTPER) PERSON                              
         DC    CL3'PEA',AL1(0),AL4(UPDTPEA) PERSON ASSIGNMENT                   
         DC    CL3'OFL',AL1(0),AL4(UPDTOFL) OFFICE LIST                         
         DC    CL3'OLE',AL1(0),AL4(UPDTOLE) OFFICE LIST ENTRY                   
         DC    CL3'PED',AL1(0),AL4(UPDTPED) PERIOD                              
         DC    CL3'LOC',AL1(0),AL4(UPDTLOC) LOCALITY                            
         DC    CL3'LCI',AL1(0),AL4(UPDTLCI) LOCALITY INFORMATION                
         DC    CL3'MGR',AL1(0),AL4(UPDTMGR) MEDIA GROUP                         
         DC    CL3'MED',AL1(0),AL4(UPDTMED) MEDIA                               
         DC    CL3'POF',AL1(0),AL4(UPDTPOF) PRODUCTION OFFICE                   
         DC    CL3'CLI',AL1(0),AL4(UPDTCLI) CLIENT                              
         DC    CL3'PRO',AL1(0),AL4(UPDTPRO) PRODUCT                             
         DC    CL3'JOB',AL1(0),AL4(UPDTJOB) JOB                                 
         DC    CL3'TSK',AL1(0),AL4(UPDTTSK) TASK                                
         DC    CL3'TGR',AL1(2),AL4(UPDTTGR) TASKCODE GROUP                      
         DC    CL3'NCT',AL1(0),AL4(UPDTNCT) NON-CLIENT TIME                     
         DC    CL3'INA',AL1(0),AL4(UPDTINA) INCOME ACCOUNT                      
         DC    CL3'ISA',AL1(0),AL4(UPDTISA) INCOME SUSPENSE ACCOUNT             
         DC    CL3'ADR',AL1(0),AL4(UPDTADR) ADJUSTED RATE                       
         DC    CL3'CSA',AL1(0),AL4(UPDTCSA) COSTING ACCOUNT                     
         DC    CL3'EDT',AL1(0),AL4(UPDTEDT) EDIT HOURS                          
         DC    CL3'RAT',AL1(0),AL4(UPDTRAT) RATE                                
         DC    CL3'STH',AL1(0),AL4(UPDTSTH) STANDARD HOURS                      
         DC    CL3'OPT',AL1(0),AL4(UPDTOPT) OPTION                              
         DC    CL3'TIS',AL1(0),AL4(UPDTTIS) TIMESHEET                           
         DC    CL3'TAL',AL1(0),AL4(UPDTTAL) TIMESHEET ALLOCATION (+TAT)         
         DC    CL3'CAP',AL1(0),AL4(UPDTCAP) COST ALLOC PROFILE                  
*        DC    CL3'CAL',AL1(0),AL4(UPDTCAL) CALENDAR (18NOV94 COMMENT)          
         DC    X'00'                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALISE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
***********************************************************************         
* LOAD CALENDAR RECORDS                                               *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE CALENDAR RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
INITCAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCALDL          R1=L'CAL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD ADJUSTED RATE RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE ADJUSTED RATE RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
INITADR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXADRDL          R1=L'ADR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
*&&US*&& J     YES                                                              
*&&UK                                                                           
         CLI   DXMODE,DXLOADQ      LOAD?                                        
         JNE   YES                 NO                                           
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
         SPACE 1                                                                
INITCLI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCLIDL          R1=L'CLI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD COMPANY RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE COMPANY RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
INITCPY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCPYDL          R1=L'CPY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD COSTING ACCOUNT RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE COSTING ACCOUNT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITCSA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCSADL          R1=L'CSA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD DEPARTMENT RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         JE    YES                                                              
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
         SPACE 1                                                                
INITDEP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXDEPDL          R1=L'DEP RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD DEPARTMENT GROUP RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE DEPARTMENT GROUP RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITDEG  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXDEGDL          R1=L'DEG RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD INCOME ACCOUNT RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         JO    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE INCOME ACCOUNT RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITINA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXINADL          R1=L'INA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD INCOME SUSPENSE ACCOUNT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         JO    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE INCOME SUSPENSE ACCOUNT                                  *         
***********************************************************************         
         SPACE 1                                                                
INITISA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXISADL          R1=L'ISA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD JOB RECORD                                                     *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* UPDATE JOB RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* FILTER JOB RECORD AT R2                                             *         
***********************************************************************         
         SPACE 1                                                                
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
*&&US*&& J     YES                                                              
*&&UK                                                                           
         CLI   DXMODE,DXLOADQ      LOAD                                         
         JNE   YES                                                              
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
         SPACE 1                                                                
INITJOB  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXJOBDL          R1=L'JOB RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LEDGER RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LEDGER RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITLED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLEDDL          R1=L'LED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LEDGER STRUCTURE RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
FILTLES  NTR1  BASE=*,LABEL=*                                                   
         USING LDGRECD,R2                                                       
         CLC   LDGKEY+LDGKEND(L'LDGKEY-LDGKEND),SPACES                          
         JE    YES                                                              
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LEDGER STRUCTURE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITLES  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLESDL          R1=L'LES RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LOCALITY RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LOCALITY RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
INITLOC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLOCDL          R1=L'LOC RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD LOCALITY INFORMATION RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LOCALITY INFORMATION RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
INITLCI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXLCIDL          R1=L'LCI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD MEDIA RECORD                                                   *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE MEDIA RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
INITMED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXMEDDL          R1=L'MED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD MEDIA GROUP RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE MEDIA GROUP RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
INITMGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXMGRDL          R1=L'MGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD NON - CLIENT TIME RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE NON - CLIENT TIME ACCOUNT RECORD                         *         
***********************************************************************         
         SPACE 1                                                                
INITNCT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXNCTDL          R1=L'NCT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         J     YES                                                              
*                                                                               
FOFFCLC  CLC   ACTKACT(0),SPACES   OFFICE CODE MUST BE NON-ZERO                 
FOFFCLC1 CLC   0(0,RF),SPACES      MUST HAVE ONLY OFFICE CODE                   
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE OFFICE RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITOFF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOFFDL          R1=L'OFF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE GROUP RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE OFFICE GROUP RECORD                                      *         
***********************************************************************         
         SPACE 1                                                                
INITOGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOGRDL          R1=L'OGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE LIST RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE OFFICE LIST RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
INITOFL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOFLDL          R1=L'OFL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCTION OFFICE RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PRODUCTION OFFICE                                        *         
***********************************************************************         
         SPACE 1                                                                
INITPOF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPOFDL          R1=L'POF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE LIST ENTRY RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE OFFICE LIST ENTRY RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITOLE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOLEDL          R1=L'OLE RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD OPTION RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE OPTION RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITOPT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXOPTDL          R1=L'OPT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PERIOD RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
INITPED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPEDDL          R1=L'PED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         SPACE 1                                                                
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PERSON RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITPER  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPERDL          R1=L'PER RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON ASSIGNMENT                                              *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PERSON ASSIGNMENT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITPEA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPEADL          R1=L'PEA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT RECORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
*&&US*&& J     YES                                                              
*&&UK                                                                           
         CLI   DXMODE,DXLOADQ      LOAD?                                        
         JNE   YES                 NO                                           
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
         SPACE 1                                                                
INITPRO  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPRODL          R1=L'PRO RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD COST ALLOCATION PROFILE RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE COST ALLOCATION PROFILE                                  *         
***********************************************************************         
         SPACE 1                                                                
INITCAP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXCAPDL          R1=L'CAP RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD RATE RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADRAT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
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
         GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LRAT04   GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS                      
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
         SPACE 1                                                                
UPDTRAT  NTR1  BASE=*,LABEL=*                                                   
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
         GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(0,0),(R6) BUILD KILL RECORD           
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
*                                                   BUILD RECORDS               
URAT04   GOTO1 VAXRATC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS                      
*                                                   GET NEXT RECORD             
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE RATE RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
INITRAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXRATDL          R1=L'RAT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD STANDARD HOURS RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE STANDARD HOURS RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITSTH  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXSTHDL          R1=L'STH RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD EDIT HOURS RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE EDIT HOURS RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
INITEDT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXEDTDL          R1=L'EDT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD SUB - DEPARTMENT RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         JE    YES                                                              
         J     NO                                                               
*                                                                               
FSBDCLC  CLC   0(0,RF),SPACES      SUB DEPARTMENT MUST CONTAIN DATA             
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE SUB - DEPARTMENT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITSBD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXSBDDL          R1=L'SBD RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PERSON1R RECORD                                                *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         JH    YES                                                              
         J     NO                                                               
*                                                                               
FP1RCLC  CLC   0(0,RF),SPACES      PERSON CODE MUST CONTAIN DATA                
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PERSON1R RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
INITP1R  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXP1RDL          R1=L'P1R RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD TASK RECORD                                                    *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE TASK RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
INITTSK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTSKDL          R1=L'TSK RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD TIMESHEET RECORDS                                              *         
***********************************************************************         
         SPACE 1                                                                
LOADTIS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST TAL RECORD             
         USING TSXRECD,R2                                                       
         XC    TSXKEY,TSXKEY                                                    
         MVC   TSXKCPY,COMPANY                                                  
         MVI   TSXKTYP,TSXKTYPQ                                                 
         MVI   TSXKSUB,TSXKSUBQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTIS02   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TSXKEY(TSXKPER-TSXKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY CHANGES                  
*                                                                               
         MVC   ACCADDR,TSXKDA                                                   
         GOTO1 AACCLOAD,DMCB,VAXTISC,AINITTIS,AFILTTIS                          
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTIS02                                                           
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* UPDATE TIMESHEET RECORDS                                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTTIS  NTR1  BASE=*,LABEL=*                                                   
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING TSXRECD,R2                                                       
         GOTO1 AFILTTIS                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITTIS                                                         
         GOTO1 AACCUPDT,DMCB,VAXTISC,=C'TIS'                                    
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* FILTER TIS RECORD AT R2                                             *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE TIME SHEET RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
INITTIS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTISDL          R1=L'TIS RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD TIMESHEET ALLOCATION / TAX RECORDS                             *         
***********************************************************************         
         SPACE 1                                                                
LOADTAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
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
*                                                                               
LTAL02   TM    8(R1),X'80'         ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TIMKEY(TIMKACT-TIMKEY),IOKEY                                     
         JNE   YES                                                              
*                                                                               
         MVC   ACCADDR,TIMKDA                                                   
         GOTO1 AGETIT                                                           
         JNE   NO                  ERROR ON READ                                
*                                                                               
         GOTO1 AFILTTAL            FILTER RECORD                                
         JNE   LTAL08              NOT VALID                                    
*                                                                               
         GOTO1 AINITTAL            INITIALISE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VAXTALC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                  GET NEXT UNCOMMITTED RECORD                  
LTAL04   GOTO1 VAXTALC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
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
*                                                                               
         GOTO1 VAXCNVX,DMCB,(C'L',(RF)),(1,DXASQLB),(0,(R0)),VERSION            
*                                                                               
         L     RF,DXASQLB                                                       
LTAL06   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LTAL04                                                           
         J     YES                                                              
*                                                                               
LTAL08   MVC   IOKEY(L'ACTKEY),0(R2)                                            
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
         SPACE 1                                                                
UPDTTAL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING TIMRECD,R2                                                       
         GOTO1 AFILTTAL            NOT A VALID TIMESHEET RECORD                 
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITTAL            INITIALISE TAL RECORD                        
         L     R3,DXAXREC                                                       
         USING AXTALD,R3                                                        
         CLI   AXTALACT,C'A'       ADD?                                         
         JE    UTAL08              YES- NO TABLES TO CLEAR THEN                 
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R2,RECVHDR+L'RECVHDR                                             
         MVI   AXTALACT,C'D'       SET ACTION TO DELETE                         
*                                                                               
         GOTO1 VAXTALC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD COPY RECORDS          
*                                                                               
UTAL02   GOTO1 VAXTALC,DMCB,DXAXREC,(R2),(2,0),(R6) GET COPY RECORDS            
         CLI   8(R1),FF                                                         
         JE    UTAL08              FINISHED                                     
         CLI   DXWRITE,C'Y'                                                     
         JNE   UTAL06              DO NOT WRITE THIS THIS RECORD                
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UTAL04                                                           
*                                                                               
         LA    R0,=C'TAL'                                                       
         CLI   DMCB+8,X'EE'        TAX RECORDS IF EE PASSED BACK                
         JNE   *+8                                                              
         LA    R0,=C'TAT'                                                       
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,(R0)),VERSION            
*                                                                               
         L     RF,DXASQLB                                                       
UTAL04   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UTAL06   GOTO1 ADECIOC                                                          
         JE    UTAL02                                                           
         J     NO                                                               
*                                                                               
UTAL08   L     R5,DXARECB          NOW ADD LINES FROM CHANGE RECORD             
         LA    R2,RECVHDR+L'RECVHDR                                             
         TM    TIMRSTA,X'80'       IS THIS RECORD DELETED?                      
         JO    YES                 YES - DO NOT RE-ADD                          
         GOTO1 AINITTAL                                                         
         MVI   AXTALACT,C'A'       SET ACTION TO ADD                            
         DROP  R3                                                               
*                                                                               
         GOTO1 VAXTALC,DMCB,DXAXREC,(R2),(1,0),(R6)                             
*                                                                               
UTAL10   GOTO1 VAXTALC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD             
         CLI   8(R1),FF                                                         
         JE    YES                 NO MORE RECORDS                              
         CLI   DXWRITE,C'Y'                                                     
         JNE   UTAL14              DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UTAL12              DO NOT CONVERT RECORD                        
*                                                                               
         LA    R0,=C'TAL'                                                       
         CLI   DMCB+8,X'EE'        TAX RECORDS IF EE PASSED BACK                
         JNE   *+8                                                              
         LA    R0,=C'TAT'                                                       
         GOTO1 VAXCNVX,DMCB,(C'U',(RF)),(1,DXASQLB),(0,(R0)),VERSION            
*                                                                               
         L     RF,DXASQLB                                                       
UTAL12   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
*                                                                               
UTAL14   GOTO1 ADECIOC                                                          
         JE    UTAL10                                                           
         J     NO                                                               
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER TAL RECORD AT R2                                             *         
***********************************************************************         
         SPACE 1                                                                
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
*                                                                               
         ICM   RE,1,1(RF)          BUMP TO NEXT                                 
         JZ    FTAL40                                                           
         AR    RF,RE                                                            
         J     FTAL30                                                           
*                                                                               
FTAL40   LTR   R0,R0               NO TIMEL                                     
         JZ    NO                                                               
         LTR   R1,R1               NO PIDEL                                     
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
         LTORG                                                                  
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE TIME SHEET ALLOCATION AND TAX RECORDS                    *         
***********************************************************************         
         SPACE 1                                                                
INITTAL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTALDL          R1=L'TAL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LOAD PAYROLL HISTORY RECORDS                                        *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PAYROLL HISTORY RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITPHI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXPHIDL          R1=L'PHI RECORD (LONGEST)                    
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
         SPACE 1                                                                
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
         GOTO1 (R4)                INITIALISE EXTRACT BUFFER                    
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* INITIALISE WORKCODE GROUP RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITTGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,AXTGRDL          R1=L'TGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* AREA USED FOR DETERMINING WHETHER CHANGES NEED TO BE EXTRACTED      *         
***********************************************************************         
         SPACE 3                                                                
COPYBUFF DS    8096C                                                            
COPYBUFL EQU   *-COPYBUFF                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER GLOBAL WORKING STORAGE                               *         
***********************************************************************         
         SPACE 1                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
PARM     DS    6F                                                               
DMWORK   DS    12D                                                              
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
ACCADDR  DS    XL4                                                              
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
BYTE     DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
COMPANY  DS    XL1                                                              
UNIT     DS    CL1                                                              
LEDGER   DS    CL1                                                              
VERSION  DS    XL1                                                              
*                                                                               
CLISTAT  DS    XL1                 ACTKSTAT FROM CLIENT                         
PROSTAT  DS    XL1                 ACTKSTAT FROM PRODUCT                        
*                                                                               
CURRCPY  DS    X                   CURRENT COMPANY FOR LEVELS                   
CURRUL   DS    XL2                 CURRENT UNIT/LEDGER FOR LEVELS               
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
TYPECODE DS    CL3                                                              
TYPENAME DS    CL3                                                              
TYPEDEEP DS    XL1                                                              
TYPEFLAG DS    XL1                                                              
TYPEALOD DS    A                                                                
TYPEAUPD DS    A                                                                
*                                                                               
WORK     DS    XL64                                                             
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    2048X                                                            
WORKL    EQU   *-WORKD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER TYPTAB TABLE                                         *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
ADDRESSD DSECT                                                                  
COMMON   DS    CL8                                                              
VDATAMGR DS    V                                                                
VDMOD000 DS    V                                                                
VDADDS   DS    V                                                                
VLOGIO   DS    V                                                                
VDATCON  DS    V                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
         SPACE 1                                                                
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
AFILTTAL DS    A                   TIMESHEET ALLOCATION/TAX                     
AFILTPHI DS    A                   COST RATE                                    
         DS    A                                                                
         DS    A                                                                
AFILTOPT DS    A                   OPTIONS                                      
AFILTP1R DS    A                   PERSON1R                                     
AFILTPOF DS    A                   PRODUCTION OFFICE                            
AFILTTGR DS    A                   WORKCODE GROUP                               
         SPACE 1                                                                
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
         SPACE 1                                                                
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
SPACES   DS    CL80                                                             
CMPEL    DS    CL(CPYLN2Q)                                                      
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECOVERY HEADER                                      *         
***********************************************************************         
         SPACE 1                                                                
RECDS    DSECT                                                                  
RECLN    DS    XL2                 TOTAL RECORD LENGTH                          
         DS    XL2                 N/D                                          
       ++INCLUDE DMRCVRHDR                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS REQUIRED FOR THIS PROGRAM                              *         
***********************************************************************         
         SPACE 1                                                                
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
         SPACE 1                                                                
* AXRECD                                                                        
         PRINT OFF                                                              
       ++INCLUDE AXRECD                                                         
         PRINT ON                                                               
         SPACE 1                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
         SPACE 1                                                                
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'087JSHTRACT  03/19/03'                                      
         END                                                                    
