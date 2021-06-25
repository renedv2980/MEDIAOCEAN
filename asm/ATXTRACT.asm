*          DATA SET ATXTRACT   AT LEVEL 003 AS OF 06/09/03                      
*PHASE TXTRACTA                                                                 
*INCLUDE TXROUTS                   XTRACT RECORD CREATION MODULE                
*INCLUDE TXCNVX                    CONVERSION ROUTINES FOR ALL ABOVE            
*INCLUDE DMUTLCT                                                                
*INCLUDE DMDMGRL                                                                
*INCLUDE GETOPT                                                                 
*INCLUDE CASHVAL                                                                
*INCLUDE EUREKA                                                                 
*INCLUDE PERVAL                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE DATCON                                                                 
*INCLUDE CUREDIT                                                                
*INCLUDE PRORATA                                                                
         SPACE 1                                                                
         TITLE 'ATXTRACT-EXTRACT PROD SYSTEM FILE DB2 DATA-@TRACKER'            
***********************************************************************         
* PRODUCTION AND ACCOUNTING DB2 SUB SYSTEM EXTRACT CONTROL MODULE     *         
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
***********************************************************************         
         EJECT                                                                  
ATXTRACT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*ATXTR**,R8,CLEAR=YES                                      
         USING WORKD,RC            RC=A(LOCAL/GLOBAL W/S)                       
         LA    RA,ADDRESS                                                       
         USING ADDRESSD,RA                                                      
*                                                                               
         ENTRY SSB                                                              
*                                                                               
         LA    RF,TYPTAB                                                        
         ST    RF,ATYPTAB                                                       
         USING DXBLOCKD,R7                                                      
         L     R7,0(R1)            R7=A(EXTRACT CONTROL DATA BLOCK)             
         USING SXDTABD,R6                                                       
         L     R6,DXSTPTR          R6=A(EXTRACT SYSTEM TABLE ENTRY)             
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
MAIN     DS    0H                                                               
         BAS   RE,GENINIT          GENERAL INITIALISATION                       
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
         CLI   CONSWTCH,C'Y'       IF RUNNING ON CONTROL FILE                   
         BNE   *+8                                                              
         BAS   RE,GETSAG           ALWAYS GET SEC AGY                           
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
*         INITIALIZE CONSWITCH                                        *         
***********************************************************************         
         SPACE 1                                                                
GENINIT  NTR1                                                                   
         MVI   CONSWTCH,C'N'       INITIALIZE TO NOT CONTROL                    
         LA    R1,CONTAB           SET CONTROL SWITCH                           
GENIN10  CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    GENIN20                                                          
         CLC   SXDTTYP,0(R1)                                                    
         BE    *+12                                                             
         LA    R1,L'CONTAB(R1)                                                  
         B     GENIN10                                                          
*                                                                               
         MVI   CONSWTCH,C'Y'       SET SWITCH TO SHOW RUNNING CONTROL           
GENIN20  DS    0H                                                               
         B     EXIT                                                             
*                                                                               
CONTAB   DS    0CL3                                                             
         DC    C'ALL'              ALL            RECORD                        
         DC    C'CON'              ALL CONTROL    RECORD                        
         DC    C'CAG'              CONTROL AGENCY RECORD                        
         DC    C'CPE'              CONTROL PERSON RECORD                        
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN ACCOUNT SYSTEM FILES                        *         
***********************************************************************         
         SPACE 1                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         CLI   CONSWTCH,C'Y'       ARE WE DOING CONTROL?                        
         BNE   *+12                                                             
         L     RF,=A(SSB)          SET DSPACE= FLAG                             
         MVI   SSODSPAC-SSOOFF(RF),C'T'                                         
*                                                                               
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         JZ    NO                                                               
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
*        MVI   4(RE),X'0A'         CONTROL FILE                                 
*                                                                               
*        GOTO1 VDATAMGR,DMCB,DMOPEN,CONTROL,CTFILES,IO                          
*        J     YES                                                              
*                                                                               
         XC    IOKEY,IOKEY         GET DTF ADDRESS                              
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,DMFAST,ACCDIR,IOKEY,(R2),DMWORK                    
         L     RF,12(R1)                                                        
         LA    RF,0(RF)                                                         
         ST    RF,DTFADDR          OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,ACCOUNT,ACCFILES,IO                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE ACCOUNT SYSTEM FILES                      *         
***********************************************************************         
         SPACE 1                                                                
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
         CLI   CONSWTCH,C'Y'       ARE WE DOING CONTROL?                        
         BNE   PROCC10                                                          
         MVI   4(RE),X'0A'         CONTROL FILE                                 
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,CONTROL,0,IO                                
         J     PROCC20                                                          
*                                                                               
PROCC10  GOTO1 VDATAMGR,DMCB,DMCLSE,ACCOUNT,0,IO                                
PROCC20  CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
ACCOUNT  DC    CL8'ACCOUNT'                                                     
ACCFILES DC    C'NACCDIR NACCMST NACCARC NACCRCV NCTFILE X'                     
*                                                                               
CTFILES  DC    C'NCTFILE NGENDIR NGENFIL NCTRCVR X'                             
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
***********************************************************************         
* INIT SECURITY AGENCY TABLE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CT5KEY,R2                                                        
GETSAG   NTR1                                                                   
         LA    R2,IOKEY            SET KEY TO READ FIRST AGENCY RECORD          
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,SXDTAGY     CURRENT AGENCY                              
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,IO                     
         CLC   0(CT5LEN-CT5REC,R2),IO   SAME KEY?                               
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,IO                                                            
         MVC   SVSECAGY,CT5KALPH                                                
         LA    R3,CT5DATA                                                       
GETSAG10 CLI   0(R3),0                                                          
         BE    GETSAGX                                                          
         CLI   0(R3),CTSEAELQ                                                   
         BE    GETSAG20                                                         
         SR    R1,R1                                                            
         IC    R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GETSAG10                                                         
*                                                                               
         USING CTSEAD,R3                                                        
GETSAG20 MVC   SVSECAGY,CTSEAAID                                                
*                                                                               
GETSAGX  B     EXIT                                                             
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
         MVC   PRIALPHA,SXDTAGY    SET PRIMARY ALPHA FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
         CLI   RFILTY,ACCMSTQ      TEST ACCMST FILE RECORD TYPE                 
         JNE   YES                 ELSE IGNORE RECORD                           
         BAS   RE,PROCKEY          PROCESS RECORD KEY VALUES                    
         JNE   YES                 EITHER IGNORE RECORD                         
         ICM   RF,15,TYPEAUPD      ELSE CALL UPDATE PROCESS ROUTINE             
         JZ    YES                 NO UPDATE ROUTINE SO SKIP                    
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
         BZ    PROCK10             NO                                           
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
PROCK10  CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
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
         DC    V(ATXCNVX)                                                       
         DC    V(ATXCPYC)          COMPANY                                      
         DC    V(ATXAGRC)          ACCOUNT GROUP                                
         DC    V(ATXCLIC)          CLIENT                                       
         DC    V(ATXPROC)          PRODUCT                                      
         DC    V(ATXJOBC)          JOB                                          
         DC    V(ATXMEDC)          MEDIA                                        
         DC    V(ATXMGRC)          MEDIA GROUP                                  
         DC    V(ATXTSKC)          TASK CODE                                    
         DC    V(ATXSTKC)          SUB-TASK CODE                                
         DC    V(ATXTGRC)          TASK CODE GROUP                              
         DC    V(ATXPOFC)          PRODUCTION OFFICE                            
         DC    V(ATXOGRC)          OFFICE GROUP                                 
         DC    V(ATXOFLC)          OFFICE LIST                                  
         DC    V(ATXOLEC)          OFFICE LIST ENTRY                            
         DC    V(ATXOFFC)          1R OFFICE                                    
         DC    V(ATXDEPC)          1R DEPARTMENT                                
         DC    V(ATXSBDC)          1R SUBDEPARTMENT                             
         DC    V(ATXPERC)          PERSON                                       
         DC    V(ATXPEAC)          PERSON ASSIGNMENT                            
         DC    V(ATXCSAC)          ACCOUNT                                      
         DC    V(ATXLEDC)          LEDGER                                       
         DC    V(ATXLESC)          LEDGER STUCTURE                              
*        DC    V(ATXOPTC)          OPTIONS                                      
         DC    V(ATXSTCC)          STANDARD COMMENT                             
         DC    V(ATXUSRC)          USERFIELD RECORD                             
         DC    V(ATXCAGC)          CONTROL AGENCY RECORD                        
         DC    V(ATXCPEC)          CONTROL PERSON RECORD                        
*                                                                               
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(ACCLOAD)          ACCOUNT LOAD                                 
         DC    A(CXLOAD)           CONTROL FILE LOAD                            
         DC    A(ACCUPDT)          ACCUPDT                                      
         DC    A(DECIOC)                                                        
         DC    A(CHKSEQIO)                                                      
         DC    A(RDLEDG)           RDLEDG                                       
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(READCHI)                                                       
         DC    A(RECCMP)                                                        
*                                                                               
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(LOADALL)          ALL RECORDS                                  
         DC    A(LOADCON)          ALL CONTROL RECORDS                          
         DC    A(LOADCPY)          COMPANY RECORDS                              
         DC    A(LOADAGR)          ACCOUNT GROUP RECORDS                        
         DC    A(LOADCLI)          CLIENT RECORDS                               
         DC    A(LOADPRO)          PRODUCT RECORDS                              
         DC    A(LOADJOB)          JOB RECORDS                                  
         DC    A(LOADMED)          MEDIA RECORDS                                
         DC    A(LOADMGR)          MEDIA GROUP RECORDS                          
         DC    A(LOADTSK)          TASK CODE ROUTINE                            
         DC    A(LOADSTK)          SUB-TASK CODE RECORDS                        
         DC    A(LOADTGR)          TASK CODE GROUP RECORDS                      
         DC    A(LOADPOF)          PRODUCTION OFFICE                            
         DC    A(LOADOGR)          OFFICE GROUP RECORDS                         
         DC    A(LOADOFL)          OFFICE LIST                                  
         DC    A(LOADOLE)          OFFICE LIST ENTRY                            
         DC    A(LOADOFF)          1R OFFICE RECORDS                            
         DC    A(LOADDEP)          1R DEPARTMENT RECORDS                        
         DC    A(LOADSBD)          1R SUBDEPARTMENT RECORDS                     
         DC    A(LOADPER)          PERSON                                       
         DC    A(LOADPEA)          PERSON ASSIGNMENT                            
         DC    A(LOADCSA)          COSTING ACCOUNT                              
         DC    A(LOADLED)          LEDGER                                       
         DC    A(LOADLES)          LEDGER STRUCTURE                             
*        DC    A(LOADOPT)          OPTIONS                                      
         DC    A(LOADSTC)          STANDARD COMMENT                             
         DC    A(LOADUSR)          USERFIELD RECORD                             
         DC    A(LOADCAG)          CONTROL AGENCY RECORD                        
         DC    A(LOADCPE)          CONTROL PERSON RECORD                        
*                                                                               
         DC    CL8'UPDTING'                                                     
         DC    A(UPDTALL)          ALL RECORDS                                  
         DC    A(0)                ALL CONTROL RECORDS                          
         DC    A(UPDTCPY)          COMPANY RECORDS                              
         DC    A(UPDTAGR)          ACCOUNT GROUP RECORDS                        
         DC    A(UPDTCLI)          CLIENT RECORDS                               
         DC    A(UPDTPRO)          PRODUCT RECORDS                              
         DC    A(UPDTJOB)          JOB RECORDS                                  
         DC    A(UPDTMED)          MEDIA RECORDS                                
         DC    A(UPDTMGR)          MEDIA GROUP RECORDS                          
         DC    A(UPDTTSK)          TASK CODE ROUTINE                            
         DC    A(UPDTSTK)          SUB-TASK CODE RECORDS                        
         DC    A(UPDTTGR)          TASK CODE GROUP RECORDS                      
         DC    A(UPDTPOF)          PRODUCTION OFFICE                            
         DC    A(UPDTOGR)          OFFICE GROUP RECORDS                         
         DC    A(UPDTOFL)          OFFICE LIST                                  
         DC    A(UPDTOLE)          OFFICE LIST ENTRY                            
         DC    A(UPDTOFF)          1R OFFICE RECORDS                            
         DC    A(UPDTDEP)          1R DEPARTMENT RECORDS                        
         DC    A(UPDTSBD)          1R SUBDEPARTMENT RECORDS                     
         DC    A(UPDTPER)          PERSON                                       
         DC    A(UPDTPEA)          PERSON ASSIGNMENT                            
         DC    A(UPDTCSA)          COSTING ACCOUNT                              
         DC    A(UPDTLED)          LEDGER                                       
         DC    A(UPDTLES)          LEDGER STRUCTURE                             
*        DC    A(UPDTOPT)          OPTIONS                                      
         DC    A(UPDTSTC)          STANDARD COMMENT                             
         DC    A(UPDTUSR)          USERFIELD RECORD                             
         DC    A(0)                CONTROL AGENCY RECORD                        
         DC    A(0)                CONTROL PERSON RECORD                        
*                                                                               
         DC    CL8'FILTERS'                                                     
         DC    A(FILTCPY)          COMPANY                                      
         DC    A(FILTAGR)          ACCOUNT GROUP                                
         DC    A(FILTCLI)          CLIENT                                       
         DC    A(FILTPRO)          PRODUCT                                      
         DC    A(FILTJOB)          JOB                                          
         DC    A(FILTMED)          MEDIA                                        
         DC    A(FILTMGR)          MEDIA GROUP                                  
         DC    A(FILTTSK)          TASK                                         
         DC    A(FILTSTK)          SUB TASK                                     
         DC    A(FILTTGR)          TASK GROUP                                   
         DC    A(FILTPOF)          PRODUCTION OFFICE                            
         DC    A(FILTOGR)          OFFICE GROUP                                 
         DC    A(FILTOFL)          OFFICE LIST                                  
         DC    A(FILTOLE)          OFFICE LIST ENTRY                            
         DC    A(FILTOFF)          1R OFFICE                                    
         DC    A(FILTDEP)          1R DEPARTMENT                                
         DC    A(FILTSBD)          1R SUB-DEPARTMENT                            
         DC    A(FILTPER)          PERSON                                       
         DC    A(FILTPEA)          PERSON ASSIGNMENT                            
         DC    A(FILTCSA)          COSTING ACCOUNT                              
         DC    A(FILTLED)          LEDGER                                       
         DC    A(FILTLES)          LEDGER STRUCTURE                             
*        DC    A(FILTOPT)          OPTIONS                                      
         DC    A(FILTSTC)          STANDARD COMMENT                             
         DC    A(FILTUSR)          USERFIELD RECORD                             
         DC    A(FILTCAG)          CONTROL AGENCY RECORD                        
         DC    A(FILTCPE)          CONTROL PERSON RECORD                        
*                                                                               
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          ALL                                          
         DC    A(INITCPY)          COMPANY                                      
         DC    A(INITAGR)          ACCOUNT GROUP                                
         DC    A(INITCLI)          CLIENT                                       
         DC    A(INITPRO)          PRODUCT                                      
         DC    A(INITJOB)          JOB                                          
         DC    A(INITMED)          MEDIA                                        
         DC    A(INITMGR)          MEDIA GROUP                                  
         DC    A(INITTSK)          TASK                                         
         DC    A(INITSTK)          SUB-TASK                                     
         DC    A(INITTGR)          TASK GROUP                                   
         DC    A(INITPOF)          PRODUCTION OFFICE                            
         DC    A(INITOGR)          OFFICE GROUP                                 
         DC    A(INITOFL)          OFFICE LIST                                  
         DC    A(INITOLE)          OFFICE LIST ENTRY                            
         DC    A(INITOFF)          1R OFFICE                                    
         DC    A(INITDEP)          1R DEPARTMENT                                
         DC    A(INITSBD)          1R SUB-DEPARTMENT                            
         DC    A(INITPER)          PERSON                                       
         DC    A(INITPEA)          PERSON ASSIGNMENT                            
         DC    A(INITCSA)          COSTING ACCOUNTS                             
         DC    A(INITLED)          LEDGER                                       
         DC    A(INITLES)          LEDGER STRUCTURE                             
*        DC    A(INITOPT)          OPTIONS                                      
         DC    A(INITSTC)          STANDARD COMMENT                             
         DC    A(INITUSR)          USERFIELD RECORD                             
         DC    A(INITCAG)          CONTROL AGENCY RECORD                        
         DC    A(INITCPE)          CONTROL PERSON RECORD                        
*                                                                               
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
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    80C' '                                                           
*                                                                               
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
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
         DC    CL3'CON',AL1(00,00,00,00,00),AL4(LOADCON,0)                      
         DC    CL3'CPY',AL1(00,00,00,00,00),AL4(LOADCPY,UPDTCPY)                
         DC    CL3'CLI',AL1(00,00,00,00,00),AL4(LOADCLI,UPDTCLI)                
         DC    CL3'PRO',AL1(00,00,00,00,00),AL4(LOADPRO,UPDTPRO)                
         DC    CL3'JOB',AL1(00,00,00,00,00),AL4(LOADJOB,UPDTJOB)                
         DC    CL3'MED',AL1(00,00,00,00,00),AL4(LOADMED,UPDTMED)                
         DC    CL3'MGR',AL1(00,00,00,00,00),AL4(LOADMGR,UPDTMGR)                
         DC    CL3'TSK',AL1(00,00,00,00,00),AL4(LOADTSK,UPDTTSK)                
         DC    CL3'STK',AL1(00,00,00,00,00),AL4(LOADSTK,UPDTSTK)                
         DC    CL3'TGR',AL1(00,00,00,00,00),AL4(LOADTGR,UPDTTGR)                
         DC    CL3'POF',AL1(00,00,00,00,00),AL4(LOADPOF,UPDTPOF)                
         DC    CL3'OGR',AL1(00,00,00,00,00),AL4(LOADOGR,UPDTOGR)                
         DC    CL3'OFL',AL1(00,00,00,00,00),AL4(LOADOFL,UPDTOFL)                
         DC    CL3'OLE',AL1(00,00,00,00,00),AL4(LOADOLE,UPDTOLE)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'DEP',AL1(00,00,00,00,00),AL4(LOADDEP,UPDTDEP)                
         DC    CL3'SBD',AL1(03,00,00,00,00),AL4(LOADSBD,UPDTSBD)                
         DC    CL3'PER',AL1(00,00,00,00,00),AL4(LOADPER,UPDTPER)                
         DC    CL3'PEA',AL1(00,00,00,00,00),AL4(LOADPEA,UPDTPEA)                
         DC    CL3'CSA',AL1(00,00,00,00,00),AL4(LOADCSA,UPDTCSA)                
         DC    CL3'LED',AL1(00,00,00,00,00),AL4(LOADLED,UPDTLED)                
         DC    CL3'LES',AL1(00,00,00,00,00),AL4(LOADLES,UPDTLES)                
*        DC    CL3'OPT',AL1(00,00,00,00,00),AL4(LOADOPT,UPDTOPT)                
         DC    CL3'STC',AL1(00,00,00,00,00),AL4(LOADSTC,UPDTSTC)                
         DC    CL3'USR',AL1(00,00,00,00,00),AL4(LOADUSR,UPDTUSR)                
         DC    CL3'AGR',AL1(00,00,00,00,00),AL4(LOADAGR,UPDTAGR)                
         DC    CL3'CAG',AL1(00,00,00,00,00),AL4(LOADCAG,0)                      
         DC    CL3'CPE',AL1(00,00,00,00,00),AL4(LOADCPE,0)                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                           *         
***********************************************************************         
         SPACE 1                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
         PRINT OFF                                                              
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
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
GTYP10   CLI   0(RF),FF            END OF TABLE                                 
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   TYPECODE,TYPNAME    COMPARE NAME                                 
         BE    GTYP20                                                           
         LA    RF,TYPTABLQ(RF)     GET NEXT ENTRY                               
         B     GTYP10                                                           
*                                                                               
GTYP20   MVC   TYPENAME,TYPNAME    MATCH FOUND - GET TABLE INFORMATION          
         MVC   TYPEDEEP,TYPLDEEP                                                
         MVC   TYPEFLAG,TYPFLAG                                                 
         MVC   TYPEALOD,TYPLOAD                                                 
         MVC   TYPEAUPD,TYPUPDT                                                 
         J     YES                                                              
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DC    C'IO COUNT EXCEEDED - TYPECODE = '                               
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
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),ACCMST,ACCADDR,(R2),DMWORK          
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
         DC    C'DMGR GETREC ERROR - D/A = '                                    
GETDA    DC    CL8' '                                                           
         DC    C','                                                             
         DC    C' RC = '                                                        
GETRC    DC    CL2' '                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO PERFORM A READCHI                                      *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
         SPACE 1                                                                
READCHI  NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,(R2)                           
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
         J     NO                                                               
*                                                                               
         EJECT                                                                  
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
         SR    R0,R0                                                            
         WTO   TEXT=((RDHHL,C),(RDH1L,D),(0,E)),MCSFLAG=HRDCPY                  
         J     NO                                                               
*                                                                               
RDHHL    DC    AL2(40)                                                          
         DC    CL40'DMGR READHI ERROR KEY HEXOUT FOLLOWS'                       
*                                                                               
RDH1L    DC    AL2(90)                                                          
RDH1M    DC    CL90' '                                                          
         ORG   RDH1M                                                            
         DC    C'KEY='                                                          
RDHKEY   DC    CL84' '                                                          
         ORG   RDH1M+L'RDH1M                                                    
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITALL  NTR1  BASE=*,LABEL=*                                                   
         L     R0,DXAXREC          R0=A(EXTRACT RECORD)                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
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
         JE    IALL10              YES                                          
*                                                                               
         L     R5,DXARECB          HERE IF UPDATE MODE                          
         USING RECDS,R5                                                         
         GOTO1 VDATCON,DMCB,(3,RDATE),(20,DXHDRCDT)                             
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
IALL10   MVC   DXHDRCDT,DXDATEN    SET TODAYS DATE                              
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
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ALL RECORD DATA FOR BOTH PRODUCTION AND ACCOUNTING            *          
***********************************************************************         
         SPACE 1                                                                
LOADALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADTAB                                                       
*                                                                               
LOAD10   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOAD20                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOAD20   LA    R3,L'LOADTAB(R3)                                                 
         J     LOAD10                                                           
*                                                                               
LOADTAB  DS    0XL8                                                             
         DC    CL3'CPY',AL1(0),AL4(LOADCPY) COMPANY RECORDS                     
         DC    CL3'CLI',AL1(0),AL4(LOADCLI) CLIENT RECORDS                      
         DC    CL3'PRO',AL1(0),AL4(LOADPRO) PRODUCT RECORDS                     
         DC    CL3'JOB',AL1(0),AL4(LOADJOB) JOB RECORDS                         
         DC    CL3'MED',AL1(0),AL4(LOADMED) MEDIA RECORDS                       
         DC    CL3'MGR',AL1(0),AL4(LOADMGR) MEDIA GROUP RECORDS                 
         DC    CL3'TSK',AL1(0),AL4(LOADTSK) TASK CODE                           
         DC    CL3'STK',AL1(0),AL4(LOADSTK) SUB-TASK CODES                      
         DC    CL3'TGR',AL1(0),AL4(LOADTGR) TASK CODE GROUP RECORDS             
         DC    CL3'POF',AL1(0),AL4(LOADPOF) PRODUCTION OFFICE RECORDS           
         DC    CL3'OGR',AL1(0),AL4(LOADOGR) PROD OFFICE GROUP RECORDS           
         DC    CL3'AGR',AL1(0),AL4(LOADAGR) ACCOUNT GROUP RECORDS               
         DC    CL3'OFL',AL1(0),AL4(LOADOFL) PRODUCTION OFFICE LIST              
         DC    CL3'OLE',AL1(0),AL4(LOADOLE) PROD OFFICE LIST ENTRY              
         DC    CL3'OFF',AL1(0),AL4(LOADOFF) OFFICE RECORDS                      
         DC    CL3'DEP',AL1(0),AL4(LOADDEP) DEPARTMENT                          
         DC    CL3'SBD',AL1(0),AL4(LOADSBD) SUB-DEPARTMENT                      
         DC    CL3'PER',AL1(0),AL4(LOADPER) PERSON                              
         DC    CL3'PEA',AL1(0),AL4(LOADPEA) PERSON ASSIGNMENT                   
         DC    CL3'CSA',AL1(0),AL4(LOADCSA) COSTING ACCOUNT                     
         DC    CL3'LED',AL1(0),AL4(LOADLED) LEDGER                              
         DC    CL3'LES',AL1(0),AL4(LOADLES) LEDGER STRUCTURE                    
*        DC    CL3'OPT',AL1(0),AL4(LOADOPT) OPTION                              
         DC    CL3'STC',AL1(0),AL4(LOADSTC) STANDARD COMMENT                    
         DC    CL3'USR',AL1(0),AL4(LOADUSR) USER FIELD                          
         DC    CL3'CAG',AL1(0),AL4(LOADCAG) CONTROL AGENCY RECORDS              
         DC    CL3'CPE',AL1(0),AL4(LOADCPE) CONTROL PERSON RECORDS              
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
UPDT10   CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    UPDT20                                                           
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
UPDT20   LA    R3,L'UPDTTAB(R3)                                                 
         J     UPDT10                                                           
*                                                                               
UPDTTAB  DS    0XL8                                                             
         DC    CL3'CPY',AL1(0),AL4(UPDTCPY) COMPANY RECORDS                     
         DC    CL3'AGR',AL1(0),AL4(UPDTAGR) ACCOUNT GROUP RECORDS               
         DC    CL3'CLI',AL1(0),AL4(UPDTCLI) CLIENT RECORDS                      
         DC    CL3'PRO',AL1(0),AL4(UPDTPRO) PRODUCT RECORDS                     
         DC    CL3'JOB',AL1(0),AL4(UPDTJOB) JOB RECORDS                         
         DC    CL3'MED',AL1(0),AL4(UPDTMED) MEDIA RECORDS                       
         DC    CL3'MGR',AL1(0),AL4(UPDTMGR) MEDIA GROUP RECORDS                 
         DC    CL3'TSK',AL1(0),AL4(UPDTTSK) TASK CODE                           
         DC    CL3'STK',AL1(0),AL4(UPDTSTK) SUB-TASK CODES                      
         DC    CL3'TGR',AL1(0),AL4(UPDTTGR) TASK CODE GROUP RECORDS             
         DC    CL3'POF',AL1(0),AL4(UPDTPOF) PRODUCTION OFFICE RECORDS           
         DC    CL3'OGR',AL1(0),AL4(UPDTOGR) PROD OFFICE GROUP RECORDS           
         DC    CL3'OFL',AL1(0),AL4(UPDTOFL) PRODUCTION OFFICE LIST              
         DC    CL3'OLE',AL1(0),AL4(UPDTOLE) PROD OFFICE LIST ENTRY              
         DC    CL3'OFF',AL1(0),AL4(UPDTOFF) OFFICE RECORDS                      
         DC    CL3'DEP',AL1(0),AL4(UPDTDEP) DEPARTMENT                          
         DC    CL3'SBD',AL1(0),AL4(UPDTSBD) SUB-DEPARTMENT                      
         DC    CL3'PER',AL1(0),AL4(UPDTPER) PERSON                              
         DC    CL3'PEA',AL1(0),AL4(UPDTPEA) PERSON ASSIGNMENT                   
         DC    CL3'CSA',AL1(0),AL4(UPDTCSA) COSTING ACCOUNT                     
         DC    CL3'LED',AL1(0),AL4(UPDTLED) LEDGER                              
         DC    CL3'LES',AL1(0),AL4(UPDTLES) LEDGER STRUCTURE                    
*        DC    CL3'OPT',AL1(0),AL4(UPDTOPT) OPTION                              
         DC    CL3'STC',AL1(0),AL4(UPDTSTC) STANDARD COMMENT                    
         DC    CL3'USR',AL1(0),AL4(UPDTUSR) USER FIELD                          
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ALL CONTROL RECORDS DATA FOR BOTH PRODUCTION AND ACCOUNTING    *         
***********************************************************************         
         SPACE 1                                                                
LOADCON  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,LOADCTB                                                       
*                                                                               
LOADC10  CLI   0(R3),0                                                          
         JE    YES                                                              
         CLC   VERSION,3(R3)                                                    
         BL    LOADC20                                                          
         MVC   TYPECODE,0(R3)                                                   
         GOTO1 AGETTYP                                                          
         JNE   NO                                                               
         ICM   RF,15,4(R3)                                                      
         BASR  RE,RF                                                            
         JNE   NO                                                               
*                                                                               
LOADC20  LA    R3,L'LOADCTB(R3)                                                 
         J     LOADC10                                                          
*                                                                               
LOADCTB  DS    0XL8                                                             
         DC    CL3'CAG',AL1(0),AL4(LOADCAG) CONTROL AGENCY RECORDS              
         DC    CL3'CPE',AL1(0),AL4(LOADCPE) CONTROL PERSON RECORDS              
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
LCPY10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKCPY,COMPANY     ALL DONE IF COMPANY CHANGES                  
         JNE   YES                                                              
                                                                                
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXCPYC,AINITCPY,AFILTCPY,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCPY10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE COMPANY RECORD DATA                                          *         
***********************************************************************         
         SPACE 1                                                                
UPDTCPY  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCPY                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCPY                                                         
         GOTO1 AACCUPDT,DMCB,VATXCPYC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER COMPANY RECORD AT R2                                         *         
***********************************************************************         
         SPACE 1                                                                
FILTCPY  NTR1  BASE=*,LABEL=*                                                   
         USING ACTRECD,R2                                                       
         CLC   ACTKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   ACTKULA(L'ACTKULA),SPACES                                        
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE COMPANY RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
INITCPY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXCPYL          R1=L'CPY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ACCOUNT GROUP RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
LOADAGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGR RECORD             
         USING AGRRECD,R2                                                       
         XC    AGRKEY,AGRKEY                                                    
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   AGRKEY(AGRKGTYP-AGRKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,AGRKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXAGRC,AINITAGR,AFILTAGR,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGR10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ACCOUNT GROUP RECORD DATA                                    *         
***********************************************************************         
         SPACE 1                                                                
UPDTAGR  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING AGRRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTAGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGR                                                         
         GOTO1 AACCUPDT,DMCB,VATXAGRC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER ACCOUNT GROUP RECORD AT R2                                   *         
***********************************************************************         
         SPACE 1                                                                
FILTAGR  NTR1  BASE=*,LABEL=*                                                   
         USING AGRRECD,R2                                                       
         CLI   AGRKTYP,AGRKTYPQ    X'12'                                        
         JNE   NO                                                               
         CLC   AGRKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ACCOUNT GROUP RECORD                                     *         
***********************************************************************         
         SPACE 1                                                                
INITAGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXAGRL          R1=L'OGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT RECORDS                                                 *         
***********************************************************************         
LOADCLI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG             GET LEDGER STRUCTURE                         
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CLI RE                 
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
         GOTO1 AACCLOAD,DMCB,VATXCLIC,AINITCLI,AFILTCLI,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLI02                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CLIENT RECORDS DATA                                          *         
***********************************************************************         
UPDTCLI  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         L     R5,DXARECB                                                       
         USING RECDS,R5                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         USING ACTRECD,R2                                                       
*                                                                               
         GOTO1 AFILTCLI                                                         
         JNE   YES                                                              
         GOTO1 AINITCLI                                                         
         GOTO1 AACCUPDT,DMCB,VATXCLIC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CLIENT RECORD AT R2                                          *         
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
         SR    RE,RE                                                            
         IC    RE,L1               LENGTH OF CLIENT CODE                        
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(DATA AFTER CLIENT CODE)                 
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),SPACES      THIS MUST BE EMPTY                           
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CLIENT RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITCLI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXCLILN         R1=L'CLI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT RECORDS                                                *         
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
         GOTO1 AACCLOAD,DMCB,VATXPROC,AINITPRO,AFILTPRO,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRO02                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT RECORDS DATA                                         *         
***********************************************************************         
         SPACE 1                                                                
UPDTPRO  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPRO                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPRO                                                         
         GOTO1 AACCUPDT,DMCB,VATXPROC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCT RECORD AT R2                                         *         
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
         SR    RE,RE                                                            
         IC    RE,L1               L' CLIENT CODE                               
         LA    RE,ACTKACT(RE)      DISPLACEMENT TO PRO CODE                     
         SR    RF,RF                                                            
         IC    RF,L2               L'PRODUCT CODE                               
         BCTR  RF,0                LEN-1 OF THE PRO CODE                        
         EX    RF,FPROCLC                                                       
         JNE   FPRO10                                                           
         MVC   CLISTAT,ACTKSTAT                                                 
         J     NO                                                               
*                                                                               
FPRO10   LA    RE,1(RE,RF)         A(WHAT COMES AFTER CLI/PRO)                  
         IC    RF,L3               LENGTH OF JOB CODE                           
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FPROCLC          MUST BE EMPTY                                
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
FPROCLC  CLC   0(0,RE),SPACES      MUST BE A PRO CODE PRESENT                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRODUCT RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
INITPRO  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXPRDL          R1=L'PRO RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
LJOB10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         GOTO1 AFILTJOB            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LJOB40                                                           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
         GOTO1 AINITJOB            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VATXJOBC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LJOB20   GOTO1 VATXJOBC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS                     
         CLI   DMCB+8,FF                                                        
         JE    LJOB40              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LJOB40              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LJOB30              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
LJOB30   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LJOB20                                                           
         J     YES                                                              
*                                                                               
LJOB40   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LJOB50                                                           
         GOTO1 AREADHI                                                          
         JE    LJOB50                                                           
         DC    H'0'                                                             
*                                                                               
LJOB50   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LJOB10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE JOB RECORD DATA                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDTJOB  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         MVI   COPYFLAG,X'00'      SET FLAG TO DIFFER COPY FROM CHANGE          
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTJOB                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITJOB                                                         
         L     R3,DXAXREC                                                       
         USING ATXRECD,R3                                                       
*                                                                               
         GOTO1 VATXJOBC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS  BUILD              
*                                                                               
UJOB10   GOTO1 VATXJOBC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS  GET                
         CLI   8(R1),FF                                                         
         JE    UJOBX               NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   UJOBX               DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UJOB20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UJOB20   DS    0H                                                               
         CLI   ATXREACT-ATXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UJOB50                                                           
         TM    TRNRSTA-TRNRECD(R2),TRNSDELT    TRANSACTION DELETED?             
         BO    UJOB30                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   (ATXRETYP-ATXRECD)(5,RF),=C'05103'                               
         JNE   UJOB40                                                           
UJOB30   MVI   ATXREACT-ATXRECD(RF),C'D'                                        
         J     UJOB50                                                           
*                                                                               
UJOB40   DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JNE   UJOB10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   ATXREACT-ATXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UJOB50   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         TM    TRNRSTA-TRNRECD(R2),TRNSDELT    TRANSACTION DELETED?             
         BO    UJOBX                                                            
         J     UJOB40                                                           
*                                                                               
UJOBX    DS    0H                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
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
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    RF,L'ACTKACT                                                     
         SR    RE,RE                                                            
         IC    RE,L1               LENGTH OF CLIENT CODE                        
                                                                                
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(DATA AFTER CLIENT CODE)                 
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FJOBCLC                                                       
         JNE   FJOB10                                                           
         MVC   CLISTAT,ACTKSTAT                                                 
         J     NO                                                               
*                                                                               
FJOB10   SR    RE,RE                                                            
         IC    RE,L1                                                            
         SR    RF,RF                                                            
         IC    RF,L2                                                            
         AR    RE,RF                                                            
         LA    RF,L'ACTKACT                                                     
         SR    RF,RE                                                            
         LA    RE,ACTKACT(RE)      RE=A(DATA AFTER PRODUCT CODE)                
         BCTR  RF,0                RF=LENGTH-1 OF THIS DATA                     
         EX    RF,FJOBCLC          THIS MUST BE EMPTY FOR A PRODUCT             
         JNE   FJOB20                                                           
         MVC   PROSTAT,ACTKSTAT                                                 
         J     NO                                                               
*                                                                               
FJOB20   LA    RE,L'ACTKACT                                                     
         SR    RF,RF                                                            
         IC    RF,L3                                                            
         SR    RE,RF                                                            
         LA    RE,ACTKACT(RE)      DISPLACEMENT TO JOB CODE                     
         BCTR  RF,0                LEN-1 OF THE JOB CODE                        
         EX    RF,FJOBCLC          MUST BE A JOB CODE PRESENT                   
         JE    NO                                                               
         J     YES                                                              
*                                                                               
FJOBCLC  CLC   0(0,RE),SPACES                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE JOB RECORD                                               *         
***********************************************************************         
         SPACE 1                                                                
INITJOB  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXJOBL          R1=L'JOB RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
*                                                                               
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMED02   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   PMDKEY(PMDKMED-PMDKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,PMDKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXMEDC,AINITMED,AFILTMED,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMED02                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE MEDIA RECORD DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
UPDTMED  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING PMDRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTMED                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITMED                                                         
         GOTO1 AACCUPDT,DMCB,VATXMEDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE MEDIA RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
INITMED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXMEDL          R1=L'MED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         GOTO1 AACCLOAD,DMCB,VATXMGRC,AINITMGR,AFILTMGR,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMGR02                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE MEDIA GROUP RECORD DATA                                      *         
***********************************************************************         
         SPACE 1                                                                
UPDTMGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING MGRRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTMGR            FILTER RECORD                                
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITMGR            INITIALIZE EXTRACT BUFFER                    
         GOTO1 AACCUPDT,DMCB,VATXMGRC,TYPECODE                                  
         JE    YES                                                              
         J     NO                                                               
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE MEDIA GROUP RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
INITMGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXMDGL          R1=L'MGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD TASK CODE RECORD (WORKCODE)                                    *         
***********************************************************************         
         SPACE 1                                                                
LOADTSK  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY                                                         
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
LTSK10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   WCOKEY(WCOKWRK-WCOKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COM/UNIT/LEDGER CHANGES          
*                                                                               
         MVC   ACCADDR,WCOKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXTSKC,AINITTSK,AFILTTSK,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTSK10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE TASK CODE RECORDS DATA                                       *         
***********************************************************************         
         SPACE 1                                                                
UPDTTSK  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING MGRRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTTSK            FILTER RECORD                                
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITTSK            INITIALIZE EXTRACT BUFFER                    
         GOTO1 AACCUPDT,DMCB,VATXTSKC,TYPECODE                                  
         JE    YES                                                              
         J     NO                                                               
         DROP  R2,R5                                                            
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER TASK CODE RECORD AT R2                                       *         
***********************************************************************         
         SPACE 1                                                                
FILTTSK  NTR1  BASE=*,LABEL=*                                                   
         USING WCORECD,R2                                                       
         CLC   WCOKCPY,COMPANY                                                  
         JNE   NO                                                               
         CLI   WCOKTYP,WCOKTYPQ                                                 
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE TASK CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
INITTSK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXWKCL          R1=L'TASK RECORD (LONGEST)                   
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD SUB-TASK CODE RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
LOADSTK  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY                                                         
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
LOAST10  TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LOASTEND                                                         
         CLC   WCOKEY(WCOKWRK-WCOKEY),IOKEY                                     
         JNE   LOASTEND            ALL DONE IF COM/UNIT/LEDGER CHANGES          
*                                                                               
         MVC   ACCADDR,WCOKDA                                                   
         GOTO1 AGETIT              GET RECORD NOW INSTEAD OF IN ACLOAD          
         JNE   NO                                                               
         OI    SUBIOIND,SUBIOYES   SET SUB IO RECORDS REQUIRED                  
         XC    ASUBLAST,ASUBLAST   CLEAR ADDRESS OF LAST SUB REC                
*                                                                               
         GOTO1 AACCLOAD,DMCB,VATXSTKC,AINITSTK,AFILTSTK,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOAST10                                                          
*                                                                               
LOASTEND NI    SUBIOIND,X'FF'-SUBIOYES                                          
         XC    ASUBLAST,ASUBLAST                                                
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE SUB-TASK CODE DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTSTK  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING MGRRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
         OI    SUBIOIND,SUBIOYES   SET SUB IO RECORDS REQUIRED                  
         XC    ASUBLAST,ASUBLAST   CLEAR ADDRESS OF LAST SUB REC                
*                                                                               
         GOTO1 AFILTSTK            FILTER RECORD                                
         JNE   UPDTST10                                                         
*                                                                               
         GOTO1 AINITSTK            INITIALIZE EXTRACT BUFFER                    
         GOTO1 AACCUPDT,DMCB,VATXSTKC,TYPECODE                                  
*                                                                               
UPDTST10 NI    SUBIOIND,X'FF'-SUBIOYES                                          
         XC    ASUBLAST,ASUBLAST                                                
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER SUB-TASK CODE RECORD AT R2                                   *         
***********************************************************************         
         SPACE 1                                                                
FILTSTK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    ASUBLAST,ASUBLAST   STARTED PROCESSING SUBS?                     
         JZ    *+12                                                             
         L     R3,ASUBLAST                                                      
         J     FSTK20                                                           
*                                                                               
         USING WCORECD,R2                                                       
         CLC   WCOKCPY,COMPANY                                                  
         JNE   NO                                                               
         CLC   WCOKUNT,UNIT                                                     
         JNE   NO                                                               
         MVC   WCOKLDG,LEDGER                                                   
         JNE   NO                                                               
         CLI   WCOKTYP,WCOKTYPQ                                                 
         JNE   NO                                                               
*                                                                               
         LA    R3,WCORFST                                                       
         USING FFTELD,R3                                                        
                                                                                
FSTK10   CLI   FFTEL,0                                                          
         JE    FSTK40              NO SUBTASK CODES FOR THIS TASK CODE          
         CLI   FFTEL,FFTELQ                                                     
         JNE   *+12                                                             
         CLI   FFTTYPE,FFTTFREE                                                 
         JE    FSTK30                                                           
FSTK20   SR    RF,RF               BUMP TO NEXT ELEMENT                         
         ICM   RF,1,FFTLN                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,RF                                                            
         J     FSTK10                                                           
*                                                                               
FSTK30   ST    R3,ASUBLAST                                                      
         J     YES                                                              
*                                                                               
FSTK40   XC    ASUBLAST,ASUBLAST                                                
         J     NO                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SUB-TASK CODE                                            *         
***********************************************************************         
         SPACE 1                                                                
INITSTK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXSWCL           R1=L'TASK RECORD (LONGEST)                  
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD TASK CODE GROUP RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
LOADTGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST TGR RECORD             
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
LTGR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   WGRKEY(WGRKCODE-WGRKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,WGRKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXTGRC,AINITTGR,AFILTTGR,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTGR10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE TASK CODE GROUP RECORD DATA                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDTTGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING WGRRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTTGR            FILTER RECORD                                
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITTGR            INITIALIZE EXTRACT BUFFER                    
         GOTO1 AACCUPDT,DMCB,VATXTGRC,TYPECODE                                  
         JE    YES                                                              
         J     NO                                                               
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER TASK CODE GROUP RECORD AT R2                                 *         
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
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE TASK CODE GROUP RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITTGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXWCGL          R1=L'TGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCTION OFFICE RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
LOADPOF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         GOTO1 ARDLEDG                                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST OFF RECORD             
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
LPOF10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   OGRKEY(OGRKCODE-OGRKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGESR          
*                                                                               
         MVC   ACCADDR,OGRKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXPOFC,AINITPOF,AFILTPOF,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPOF10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCTION OFFICE RECORD DATA                                *         
***********************************************************************         
         SPACE 1                                                                
UPDTPOF  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING OGRRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPOF                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPOF                                                         
         GOTO1 AACCUPDT,DMCB,VATXPOFC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCTION OFFICE RECORD AT R2                               *         
***********************************************************************         
FILTPOF  NTR1  BASE=*,LABEL=*                                                   
         USING OGRRECD,R2                                                       
         CLC   OGRKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   OGRKTYP,OGRKTYPQ    X'2C'                                        
         JNE   NO                                                               
         CLI   OGRKSUB,OGRKOFFQ    X'04'                                        
         JNE   NO                                                               
         CLC   OGRKUNT,UNIT        UNIT OK?                                     
         JNE   NO                                                               
         CLC   OGRKLDG,LEDGER      LEDGER OK?                                   
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
         GOTO1 ARDLEDG             PULL OUT LEDGER STRUCTURE                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,L1               L' OFFICE CODE                               
         BCTR  RF,0                                                             
         EX    RF,FPOFCLC                                                       
         JE    NO                                                               
         LA    RF,1(RF)                                                         
         LA    RE,L'OGRKOFC                                                     
         SR    RE,RF                                                            
         LA    RF,OGRKOFC(RF)      DISP TO FIRST NON-OFFICE                     
         BCTR  RE,0                                                             
         EX    RE,FPOFCLC1                                                      
         JNE   NO                                                               
         J     YES                                                              
*                                                                               
FPOFCLC  CLC   OGRKOFC(0),SPACES   OFFICE CODE MUST BE NON-ZERO                 
FPOFCLC1 CLC   0(0,RF),SPACES      MUST HAVE ONLY OFFICE CODE                   
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRODUCTION OFFICE RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITPOF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXPOFL          R1=L'OFF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCTION OFFICE GROUP RECORD                                 *         
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
         GOTO1 AACCLOAD,DMCB,VATXOGRC,AINITOGR,AFILTOGR,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOGR02                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCTION OFFICE GROUP RECORD DATA                          *         
***********************************************************************         
         SPACE 1                                                                
UPDTOGR  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING OGRRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTOGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOGR                                                         
         GOTO1 AACCUPDT,DMCB,VATXOGRC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         LTORG                                                                  
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCTION OFFICE GROUP RECORD AT R2                         *         
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
* INITIALIZE PRODUCTION OFFICE GROUP RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
INITOGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXOGRL          R1=L'OGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE LIST RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
LOADOFL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING OFFRECD,R2                                                       
         LA    R2,IOKEY            SET KEY TO READ FIRST OFL RECORD             
         XC    OFFKEY,OFFKEY                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOFL10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   OFFKEY(OFFKCPY+1-OFFKEY),IOKEY                                   
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,OFFKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXOFLC,AINITOFL,AFILTOFL,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFL10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE LIST RECORD DATA                                      *         
***********************************************************************         
         SPACE 1                                                                
UPDTOFL  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING OFFRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTOFL            FILTER OFFICE LIST                           
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOFL                                                         
         GOTO1 AACCUPDT,DMCB,VATXOFLC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OFFICE LIST RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
INITOFL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXOFLL          R1=L'OFL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE LIST ENTRY RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
LOADOLE  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING OFFRECD,R2                                                       
         LA    R2,IOKEY            SET KEY TO READ FIRST OLE RECORD             
         XC    OFFKEY,OFFKEY                                                    
         MVC   OFFKCPY,COMPANY                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOLE10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   OFFKEY(OFFKCPY+1-OFFKEY),IOKEY                                   
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTOLE            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LOLE40                                                           
*                                                                               
         MVC   ACCADDR,OFFKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
         GOTO1 AINITOLE            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VATXOLEC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LOLE20   GOTO1 VATXOLEC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS                     
         CLI   DMCB+8,FF                                                        
         JE    LOLE40              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LOLE40              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LOLE30              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
LOLE30   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LOLE20                                                           
         J     YES                                                              
*                                                                               
LOLE40   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LOLE50                                                           
         GOTO1 AREADHI                                                          
         JE    LOLE50                                                           
         DC    H'0'                                                             
*                                                                               
LOLE50   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOLE10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE LIST ENTRY RECORD DATA                                *         
***********************************************************************         
         SPACE 1                                                                
UPDTOLE  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTOLE                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOLE                                                         
         L     R3,DXAXREC                                                       
         USING ATXRECD,R3                                                       
*                                                                               
         GOTO1 VATXOLEC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS              
*                                                                               
UOLE10   GOTO1 VATXOLEC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD            
         CLI   8(R1),FF                                                         
         JE    UOLEX               NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   UOLEX               DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UOLE20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UOLE20   DS    0H                                                               
         CLI   ATXREACT-ATXRECD(RF),C'A' IF ADD SKIP TO PUT                     
         JE    UOLE40                                                           
         MVI   ATXREACT-ATXRECD(RF),C'D'                                        
         MVI   COPYFLAG,X'01'                                                   
         J     UOLE40                                                           
*                                                                               
UOLE30   DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JNE   UOLE10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   ATXREACT-ATXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UOLE40   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UOLE30                                                           
*                                                                               
UOLEX    DS    0H                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCTION OFFICE LIST ENTRY RECORD AT R2                    *         
***********************************************************************         
         SPACE 1                                                                
FILTOLE  NTR1  BASE=*,LABEL=*                                                   
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OFFICE LIST ENTRY RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITOLE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXOLEL          R1=L'OLE RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
LOFF10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXOFFC,AINITOFF,AFILTOFF,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFF10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTOFF  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTOFF                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOFF                                                         
         GOTO1 AACCUPDT,DMCB,VATXOFFC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         SR    RF,RF                                                            
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OFFICE RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITOFF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXOFFL          R1=L'OFF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
LDEP10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXDEPC,AINITDEP,AFILTDEP,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDEP10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE DEPARTMENT RECORD DATA                                       *         
***********************************************************************         
         SPACE 1                                                                
UPDTDEP  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTDEP                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITDEP                                                         
         GOTO1 AACCUPDT,DMCB,VATXDEPC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
*                                                                               
         GOTO1 ARDLEDG                                                          
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,L2             LENGTH OF DEPARTMENT CODE                    
         JNZ   *+6                                                              
         DC    H'0'                MUST HAVE A LENGTH OR DIE                    
*                                                                               
         SR    RE,RE                                                            
         IC    RE,L1               LENGTH OF CLIENT CODE                        
         LA    RE,ACTKACT(RE)      DISP TO START OF DEPARTMENT                  
         BCTR  RF,0                                                             
         EX    RF,FDEPCLC                                                       
         JE    NO                                                               
         SR    RE,RE                                                            
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE DEPARTMENT RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
INITDEP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXDEPL          R1=L'DEP RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
LSBD10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXSBDC,AINITSBD,AFILTSBD,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSBD10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSBD                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSBD                                                         
         GOTO1 AACCUPDT,DMCB,VATXSBDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
*                                                                               
         GOTO1 ARDLEDG                                                          
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
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
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE SUB - DEPARTMENT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITSBD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXSDPL          R1=L'SBD RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
LPER10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   PERKEY(PERKCODE-PERKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   ACCADDR,PERKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXPERC,AINITPER,AFILTPER,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPER10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTPER  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING PERRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPER                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPER                                                         
         GOTO1 AACCUPDT,DMCB,VATXPERC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PERSON RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITPER  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXPERL          R1=L'PER RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         GOTO1 AINITPEA            INITIALIZE EXTRACT BUFFER                    
         GOTO1 VATXPEAC,DMCB,DXAXREC,(R2),(1,0),(R6)                            
*                                  GET NEXT UNCOMMITTED RECORD                  
LPEA04   GOTO1 VATXPEAC,DMCB,DXAXREC,(R2),(2,0),(R6)                            
         CLI   DMCB+8,FF                                                        
         JE    LPEA08              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LPEA08              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LPEA06                                                           
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PERSON ASSIGNMENT RECORD DATA                                *         
***********************************************************************         
         SPACE 1                                                                
UPDTPEA  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPEA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPEA                                                         
         L     R3,DXAXREC                                                       
         USING ATXRECD,R3                                                       
*                                                                               
         GOTO1 VATXPEAC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS              
*                                                                               
UPEA10   GOTO1 VATXPEAC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD            
         CLI   8(R1),FF                                                         
         JE    UPEAX               NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPEAX               DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPEA20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UPEA20   DS    0H                                                               
         CLI   ATXREACT-ATXRECD(RF),C'A' IF ADD SKIP TO PUT                     
         JE    UPEA40                                                           
         MVI   ATXREACT-ATXRECD(RF),C'D'                                        
         MVI   COPYFLAG,X'01'                                                   
         J     UPEA40                                                           
*                                                                               
UPEA30   DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JNE   UPEA10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   ATXREACT-ATXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPEA40   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     UPEA30                                                           
*                                                                               
UPEAX    DS    0H                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PERSON ASSIGNMENT RECORD                                 *         
***********************************************************************         
         SPACE 1                                                                
INITPEA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXPEAL          R1=L'PEA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
LCSA10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   ACTKEY(ACTKACT-ACTKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES           
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXCSAC,AINITCSA,AFILTCSA,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCSA10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE COSTING ACCOUNT RECORD DATA                                  *         
***********************************************************************         
         SPACE 1                                                                
UPDTCSA  NTR1  BASE=*,LABEL=*                                                   
         MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'C'                                                      
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING ACTRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCSA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCSA                                                         
         GOTO1 AACCUPDT,DMCB,VATXCSAC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE COSTING ACCOUNT RECORD                                   *         
***********************************************************************         
         SPACE 1                                                                
INITCSA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXCSAL          R1=L'CSA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD LEDGER RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
LOADLED  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   BYTE2,1                                                          
         LA    RF,1                                                             
*                                                                               
LLED10   SLL   RF,2                                                             
         B     *+0(RF)                                                          
         J     LLED20                                                           
         J     LLED30                                                           
         J     LLED40                                                           
         J     LLED50                                                           
         J     LLED60                                                           
         J     LLED70                                                           
         J     YES                                                              
*                                                                               
LLED20   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         J     LLED80                                                           
*                                                                               
LLED30   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'K'                                                      
         J     LLED80                                                           
*                                                                               
LLED40   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'I'                                                      
         J     LLED80                                                           
*                                                                               
LLED50   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'C'                                                      
         J     LLED80                                                           
*                                                                               
LLED60   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'N'                                                      
         J     LLED80                                                           
*                                                                               
LLED70   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
         J     LLED80                                                           
*                                                                               
LLED80   LA    R2,IOKEY            SET KEY TO READ FIRST LED RECORD             
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
         JO    LLED90                                                           
         CLI   DMCB+8,0            DIE IF DISK ERROR                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXLEDC,AINITLED,AFILTLED,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JZ    YES                                                              
*                                                                               
LLED90   XR    RF,RF                                                            
         IC    RF,BYTE2                                                         
         LA    RF,1(RF)                                                         
         STC   RF,BYTE2                                                         
         J     LLED10                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE LEDGER RECORD DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
UPDTLED  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING LDGRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         CLC   LDGKCPY,COMPANY     COMPANY OK?                                  
         JNE   YES                                                              
*                                                                               
         LA    RF,ULEDLST                                                       
ULED10   CLI   0(RF),0             END OF LIST                                  
         JE    YES                                                              
         CLC   LDGKUNT(2),0(RF)                                                 
         JE    ULED20                                                           
         LA    RF,2(RF)                                                         
         J     ULED10                                                           
*                                                                               
ULEDLST  DC    CL2'SJ'                                                          
         DC    CL2'SK'                                                          
         DC    CL2'SI'                                                          
         DC    CL2'1C'                                                          
         DC    CL2'1N'                                                          
         DC    CL2'1R'                                                          
         DC    X'00'                                                            
*                                                                               
ULED20   MVC   UNIT,0(RF)                                                       
         MVC   LEDGER,1(RF)                                                     
         GOTO1 AFILTLED                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITLED                                                         
         GOTO1 AACCUPDT,DMCB,VATXLEDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE LEDGER RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
INITLED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXLEDL          R1=L'LED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD LEDGER STRUCTURE RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
LOADLES  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   BYTE2,1                                                          
         LA    RF,1                                                             
*                                                                               
LLES10   SLL   RF,2                                                             
         B     *+0(RF)                                                          
         J     LLES20                                                           
         J     LLES30                                                           
         J     LLES40                                                           
         J     LLES50                                                           
         J     LLES60                                                           
         J     LLES70                                                           
         J     YES                                                              
*                                                                               
LLES20   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
         B     LLES80                                                           
*                                                                               
LLES30   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'K'                                                      
         J     LLES80                                                           
*                                                                               
LLES40   MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'I'                                                      
         J     LLES80                                                           
*                                                                               
LLES50   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'C'                                                      
         J     LLES80                                                           
*                                                                               
LLES60   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'N'                                                      
         J     LLES80                                                           
*                                                                               
LLES70   MVI   UNIT,C'1'                                                        
         MVI   LEDGER,C'R'                                                      
*                                                                               
LLES80   LA    R2,IOKEY            SET KEY TO READ FIRST LES RECORD             
         USING ACTRECD,R2                                                       
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
         JO    LLES110                                                          
         CLI   DMCB+8,0            DIE IF DISK ERROR                            
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   ACCADDR,ACTKDA                                                   
         GOTO1 AFILTLES            FILTER RECORD, GET NEXT IF NOT VALID         
         BNE   LLES110                                                          
*                                                                               
         GOTO1 AGETIT                                                           
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 AINITLES            INITIALIZE EXTRACT BUFFER                    
         GOTO1 VATXLESC,DMCB,DXAXREC,(R2),(1,0),(R6)                            
*                                                                               
LLES90   GOTO1 VATXLESC,DMCB,DXAXREC,(R2),(2,0),(R6)                            
         CLI   DMCB+8,FF                                                        
         BE    LLES110             NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         BNE   LLES90              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         BE    LLES100             DO NOT CONVERT RECORDS                       
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
LLES100  GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LLES90                                                           
         J     YES                                                              
*                                                                               
LLES110  OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JZ    YES                                                              
         XR    RF,RF                                                            
         ZIC   RF,BYTE2                                                         
         LA    RF,1(RF)                                                         
         STC   RF,BYTE2                                                         
         J     LLES10                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE LEDGER STRUCTURE RECORD DATA                                 *         
***********************************************************************         
         SPACE 1                                                                
UPDTLES  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING LDGRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         CLC   LDGKCPY,COMPANY     COMPANY OK?                                  
         JNE   YES                                                              
*                                                                               
         LA    RF,ULESLST                                                       
ULES10   CLI   0(RF),0             END OF LIST                                  
         JE    YES                                                              
         CLC   LDGKUNT(2),0(RF)                                                 
         JE    ULES20                                                           
         LA    RF,2(RF)                                                         
         J     ULES10                                                           
*                                                                               
ULESLST  DC    CL2'SJ'                                                          
         DC    CL2'SK'                                                          
         DC    CL2'SI'                                                          
         DC    CL2'1C'                                                          
         DC    CL2'1N'                                                          
         DC    CL2'1R'                                                          
         DC    X'00'                                                            
*                                                                               
ULES20   MVC   UNIT,0(RF)                                                       
         MVC   LEDGER,1(RF)                                                     
         GOTO1 AFILTLES                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITLES                                                         
         L     R3,DXAXREC                                                       
         USING ATXRECD,R3                                                       
*                                                                               
         GOTO1 VATXLESC,DMCB,DXAXREC,(R2),(1,0),(R6) BUILD RECORDS              
*                                                                               
ULES30   GOTO1 VATXLESC,DMCB,DXAXREC,(R2),(2,0),(R6) GET NEXT RECORD            
         CLI   8(R1),FF                                                         
         JE    ULESX               NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   ULESX               DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ULES40              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
ULES40   DS    0H                                                               
         CLI   ATXREACT-ATXRECD(RF),C'A' IF ADD SKIP TO PUT                     
         JE    ULES60                                                           
         MVI   ATXREACT-ATXRECD(RF),C'D'                                        
         MVI   COPYFLAG,X'01'                                                   
         J     ULES60                                                           
*                                                                               
ULES50   DS    0H                                                               
         CLI   COPYFLAG,X'01'                                                   
         JNE   ULES30                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   ATXREACT-ATXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
ULES60   DS    0H                                                               
         GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     ULES50                                                           
*                                                                               
ULESX    DS    0H                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE LEDGER STRUCTURE RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITLES  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXLESL          R1=L'LES RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OPTION RECORD                                                  *         
***********************************************************************         
*         SPACE 1                                                               
*LOADOPT  NTR1  BASE=*,LABEL=*                                                  
*         MVC   MAXIOS,DXMAXREC                                                 
*         MVI   UNIT,C'S'                                                       
*         MVI   LEDGER,C'J'                                                     
*                                                                               
*         LA    R2,IOKEY            SET KEY TO READ FIRST OPT RECORD            
*         USING POPRECD,R2                                                      
*         XC    POPKEY,POPKEY                                                   
*         MVC   POPKCPY,COMPANY                                                 
*         MVI   POPKTYP,POPKTYPQ                                                
*         MVI   POPKSUB,POPKSUBQ                                                
*         MVC   POPKUNT,UNIT                                                    
*         MVC   POPKLDG,LEDGER                                                  
*         L     R2,DXARECB                                                      
*         GOTO1 AREADHI                                                         
*         JNE   NO                                                              
*                                                                               
*LOPT10   TM    DMCB+8,X'80'        ALL DONE IF EOF                             
*         JO    YES                                                             
*         CLC   POPKEY(POPKOFG-POPKEY),IOKEY                                    
*         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGES          
*                                                                               
*         MVC   ACCADDR,POPKDA                                                  
*         GOTO1 AACCLOAD,DMCB,VATXOPTC,AINITOPT,AFILTOPT,VATXCNVX               
*         JNE   NO                                                              
*         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                
*         JNZ   LOPT10                                                          
*         J     YES                                                             
*         DROP  R2                                                              
*         EJECT                                                                 
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
*         SPACE 1                                                               
*         LTORG                                                                 
*         EJECT                                                                 
***********************************************************************         
* UPDATE OPTION RECORD                                                *         
***********************************************************************         
*         SPACE 1                                                               
*UPDTOPT  NTR1  BASE=*,LABEL=*                                                  
*         MVI   UNIT,C'S'                                                       
*         MVI   LEDGER,C'J'                                                     
*                                                                               
*         USING RECDS,R5                                                        
*         L     R5,DXARECB                                                      
*         USING POPRECD,R2                                                      
*         LA    R2,RECVHDR+L'RECVHDR                                            
*                                                                               
*         GOTO1 AFILTOPT                                                        
*         JNE   YES                                                             
*                                                                               
*         GOTO1 AINITOPT                                                        
*         GOTO1 AACCUPDT,DMCB,VATXOPTC,TYPECODE                                 
*         JNE   NO                                                              
*         J     YES                                                             
*         DROP  R2                                                              
*         EJECT                                                                 
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
*         SPACE 1                                                               
*         LTORG                                                                 
*         EJECT                                                                 
***********************************************************************         
* FILTER OPT RECORD AT R2                                             *         
***********************************************************************         
*         SPACE 1                                                               
*FILTOPT  NTR1  BASE=*,LABEL=*                                                  
*         USING POPRECD,R2                                                      
*         CLC   POPKCPY,COMPANY     COMPANY OK?                                 
*         JNE   NO                                                              
*         CLI   POPKTYP,POPKTYPQ                                                
*         JNE   NO                                                              
*         CLI   POPKSUB,POPKSUBQ                                                
*         JNE   NO                                                              
*         CLC   POPKUNT,UNIT                                                    
*         JNE   NO                                                              
*         CLC   POPKLDG,LEDGER                                                  
*         JNE   NO                                                              
*                                                                               
* ONLY PASS RECORDS THAT HAVE SOME DATA IN IT                                   
*                                                                               
*          OC    POPKOFG,POPKOFG                                                
*         JNZ   YES                                                             
*         OC    POPKOFC,POPKOFC                                                 
*         JNZ   YES                                                             
*         OC    POPKCLI,POPKCLI                                                 
*         JNZ   YES                                                             
*         OC    POPKPRO,POPKPRO                                                 
*         JNZ   YES                                                             
*         OC    POPKJOB,POPKJOB                                                 
*         JNZ   YES                                                             
*         OC    POPKMGR,POPKMGR                                                 
*         JNZ   YES                                                             
*         OC    POPKMED,POPKMED                                                 
*         JNZ   YES                                                             
*         J     NO                                                              
*         DROP  R2                                                              
*         EJECT                                                                 
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
*         SPACE 1                                                               
*         LTORG                                                                 
*         EJECT                                                                 
***********************************************************************         
* INITIALIZE OPTION RECORD                                            *         
***********************************************************************         
*         SPACE 1                                                               
*INITOPT  NTR1  BASE=*,LABEL=*                                                  
*         LA    R1,ATXOPTL          R1=L'OPT RECORD (LONGEST)                   
*         GOTO1 AINITALL                                                        
*         J     YES                                                             
*         EJECT                                                                 
***********************************************************************         
* LOAD STANDARD COMMENT RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
LOADSTC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING SCMRECD,R2                                                       
         LA    R2,IOKEY            SET KEY TO READ FIRST OFF RECORD             
         XC    SCMKEY,SCMKEY                                                    
         MVI   SCMKTYP,SCMKTYPQ                                                 
         MVC   SCMKCPY,COMPANY                                                  
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTC10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   SCMKEY(SCMKCODE-SCMKEY),IOKEY                                    
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGESR          
*                                                                               
         MVC   ACCADDR,SCMKDA                                                   
         GOTO1 AACCLOAD,DMCB,VATXSTCC,AINITSTC,AFILTSTC,VATXCNVX                
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTC10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE STANDARD COMMENT RECORD DATA                                 *         
***********************************************************************         
         SPACE 1                                                                
UPDTSTC  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING SCMRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTSTC                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSTC                                                         
         GOTO1 AACCUPDT,DMCB,VATXSTCC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER STANDARD COMMENT RECORD AT R2                                *         
***********************************************************************         
FILTSTC  NTR1  BASE=*,LABEL=*                                                   
         USING SCMRECD,R2                                                       
         CLC   SCMKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLI   SCMKTYP,SCMKTYPQ    X'0C'                                        
         JNE   NO                                                               
         CLC   SCMKCODE,SPACES                                                  
         JE    NO                                                               
         J     YES                                                              
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE STANDARD COMMENT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITSTC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXSTCL          R1=L'STANDARD COM RECORD (LONGEST)           
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD USER FIELD RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
LOADUSR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         MVI   UNIT,C'S'                                                        
         MVI   LEDGER,C'J'                                                      
*                                                                               
         USING UFSRECD,R2                                                       
         LA    R2,IOKEY            SET KEY TO READ FIRST OFF RECORD             
         XC    UFSKEY,UFSKEY                                                    
         MVI   UFSKTYP,UFSKTYPQ                                                 
         MVI   UFSKSUB,UFSKSUBQ                                                 
         MVC   UFSKCPY,COMPANY                                                  
         MVC   UFSKUNT,UNIT                                                     
         MVC   UFSKLDG,LEDGER                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUSR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   UFSKEY(UFSKOFG-UFSKEY),IOKEY                                     
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGESR          
*                                                                               
         GOTO1 AFILTUSR            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LJOB40                                                           
*                                                                               
         MVC   ACCADDR,UFSKDA                                                   
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  ERROR ON READ                                
         GOTO1 AINITUSR            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VATXUSRC,DMCB,DXAXREC,(R2),(1,0),(R6),LEVELS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LUSR20   GOTO1 VATXUSRC,DMCB,DXAXREC,(R2),(2,0),(R6),LEVELS                     
         CLI   DMCB+8,FF                                                        
         JE    LUSR40              NO MORE RECORDS LEFT                         
         CLI   DXWRITE,C'Y'                                                     
         JNE   LUSR40              DO NOT WRITE RECORDS TO FILE                 
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    LUSR30              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
LUSR30   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         GOTO1 ADECIOC                                                          
         JE    LUSR20                                                           
         J     YES                                                              
*                                                                               
LUSR40   MVC   IOKEY(L'ACTKEY),0(R2)                                            
         GOTO1 ACHKSEQ                                                          
         JE    LUSR50                                                           
         GOTO1 AREADHI                                                          
         JE    LUSR50                                                           
         DC    H'0'                                                             
*                                                                               
LUSR50   GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),ACCDIR,IOKEY,(R2),DMWORK            
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUSR10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE USER FIELD RECORD DATA                                       *         
***********************************************************************         
         SPACE 1                                                                
UPDTUSR  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING UFSRECD,R2                                                       
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTUSR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITUSR                                                         
         GOTO1 AACCUPDT,DMCB,VATXUSRC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER STANDARD COMMENT RECORD AT R2                                *         
***********************************************************************         
         SPACE 1                                                                
         USING UFSRECD,R2                                                       
FILTUSR  NTR1  BASE=*,LABEL=*                                                   
         CLI   UFSKTYP,UFSKTYPQ    X'2C'                                        
         JNE   NO                                                               
         CLI   UFSKSUB,UFSKSUBQ    X'10'                                        
         JNE   NO                                                               
         CLC   UFSKCPY,COMPANY     COMPANY OK?                                  
         JNE   NO                                                               
         CLC   UFSKUNT,UNIT        SAME UNIT                                    
         JNE   NO                                                               
         CLC   UFSKLDG,LEDGER      SAME LEDGER                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE STANDARD COMMENT RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
INITUSR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXUSRL          R1=L'USERFIELD RECORD (LONGEST)              
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CONTROL AGENCY RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
LOADCAG  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING CT5REC,R2                                                        
         LA    R2,IOKEY            SET KEY TO READ FIRST AGENCY RECORD          
         XC    CT5KEY,CT5KEY                                                    
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,SXDTAGY     CURRENT AGENCY                              
         L     R2,DXARECB                                                       
         GOTO1 AREADCHI                                                         
         JNE   NO                                                               
*                                                                               
LCAG10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   CT5KEY(CT5LEN-CT5KEY),IOKEY                                      
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGESR          
*                                                                               
         GOTO1 ACXLOAD,DMCB,VATXCAGC,AINITCAG,AFILTCAG,VATXCNVX                 
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCAG10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CONTROL AGENCY RECORD AT R2                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CT5REC,R2                                                        
FILTCAG  NTR1  BASE=*,LABEL=*                                                   
         CLI   CT5KTYP,CT5KTYPQ                                                 
         JNE   NO                                                               
         CLC   CT5KALPH,SXDTAGY     CURRENT AGENCY                              
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CONTROL AGENCY RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITCAG  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXCAGL          R1=L'CONTROL AGY RECORD (LONGEST)            
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CONTROL PERSON RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
LOADCPE  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING SAPEREC,R2                                                       
         LA    R2,IOKEY            SET KEY TO READ FIRST PERSON RECORD          
         XC    SAPEKEY,SAPEKEY                                                  
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,SVSECAGY                                                 
*                                                                               
         L     R2,DXARECB                                                       
         GOTO1 AREADCHI                                                         
         JNE   NO                                                               
*                                                                               
LCPE10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   SAPEREC(SAPEPID-SAPEKEY),IOKEY                                   
         JNE   YES                 ALL DONE IF COMPANY LEDGER CHANGESR          
*                                                                               
         GOTO1 ACXLOAD,DMCB,VATXCPEC,AINITCPE,AFILTCPE,VATXCNVX                 
         JNE   NO                                                               
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCPE10                                                           
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CONTROL AGENCY RECORD AT R2                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SAPEREC,R2                                                       
FILTCPE  NTR1  BASE=*,LABEL=*                                                   
         CLI   SAPETYP,SAPETYPQ                                                 
         JNE   NO                                                               
         CLI   SAPESUB,SAPESUBQ                                                 
         JNE   NO                                                               
         CLC   SAPEAGY,SVSECAGY                                                 
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CONTROL AGENCY RECORD                                    *         
***********************************************************************         
         SPACE 1                                                                
INITCPE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,ATXCPEL          R1=L'CONTROL PER RECORD (LONGEST)            
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ LEDGER RECORD TO FIND LENGTHS OF SUB RECORD TYPES              *         
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
         SR    RF,RF                                                            
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
         SR    R2,R2                                                            
         LA    R1,DISPS                                                         
*                                                                               
RLED06   MVC   0(1,R1),0(RE)                                                    
         LA    RE,16(RE)                                                        
         LA    R1,1(R1)                                                         
         BCT   RF,RLED06                                                        
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
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
RLEDWTO  TM    LDGIND,LDGIOPT      IS LEDGER OPTIONAL                           
         JNZ   NO                  YES                                          
         MVC   MSGUNT,UNIT                                                      
         MVC   MSGLDG,LEDGER                                                    
         MVC   MSGCPY,SXDTAGY                                                   
         LA    R3,MSGL                                                          
         WTO   TEXT=(R3),MCSFLAG=HRDCPY                                         
         ABEND 922,DUMP                                                         
MSGL     DC    AL2(70)                                                          
MSG      DC    CL70' '                                                          
         ORG   MSG                                                              
         DC    C'REQUIRED LEDGER - '                                            
MSGUNT   DC    C'?'                                                             
MSGLDG   DC    C'?'                                                             
         DC    C' - MISSING/INVALID - AXTRACT ABENDING (CPYALPH='               
MSGCPY   DC    C'??'                                                            
         DC    C')'                                                             
         ORG   MSG+L'MSG                                                        
*                                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
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
         DROP  CHG,CPY                                                          
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT ACCOUNT RECORDS IN LOAD MODE                  *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(FORMAT CONVERT ROUTINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
ACCLOAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   ALPARMS,0(R1)                                                    
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    ALOA10                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   ALOA60              NOT VALID - GET NEXT                         
*                                                                               
ALOA10   TM    SUBIOIND,SUBIOYES   RECORD ALREADY GOT IF SUB REQUIRED           
         JO    ALOA20                                                           
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
ALOA20   GOTO1 (R4)                INITIALIZE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),LEVELS,ASUBLAST                    
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    ALOA40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    ALOA50              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   ALOA50                                                           
         CLI   SXDTPLFM,0                                                       
         JE    ALOA30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 ALPACNVX,DMCB,(R7)                                               
*                                                                               
ALOA30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
ALOA40   TM    BYTE,X'40'          TEST CALL BACK REQUIRED                      
         JO    ALOA20                                                           
*                                                                               
ALOA50   TM    SUBIOIND,SUBIOYES   SUB IO REQUIRED?                             
         JNO   ALOA60                                                           
         GOTO1 (R5)                FILTER RECORD WILL FIND NET SUB              
         JE    ALOA20              CONTINUE WITH SUB RECORDS                    
*                                                                               
ALOA60   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         MVC   IOKEY(L'ACTKEY),0(R2) READ NEXT RECORD - SEQUENT                 
         CLI   TYPEDEEP,0                                                       
         JNE   ALOA70              YES, NEED TO READ HIGH                       
*                                                                               
         GOTO1 ACHKSEQ             SEE IF READ SEQUENCE BROKEN                  
         JE    ALOA80              NO                                           
         GOTO1 AREADHI                                                          
         JE    ALOA80                                                           
         DC    H'0'                                                             
*                                                                               
ALOA70   LA    RF,IOKEY            READ NEXT RECORD - HIGH                      
         USING ACTRECD,RF                                                       
         SR    RE,RE                                                            
         IC    RE,TYPEDEEP         LEVEL TO FILTER AT                           
         BCTR  RE,0                                                             
         LA    RE,DISPS(RE)        DISP TO START OF NEXT IN R1 HERE             
         SR    R1,R1                                                            
         IC    R1,0(RE)            WE WANT LAST OF THIS LEVEL                   
         BCTR  R1,0                - SO SUBTRACT 1...                           
         LA    R1,ACTKACT(R1)      R1=A(BYTE TO INCREMENT BY 1)                 
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    RE,1(RE)            INCREMENT IT                                 
         STC   RE,0(R1)                                                         
         DROP  RF                                                               
*                                                                               
         GOTO1 AREADHI                                                          
         JE    YES                                                              
         J     NO                                                               
*                                                                               
ALOA80   GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT CONTROL TEMPO RECORDS IN LOAD MODE            *         
* R2 = A(ACCOUNT DIRECTORY RECORD BUFFER)                             *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(FORMAT CONVERT ROUTINE)                                      *         
***********************************************************************         
         SPACE 1                                                                
CXLOAD   NTR1  BASE=*,LABEL=*                                                   
         MVC   ALPARMS,0(R1)                                                    
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               TEST IF FILTER ROUTINE PASSED                
         JZ    CXLOAD10                                                         
         GOTO1 (R5)                FILTER RECORD                                
         JNE   CXLOAD40            GET NEXT - NOT VALID                         
*                                                                               
CXLOAD10 GOTO1 (R4)                INITIALIZE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6)                                    
         MVC   BYTE,DMCB+8         SAVE DMCB+8                                  
         NI    DMCB+8,X'FF'-X'01'                                               
         CLI   DMCB+8,X'88'                                                     
         BE    CXLOAD30            TEST NO CALL BACK                            
         TM    DMCB+8,X'80'                                                     
         BO    CXLOAD40            TEST NOT TO WRITE THIS RECORD                
         CLI   DXWRITE,C'Y'                                                     
         BNE   CXLOAD40                                                         
         CLI   SXDTPLFM,0                                                       
         BE    CXLOAD20                                                         
*                                  CONVERT RECORD TO SQL BUFFER                 
         GOTO1 ALPACNVX,DMCB,(R7)                                               
*                                                                               
CXLOAD20 GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         TM    BYTE,X'40'          TEST IF CALL BACK REQUIRED                   
         BO    CXLOAD10                                                         
*                                                                               
CXLOAD30 GOTO1 ADECIOC             DECREMENT IO COUNTER                         
         JNE   NO                                                               
*                                                                               
CXLOAD40 MVC   IOKEY(L'IOKEY),0(R2)   READ NEXT RECORD                          
         TM    BYTE,X'01'                                                       
         BZ    CXLOAD50            TEST READ SEQUENCE BROKEN                    
         GOTO1 VDATAMGR,DMCB,(X'00',DMRDHI),CTFILE,IOKEY,(R2)                   
         TM    8(R1),X'FD'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
CXLOAD50 GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),LEVELS,ASUBLAST                    
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ACCU20              DO NOT CONVERT RECORD                        
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   ACCU10                                                           
*                                                                               
         L     R0,ACOPYBUF         R0=A(EXTRACT RECORD AREA FOR COPY)           
         LH    R1,=Y(COPYBUFL)                                                  
         SR    RE,RE                                                            
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
         LH    R1,=AL2(ATXCPAID-ATXRECD) DISP TO COMPANY                        
         AR    R0,R1               BUMP TO COMPANY CODE IN BOTH RECS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
         SH    RF,=AL2(L'ATXCPYX)  DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    YES                 ARE THE SAME THEN SKIP                       
*                                                                               
ACCU10   GOTO1 VATXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
ACCU20   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
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
ACCADDR  DS    F                   DISK ADDRESS                                 
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
*                                                                               
SVSECAGY DS    CL2                 SAVED AREA FOR SECURITY AGENCY               
CONSWTCH DS    CL1                 IF SET-READING CONTROL SYSTEM                
COPYFLAG DS    XL1                 UPDATE FLAG FOR COPY                         
*                                                                               
PRIALPHA DS    CL2                 PRIMARY ALPHA                                
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
LDGIND   DS    XL1                                                              
LDGIOPT  EQU   X'80'               LEDGER IS OPTIONAL                           
*                                                                               
SUBIOIND DS    XL1                 SUB IO RECORDS INDICATOR                     
SUBIOYES EQU   X'80'               SUB IO RECORDS REQUIRED                      
ASUBLAST DS    A                   A(LAST SUB RECORD)                           
*                                                                               
ALPARMS  DS    0XL16                                                            
ALPAEXTR DS    A                   EXTRACT ROUTINE                              
ALPAINIT DS    A                   INITIALISATION ROUTINE                       
ALPAFILT DS    A                   FILTER ROUTINE                               
ALPACNVX DS    A                   CONVERT ROUTINE                              
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
VATXCNVX DS    V                                                                
VATXCPYC DS    V                   COMPANY RECORDS                              
VATXAGRC DS    V                   ACCOUNT GROUP RECORDS                        
VATXCLIC DS    V                   CLIENT RECORDS                               
VATXPROC DS    V                   PRODUCT RECORDS                              
VATXJOBC DS    V                   JOB RECORDS                                  
VATXMEDC DS    V                   MEDIA RECORDS                                
VATXMGRC DS    V                   MEDIA GROUP RECORDS                          
VATXTSKC DS    V                   TASK CODE RECORDS ROUTINE                    
VATXSTKC DS    V                   SUB TASK CODES                               
VATXTGRC DS    V                   TASK CODE GROUP RECORDS                      
VATXPOFC DS    V                   PRODUCTION OFFICE RECORDS                    
VATXOGRC DS    V                   PRODUCTION OFFICE GROUP RECORDS              
VATXOFLC DS    V                   PRODUCTION OFFICE LIST                       
VATXOLEC DS    V                   PRODUCTION OFFICE LIST ENTRY                 
VATXOFFC DS    V                   1R OFFICE RECORDS                            
VATXDEPC DS    V                   1R DEPARTMENT RECORDS                        
VATXSBDC DS    V                   1R SUBDEPARTMENT RECORDS                     
VATXPERC DS    V                   PERSON                                       
VATXPEAC DS    V                   PERSON ASSIGNMENT                            
VATXCSAC DS    V                   COSTING ACCOUNT                              
VATXLEDC DS    V                   LEDGER                                       
VATXLESC DS    V                   LEDGER STRUCTURE                             
*VATXOPTC DS    V                   OPTIONS                                     
VATXSTCC DS    V                   STANDARD COMMENT                             
VATXUSRC DS    V                   USER FIELD                                   
VATXCAGC DS    V                   CONTROL AGENCY RECORD                        
VATXCPEC DS    V                   CONTROL PERSON RECORD                        
*                                                                               
         DS    CL8                 COMMON INTERNAL ROUTINES                     
AACCLOAD DS    A                                                                
ACXLOAD  DS    A                                                                
AACCUPDT DS    A                                                                
ADECIOC  DS    A                                                                
ACHKSEQ  DS    A                                                                
ARDLEDG  DS    A                                                                
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                   READ HIGH ON THE ACCOUNT FILE                
AREADCHI DS    A                   READ HIGH ON THE CONTROL FILE                
ARECCMP  DS    A                   COMPARE CHANGE/COPY RECORD                   
*                                                                               
         DS    CL8                 LOAD ROUTINES                                
ALOADALL DS    A                   ALL                                          
ALOADCON DS    A                   ALL CONTROL RECORDS                          
ALOADCPY DS    A                   COMPANY                                      
ALOADAGR DS    A                   ACCOUNT GROUPS                               
ALOADCLI DS    A                   CLIENT                                       
ALOADPRO DS    A                   PRODUCT                                      
ALOADJOB DS    A                   JOB                                          
ALOADMED DS    A                   MEDIA                                        
ALOADMGR DS    A                   MEDIA GROUP                                  
ALOADTSK DS    A                   TASK CODE                                    
ALOADSTK DS    A                   SUB-TASK CODE                                
ALOADTGR DS    A                   TASK CODE-GROUP                              
ALOADPOF DS    A                   PRODUCTION OFFICE                            
ALOADOGR DS    A                   PRODUCTION OFFICE GROUP                      
ALOADOFL DS    A                   PRODUCTION OFFICE LIST                       
ALOADOLE DS    A                   PRODUCTION OFFICE LIST ENTRY                 
ALOADOFF DS    A                   1R OFFICE                                    
ALOADDEP DS    A                   1R DEPARTMENT                                
ALOADSBD DS    A                   1R SUB-DEPARTMENT                            
ALOADPER DS    A                   PERSON RECORD                                
ALOADPEA DS    A                   PERSON ASSIGNMENT                            
ALOADCSA DS    A                   COSTING ACCOUNT                              
ALOADLED DS    A                   LEDGER                                       
ALOADLES DS    A                   LEDGER STRUCTURE                             
*ALOADOPT DS    A                   OPTIONS                                     
ALOADSTC DS    A                   STANDARD COMMENT                             
ALOADUSR DS    A                   USERFIELD RECORD                             
ALOADCAG DS    A                   CONTROL AGENCY RECORD                        
ALOADCPE DS    A                   CONTROL PERSON RECORD                        
*                                                                               
         DS    CL8                 UPDATE ROUTINES                              
AUPDTALL DS    A                   ALL                                          
         DS    A                   ALL CONTROL RECORDS                          
AUPDTCPY DS    A                   COMPANY                                      
AUPDTAGR DS    A                   ACCOUNT GROUPS                               
AUPDTCLI DS    A                   CLIENT                                       
AUPDTPRO DS    A                   PRODUCT                                      
AUPDTJOB DS    A                   JOB                                          
AUPDTMED DS    A                   MEDIA                                        
AUPDTMGR DS    A                   MEDIA GROUP                                  
AUPDTTSK DS    A                   TASK CODE                                    
AUPDTSTK DS    A                   SUB-TASK CODE                                
AUPDTTGR DS    A                   TASK CODE-GROUP                              
AUPDTPOF DS    A                   PRODUCTION OFFICE                            
AUPDTOGR DS    A                   PRODUCTION OFFICE GROUP                      
AUPDTOFL DS    A                   PRODUCTION OFFICE LIST                       
AUPDTOLE DS    A                   PRODUCTION OFFICE LIST ENTRY                 
AUPDTOFF DS    A                   1R OFFICE                                    
AUPDTDEP DS    A                   1R DEPARTMENT                                
AUPDTSBD DS    A                   1R SUB-DEPARTMENT                            
AUPDTPER DS    A                   PERSON RECORD                                
AUPDTPEA DS    A                   PERSON ASSIGNMENT                            
AUPDTCSA DS    A                   COSTING ACCOUNT                              
AUPDTLED DS    A                   LEDGER                                       
AUPDTLES DS    A                   LEDGER STRUCTURE                             
*AUPDTOPT DS    A                   OPTIONS                                     
AUPDTSTC DS    A                   STANDARD COMMENT                             
AUPDTUSR DS    A                   USERFIELD RECORD                             
         DS    A                   CONTROL AGENCY RECORD                        
         DS    A                   CONTROL PERSON RECORD                        
*                                                                               
         DS    CL8                 FILTER ROUTINES                              
AFILTCPY DS    A                   COMPANY                                      
AFILTAGR DS    A                   ACCOUNT GROUP                                
AFILTCLI DS    A                   CLIENT                                       
AFILTPRO DS    A                   PRODUCT                                      
AFILTJOB DS    A                   JOB                                          
AFILTMED DS    A                   MEDIA                                        
AFILTMGR DS    A                   MEDIA GROUP                                  
AFILTTSK DS    A                   TASK CODE                                    
AFILTSTK DS    A                   SUB-TASK CODE                                
AFILTTGR DS    A                   TASK CODE-GROUP                              
AFILTPOF DS    A                   PRODUCTION OFFICE                            
AFILTOGR DS    A                   PRODUCTION OFFICE GROUP                      
AFILTOFL DS    A                   PRODUCTION OFFICE LIST                       
AFILTOLE DS    A                   PRODUCTION OFFICE LIST ENTRY                 
AFILTOFF DS    A                   1R OFFICE                                    
AFILTDEP DS    A                   1R DEPARTMENT                                
AFILTSBD DS    A                   1R SUBDEPARTMENT                             
AFILTPER DS    A                   PERSON                                       
AFILTPEA DS    A                   PERSON ASSIGNMENT                            
AFILTCSA DS    A                   COSTING ACCOUNT                              
AFILTLED DS    A                   LEDGER                                       
AFILTLES DS    A                   LEDGER STRUCTURE                             
*AFILTOPT DS    A                   OPTIONS                                     
AFILTSTC DS    A                   STANDARD COMMENT                             
AFILTUSR DS    A                   USERFIELD RECORD                             
AFILTCAG DS    V                   CONTROL AGENCY RECORD                        
AFILTCPE DS    V                   CONTROL PERSON RECORD                        
*                                                                               
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   GENERAL INITIALISATION                       
AINITCPY DS    A                   COMPANY                                      
AINITAGR DS    A                   ACCOUNT GROUP                                
AINITCLI DS    A                   CLIENT                                       
AINITPRO DS    A                   PRODUCT                                      
AINITJOB DS    A                   JOB                                          
AINITMED DS    A                   MEDIA                                        
AINITMGR DS    A                   MEDIA GROUP                                  
AINITTSK DS    A                   TASK CODE                                    
AINITSTK DS    A                   SUB-TASK CODE                                
AINITTGR DS    A                   TASK CODE-GROUP                              
AINITPOF DS    A                   PRODUCTION OFFICE                            
AINITOGR DS    A                   PRODUCTION OFFICE GROUP                      
AINITOFL DS    A                   PRODUCTION OFFICE LIST                       
AINITOLE DS    A                   PRODUCTION OFFICE LIST ENTRY                 
AINITOFF DS    A                   1R OFFICE                                    
AINITDEP DS    A                   1R DEPARTMENT                                
AINITSBD DS    A                   1R SUB-DEPARTMENT                            
AINITPER DS    A                   PERSON                                       
AINITPEA DS    A                   PERSON ASSIGNMENT                            
AINITCSA DS    A                   COSTING ACCOUNT                              
AINITLED DS    A                   LEDGER                                       
AINITLES DS    A                   LEDGER STRUCTURE                             
*AINITOPT DS    A                   OPTIONS                                     
AINITSTC DS    A                   STANDARD COMMENT                             
AINITUSR DS    A                   USERFIELD RECORD                             
AINITCAG DS    V                   CONTROL AGENCY RECORD                        
AINITCPE DS    V                   CONTROL PERSON RECORD                        
*                                                                               
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
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ACOPYBUF DS    A                                                                
SPACES   DS    CL80                                                             
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
*                                                                               
* ATXRECD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ATXRECD                                                        
         PRINT ON                                                               
                                                                                
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
                                                                                
* DDPERVALD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDPERVALD                                                      
         PRINT ON                                                               
*                                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
*                                                                               
* DMGREQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMGREQUS                                                       
         PRINT ON                                                               
*                                                                               
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*                                                                               
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
*                                                                               
* FACTRYEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ATXTRACT  06/09/03'                                      
         END                                                                    
