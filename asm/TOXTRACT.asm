*          DATA SET TOXTRACT   AT LEVEL 047 AS OF 10/30/19                      
*PHASE OXTRACTE                                                                 
*INCLUDE OXROUTS                   XTRACT RECORD CREATION MODULE                
*INCLUDE OXCNVX                    CONVERSION ROUTINES FOR ALL ABOVE            
*INCLUDE BUFFERIN                                                               
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
*INCLUDE TINVCON                                                                
*INCLUDE CRYPT                                                                  
*INCLUDE LOADER                                                                 
*INCLUDE HELLO                                                                  
*INCLUDE SQUASHER                                                               
         TITLE 'TOXTRACT-EXTRACT SYSTEM FILE SYB DATA'                          
***********************************************************************         
* TALENT SYB SUB SYSTEM EXTRACT CONTROL MODULE                        *         
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
*                                                                               
* LOAD ROUTINES READ FROM LIVE FILE                                             
* UPDATE ROUTINES READ FROM RECOVERY ONLY FOR THE UPDATED RECORDS               
***********************************************************************         
         EJECT                                                                  
TOXTRACT CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKL,*TOXTR**,R8,CLEAR=YES                                      
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
         USING RXUSERD,DXUSER                                                   
*                                                                               
*        MVC   SYSCODE,DXSYSCOD                                                 
         L     RF,=A(SSB)                                                       
         MVC   SSODSPAC-SSOOFF(L'SSODSPAC,RF),DXDSPAC                           
         L     RF,=V(DDSIO)                                                     
         LTR   RF,RF                                                            
         BZ    *+10                                                             
         MVC   0(8,RF),DXDDSIO                                                  
*                                                                               
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
                                                                                
GENINIT  NTR1                                                                   
         MVC   DATADISP,=H'40'                                                  
*                                                                               
         CLI   DXMODE,DXOPENQ                                                   
         BNE   GENIN10                                                          
         GOTOR VBUFFRIN,DMCB,('BUFFAINI',AHIDBUF),HIDREC,ACOMFACS               
*                                                                               
GENIN10  XC    AENCKEY,AENCKEY                                                  
         CLC   SXDTEKEY,SPACES                                                  
         BNH   GENIN20                                                          
         LA    RF,SXDTEKEY                                                      
         STCM  RF,15,AENCKEY                                                    
*                                  TEST FOR PASSWORD ENCRYPTION                 
GENIN20  LA    RE,PKCNTS           CLEAR RECORD COUNTS                          
         LA    R0,PKCNTQ                                                        
         ZAP   0(PKCNTLNQ,RE),=P'0'                                             
         AHI   RE,PKCNTLNQ                                                      
         BCT   R0,*-10                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXOPENQ - OPEN TALENT SYSTEM FILES                         *         
***********************************************************************         
                                                                                
PROCOPEN NTR1  ,                   SET UTL SENUM                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         XC    IOKEY,IOKEY         GET DTF ADDRESS                              
         L     R2,DXARECB                                                       
         GOTO1 VDATAMGR,DMCB,DMFAST,TALDIR,IOKEY,(R2),DMWORK                    
         L     RF,12(R1)                                                        
         LA    RF,0(RF)                                                         
         ST    RF,DTFADDR          OPEN SYSTEM DISK FILES                       
         GOTO1 VDATAMGR,DMCB,DMOPEN,TALENT,TALFILES,IO                          
*                                                                               
         OC    VT00A88,VT00A88     ALREADY LOADED?                              
         JNZ   YES                                                              
*                                                                               
POPEN10  GOTO1 =V(LOADER),DMCB,=CL8'T00A88',0                                   
         ICM   RE,15,4(R1)                 GET PHASE ADDRESS                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RE,VT00A88                                                       
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* DXMODE = DXCLOSEQ - CLOSE TALENT SYSTEM FILES                       *         
***********************************************************************         
                                                                                
PROCCLOS NTR1  ,                                                                
         L     RE,VUTL                                                          
         MVC   4(1,RE),DXSENUM                                                  
*                                                                               
         GOTO1 VDATAMGR,DMCB,DMCLSE,TALENT,0,IO                                 
         CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
TALENT   DC    CL8'TALENT'                                                      
TALFILES DC    C'NTALDIR NTALFIL NCHKDIR NCHKFIL '                              
         DC    C'NGENDIR NGENFIL NCTFILE X'                                     
*                                                                               
VUTL     DC    V(UTL)                                                           
         EJECT                                                                  
***********************************************************************         
* PROCESS TALENT FILE DATA IN LOAD MODE                               *         
***********************************************************************         
                                                                                
PROCLOAD NTR1  ,                                                                
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET UP RECORD TYPE TABLE DATA                
         L     RF,TYPEALOD         CALL EXTRACT LOAD ROUTINE                    
         BASR  RE,RF                                                            
         JNE   NO                  ERROR EXIT                                   
         L     RF,=A(LOADCNT)      RUN COUNT RECORD                             
         BASR  RE,RF                                                            
         J     YES                 EXIT OK                                      
         EJECT                                                                  
***********************************************************************         
* PROCESS TALENT FILE DATA IN UPDATE MODE READ RECOVERY FILES         *         
***********************************************************************         
                                                                                
PROCUPDT NTR1  ,                                                                
         L     R5,DXARECB          POINT TO RECOVERY RECORD BUFFER              
         USING RECDS,R5                                                         
         MVC   COMPANY,SXDTAGB     SET COMPANY CODE FROM SYSTEM TABLE           
         MVC   PRIALPHA,SXDTAGY    SET PRIMARY ALPHA FROM SYSTEM TABLE          
         MVC   TYPECODE,SXDTTYP                                                 
         GOTO1 AGETTYP             SET TYPE TABLE DATA                          
         CLI   RFILTY,TALFILQ      TEST TALFIL FILE RECORD TYPE                 
         JE    *+12                ELSE IGNORE RECORD                           
         CLI   RFILTY,CHKFILQ      TEST CHKFIL FILE RECORD TYPE                 
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
                                                                                
         USING RECDS,R5                                                         
         USING TLRCD,RECVHDR+L'RECVHDR                                          
PROCKEY  NTR1  ,                                                                
         GOTO1 ARECCMP             COMPARE COPY/CHANGE RECORDS                  
         JNE   NO                  NO CHANGES                                   
*                                                                               
         TM    TLRCSTAT,TALSDELT   IS THIS RECORD DELETED?                      
         BZ    PROCK10             NO                                           
         CLI   DXACTION,C'C'                                                    
         JNE   NO                                                               
         CLI   RRECTY,X'02'        IS THIS A CHANGE RECORD                      
         JNE   NO                  NO                                           
*                                                                               
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+TLRCSTAT-TLRCD+4(R4),TALSDELT                          
         JNZ   NO                  AVOID DELETED 'CHANGED' RECORDS              
         MVI   DXACTION,C'D'                                                    
         J     YES                                                              
*                                                                               
* TEST RESTORED RECORD USING SAVED RECOVERY COPY RECORD                         
*                                                                               
PROCK10  CLI   RRECTY,X'02'        WITH CHANGE RECOVERY RECORD TYPE             
         JNE   YES                                                              
         L     R4,DXACPYB                                                       
         TM    L'RECVHDR+TLRCSTAT-TLRCD+4(R4),TALSDELT                          
         JZ    YES                                                              
         MVI   DXACTION,C'A'                                                    
         J     YES                                                              
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUFFERIN FILE                                                       *         
***********************************************************************         
                                                                                
HIDBUF   BUFFD TYPE=D,                                                 X        
               KEYLEN=L'HIDREC,                                        X        
               COMLEN=L'HIDREC,                                        X        
               BUFFERS=20                                                       
         EJECT                                                                  
***********************************************************************         
* COMMON ADDRESSES FOR ALL ROUTINES - COVERED BY ADDRESSD DSECT       *         
* ADDRESS IS AT AADDRESS IN W/S                                       *         
***********************************************************************         
                                                                                
         DS    0L                                                               
ADDRESS  DC    CL8'EXTERNS'                                                     
         DC    V(DATAMGR)                                                       
         DC    V(DMOD000)                                                       
         DC    V(DADDS)                                                         
         DC    V(LOGIO)                                                         
         DC    V(DATCON)                                                        
         DC    V(BUFFERIN)                                                      
         DC    V(TOXCNVX)                                                       
*                                                                               
* TOXROUTS VCONS                                                                
         DC    V(TOXAGYC)          AGENCY RECORD                                
         DC    V(TOXCLIC)          CLIENT RECORD                                
         DC    V(TOXPRDC)          PRODUCT RECORD                               
         DC    V(TOXASSC)          ASSET RECORD                                 
         DC    V(TOXVERC)          VERSION RECORD                               
         DC    V(TOXALIC)          ALIAS RECORD                                 
         DC    V(TOXW4RC)          W-4 RECORD                                   
         DC    V(TOXPERC)          PERFORMER RECORD                             
         DC    V(TOXINVC)          INVOICE RECORD                               
         DC    V(TOXCGRC)          CLIENT GROUP                                 
         DC    V(TOXAGRC)          AGENCY GROUP                                 
         DC    V(TOXAREC)          AREA                                         
         DC    V(TOXPUSC)          PRINT USE                                    
         DC    V(TOXGUAC)          GUARANTEES                                   
         DC    V(TOXGUCC)          GUARANTEE CONTRACT                           
         DC    V(TOXGCCC)          GUARANTEE COMMENTS                           
         DC    V(TOXGCYC)          GUARANTEE CONTRACT YEAR TRACKING             
         DC    V(TOXVCLC)          VERSION COMMENTS                             
         DC    V(TOXAGTC)          AGENT                                        
         DC    V(TOXGTKC)          GUARANTEE TRACKING                           
         DC    V(TOXOFFC)          OFFICE                                       
         DC    V(TOXSTFF)          STAFF                                        
         DC    V(TOXCHKC)          CHECK                                        
         DC    V(TOXPGRC)          PRODUCT GROUP                                
         DC    V(TOXPTYC)          PRODUCT TYPE                                 
         DC    V(TOXMKTC)          MARKETS                                      
         DC    V(TOXUSEC)          USAGE HISTORY                                
         DC    V(TOXCTLC)          CONTROL RECORD                               
         DC    V(TOXEMPC)          EMPLOYER RECORD                              
         DC    V(TOXTIMC)          TIMESHEET DAY RECORD                         
         DC    V(TOXLIEC)          LIEN RECORD                                  
         DC    V(TOXDCC)           DUE COMPANY RECORD                           
         DC    V(TOX74C)           INTERNET NEWMEDIA                            
         DC    V(TOX76C)           COMMERCIAL POOL                              
         DC    V(TOX77C)           BILL TO                                      
         DC    V(TOX78C)           CONTRACT TYPE                                
         DC    V(TOX79C)           PUBLISHED MUSIC                              
         DC    V(TOX81C)           EPISODE                                      
         DC    V(TOX83C)           ECAST                                        
         DC    V(TOX84C)           FIXED CYCLE TRACKING                         
         DC    V(TOX85C)           USAGE HISTORY                                
         DC    V(TOX86C)           HISTORY COMMENT                              
* END OF TOXROUTS VCONS                                                         
*                                                                               
         DC    CL8'FOR_ALL'        COMMON ROUTINES USED BY ALL SUBS             
         DC    A(TALLOAD)          TALENT RECORD LOAD                           
         DC    A(CXLOAD)           CONTROL FILE LOAD                            
         DC    A(TALUPDT)          TALENT RECORD UPDATE                         
         DC    A(DECIOC)                                                        
         DC    A(CHKSEQIO)                                                      
         DC    A(GETTYP)                                                        
         DC    A(GETIT)                                                         
         DC    A(READHI)                                                        
         DC    A(READCHI)                                                       
         DC    A(RECCMP)                                                        
         DC    A(HIDBUF)           HOLDING FEE ID TABLE FOR BUFFERING           
         DC    A(COMFACS)          COMFACS                                      
         DC    A(0)                VT00A88 SET DYNAMICALLY                      
*                                                                               
* LOAD RECORD ROUTINE ADCONS                                                    
         DC    CL8'LOADING'        ADDRESSES OF LOAD ROUTINES                   
         DC    A(LOADALL)          ALL                                          
         DC    A(LOADAGY)          AGENCY                                       
         DC    A(LOADCLI)          CLIENT                                       
         DC    A(LOADPRD)          PRODUCT                                      
         DC    A(LOADASS)          ASSET                                        
         DC    A(LOADVER)          VERSION                                      
         DC    A(LOADALI)          ALIAS                                        
         DC    A(LOADW4R)          W-4                                          
         DC    A(LOADPER)          PERFORMER                                    
         DC    A(LOADINV)          INVOICE                                      
         DC    A(LOADMED)          MEDIA                                        
         DC    A(LOADAST)          ASSET TYPE                                   
         DC    A(LOADW4T)          W4 TYPE                                      
         DC    A(LOADPEC)          PERFORMER CATEGORY                           
         DC    A(LOADUST)          USE TYPE                                     
         DC    A(LOADCGR)          CLIENT GROUP                                 
         DC    A(LOADUSS)          USE SUB TYPE                                 
         DC    A(LOADACT)          ACTRA TYPE                                   
         DC    A(LOADAGR)          AGENCY GROUP                                 
         DC    A(LOADARE)          AREA                                         
         DC    A(LOADPUS)          PRINT USE                                    
         DC    A(LOADGUA)          GUARANTEE                                    
         DC    A(LOADGUC)          GUARANTEE CONTRACT                           
         DC    A(LOADGCC)          GUARANTEE COMMENTS                           
         DC    A(LOADGCY)          GUARANTEE CONTRACT YEAR TRACKING             
         DC    A(LOADAGT)          AGENT                                        
         DC    A(LOADGTK)          GUARANTEE TRACKING                           
         DC    A(LOADVCL)          VERSION COMMENTS                             
         DC    A(LOADOFF)          OFFICE                                       
         DC    A(LOADCNT)          RECORD COUNT RECORD                          
         DC    A(LOADSTF)          STAFF                                        
         DC    A(LOADCHK)          CHECK                                        
         DC    A(LOADPGR)          PRODUCT GROUP                                
         DC    A(LOADPTY)          PRODUCT TYPE                                 
         DC    A(LOADMKT)          MARKETS                                      
         DC    A(LOADUSE)          USAGE HISTORY                                
         DC    A(LOADNET)          NETWORK                                      
         DC    A(LOADCTL)          CONTROL                                      
         DC    A(LOADEMP)          EMPLOYER                                     
         DC    A(LOADTIM)          TIMESHEET DAY                                
         DC    A(LOADLIE)          LIEN                                         
         DC    A(LOADDC)           DUE COMPANY                                  
         DC    A(LOAD74)           INTERNET NEWMEDIA                            
         DC    A(LOAD76)           COMMERCIAL POOL                              
         DC    A(LOAD77)           BILLTO                                       
         DC    A(LOAD78)           CONTRACT TYPE                                
         DC    A(LOAD79)           PUBLISHED MUSIC                              
         DC    A(LOAD81)           EPISODE                                      
         DC    A(LOAD83)           ECAST                                        
         DC    A(LOAD84)           FIXED CYCLE TRACKING                         
         DC    A(LOAD85)           USAGE HISTORY                                
         DC    A(LOAD86)           HISTORY COMMENT                              
* END OF LOAD ROUTINE ADCONS                                                    
*                                                                               
* UPDATE RECORD ROUTINE ADCONS                                                  
         DC    CL8'UPDTING'                                                     
         DC    A(UPDTALL)          ALL                                          
         DC    A(UPDTAGY)          AGENCY                                       
         DC    A(UPDTCLI)          CLIENT                                       
         DC    A(UPDTPRD)          PRODUCT                                      
         DC    A(UPDTASS)          ASSET                                        
         DC    A(UPDTVER)          VERSION                                      
         DC    A(UPDTALI)          ALIAS                                        
         DC    A(UPDTW4R)          W-4                                          
         DC    A(UPDTPER)          PERFORMER                                    
         DC    A(UPDTINV)          INVOICE                                      
         DC    A(0)                MEDIA                                        
         DC    A(0)                ASSET TYPE                                   
         DC    A(0)                W4 TYPE                                      
         DC    A(0)                PERFORMER CATEGORY                           
         DC    A(0)                USE TYPE                                     
         DC    A(UPDTCGR)          CLIENT GROUP                                 
         DC    A(0)                USE SUB TYPE                                 
         DC    A(0)                ACTRA TYPE                                   
         DC    A(UPDTAGR)          AGENCY GROUP                                 
         DC    A(UPDTARE)          AREA                                         
         DC    A(UPDTPUS)          PRINT USE                                    
         DC    A(UPDTGUA)          GUARANTEE                                    
         DC    A(UPDTGUC)          GUARANTEE CONTRACT                           
         DC    A(UPDTGCC)          GUARANTEE COMMENTS                           
         DC    A(UPDTGCY)          GUARANTEE CONTRACT YEAR TRACKING             
         DC    A(UPDTAGT)          AGENT                                        
         DC    A(UPDTGTK)          GUARANTEE TRACKING                           
         DC    A(UPDTVCL)          VERSION COMMENTS                             
         DC    A(UPDTOFF)          OFFICE                                       
         DC    A(UPDTSTF)          STAFF                                        
         DC    A(UPDTCHK)          CHECK                                        
         DC    A(UPDTPGR)          PRODUCT GROUP                                
         DC    A(UPDTPTY)          PRODUCT TYPE                                 
         DC    A(UPDTMKT)          MARKETS                                      
         DC    A(UPDTUSE)          USAGE HISTORY                                
         DC    A(UPDTCTL)          CONTROL                                      
         DC    A(UPDTEMP)          EMPLOYER                                     
         DC    A(UPDTTIM)          TIMESHEET DAY                                
         DC    A(UPDTLIE)          LIEN                                         
         DC    A(UPDTDC)           DUE COMPANY                                  
         DC    A(UPDT74)           INTERNET NEWMEDIA                            
         DC    A(UPDT76)           COMMERCIAL POOL                              
         DC    A(UPDT77)           BILLTO                                       
         DC    A(0)                CONTRACT TYPE                                
         DC    A(UPDT79)           PUBLISHED MUSIC                              
         DC    A(UPDT81)           EPISODE                                      
         DC    A(UPDT83)           ECAST                                        
         DC    A(UPDT84)           FIXED CYCLE TRACKING                         
         DC    A(UPDT85)           USAGE HISTORY                                
         DC    A(UPDT86)           HISTORY COMMENT                              
         DC    A(0)                RECORD COUNT RECORD                          
* END OF UPDATE ROUTINE ADCONS                                                  
*                                                                               
* FILTER ROUTINE ADCONS                                                         
         DC    CL8'FILTERS'                                                     
         DC    A(FILTAGY)          AGENCY                                       
         DC    A(FILTCLI)          CLIENT                                       
         DC    A(FILTPRD)          PRODUCT                                      
         DC    A(FILTASS)          ASSET                                        
         DC    A(FILTVER)          VERSION                                      
         DC    A(FILTALI)          ALIAS                                        
         DC    A(FILTW4R)          W-4                                          
         DC    A(FILTPER)          PERFORMER                                    
         DC    A(FILTINV)          INVOICE                                      
         DC    A(0)                MEDIA                                        
         DC    A(0)                ASSET TYPE                                   
         DC    A(0)                W4 TYPE                                      
         DC    A(0)                PERFORMER CATEGORY                           
         DC    A(0)                USE TYPE                                     
         DC    A(FILTCGR)          CLIENT GROUP                                 
         DC    A(0)                USE SUB TYPE                                 
         DC    A(0)                ACTRA TYPE                                   
         DC    A(FILTAGR)          AGENCY GROUP                                 
         DC    A(FILTARE)          AREA                                         
         DC    A(FILTPUS)          PRINT USE                                    
         DC    A(FILTGUA)          GUARANTEE                                    
         DC    A(FILTGUC)          GUARANTEE CONTRACT                           
         DC    A(FILTGCC)          GUARANTEE COMMENTS                           
         DC    A(FILTGCY)          GUARANTEE CONTRACT YEAR TRACKING             
         DC    A(FILTAGT)          AGENT                                        
         DC    A(FILTGTK)          GUARANTEE TRACKING                           
         DC    A(FILTVCL)          VERSION COMMENTS                             
         DC    A(FILTOFF)          OFFICE                                       
         DC    A(FILTSTF)          STAFF                                        
         DC    A(FILTCHK)          CHECK                                        
         DC    A(FILTPGR)          PRODUCT GROUP                                
         DC    A(FILTPTY)          PRODUCT TYPE                                 
         DC    A(FILTMKT)          MARKETS                                      
         DC    A(FILTUSE)          USAGE HISTORY                                
         DC    A(FILTCTL)          CONTROL                                      
         DC    A(FILTEMP)          EMPLOYER                                     
         DC    A(FILTTIM)          TIMESHEET DAY                                
         DC    A(FILTLIE)          LIEN                                         
         DC    A(FILTDC)           DUE COMPANY                                  
         DC    A(FILT74)           INTERNET NEWMEDIA                            
         DC    A(FILT76)           COMMERCIAL POOL                              
         DC    A(FILT77)           BILLTO                                       
         DC    A(0)                CONTRACT TYPE                                
         DC    A(FILT79)           PUBLISHED MUSIC                              
         DC    A(FILT81)           EPISODE                                      
         DC    A(FILT83)           ECAST                                        
         DC    A(FILT84)           FIXED CYCLE TRACKING                         
         DC    A(FILT85)           USAGE HISTORY                                
         DC    A(FILT86)           HISTORY COMMENT                              
         DC    A(0)                RECORD COUNT RECORD                          
* END OF FILTER ROUTINE ADCONS                                                  
*                                                                               
* INITIALIZATION ROUTINE ADCONS                                                 
         DC    CL8'INITS'                                                       
         DC    A(INITALL)          ALL                                          
         DC    A(INITAGY)          AGENCY                                       
         DC    A(INITCLI)          CLIENT                                       
         DC    A(INITPRD)          PRODUCT                                      
         DC    A(INITASS)          ASSET                                        
         DC    A(INITVER)          VERSION                                      
         DC    A(INITALI)          ALIAS                                        
         DC    A(INITW4R)          W-4                                          
         DC    A(INITPER)          PERFORMER                                    
         DC    A(INITINV)          INVOICE                                      
         DC    A(INITMED)          MEDIA                                        
         DC    A(INITAST)          ASSET TYPE                                   
         DC    A(INITW4T)          W4 TYPE                                      
         DC    A(INITPEC)          PERFORMER CATEGORY                           
         DC    A(INITUST)          USE TYPE                                     
         DC    A(INITCGR)          CLIENT GROUP                                 
         DC    A(INITUSS)          USE SUB TYPE                                 
         DC    A(INITACT)          ACTRA TYPE                                   
         DC    A(INITAGR)          AGENCY GROUP                                 
         DC    A(INITARE)          AREA                                         
         DC    A(INITPUS)          PRINT USE                                    
         DC    A(INITGUA)          GUARANTEE                                    
         DC    A(INITGUC)          GUARANTEE CONTRACT                           
         DC    A(INITGCC)          GUARANTEE COMMENTS                           
         DC    A(INITGCY)          GUARANTEE CONTRACT YEAR TRACKING             
         DC    A(INITAGT)          AGENT                                        
         DC    A(INITGTK)          GUARANTEE TRACKING                           
         DC    A(INITVCL)          VERSION COMMENTS                             
         DC    A(INITOFF)          OFFICE                                       
         DC    A(INITCNT)          RECORD COUNT RECORD                          
         DC    A(INITSTF)          STAFF                                        
         DC    A(INITCHK)          CHECK                                        
         DC    A(INITPGR)          PRODUCT GROUP                                
         DC    A(INITPTY)          PRODUCT TYPE                                 
         DC    A(INITMKT)          MARKETS                                      
         DC    A(INITUSE)          USAGE HISTORY                                
         DC    A(INITNET)          NETWORK                                      
         DC    A(INITCTL)          CONTROL                                      
         DC    A(INITEMP)          EMPLOYER                                     
         DC    A(INITTIM)          TIMESHEET DAY                                
         DC    A(INITLIE)          LIEN                                         
         DC    A(INITDC)           DUE COMPANY                                  
         DC    A(INIT74)           INTERNET NEWMEDIA                            
         DC    A(INIT76)           COMMERCIAL POOL                              
         DC    A(INIT77)           BILLTO                                       
         DC    A(INIT78)           CONTRACT TYPE                                
         DC    A(INIT79)           PUBLISHED MUSIC                              
         DC    A(INIT81)           EPISODE                                      
         DC    A(INIT83)           ECAST                                        
         DC    A(INIT84)           FIXED CYCLE TRACKING                         
         DC    A(INIT85)           USAGE HISTORY                                
         DC    A(INIT86)           HISTORY COMMENT                              
* END OF INITIALIZATION ADCONS                                                  
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
         DC    C'TALDIR '                                                       
         DC    C'TALFIL '                                                       
         DC    C'CHKDIR '                                                       
         DC    C'CHKFIL '                                                       
         DC    F'0'                                                             
         DC    F'0'                                                             
         DC    A(COPYBUFF)                                                      
         DC    80C' '                                                           
*                                                                               
COMFACS  DS    0D                                                               
       ++INCLUDE DDCOMFACSC                                                     
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EQUATES                                                             *         
***********************************************************************         
                                                                                
TALDIRQ  EQU   X'71'                                                            
TALFILQ  EQU   X'72'                                                            
CHKDIRQ  EQU   X'75'                                                            
CHKFILQ  EQU   X'76'                                                            
MXTRTQ   EQU   X'5E'               FIELD SEPERATOR CHR                          
FF       EQU   X'FF'                                                            
TALSDELT EQU   X'80'               RECORD IS DELETED                            
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
         DC    CL3'AGY',AL1(00,00,00,00,00),AL4(LOADAGY,UPDTAGY)                
         DC    CL3'CLI',AL1(00,00,00,00,00),AL4(LOADCLI,UPDTCLI)                
         DC    CL3'PRD',AL1(00,00,00,00,00),AL4(LOADPRD,UPDTPRD)                
         DC    CL3'ASS',AL1(00,00,00,00,00),AL4(LOADASS,UPDTASS)                
         DC    CL3'VER',AL1(00,00,00,00,00),AL4(LOADVER,UPDTVER)                
         DC    CL3'ALI',AL1(00,00,00,00,00),AL4(LOADALI,UPDTALI)                
         DC    CL3'W4R',AL1(00,00,00,00,00),AL4(LOADW4R,UPDTW4R)                
         DC    CL3'PER',AL1(00,00,00,00,00),AL4(LOADPER,UPDTPER)                
         DC    CL3'INV',AL1(00,00,00,00,00),AL4(LOADINV,UPDTINV)                
         DC    CL3'MED',AL1(00,00,00,00,00),AL4(LOADMED,0)                      
         DC    CL3'AST',AL1(00,00,00,00,00),AL4(LOADAST,0)                      
         DC    CL3'W4T',AL1(00,00,00,00,00),AL4(LOADW4T,0)                      
         DC    CL3'PEC',AL1(00,00,00,00,00),AL4(LOADPEC,0)                      
         DC    CL3'UST',AL1(00,00,00,00,00),AL4(LOADUST,0)                      
         DC    CL3'CGR',AL1(00,00,00,00,00),AL4(LOADCGR,UPDTCGR)                
         DC    CL3'USS',AL1(00,00,00,00,00),AL4(LOADUSS,0)                      
         DC    CL3'ACT',AL1(00,00,00,00,00),AL4(LOADACT,0)                      
         DC    CL3'AGR',AL1(00,00,00,00,00),AL4(LOADAGR,UPDTAGR)                
         DC    CL3'ARE',AL1(00,00,00,00,00),AL4(LOADARE,UPDTARE)                
         DC    CL3'PUS',AL1(00,00,00,00,00),AL4(LOADPUS,UPDTPUS)                
         DC    CL3'GUA',AL1(00,00,00,00,00),AL4(LOADGUA,UPDTGUA)                
         DC    CL3'GUC',AL1(00,00,00,00,00),AL4(LOADGUC,UPDTGUC)                
         DC    CL3'GCC',AL1(00,00,00,00,00),AL4(LOADGCC,UPDTGCC)                
         DC    CL3'GCY',AL1(00,00,00,00,00),AL4(LOADGCY,UPDTGCY)                
         DC    CL3'AGT',AL1(00,00,00,00,00),AL4(LOADAGT,UPDTAGT)                
         DC    CL3'GTK',AL1(00,00,00,00,00),AL4(LOADGTK,UPDTGTK)                
         DC    CL3'VCL',AL1(00,00,00,00,00),AL4(LOADVCL,UPDTVCL)                
         DC    CL3'OFF',AL1(00,00,00,00,00),AL4(LOADOFF,UPDTOFF)                
         DC    CL3'STF',AL1(00,00,00,00,00),AL4(LOADSTF,UPDTSTF)                
         DC    CL3'CHK',AL1(00,00,00,00,00),AL4(LOADCHK,UPDTCHK)                
         DC    CL3'PGR',AL1(00,00,00,00,00),AL4(LOADPGR,UPDTPGR)                
         DC    CL3'PTY',AL1(00,00,00,00,00),AL4(LOADPTY,UPDTPTY)                
         DC    CL3'MKT',AL1(00,00,00,00,00),AL4(LOADMKT,UPDTMKT)                
         DC    CL3'USE',AL1(00,00,00,00,00),AL4(LOADUSE,UPDTUSE)                
         DC    CL3'NET',AL1(00,00,00,00,00),AL4(LOADNET,0)                      
         DC    CL3'CTL',AL1(00,00,00,00,00),AL4(LOADCTL,UPDTCTL)                
         DC    CL3'EMP',AL1(00,00,00,00,00),AL4(LOADEMP,UPDTEMP)                
         DC    CL3'TIM',AL1(00,00,00,00,00),AL4(LOADTIM,UPDTTIM)                
         DC    CL3'LIE',AL1(00,00,00,00,00),AL4(LOADLIE,UPDTLIE)                
         DC    CL3'DUE',AL1(00,00,00,00,00),AL4(LOADDC,UPDTDC)                  
* NEWMEDIA                                                                      
         DC    CL3'NEW',AL1(00,00,00,00,00),AL4(LOAD74,UPDT74)                  
* COMMERCIAL POOL                                                               
         DC    CL3'CPL',AL1(00,00,00,00,00),AL4(LOAD76,UPDT76)                  
* BILL TO                                                                       
         DC    CL3'BTO',AL1(00,00,00,00,00),AL4(LOAD77,UPDT77)                  
* CONTRACT TYPE                                                                 
         DC    CL3'CTY',AL1(00,00,00,00,00),AL4(LOAD78,0)                       
* PUBLISHED MUSIC                                                               
         DC    CL3'PMU',AL1(00,00,00,00,00),AL4(LOAD79,UPDT79)                  
* EPISODE                                                                       
         DC    CL3'EPI',AL1(00,00,00,00,00),AL4(LOAD81,UPDT81)                  
* ECAST                                                                         
         DC    CL3'ECA',AL1(00,00,00,00,00),AL4(LOAD83,UPDT83)                  
* FIXED CYCLE TRACKING                                                          
         DC    CL3'FCT',AL1(00,00,00,00,00),AL4(LOAD84,UPDT84)                  
* USAGE HISTORY                                                                 
         DC    CL3'USH',AL1(00,00,00,00,00),AL4(LOAD85,UPDT85)                  
* HISTORY COMMENT                                                               
         DC    CL3'HSC',AL1(00,00,00,00,00),AL4(LOAD86,UPDT86)                  
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DON'T WORRY ABOUT THE FIELDS ON THIS PAGE                           *         
***********************************************************************         
                                                                                
         DS    0D                                                               
* ++INCLUDE FASSBOFF                                                            
       ++INCLUDE FASSBOFF                                                       
         PRINT ON                                                               
         ORG   SSOOFF                                                           
SSB      DC    XL(SSOOFFX-SSOOFF)'00'                                           
         ORG   SSOXTND                                                          
         DC    X'FF'               SET EXTENDED OFFLINE SSB                     
         ORG                                                                    
SSBL     EQU   *-SSB                                                            
         DROP  RB,R8                                                            
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
                                                                                
         LTORG                                                                  
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
         DC    C'IO COUNT EXCEEDED - TYPECODE = '                               
DECTYPE  DC    CL3' '                                                           
         DS    XL2                                                              
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO GET A RECORD                                           *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       TALADDR      = DISK ADDRESS TO READ                           *         
* EXIT: CC EQUAL     = RECORD READ OK                                 *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
                                                                                
GETIT    NTR1  BASE=*,LABEL=*                                                   
         CLC   TYPECODE,=C'CHK'                                                 
         BNE   GETITA                                                           
         GOTO1 VDATAMGR,DMCB,(X'00',GETREC),CHKFIL,TALADDR,(R2),DMWORK          
         B     GETITB                                                           
GETITA   GOTO1 VDATAMGR,DMCB,(X'00',GETREC),TALFIL,TALADDR,(R2),DMWORK          
GETITB   CLI   8(R1),0                                                          
         JE    YES                                                              
*                                                                               
         GOTO1 =V(HEXOUT),PARM,TALADDR,GETDA,L'TALADDR                          
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
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL DMGR TO PERFORM A READCHI                                      *         
* NTRY: R2           = A(RECORD BUFFER)                               *         
*       IOKEY        = KEY TO READ HIGH FOR                           *         
* EXIT: CC EQUAL     = RECORD READ OK OR EOF SET                      *         
*       CC NOT EQUAL = ERROR ON READ                                  *         
***********************************************************************         
                                                                                
READCHI  NTR1  BASE=*,LABEL=*                                                   
         MVC   IOKEYSAV,IOKEY                                                   
         GOTO1 VDATAMGR,DMCB,DMRDHI,CTFILE,IOKEY,(R2)                           
         CLI   8(R1),0                                                          
         JE    YES                                                              
         TM    8(R1),X'80'                                                      
         JO    YES                                                              
         J     NO                                                               
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
         CLC   TYPECODE,=C'CHK'                                                 
         BNE   RDHA                                                             
         GOTO1 VDATAMGR,DMCB,DMRDHI,CHKDIR,IOKEY,(R2),DMWORK                    
         B     RDHB                                                             
RDHA     GOTO1 VDATAMGR,DMCB,DMRDHI,TALDIR,IOKEY,(R2),DMWORK                    
RDHB     CLI   8(R1),0                                                          
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
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ALL EXTRACT RECORDS                                      *         
* NTRY: R1 = LENGTH OF EXTRACT RECORD                                 *         
***********************************************************************         
                                                                                
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
         CLI   DXACTION,C'L'                                                    
         JNE   *+8                                                              
         MVI   DXHDRRTY,C'A'                                                    
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
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ALL RECORD DATA FOR TALENT                                    *          
***********************************************************************         
                                                                                
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
         DC    CL3'AGY',AL1(0),AL4(LOADAGY) AGENCY RECORDS                      
         DC    CL3'CLI',AL1(0),AL4(LOADCLI) CLIENT RECORDS                      
         DC    CL3'PRD',AL1(0),AL4(LOADPRD) PRODUCT RECORDS                     
         DC    CL3'ASS',AL1(0),AL4(LOADASS) ASSET RECORDS                       
         DC    CL3'VER',AL1(0),AL4(LOADVER) VERSION RECORDS                     
         DC    CL3'ALI',AL1(0),AL4(LOADALI) ALIAS RECORDS                       
         DC    CL3'W4R',AL1(0),AL4(LOADW4R) W-4 RECORDS                         
         DC    CL3'PER',AL1(0),AL4(LOADPER) PERFORMER                           
*        DC    CL3'INV',AL1(0),AL4(LOADINV) INVOICE RECORDS                     
         DC    CL3'MED',AL1(0),AL4(LOADMED) MEDIA RECORDS                       
         DC    CL3'AST',AL1(0),AL4(LOADAST) ASSET TYPE                          
         DC    CL3'W4T',AL1(0),AL4(LOADW4T) W4 TYPE                             
         DC    CL3'PEC',AL1(0),AL4(LOADPEC) PERFORMER CATEGORY                  
         DC    CL3'UST',AL1(0),AL4(LOADUST) USE TYPE                            
         DC    CL3'CGR',AL1(0),AL4(LOADCGR) CLIENT GROUP                        
         DC    CL3'USS',AL1(0),AL4(LOADUSS) USE SUBTYPE                         
         DC    CL3'ACT',AL1(0),AL4(LOADACT) ACTRA TYPE                          
         DC    CL3'AGR',AL1(0),AL4(LOADAGR) AGENCY GROUP                        
         DC    CL3'ARE',AL1(0),AL4(LOADARE) AREA                                
         DC    CL3'PUS',AL1(0),AL4(LOADPUS) PRINT USE                           
         DC    CL3'GUA',AL1(0),AL4(LOADGUA) GUARANTEE                           
         DC    CL3'GUC',AL1(0),AL4(LOADGUC) GUARANTEE CONTRACT                  
         DC    CL3'GCC',AL1(0),AL4(LOADGCC) GUARANTEE COMMENTS                  
         DC    CL3'GCY',AL1(0),AL4(LOADGCY) GUARANTEE COMMENTS                  
         DC    CL3'AGT',AL1(0),AL4(LOADAGT) AGENT                               
         DC    CL3'GTK',AL1(0),AL4(LOADGTK) GUARANTEE TRACKING                  
         DC    CL3'VCL',AL1(0),AL4(LOADVCL) COMMENTS                            
         DC    CL3'OFF',AL1(0),AL4(LOADOFF) OFFICE                              
         DC    CL3'STF',AL1(0),AL4(LOADSTF) STAFF                               
         DC    CL3'PGR',AL1(0),AL4(LOADPGR) PRODUCT GROUP                       
         DC    CL3'PTY',AL1(0),AL4(LOADPTY) PRODUCT TYPE                        
         DC    CL3'MKT',AL1(0),AL4(LOADMKT) MARKETS                             
*        DC    CL3'USE',AL1(0),AL4(LOADUSE) USAGE HISTORY                       
         DC    CL3'NET',AL1(0),AL4(LOADNET) NETWORK                             
         DC    CL3'CTL',AL1(0),AL4(LOADCTL) CONTROL                             
         DC    CL3'EMP',AL1(0),AL4(LOADEMP) EMPLOYER                            
         DC    CL3'LIE',AL1(0),AL4(LOADLIE) LIEN                                
         DC    CL3'DUE',AL1(0),AL4(LOADDC)  DUE COMPANY                         
         DC    CL3'NEW',AL1(0),AL4(LOAD74)  NEW MEDIA                           
         DC    CL3'CPL',AL1(0),AL4(LOAD76)  COMMERCIAL POOL                     
         DC    CL3'BTO',AL1(0),AL4(LOAD77)  BILL TO                             
         DC    CL3'CTY',AL1(0),AL4(LOAD78)  CONTRACT TYPE                       
         DC    CL3'PMU',AL1(0),AL4(LOAD79)  PUBLISHED MUSIC                     
         DC    CL3'EPI',AL1(0),AL4(LOAD81)  EPISODE                             
         DC    CL3'ECA',AL1(0),AL4(LOAD83)  ECAST                               
         DC    CL3'FCT',AL1(0),AL4(LOAD84)  FIXED CYCLE TRACKING                
         DC    CL3'USH',AL1(0),AL4(LOAD85)  USAGE HISTORY                       
         DC    CL3'HSC',AL1(0),AL4(LOAD86)  HISTORY COMMENT                     
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALL RECORD DATA                                              *         
***********************************************************************         
                                                                                
UPDTALL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,UPDTTAB                                                       
*                                                                               
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
         DC    CL3'AGY',AL1(0),AL4(UPDTAGY) AGENCY RECORDS                      
         DC    CL3'CLI',AL1(0),AL4(UPDTCLI) CLIENT RECORDS                      
         DC    CL3'PRD',AL1(0),AL4(UPDTPRD) PRODUCT RECORDS                     
         DC    CL3'ASS',AL1(0),AL4(UPDTASS) ASSET RECORDS                       
         DC    CL3'VER',AL1(0),AL4(UPDTVER) VERSION RECORDS                     
         DC    CL3'ALI',AL1(0),AL4(UPDTALI) ALIAS RECORDS                       
         DC    CL3'W4R',AL1(0),AL4(UPDTW4R) W-4 RECORDS                         
         DC    CL3'PER',AL1(0),AL4(UPDTPER) PERFORMER                           
         DC    CL3'INV',AL1(0),AL4(UPDTINV) INVOICE RECORDS                     
         DC    CL3'CGR',AL1(0),AL4(UPDTCGR) CLIENT GROUP RECORD                 
         DC    CL3'AGR',AL1(0),AL4(UPDTAGR) AGENCY GROUP RECORD                 
         DC    CL3'ARE',AL1(0),AL4(UPDTARE) AREA RECORD                         
         DC    CL3'PUS',AL1(0),AL4(UPDTPUS) PRINT USE                           
         DC    CL3'GUA',AL1(0),AL4(UPDTGUA) GUARANTEE                           
         DC    CL3'GUC',AL1(0),AL4(UPDTGUC) GUARANTEE CONTRACT                  
         DC    CL3'GCC',AL1(0),AL4(UPDTGCC) GUARANTEE COMMENTS                  
         DC    CL3'GCY',AL1(0),AL4(UPDTGCY) GUARANTEE COMMENTS                  
         DC    CL3'AGT',AL1(0),AL4(UPDTAGT) AGENT                               
         DC    CL3'GTK',AL1(0),AL4(UPDTGTK) GUARANTEE TRACKING                  
         DC    CL3'VCL',AL1(0),AL4(UPDTVCL) COMMENTS                            
         DC    CL3'OFF',AL1(0),AL4(UPDTOFF) OFFICE                              
         DC    CL3'STF',AL1(0),AL4(UPDTSTF) STAFF RECORD                        
         DC    CL3'CHK',AL1(0),AL4(UPDTCHK) CHECK                               
         DC    CL3'PGR',AL1(0),AL4(UPDTPGR) PRODUCT GROUP                       
         DC    CL3'PTY',AL1(0),AL4(UPDTPTY) PRODUCT TYPE                        
         DC    CL3'MKT',AL1(0),AL4(UPDTMKT) MARKETS                             
         DC    CL3'USE',AL1(0),AL4(UPDTUSE) USAGE HISTORY                       
         DC    CL3'CTL',AL1(0),AL4(UPDTCTL) CONTROL                             
         DC    CL3'EMP',AL1(0),AL4(UPDTEMP) EMPLOYER                            
         DC    CL3'TIM',AL1(0),AL4(UPDTTIM) TIMESHEET DAY                       
         DC    CL3'LIE',AL1(0),AL4(UPDTLIE) LIEN                                
         DC    CL3'DUE',AL1(0),AL4(UPDTDC)  DUE COMPANY                         
         DC    CL3'FCT',AL1(0),AL4(UPDT84)  FIXED CYCLE TRACKING                
         DC    CL3'USH',AL1(0),AL4(UPDT85)  USAGE HISTORY                       
         DC    CL3'HSC',AL1(0),AL4(UPDT86)  HISTORY COMMENT                     
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY RECORD AND SUBSETS                                      *         
*      AGENCY RECORD (05000)                                          *         
***********************************************************************         
                                                                                
LOADAGY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLAYD,R2                                                         
         XC    TLAYKEY,TLAYKEY     CLEAR FILL THE KEY                           
         MVI   TLAYCD,TLAYCDQ      RECORD 10 FOR AGENCY                         
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGY10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LAGYX                                                            
         CLC   TLAYCD(TLAYCD+L'TLAYCD-TLAYD),IOKEY                              
         JNE   LAGYX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTAGY            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LAGY40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITAGY            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXAGYC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LAGY20   GOTO1 VTOXAGYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LAGY40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LAGY40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LAGY40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LAGY30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LAGY30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LAGY20              TOO MANY IOS                                 
         J     LAGYX                                                            
*                                                                               
LAGY40   MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LAGY50                                                           
         GOTO1 AREADHI                                                          
         JE    LAGY50                                                           
         DC    H'0'                                                             
*                                                                               
LAGY50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGY10                                                           
*                                                                               
LAGYX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY RECORD AND SUBSETS                                    *         
*        AGENCY RECORD (05000)                                        *         
***********************************************************************         
                                                                                
UPDTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLAYD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTAGY                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGY                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXAGYC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UAGY10   GOTO1 VTOXAGYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UAGYX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UAGYX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UAGY20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UAGY20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UAGY40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UAGY40                                                           
*                                                                               
UAGY30   CLI   COPYFLAG,X'01'                                                   
         JNE   UAGY10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UAGY10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UAGY40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UAGY30                                                           
*                                                                               
UAGYX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AGENCY RECORD AT R2                                          *         
***********************************************************************         
                                                                                
FILTAGY  NTR1  BASE=*,LABEL=*                                                   
         USING TLAYD,R2                                                         
         CLI   TLAYCD,TLAYCDQ      AGENCY RECORD?                               
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENCY RECORD                                            *         
***********************************************************************         
                                                                                
INITAGY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX00LEN          R1=L'AGY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT RECORD (05001)                                          *         
***********************************************************************         
                                                                                
LOADCLI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CLI RECORD             
         USING TLCLD,R2                                                         
         XC    TLCLKEY,TLCLKEY                                                  
         MVI   TLCLCD,TLCLCDQ      20 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCLI10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLCLKEY(TLCLCD+L'TLCLCD-TLCLD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTOXCLIC,AINITCLI,AFILTCLI,VTOXCNVX                
         JNE   NO                                                               
         AP    PK01CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCLI10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CLIENT RECORD DATA (05001)                                   *         
***********************************************************************         
                                                                                
UPDTCLI  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCLD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCLI                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCLI                                                         
         GOTO1 ATALUPDT,DMCB,VTOXCLIC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CLIENT RECORD (05001)                                        *         
***********************************************************************         
                                                                                
FILTCLI  NTR1  BASE=*,LABEL=*                                                   
         USING TLCLD,R2                                                         
         CLI   TLCLCD,TLCLCDQ      X'20'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CLIENT RECORD (05001)                                    *         
***********************************************************************         
                                                                                
INITCLI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX01LEN          R1=L'CLI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT RECORD (05002)                                         *         
***********************************************************************         
                                                                                
LOADPRD  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PRD RECORD             
         USING TLPRD,R2                                                         
         XC    TLPRKEY,TLPRKEY                                                  
         MVI   TLPRCD,TLPRCDQ      30 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPRD10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLPRKEY(TLPRCD+L'TLPRCD-TLPRD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTOXPRDC,AINITPRD,AFILTPRD,VTOXCNVX                
         JNE   NO                                                               
         AP    PK02CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPRD10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT RECORD DATA (05002)                                  *         
***********************************************************************         
                                                                                
UPDTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLPRD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPRD                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPRD                                                         
         GOTO1 ATALUPDT,DMCB,VTOXPRDC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCT RECORD (05002)                                       *         
***********************************************************************         
                                                                                
FILTPRD  NTR1  BASE=*,LABEL=*                                                   
         USING TLPRD,R2                                                         
         CLI   TLPRCD,TLPRCDQ      X'30'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRODUCT RECORD (05002)                                   *         
***********************************************************************         
                                                                                
INITPRD  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX02LEN          R1=L'PRD RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ASSET RECORD AND SUBSETS                                       *         
*      ASSET RECORD (05003)                                           *         
*      TRACK RECORD (05011)                                           *         
***********************************************************************         
                                                                                
LOADASS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         XC    SVVALS(SVVALLNQ),SVVALS                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST ASS RECORD             
         USING TLCOPD,R2                                                        
         XC    TLCOPKEY,TLCOPKEY   CLEAR FILL THE KEY                           
         MVI   TLCOPCD,TLCOECDQ    RECORD 5D FOR ASSET                          
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LASS10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LASSX                                                            
         CLC   TLCOPCD(TLCOPCD+L'TLCOPCD-TLCOPD),IOKEY                          
         JNE   LASSX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   SVCOKEY,0(R2)                                                    
*                                                                               
         GOTO1 AFILTASS            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LASS40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         USING TLCOD,R2                                                         
         MVC   SVICOM,TLCOCOM      SAVE INTERNAL COMMERCIAL NUMBER              
*                                                                               
         GOTO1 AINITASS            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXASSC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LASS20   GOTO1 VTOXASSC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LASS40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LASS40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LASS40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LASS30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LASS30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LASS20              TOO MANY IOS                                 
         J     LASSX                                                            
*                                                                               
LASS40   MVC   IOKEY(L'TLCOPKEY),SVCOKEY   READ NEXT RECORD - SEQUENT           
         GOTO1 ACHKSEQ                                                          
         JE    LASS50                                                           
         GOTO1 AREADHI                                                          
         JE    LASS50                                                           
         DC    H'0'                                                             
*                                                                               
LASS50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LASS10                                                           
*                                                                               
LASSX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ASSET RECORD AND SUBSETS                                     *         
*        ASSET RECORD (05003)                                         *         
*        TRACK RECORD (05011)                                         *         
***********************************************************************         
                                                                                
UPDTASS  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCOD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         CLI   TLCOCD,TLCOCDQ      ASSET RECORD?                                
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITASS                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXASSC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UASS10   GOTO1 VTOXASSC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UASSX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UASSX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UASS20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UASS20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UASS40                                                           
         TM    TLCOSTAT,X'80'      IS ASSET DELETED?                            
         JO    UASS25                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05003'                        
         JE    *+14                                                             
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05004'                        
         JNE   UASS30                                                           
UASS25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UASS40                                                           
*                                                                               
UASS30   CLI   COPYFLAG,X'01'                                                   
         JNE   UASS10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UASS40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLCOSTAT,X'80'      IS ASSET DELETED?                            
         JO    UASSX                                                            
         J     UASS30                                                           
*                                                                               
UASSX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER ASSET RECORD AT R2                                           *         
***********************************************************************         
                                                                                
FILTASS  NTR1  BASE=*,LABEL=*                                                   
         USING TLCOPD,R2                                                        
         CLI   TLCOPCD,TLCOECDQ    ASSET RECORD?                                
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ASSET RECORD                                             *         
***********************************************************************         
                                                                                
INITASS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX03LEN          R1=L'ASS RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD VERSION RECORD AND SUBSETS                                     *         
*      VERSION RECORD (05004)                                         *         
*      TRACK VERSION RECORD (05013)                                   *         
*      PREVIOUS VERSION ID (05047)                                    *         
***********************************************************************         
                                                                                
LOADVER  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST VER RECORD             
         USING TLVRD,R2                                                         
         XC    TLVRKEY,TLVRKEY     CLEAR FILL THE KEY                           
         MVI   TLVRCD,TLVRCDQ      RECORD 40 FOR VERSION                        
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LVER10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LVERX                                                            
         CLC   TLVRCD(TLVRCD+L'TLVRCD-TLVRD),IOKEY                              
         JNE   LVERX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTVER            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LVER40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITVER            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXVERC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LVER20   GOTO1 VTOXVERC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LVER40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LVER40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LVER40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LVER30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LVER30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LVER20              TOO MANY IOS                                 
         J     LVERX                                                            
*                                                                               
LVER40   MVC   IOKEY(L'TLVRKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LVER50                                                           
         GOTO1 AREADHI                                                          
         JE    LVER50                                                           
         DC    H'0'                                                             
*                                                                               
LVER50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LVER10                                                           
*                                                                               
LVERX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE VERSION RECORD AND SUBSETS                                   *         
*        VERSION RECORD (05004)                                       *         
*        TRACK VERSION RECORD (05013)                                 *         
*        PREVIOUS VERSION ID (05047)                                  *         
***********************************************************************         
                                                                                
UPDTVER  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLVRD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTVER                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITVER                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXVERC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UVER10   GOTO1 VTOXVERC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UVERX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UVERX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UVER20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UVER20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UVER40                                                           
         TM    TLVRSTAT,X'80'      IS VERSION DELETED?                          
         JO    UVER25                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05004'                        
         JNE   UVER30                                                           
UVER25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UVER40                                                           
*                                                                               
UVER30   CLI   COPYFLAG,X'01'                                                   
         JNE   UVER10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UVER40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLVRSTAT,X'80'      IS VERSION DELETED?                          
         JO    UVERX                                                            
         J     UVER30                                                           
*                                                                               
UVERX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER VERSION RECORD AT R2                                         *         
***********************************************************************         
                                                                                
FILTVER  NTR1  BASE=*,LABEL=*                                                   
         USING TLVRD,R2                                                         
         CLI   TLVRCD,TLVRCDQ      VERSION RECORD?                              
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE VERSION RECORD                                           *         
***********************************************************************         
                                                                                
INITVER  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX04LEN          R1=L'VER RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ALIAS RECORD (05005)                                           *         
***********************************************************************         
                                                                                
LOADALI  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CLI RECORD             
         USING TLAKD,R2                                                         
         XC    TLAKKEY,TLAKKEY                                                  
         MVI   TLAKCD,TLAKCDQ      20 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LALI10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLAKKEY(TLAKCD+L'TLAKCD-TLAKD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTOXALIC,AINITALI,AFILTALI,VTOXCNVX                
         JNE   NO                                                               
         AP    PK05CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LALI10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ALIAS RECORD DATA (05005)                                    *         
***********************************************************************         
                                                                                
UPDTALI  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLAKD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTALI                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITALI                                                         
         GOTO1 ATALUPDT,DMCB,VTOXALIC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER ALIAS RECORD (05005)                                         *         
***********************************************************************         
                                                                                
FILTALI  NTR1  BASE=*,LABEL=*                                                   
         USING TLAKD,R2                                                         
         CLI   TLAKCD,TLAKCDQ      X'D8'                                        
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ALIAS RECORD (05005)                                     *         
***********************************************************************         
                                                                                
INITALI  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX05LEN          R1=L'ALI RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD W-4 RECORD AND SUBSETS                                         *         
*      W-4 RECORD (05006)                                             *         
***********************************************************************         
                                                                                
LOADW4R  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST W4 RECORD              
         USING TLW4D,R2                                                         
         XC    TLW4KEY,TLW4KEY                                                  
         MVI   TLW4CD,TLW4CDQ      60 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LW4R10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LW4RX                                                            
         CLC   TLW4KEY(TLW4CD+L'TLW4CD-TLW4D),IOKEY                             
         JNE   LW4RX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTW4R            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LW4R40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITW4R            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXW4RC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LW4R20   GOTO1 VTOXW4RC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LW4R40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LW4R40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LW4R40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LW4R30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LW4R30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LW4R20              TOO MANY IOS                                 
         J     LW4RX                                                            
*                                                                               
LW4R40   MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LW4R50                                                           
         GOTO1 AREADHI                                                          
         JE    LW4R50                                                           
         DC    H'0'                                                             
*                                                                               
LW4R50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LW4R10                                                           
*                                                                               
LW4RX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE W-4 RECORD AND SUBSETS                                       *         
*        W-4 RECORD (05006)                                           *         
***********************************************************************         
                                                                                
UPDTW4R  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLW4D,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTW4R                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITW4R                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXW4RC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UW4R10   GOTO1 VTOXW4RC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UW4RX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UW4RX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UW4R20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UW4R20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UW4R40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UW4R40                                                           
*                                                                               
UW4R30   CLI   COPYFLAG,X'01'                                                   
         JNE   UW4R10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UW4R10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UW4R40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UW4R30                                                           
*                                                                               
UW4RX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER W-4 GROUP RECORD (05006)                                     *         
***********************************************************************         
                                                                                
FILTW4R  NTR1  BASE=*,LABEL=*                                                   
         USING TLW4D,R2                                                         
         CLI   TLW4CD,TLW4CDQ      X'60'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE W-4 GROUP RECORD (05006)                                 *         
***********************************************************************         
                                                                                
INITW4R  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX06LEN          R1=L'W4R RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PERFORMER RECORD AND SUBSETS                                   *         
*      PERFORMER RECORD (05007)                                       *         
*      FIXED CYCLE (05008)                                            *         
*      VERSION-PERFORMER (05009)                                      *         
*      TRACK-PERFORMER (05012)                                        *         
*      TRACK PERFORMER-ASSET PERFORMER (05014)                        *         
***********************************************************************         
                                                                                
LOADPER  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PER RECORD             
         USING TLCAD,R2                                                         
         XC    TLCAKEY,TLCAKEY     CLEAR FILL THE KEY                           
         MVI   TLCACD,TLCACDQ      RECORD 80 FOR PERFORMER                      
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPER10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LPERX                                                            
         CLC   TLCACD(TLCACD+L'TLCACD-TLCAD),IOKEY                              
         JNE   LPERX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTPER            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LPER40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITPER            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXPERC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LPER20   GOTO1 VTOXPERC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LPER40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LPER40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LPER40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LPER30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LPER30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LPER20              TOO MANY IOS                                 
         J     LPERX                                                            
*                                                                               
LPER40   MVC   IOKEY(L'TLCAKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LPER50                                                           
         GOTO1 AREADHI                                                          
         JE    LPER50                                                           
         DC    H'0'                                                             
*                                                                               
LPER50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPER10                                                           
*                                                                               
LPERX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PERFORMER RECORD AND SUBSETS                                 *         
*        PERFORMER RECORD (05007)                                     *         
*        FIXED CYCLE (05008)                                          *         
*        VERSION-PERFORMER (05009)                                    *         
***********************************************************************         
                                                                                
UPDTPER  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCAD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPER                                                         
         JNE   YES                                                              
*                                                                               
         XC    DXCLI,DXCLI         FLAG (01 = DELETE 01 VERS/PERF REC)          
*                                                                               
         GOTO1 AINITPER                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXPERC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UPER10   GOTO1 VTOXPERC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UPERX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPERX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPER20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UPER20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UPER40                                                           
         TM    TLCASTAT,X'80'      IS PERFORMER DELETED?                        
         JO    UPER25                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05007'                        
         JNE   UPER30                                                           
UPER25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UPER40                                                           
*                                                                               
UPER30   CLI   COPYFLAG,X'01'                                                   
         JNE   UPER10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPER40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLCASTAT,X'80'      IS PERFORMER DELETED?                        
         JO    UPERX                                                            
         J     UPER30                                                           
*                                                                               
UPERX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PERFORMER RECORD AT R2                                       *         
***********************************************************************         
                                                                                
FILTPER  NTR1  BASE=*,LABEL=*                                                   
         USING TLCAD,R2                                                         
         CLI   TLCACD,TLCACDQ      PERFORMER RECORD?                            
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PERFORMER RECORD                                         *         
***********************************************************************         
                                                                                
INITPER  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX07LEN          R1=L'PER RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD INVOICE RECORD AND SUBSETS                                     *         
*      INVOICE RECORD (05010)                                         *         
*      PRIMARY INVOICE-SUBSIDIARY (05015)                             *         
***********************************************************************         
                                                                                
LOADINV  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST INV RECORD             
         USING TLINPD,R2                                                        
         XC    TLINPKEY,TLINPKEY   CLEAR FILL THE KEY                           
         MVI   TLINPCD,TLINHCDQ    RECORD 91 FOR INVOICE                        
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LINV10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LINV60                                                           
         CLC   TLINPCD(TLINPCD+L'TLINPCD-TLINPD),IOKEY                          
         JNE   LINV60              ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   SVINHKEY,0(R2)                                                   
*                                                                               
         GOTO1 AFILTINV            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LINV40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         CLI   0(R2),TLINCDQ       MUST BE INVOICE                              
         JNE   LINV40                                                           
*                                                                               
         GOTO1 AINITINV            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXINVC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS,VT00A88             
*                                  GET NEXT UNCOMMITTED RECORD                  
LINV20   GOTO1 VTOXINVC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LINV40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LINV40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LINV40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LINV30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LINV30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LINV20              TOO MANY IOS                                 
         J     LINVX                                                            
*                                                                               
LINV40   MVC   IOKEY(L'TLINPKEY),SVINHKEY  READ NEXT RECORD - SEQUENT           
         GOTO1 ACHKSEQ                                                          
         JE    LINV50                                                           
         GOTO1 AREADHI                                                          
         JE    LINV50                                                           
         DC    H'0'                                                             
*                                                                               
LINV50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LINV10                                                           
*                                                                               
LINV60   BRAS  RE,LOADUSE          LOAD USAGE DETAILS                           
*                                                                               
LINVX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE INVOICE RECORD AND SUBSETS                                   *         
*        INVOICE RECORD (05010)                                       *         
*        PRIMARY INVOICE-SUBSIDIARY (05015)                           *         
***********************************************************************         
                                                                                
UPDTINV  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLIND,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         CLI   TLINCD,TLINCDQ      INVOICE RECORD?                              
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITINV                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXINVC,DMCB,DXAXREC,(R2),(1,0),(R6),0,VT00A88                  
*                                  GET NEXT UNCOMMITTED RECORD                  
UINV10   GOTO1 VTOXINVC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UINVX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UINVX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UINV20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UINV20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UINV40                                                           
         TM    TLINSTAT,X'80'      IS INVOICE DELETED?                          
         JO    UINV25                                                           
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05010'                        
         JNE   UINV35                                                           
         MVI   COPYFLAG,X'01'                                                   
UINV25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UINV40                                                           
*                                                                               
UINV30   CLI   COPYFLAG,X'01'                                                   
         JNE   UINV10                                                           
UINV35   L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UINV40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLINSTAT,X'80'      IS INVOICE DELETED?                          
         JO    UINVX                                                            
         J     UINV30                                                           
*                                                                               
UINVX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER INVOICE RECORD AT R2                                         *         
***********************************************************************         
                                                                                
FILTINV  NTR1  BASE=*,LABEL=*                                                   
         USING TLINPD,R2                                                        
         CLI   TLINPCD,TLINHCDQ    INVOICE RECORD?                              
         JNE   NO                                                               
         CLI   TLINHSEQ,X'80'      NOT HISTORY                                  
         JNH   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE INVOICE RECORD                                           *         
***********************************************************************         
                                                                                
INITINV  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX10LEN          R1=L'INV RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MEDIA RECORD (05016)                                                *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADMED  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING MEDIAD,R2                                                        
         LA    R2,TAMEDS           SET KEY TO READ FIRST GRT RECORD             
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LMED10   CLI   0(R2),EOF                                                        
         BE    LMEDX                                                            
*                                                                               
         GOTO1 AINITMED            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX16LEN)                                          
         MVC   TOXRETYP,=C'05016'                                               
*                                                                               
         MVI   TX16MED-1,MXTRTQ                                                 
         MVI   TX16DESC-1,MXTRTQ                                                
*                                                                               
         MVI   TX16MED,C'T'                                                     
         CLI   MEDEQU,TV                                                        
         JE    LMED20                                                           
         MVI   TX16MED,C'R'                                                     
         CLI   MEDEQU,RADIO                                                     
         JE    LMED20                                                           
         MVI   TX16MED,C'C'                                                     
         CLI   MEDEQU,CABLE                                                     
         JE    LMED20                                                           
         MVI   TX16MED,C'P'                                                     
         CLI   MEDEQU,PRINT                                                     
         JE    LMED20                                                           
         MVI   TX16MED,C'I'                                                     
         CLI   MEDEQU,INTERNET                                                  
         JE    LMED20                                                           
         MVI   TX16MED,C'N'                                                     
         CLI   MEDEQU,NEWMEDIA                                                  
         JE    LMED20                                                           
         MVI   TX16MED,C'E'                                                     
*                                                                               
LMED20   MVC   TX16DESC,MEDNAME                                                 
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         LA    R2,MEDNEXT                                                       
         AP    PK16CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMED10                                                           
*                                                                               
LMEDX    J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
       ++INCLUDE TASYSMEDS                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD COUNT RECORD (05016)                              *         
***********************************************************************         
                                                                                
INITMED  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX16LEN          R1=L'MED RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ASSET TYPE RECORD (05017)                                           *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADAST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING CTYD,R2                                                          
         LA    R2,TACOMS           SET KEY TO READ FIRST GRT RECORD             
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LAST10   CLI   0(R2),EOF                                                        
         BE    LASTX                                                            
*                                                                               
         GOTO1 AINITAST            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX17LEN)                                          
         MVC   TOXRETYP,=C'05017'                                               
*                                                                               
         MVI   TX17ASST-1,MXTRTQ                                                
         MVI   TX17DESC-1,MXTRTQ                                                
*                                                                               
         MVC   TX17ASST,CTYEQU                                                  
         MVC   TX17DESC,CTYNAME                                                 
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         LA    R2,CTYNEXT                                                       
         AP    PK17CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAST10                                                           
*                                                                               
LASTX    J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
       ++INCLUDE TASYSCTYP                                                      
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD ASSET TYPE RECORD (05017)                         *         
***********************************************************************         
                                                                                
INITAST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX17LEN          R1=L'AST RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* W4 TYPE RECORD (05018)                                              *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADW4T  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING W4TYD,R2                                                         
         LA    R2,W4TYPES          SET KEY TO READ FIRST GRT RECORD             
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LW4T10   CLI   0(R2),EOF                                                        
         BE    LW4TX                                                            
*                                                                               
         GOTO1 AINITW4T            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX18LEN)                                          
         MVC   TOXRETYP,=C'05018'                                               
*                                                                               
         MVI   TX18W4T-1,MXTRTQ                                                 
         MVI   TX18DESC-1,MXTRTQ                                                
*                                                                               
         MVC   TX18W4T,W4TEQU                                                   
         MVC   TX18DESC,W4TNAME                                                 
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         AHI   R2,W4TNEXT                                                       
         AP    PK18CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LW4T10                                                           
*                                                                               
LW4TX    J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
W4TYPES  DC    C'A',CL11'CANADIAN'                                              
         DC    C'I',CL11'INDIVIDUAL'                                            
         DC    C'C',CL11'CORPORATION'                                           
         DC    C'E',CL11'ESTATE'                                                
         DC    C'F',CL11'FOREIGNER'                                             
         DC    C'T',CL11'TRUSTEE'                                               
         DC    X'FF'                                                            
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD W4 TYPE RECORD (05018)                            *         
***********************************************************************         
                                                                                
INITW4T  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX18LEN          R1=L'W4T RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* PERFORMER CATEGORY RECORD (05019)                                   *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
         SPACE 1                                                                
LOADPEC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING CATTABD,R2                                                       
         LA    R2,TACATS           SET KEY TO READ FIRST GRT RECORD             
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LPEC10   CLI   0(R2),EOF                                                        
         BE    LPECX                                                            
*                                                                               
         GOTO1 AINITPEC            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX19LEN)                                          
         MVC   TOXRETYP,=C'05019'                                               
*                                                                               
         MVI   TX19PERC-1,MXTRTQ                                                
         MVI   TX19DESC-1,MXTRTQ                                                
         MVI   TX19EHON-1,MXTRTQ                                                
         MVI   TX19EHOF-1,MXTRTQ                                                
         MVI   TX19UWRK-1,MXTRTQ                                                
         MVI   TX19AFM-1,MXTRTQ                                                 
         MVI   TX19SING-1,MXTRTQ                                                
         MVI   TX19EXTR-1,MXTRTQ                                                
         MVI   TX194AFT-1,MXTRTQ                                                
         MVI   TX194SAG-1,MXTRTQ                                                
         MVI   TX194NON-1,MXTRTQ                                                
         MVI   TX194ACT-1,MXTRTQ                                                
         MVI   TX194DGA-1,MXTRTQ                                                
         MVI   TX194WGA-1,MXTRTQ                                                
         MVI   TX194UDA-1,MXTRTQ                                                
         MVI   TX194ONC-1,MXTRTQ                                                
         MVI   TX194OFC-1,MXTRTQ                                                
*                                                                               
         MVC   TX19PERC,CATCDE                                                  
*                                                                               
         ZIC   R1,CATLEN                                                        
         SHI   R1,CATNAME-CATLEN                                                
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TX19DESC(0),CATNAME                                              
*                                                                               
         MVI   TX19EHON,C'N'       ELIGIBLE FOR HOLDING FEES ON CAM?            
         TM    CATTYPE,NOHLD+NHLDOF88                                           
         JNZ   *+8                                                              
         MVI   TX19EHON,C'Y'                                                    
*                                                                               
         MVI   TX19EHOF,C'N'       ELIGIBLE FOR HOLDING FEES OFF CAM?           
         TM    CATTYPE,NOHLD+NOHLDOFF+NHLDOF88                                  
         JNZ   *+8                                                              
         MVI   TX19EHOF,C'Y'                                                    
*                                                                               
         MVI   TX19UWRK,C'N'       FOR UNION WORK?                              
         CLI   CATUNI,0                                                         
         JE    *+8                                                              
         MVI   TX19UWRK,C'Y'                                                    
*                                                                               
         MVI   TX19AFM,C'N'        FOR AFM?                                     
         TM    CATUNI,AFM                                                       
         JZ    *+8                                                              
         MVI   TX19AFM,C'Y'                                                     
*                                                                               
         MVI   TX19SING,C'N'       SINGER?                                      
         TM    CATSTAT,SINGER                                                   
         JZ    *+8                                                              
         MVI   TX19SING,C'Y'                                                    
*                                                                               
         MVI   TX19EXTR,C'N'       EXTRA?                                       
         TM    CATTYPE,EXTRA                                                    
         JZ    *+8                                                              
         MVI   TX19EXTR,C'Y'                                                    
*                                                                               
         MVI   TX194AFT,C'N'       AFT?                                         
         TM    CATUNI,AFT                                                       
         JZ    *+8                                                              
         MVI   TX194AFT,C'Y'                                                    
*                                                                               
         MVI   TX194SAG,C'N'       SAG?                                         
         TM    CATUNI,SAG                                                       
         JZ    *+8                                                              
         MVI   TX194SAG,C'Y'                                                    
*                                                                               
         MVI   TX194NON,C'N'       NON?                                         
         TM    CATUNI,NON                                                       
         JZ    *+8                                                              
         MVI   TX194NON,C'Y'                                                    
*                                                                               
         MVI   TX194ACT,C'N'       ACT?                                         
         TM    CATUNI,ACT                                                       
         JZ    *+8                                                              
         MVI   TX194ACT,C'Y'                                                    
*                                                                               
         MVI   TX194DGA,C'N'       DGA?                                         
         TM    CATUNI,DGA                                                       
         JZ    *+8                                                              
         MVI   TX194DGA,C'Y'                                                    
*                                                                               
         MVI   TX194WGA,C'N'       WGA?                                         
         TM    CATUNI,WGA                                                       
         JZ    *+8                                                              
         MVI   TX194WGA,C'Y'                                                    
*                                                                               
         MVI   TX194UDA,C'N'       UDA?                                         
         TM    CATUNI,UDA                                                       
         JZ    *+8                                                              
         MVI   TX194UDA,C'Y'                                                    
*                                                                               
         MVI   TX194ONC,C'N'       ON CAMERA                                    
         TM    CATSTAT,OKON                                                     
         JZ    *+8                                                              
         MVI   TX194ONC,C'Y'                                                    
*                                                                               
         MVI   TX194OFC,C'N'       OFF CAMERA                                   
         TM    CATSTAT,OKOFF                                                    
         JZ    *+8                                                              
         MVI   TX194OFC,C'Y'                                                    
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         ZIC   R1,CATLEN                                                        
         AR    R2,R1                                                            
         AP    PK19CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPEC10                                                           
*                                                                               
LPECX    J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
       ++INCLUDE TASYSCATS                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD PERFORMER CATEGORY (05019)                        *         
***********************************************************************         
         SPACE 1                                                                
INITPEC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX19LEN          R1=L'PEC RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USE TYPE RECORD (05020)                                             *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADUST  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING USETABD,R2                                                       
         L     R2,VT00A88                                                       
         ICM   R0,15,TGAUSES-TGTABLED(R2)                                       
         AR    R2,R0                                                            
*                                                                               
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LUST10   CLI   0(R2),EOF                                                        
         BE    LUSTX                                                            
*                                                                               
         GOTO1 AINITUST            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX20LEN)                                          
         MVC   TOXRETYP,=C'05020'                                               
*                                                                               
         MVI   TX20USET-1,MXTRTQ                                                
         MVI   TX20DESC-1,MXTRTQ                                                
         MVI   TX20SERE-1,MXTRTQ                                                
*                                                                               
         MVC   TX20USET,USECDE                                                  
*                                                                               
         LLC   RE,USEDSP           LENGTH OF ELEM INCLUDING NAME                
         AHI   RE,-(USENAME-USETABD) LESS FIXED LEN GIVES LEN OF NAME           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TX20DESC(0),USENAME                                              
*                                                                               
         MVI   TX20SERE,C'R'       REUSE                                        
         TM    USESTAT,SESSION                                                  
         JZ    *+8                                                              
         MVI   TX20SERE,C'S'       SESSION                                      
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         SR    R1,R1                                                            
         LH    R1,USELEN                                                        
         AR    R2,R1                                                            
         AP    PK20CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUST10                                                           
*                                                                               
LUSTX    J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD USE TYPE RECORD (05020)                           *         
***********************************************************************         
                                                                                
INITUST  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX20LEN          R1=L'UST RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CLIENT GROUP RECORD                                            *         
*      CLIENT GROUP RECORD (05021)                                    *         
***********************************************************************         
                                                                                
LOADCGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLCGD,R2                                                         
         XC    TLCGKEY,TLCGKEY     CLEAR FILL THE KEY                           
         MVI   TLCGCD,TLCGCDQ      RECORD 28 FOR CLIENT GROUP                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCGR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LCGRX                                                            
         CLC   TLCGCD(TLCGCD+L'TLCGCD-TLCGD),IOKEY                              
         JNE   LCGRX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTCGR            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LCGR40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITCGR            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXCGRC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LCGR20   GOTO1 VTOXCGRC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LCGR40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LCGR40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LCGR40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LCGR30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LCGR30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LCGR20              TOO MANY IOS                                 
         J     LCGRX                                                            
*                                                                               
LCGR40   MVC   IOKEY(L'TLCGKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LCGR50                                                           
         GOTO1 AREADHI                                                          
         JE    LCGR50                                                           
         DC    H'0'                                                             
*                                                                               
LCGR50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCGR10                                                           
*                                                                               
LCGRX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CLIENT GROUP RECORD                                          *         
*        CLIENT GROUP RECORD (05021)                                  *         
***********************************************************************         
                                                                                
UPDTCGR  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCGD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCGR                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXCGRC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UCGR10   GOTO1 VTOXCGRC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UCGRX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UCGRX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UCGR20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UCGR20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UCGR40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UCGR40                                                           
*                                                                               
UCGR30   CLI   COPYFLAG,X'01'                                                   
         JNE   UCGR10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UCGR10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UCGR40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UCGR30                                                           
*                                                                               
UCGRX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CLIENT GROUP RECORD AT R2                                    *         
***********************************************************************         
                                                                                
FILTCGR  NTR1  BASE=*,LABEL=*                                                   
         USING TLCGD,R2                                                         
         CLI   TLCGCD,TLCGCDQ      CLIENT GROUP RECORD?                         
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CLIENT GROUP RECORD                                      *         
***********************************************************************         
                                                                                
INITCGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX21LEN          R1=L'CGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USE SUBTYPE RECORD (05022)                                          *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADUSS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING USETABD,R2                                                       
         L     R2,VT00A88                                                       
         ICM   R0,15,TGAUSES-TGTABLED(R2)                                       
         AR    R2,R0                                                            
*                                                                               
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LUSS10   CLI   0(R2),EOF                                                        
         BE    LUSSX                                                            
         MVI   BYTE2,0                                                          
         LR    R4,R2                                                            
         ZIC   RE,USEDSP           BUMP TO FIRST SUB-ENTRY                      
         AR    R4,RE                                                            
         J     LUSS15                                                           
*                                                                               
         USING USESUBD,R4                                                       
LUSS12   ZIC   RE,USESBLN                                                       
         AR    R4,RE                                                            
LUSS15   CLI   0(R4),0                                                          
         JE    LUSS40                                                           
*                                                                               
         GOTO1 AINITUSS            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX22LEN)                                          
         MVC   TOXRETYP,=C'05022'                                               
*                                                                               
         MVI   TX22USET-1,MXTRTQ                                                
         MVI   TX22STYP-1,MXTRTQ                                                
         MVI   TX22DESC-1,MXTRTQ                                                
         MVI   TX22CLMO-1,MXTRTQ                                                
         MVI   TX22CLWK-1,MXTRTQ                                                
         MVI   TX22CLDY-1,MXTRTQ                                                
*                                                                               
         MVC   TX22USET,USECDE                                                  
         MVC   TX22STYP,=C'000'                                                 
*                                                                               
         CLI   USETYPE,0                                                        
         JNE   *+12                                                             
         MVI   BYTE2,1             USE HAS SUBTYPE OF 0                         
         J     LUSS20                                                           
*                                                                               
         ZIC   R1,USETYPE                                                       
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TX22STYP,DUB                                                     
*                                                                               
LUSS20   ZIC   RE,USESBLN          L'SUB-ENTRY                                  
         SHI   RE,USETYNME-USESUBD+1 LESS DISP. TO NAME + 1                     
         JM    LUSS25              = L'NAME - 1                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TX22DESC(0),USETYNME                                             
         J     LUSS26                                                           
*                                                                               
LUSS25   MVC   TX22DESC(7),=C'DEFAULT'                                          
*                                                                               
LUSS26   MVC   BYTE,USEWKS                                                      
         TM    USEWKS,X'80'        TEST IF MONTHS                               
         BO    LUSS27                                                           
         TM    USEWKS,X'40'        OR DAYS                                      
         BO    LUSS28                                                           
         EDIT  BYTE,TX22CLWK,ZERO=BLANK,ALIGN=RIGHT,FILL=0   (WEEKS)            
         J     LUSS30                                                           
                                                                                
LUSS27   NI    USEWKS,X'7F'        TURN OFF BIT    (MONTHS)                     
         MVC   BYTE,USEWKS                                                      
         EDIT  BYTE,TX22CLMO,ZERO=BLANK,ALIGN=RIGHT,FILL=0                      
         J     LUSS30                                                           
                                                                                
LUSS28   NI    USEWKS,X'BF'        TURN OFF BIT    (DAYS)                       
         MVC   BYTE,USEWKS                                                      
         EDIT  BYTE,TX22CLDY,ZERO=BLANK,ALIGN=RIGHT,FILL=0                      
*                                                                               
LUSS30   GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     LUSS12                                                           
*                                                                               
LUSS40   CLI   BYTE2,1             USE HAS SUBTYPE OF 0?                        
         JE    LUSS50                                                           
*                                  NO - ADD ONE FOR 0/DEFAULT                   
         GOTO1 AINITUSS            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX22LEN)                                          
         MVC   TOXRETYP,=C'05022'                                               
*                                                                               
         MVI   TX22USET-1,MXTRTQ                                                
         MVI   TX22STYP-1,MXTRTQ                                                
         MVI   TX22DESC-1,MXTRTQ                                                
         MVI   TX22CLMO-1,MXTRTQ                                                
         MVI   TX22CLWK-1,MXTRTQ                                                
         MVI   TX22CLDY-1,MXTRTQ                                                
*                                                                               
         MVC   TX22USET,USECDE                                                  
         MVC   TX22STYP,=C'000'                                                 
         MVC   TX22DESC(7),=C'DEFAULT'                                          
*                                                                               
         CLI   USESBLN,0           NO MORE ENTRIES                              
         BE    LUSS45                                                           
*                                                                               
         ZIC   R0,USEWKS                                                        
         TM    USEWKS,X'80'        TEST IF MONTHS                               
         BO    LUSS42                                                           
         TM    USEWKS,X'40'        OR DAYS                                      
         BO    LUSS43                                                           
         EDIT  (R0),TX22CLWK,ZERO=BLANK,ALIGN=RIGHT,FILL=0   (WEEKS)            
         J     LUSS45                                                           
                                                                                
LUSS42   NI    USEWKS,X'7F'        TURN OFF BIT    (MONTHS)                     
         ZIC   R0,USEWKS                                                        
         EDIT  (R0),TX22CLMO,ZERO=BLANK,ALIGN=RIGHT,FILL=0                      
         J     LUSS45                                                           
                                                                                
LUSS43   NI    USEWKS,X'BF'        TURN OFF BIT    (DAYS)                       
         ZIC   R0,USEWKS                                                        
         EDIT  (R0),TX22CLDY,ZERO=BLANK,ALIGN=RIGHT,FILL=0                      
         DROP  R4                                                               
*                                                                               
LUSS45   GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
LUSS50   SR    R1,R1                                                            
         LH    R1,USELEN                                                        
         AR    R2,R1                                                            
         AP    PK22CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUSS10                                                           
*                                                                               
LUSSX    J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD USE SUBTYPE RECORD (05022)                        *         
***********************************************************************         
                                                                                
INITUSS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX22LEN          R1=L'UST RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ACTRA TYPE (05023)                                                  *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADACT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING CCTYPD,R2                                                        
         LA    R2,TACTYPS                                                       
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LACT10   CLI   0(R2),EOF                                                        
         BE    LACTX                                                            
*                                                                               
         GOTO1 AINITACT            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX23LEN)                                          
         MVC   TOXRETYP,=C'05023'                                               
*                                                                               
         MVI   TX23CODE-1,MXTRTQ                                                
         MVI   TX23DESC-1,MXTRTQ                                                
*                                                                               
         ZIC   R1,CCTYPEQU         ACTRA TYPE CODE                              
         CVD   R1,DUB                                                           
         OI    DUB+L'DUB-1,X'0F'                                                
         UNPK  TX23CODE,DUB                                                     
*                                                                               
         MVC   TX23DESC,CCTYPNME   NAME                                         
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         LA    R2,CCTYPLNQ(R2)                                                  
         AP    PK23CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LACT10                                                           
*                                                                               
LACTX    J     YES                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
       ++INCLUDE TASYSCCTYP                                                     
         EJECT                                                                  
***********************************************************************         
* INITIALIZE RECORD ACTRA TYPE RECORD (05023)                         *         
***********************************************************************         
                                                                                
INITACT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX23LEN          R1=L'UST RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD AGENCY GROUP RECORD (05024)                                    *         
***********************************************************************         
                                                                                
LOADAGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CLI RECORD             
         USING TLAGD,R2                                                         
         XC    TLAGKEY,TLAGKEY                                                  
         MVI   TLAGCD,TLAGCDQ      18 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLAGKEY(TLAGCD+L'TLAGCD-TLAGD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTOXAGRC,AINITAGR,AFILTAGR,VTOXCNVX                
         JNE   NO                                                               
         AP    PK24CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGR10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENCY GROUP RECORD (05024)                                  *         
***********************************************************************         
                                                                                
UPDTAGR  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLAGD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTAGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGR                                                         
         GOTO1 ATALUPDT,DMCB,VTOXAGRC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AGENCY GROUPE RECORD (05024)                                 *         
***********************************************************************         
                                                                                
FILTAGR  NTR1  BASE=*,LABEL=*                                                   
         USING TLAGD,R2                                                         
         CLI   TLAGCD,TLAGCDQ      X'20'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENCY GROUP RECORD (05024)                              *         
***********************************************************************         
                                                                                
INITAGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX24LEN          R1=L'AGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD AREA RECORD (05028)                                            *         
***********************************************************************         
                                                                                
LOADARE  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AREA RECORD            
         USING TLARD,R2                                                         
         XC    TLARKEY,TLARKEY                                                  
         MVI   TLARCD,TLARCDQ      88 RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LARE10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLARKEY(TLARCD+L'TLARCD-TLARD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTOXAREC,AINITARE,AFILTARE,VTOXCNVX                
         JNE   NO                                                               
         AP    PK28CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LARE10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE AREA RECORD DATA (05028)                                     *         
***********************************************************************         
                                                                                
UPDTARE  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLARD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTARE                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITARE                                                         
         GOTO1 ATALUPDT,DMCB,VTOXAREC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AREA RECORD (05028)                                          *         
***********************************************************************         
                                                                                
FILTARE  NTR1  BASE=*,LABEL=*                                                   
         USING TLARD,R2                                                         
         CLI   TLARCD,TLARCDQ      X'88'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AREA RECORD (05028)                                      *         
***********************************************************************         
                                                                                
INITARE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX28LEN          R1=L'ARE RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRINT USE RECORD (05029)                                       *         
***********************************************************************         
                                                                                
LOADPUS  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PR USE RECORD          
         USING TLUSD,R2                                                         
         XC    TLUSKEY,TLUSKEY                                                  
         MVI   TLUSCD,TLUSCDQ      8C RECORD                                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPUS10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    YES                                                              
         CLC   TLUSKEY(TLUSCD+L'TLUSCD-TLUSD),IOKEY                             
         JNE   YES                 ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 ATALLOAD,DMCB,VTOXPUSC,AINITPUS,AFILTPUS,VTOXCNVX                
         JNE   NO                                                               
         AP    PK29CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPUS10                                                           
         J     YES                                                              
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRINT USE RECORD DATA (05029)                                *         
***********************************************************************         
                                                                                
UPDTPUS  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLUSD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPUS                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPUS                                                         
         GOTO1 ATALUPDT,DMCB,VTOXPUSC,TYPECODE                                  
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2,R5                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRINT USE RECORD (05029)                                     *         
***********************************************************************         
                                                                                
FILTPUS  NTR1  BASE=*,LABEL=*                                                   
         USING TLUSD,R2                                                         
         CLI   TLUSCD,TLUSCDQ      X'8C'                                        
         JNE   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRINT USE RECORD (05029)                                 *         
***********************************************************************         
                                                                                
INITPUS  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX29LEN          R1=L'PUS RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GUARANTEE RECORD AND SUBSETS                                   *         
*      GUARANTEE RECORD (05030)                                       *         
*      GUARANTEE AGENCY-CLIENT (05031)                                *         
*      GUARANTEE PERIOD-CYCLE (05032)                                 *         
***********************************************************************         
                                                                                
LOADGUA  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST GUA RECORD             
         USING TLGUD,R2                                                         
         XC    TLGUKEY,TLGUKEY     CLEAR FILL THE KEY                           
         MVI   TLGUCD,TLGUCDQ      RECORD 6C FOR GUARANTEE                      
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGUA10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LGUAX                                                            
         CLC   TLGUCD(TLGUCD+L'TLGUCD-TLGUD),IOKEY                              
         JNE   LGUAX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTGUA            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LGUA40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITGUA            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGUAC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS,VT00A88             
*                                  GET NEXT UNCOMMITTED RECORD                  
LGUA20   GOTO1 VTOXGUAC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LGUA40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LGUA40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LGUA40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LGUA30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LGUA30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LGUA20              TOO MANY IOS                                 
         J     LGUAX                                                            
*                                                                               
LGUA40   MVC   IOKEY(L'TLGUKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LGUA50                                                           
         GOTO1 AREADHI                                                          
         JE    LGUA50                                                           
         DC    H'0'                                                             
*                                                                               
LGUA50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGUA10                                                           
*                                                                               
LGUAX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE GUARANTEE RECORD AND SUBSETS                                 *         
*      GUARANTEE RECORD (05030)                                       *         
*      GUARANTEE AGENCY-CLIENT (05031)                                *         
*      GUARANTEE PERIOD-CYCLE (05032)                                 *         
***********************************************************************         
                                                                                
UPDTGUA  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLGUD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTGUA                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITGUA                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGUAC,DMCB,DXAXREC,(R2),(1,0),(R6),0,VT00A88                  
*                                  GET NEXT UNCOMMITTED RECORD                  
UGUA10   GOTO1 VTOXGUAC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UGUAX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UGUAX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UGUA20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UGUA20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UGUA40                                                           
         TM    TLGUSTAT,X'80'      IS ASSET DELETED?                            
         JO    UGUA25                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05030'                        
         JNE   UGUA30                                                           
UGUA25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UGUA40                                                           
*                                                                               
UGUA30   CLI   COPYFLAG,X'01'                                                   
         JNE   UGUA10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UGUA40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLGUSTAT,X'80'      IS ASSET DELETED?                            
         JO    UGUAX                                                            
         J     UGUA30                                                           
*                                                                               
UGUAX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER GUARANTEE RECORD AT R2                                       *         
***********************************************************************         
                                                                                
FILTGUA  NTR1  BASE=*,LABEL=*                                                   
         USING TLGUD,R2                                                         
         CLI   TLGUCD,TLGUCDQ      GUARANTEE RECORD?                            
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE GUARANTEE RECORD                                         *         
***********************************************************************         
                                                                                
INITGUA  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX32LEN          R1=L'GUA RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GUARANTEE CONTRACT RECORD AND SUBSETS                          *         
*      GUARANTEE CONTRACTS (05033)                                    *         
*      GUARANTEE CONTRACT YEAR (05034)                                *         
*      GUARANTEE COMMENTS (05035)                                     *         
***********************************************************************         
                                                                                
LOADGUC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST GUA RECORD             
         USING TLGCD,R2                                                         
         XC    TLGCKEY,TLGCKEY     CLEAR FILL THE KEY                           
         MVI   TLGCCD,TLGCCDQ      RECORD 84 FOR GUARANTEE CONTRACT             
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGUC10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LGUCX                                                            
         CLC   TLGCCD(TLGCCD+L'TLGCCD-TLGCD),IOKEY                              
         JNE   LGUCX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTGUC            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LGUC40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITGUC            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGUCC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LGUC20   GOTO1 VTOXGUCC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LGUC40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LGUC40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LGUC40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LGUC30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LGUC30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LGUC20              TOO MANY IOS                                 
         J     LGUCX                                                            
*                                                                               
LGUC40   MVC   IOKEY(L'TLGCKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LGUC50                                                           
         GOTO1 AREADHI                                                          
         JE    LGUC50                                                           
         DC    H'0'                                                             
*                                                                               
LGUC50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGUC10                                                           
*                                                                               
LGUCX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE GUARANTEE CONTRACT RECORD AND SUBSETS                        *         
*        GUARANTEE CONTRACTS (05033)                                  *         
*        GUARANTEE CONTRACT YEAR (05034)                              *         
*        GUARANTEE COMMENTS (05035)                                   *         
***********************************************************************         
                                                                                
UPDTGUC  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLGCD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTGUC                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITGUC                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGUCC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UGUC10   GOTO1 VTOXGUCC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UGUCX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UGUCX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UGUC20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UGUC20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UGUC40                                                           
         TM    TLGCSTAT,X'80'      IS ASSET DELETED?                            
         JO    UGUC25                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05033'                        
         JNE   UGUC30                                                           
UGUC25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UGUC40                                                           
*                                                                               
UGUC30   CLI   COPYFLAG,X'01'                                                   
         JNE   UGUC10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UGUC40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLGCSTAT,X'80'      IS ASSET DELETED?                            
         JO    UGUCX                                                            
         J     UGUC30                                                           
*                                                                               
UGUCX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER GUARANTEE RECORD AT R2                                       *         
***********************************************************************         
                                                                                
FILTGUC  NTR1  BASE=*,LABEL=*                                                   
         USING TLGCD,R2                                                         
         CLI   TLGCCD,TLGCCDQ      GUARANTEE CONTRACT RECORD                    
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE GUARANTEE RECORD                                         *         
***********************************************************************         
                                                                                
INITGUC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX34LEN          R1=L'GUC RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GURANTEE COMMENTS RECORD                                       *         
*      GUARANTEE COMMENT RECORD (05035)                               *         
***********************************************************************         
                                                                                
LOADGCC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLCMD,R2                                                         
         XC    TLCMKEY,TLCMKEY     CLEAR FILL THE KEY                           
         MVI   TLCMCD,TLCMCDQ      RECORD C4 FOR AGENCY                         
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGCC10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LGCCX                                                            
         CLC   TLCMCD(TLCMCD+L'TLCMCD-TLCMD),IOKEY                              
         JNE   LGCCX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AFILTGCC            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LGCC40                                                           
*                                                                               
         GOTO1 AINITGCC            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGCCC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LGCC20   GOTO1 VTOXGCCC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LGCC40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LGCC40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LGCC40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LGCC30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LGCC30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LGCC20              TOO MANY IOS                                 
         J     LGCCX                                                            
*                                                                               
LGCC40   MVC   IOKEY(L'TLCMKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LGCC50                                                           
         GOTO1 AREADHI                                                          
         JE    LGCC50                                                           
         DC    H'0'                                                             
*                                                                               
LGCC50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGCC10                                                           
*                                                                               
LGCCX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE GURANTEE COMMENTS RECORD                                     *         
*        GUARANTEE COMMENT RECORD (05035)                             *         
***********************************************************************         
                                                                                
UPDTGCC  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCMD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTGCC                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITGCC                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGCCC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UGCC10   GOTO1 VTOXGCCC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UGCCX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UGCCX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UGCC20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UGCC20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UGCC40                                                           
         TM    TLCMSTAT,X'80'      IS ASSET DELETED?                            
         JO    UGCC25                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05035'                        
         JNE   UGCC30                                                           
UGCC25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UGCC40                                                           
*                                                                               
UGCC30   CLI   COPYFLAG,X'01'                                                   
         JNE   UGCC10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UGCC40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLCMSTAT,X'80'      IS ASSET DELETED?                            
         JO    UGCCX                                                            
         J     UGCC30                                                           
*                                                                               
UGCCX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER GUARANTEE COMMENT RECORD AT R2                               *         
***********************************************************************         
                                                                                
FILTGCC  NTR1  BASE=*,LABEL=*                                                   
         USING TLCMD,R2                                                         
         CLI   TLCMCD,TLCMCDQ      COMMENT RECORD?                              
         JNE   NO                                                               
         CLI   TLCMTYP,TLCMTGUA    GUARANTEE?                                   
         JNE   NO                                                               
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE GUARANTEE COMMENT                                        *         
***********************************************************************         
                                                                                
INITGCC  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX35LEN          R1=L'GCC RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GUARANTEE CONTRACT YEAR TRACKING                               *         
*      GUARANTEE CONTRACT YEAR TRACKING (05036)                       *         
***********************************************************************         
                                                                                
LOADGCY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLOTD,R2                                                         
         XC    TLOTKEY,TLOTKEY     CLEAR FILL THE KEY                           
         MVI   TLOTCD,TLOTCDQ      RECORD 48 FOR CONTRACT YEAR TRACK            
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGCY10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LGCYX                                                            
         CLC   TLOTCD(TLOTCD+L'TLOTCD-TLOTD),IOKEY                              
         JNE   LGCYX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AFILTGCY            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LGCY40                                                           
*                                                                               
         GOTO1 AINITGCY            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGCYC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LGCY20   GOTO1 VTOXGCYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LGCY40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LGCY40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LGCY40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LGCY30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LGCY30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LGCY20              TOO MANY IOS                                 
         J     LGCYX                                                            
*                                                                               
LGCY40   MVC   IOKEY(L'TLOTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LGCY50                                                           
         GOTO1 AREADHI                                                          
         JE    LGCY50                                                           
         DC    H'0'                                                             
*                                                                               
LGCY50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGCY10                                                           
*                                                                               
LGCYX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE GUARANTEE CONTRACT YEAR TRACKING                             *         
*        GUARANTEE CONTRACT YEAR TRACKING (05036)                     *         
***********************************************************************         
                                                                                
UPDTGCY  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLOTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTGCY                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITGCY                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGCYC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UGCY10   GOTO1 VTOXGCYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UGCYX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UGCYX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UGCY20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UGCY20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UGCY40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UGCY40                                                           
*                                                                               
UGCY30   CLI   COPYFLAG,X'01'                                                   
         JNE   UGCY10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UGCY10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UGCY40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UGCY30                                                           
*                                                                               
UGCYX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AGENCY RECORD AT R2                                          *         
***********************************************************************         
                                                                                
FILTGCY  NTR1  BASE=*,LABEL=*                                                   
         USING TLOTD,R2                                                         
         CLI   TLOTCD,TLOTCDQ      TRACKING RECORD?                             
         JNE   NO                                                               
         MVI   ELCODE,TAGCELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         USING TAGCD,R2                                                         
         CLC   TAGCUNI,=C'SAG'                                                  
         JE    YES                                                              
         CLC   TAGCUNI,=C'AFT'                                                  
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENCY RECORD                                            *         
***********************************************************************         
                                                                                
INITGCY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX36LEN          R1=L'GCY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD AGENT RECORD AND SUBSETS                                       *         
*      AGENT RECORD (05038)                                           *         
***********************************************************************         
                                                                                
LOADAGT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLAND,R2                                                         
         XC    TLANKEY,TLANKEY     CLEAR FILL THE KEY                           
         MVI   TLANCD,TLANCDQ      RECORD 50 FOR AGENT                          
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LAGT10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LAGTX                                                            
         CLC   TLANCD(TLANCD+L'TLANCD-TLAND),IOKEY                              
         JNE   LAGTX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTAGT            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LAGT40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITAGT            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXAGTC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LAGT20   GOTO1 VTOXAGTC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LAGT40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LAGT40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LAGT40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LAGT30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LAGT30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LAGT20              TOO MANY IOS                                 
         J     LAGTX                                                            
*                                                                               
LAGT40   MVC   IOKEY(L'TLANKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LAGT50                                                           
         GOTO1 AREADHI                                                          
         JE    LAGT50                                                           
         DC    H'0'                                                             
*                                                                               
LAGT50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LAGT10                                                           
*                                                                               
LAGTX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE AGENT RECORD AND SUBSETS                                     *         
*        AGENT RECORD (05038)                                         *         
***********************************************************************         
                                                                                
UPDTAGT  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLAND,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTAGT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITAGT                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXAGTC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UAGT10   GOTO1 VTOXAGTC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UAGTX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UAGTX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UAGT20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UAGT20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UAGT40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UAGT40                                                           
*                                                                               
UAGT30   CLI   COPYFLAG,X'01'                                                   
         JNE   UAGT10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UAGT10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UAGT40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UAGT30                                                           
*                                                                               
UAGTX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER AGENT RECORD AT R2                                           *         
***********************************************************************         
                                                                                
FILTAGT  NTR1  BASE=*,LABEL=*                                                   
         USING TLAND,R2                                                         
         CLI   TLANCD,TLANCDQ      AGENT RECORD?                                
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE AGENT RECORD                                             *         
***********************************************************************         
                                                                                
INITAGT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX38LEN          R1=L'AGT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD GUARANTEE TRACKING RECORD AND SUBSETS                          *         
*      GUARANTEE TRACKING RECORD (05039)                              *         
***********************************************************************         
                                                                                
LOADGTK  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLGTD,R2                                                         
         XC    TLGTKEY,TLGTKEY     CLEAR FILL THE KEY                           
         MVI   TLGTCD,TLGTCDQ      RECORD A8 FOR GUARANTEE TRACKING             
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LGTK10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LGTKX                                                            
         CLC   TLGTCD(TLGTCD+L'TLGTCD-TLGTD),IOKEY                              
         JNE   LGTKX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTGTK            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LGTK40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITGTK            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGTKC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LGTK20   GOTO1 VTOXGTKC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LGTK40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LGTK40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LGTK40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LGTK30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LGTK30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LGTK20              TOO MANY IOS                                 
         J     LGTKX                                                            
*                                                                               
LGTK40   MVC   IOKEY(L'TLGTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LGTK50                                                           
         GOTO1 AREADHI                                                          
         JE    LGTK50                                                           
         DC    H'0'                                                             
*                                                                               
LGTK50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LGTK10                                                           
*                                                                               
LGTKX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE GUARANTEE TRACKING RECORD AND SUBSETS                        *         
*        GUARANTEE TRACKING RECORD (05039)                            *         
***********************************************************************         
                                                                                
UPDTGTK  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLGTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTGTK                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITGTK                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXGTKC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UGTK10   GOTO1 VTOXGTKC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UGTKX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UGTKX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UGTK20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UGTK20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UGTK40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UGTK40                                                           
*                                                                               
UGTK30   CLI   COPYFLAG,X'01'                                                   
         JNE   UGTK10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UGTK10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UGTK40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UGTK30                                                           
*                                                                               
UGTKX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER GUARANTEE TRACKING RECORD AT R2                              *         
***********************************************************************         
                                                                                
FILTGTK  NTR1  BASE=*,LABEL=*                                                   
         USING TLGTD,R2                                                         
         CLI   TLGTCD,TLGTCDQ      GUARANTEE TRACKING RECORD?                   
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE GUARANTEE TRACKING RECORD                                *         
***********************************************************************         
                                                                                
INITGTK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX39LEN          R1=L'GTK RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD COMMENT RECORD AND SUBSETS                                     *         
*      COMMENT RECORD (05037)                                         *         
***********************************************************************         
                                                                                
LOADVCL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST AGY RECORD             
         USING TLCMD,R2                                                         
         XC    TLCMKEY,TLCMKEY     CLEAR FILL THE KEY                           
         MVI   TLCMCD,TLCMCDQ      RECORD C4 FOR COMMENT                        
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LVCL10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LVCLX                                                            
         CLC   TLCMCD(TLCMCD+L'TLCMCD-TLCMD),IOKEY                              
         JNE   LVCLX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AFILTVCL            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LVCL40                                                           
*                                                                               
         GOTO1 AINITVCL            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXVCLC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LVCL20   GOTO1 VTOXVCLC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LVCL40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LVCL40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LVCL40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LVCL30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LVCL30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LVCL20              TOO MANY IOS                                 
         J     LVCLX                                                            
*                                                                               
LVCL40   MVC   IOKEY(L'TLCMKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LVCL50                                                           
         GOTO1 AREADHI                                                          
         JE    LVCL50                                                           
         DC    H'0'                                                             
*                                                                               
LVCL50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LVCL10                                                           
*                                                                               
LVCLX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE COMMENT RECORD AND SUBSETS                                   *         
*        COMMENT RECORD (05038)                                       *         
***********************************************************************         
                                                                                
UPDTVCL  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCMD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTVCL                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITVCL                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXVCLC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UVCL10   GOTO1 VTOXVCLC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UVCLX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UVCLX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UVCL20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UVCL20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UVCL40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UVCL40                                                           
*                                                                               
UVCL30   CLI   COPYFLAG,X'01'                                                   
         JNE   UVCL10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UVCL10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UVCL40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UVCL30                                                           
*                                                                               
UVCLX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER COMMENT RECORD AT R2                                         *         
***********************************************************************         
                                                                                
FILTVCL  NTR1  BASE=*,LABEL=*                                                   
         USING TLCMD,R2                                                         
         CLI   TLCMCD,TLCMCDQ      COMMENT RECORD?                              
         JNE   NO                                                               
         CLI   TLCMTYP,TLCMTCOM                                                 
         JNE   NO                                                               
         MVI   ELCODE,TAXCELQ                                                   
         BRAS  RE,GETEL                                                         
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE COMMENT RECORD                                           *         
***********************************************************************         
                                                                                
INITVCL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX37LEN          R1=L'VCL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD OFFICE RECORD AND SUBSETS                                      *         
*      OFFICE RECORD (05041)                                          *         
***********************************************************************         
                                                                                
LOADOFF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST OFF RECORD             
         USING TLOFD,R2                                                         
         XC    TLOFKEY,TLOFKEY     CLEAR FILL THE KEY                           
         MVI   TLOFCD,TLOFCDQ      RECORD 0C FOR OFFICE                         
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LOFF10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LOFFX                                                            
         CLC   TLOFCD(TLOFCD+L'TLOFCD-TLOFD),IOKEY                              
         JNE   LOFFX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTOFF            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LOFF40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITOFF            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXOFFC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LOFF20   GOTO1 VTOXOFFC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LOFF40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LOFF40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LOFF40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LOFF30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LOFF30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LOFF20              TOO MANY IOS                                 
         J     LOFFX                                                            
*                                                                               
LOFF40   MVC   IOKEY(L'TLOFKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LOFF50                                                           
         GOTO1 AREADHI                                                          
         JE    LOFF50                                                           
         DC    H'0'                                                             
*                                                                               
LOFF50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LOFF10                                                           
*                                                                               
LOFFX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE OFFICE RECORD AND SUBSETS                                    *         
*        OFFICE RECORD (05041)                                        *         
***********************************************************************         
                                                                                
UPDTOFF  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLOFD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTOFF                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITOFF                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXOFFC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UOFF10   GOTO1 VTOXOFFC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UOFFX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UOFFX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UOFF20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UOFF20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UOFF40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UOFF40                                                           
*                                                                               
UOFF30   CLI   COPYFLAG,X'01'                                                   
         JNE   UOFF10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UOFF10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UOFF40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UOFF30                                                           
*                                                                               
UOFFX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER OFFICE RECORD AT R2                                          *         
***********************************************************************         
                                                                                
FILTOFF  NTR1  BASE=*,LABEL=*                                                   
         USING TLOFD,R2                                                         
         CLI   TLOFCD,TLOFCDQ      OFFICE RECORD?                               
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE OFFICE RECORD                                            *         
***********************************************************************         
                                                                                
INITOFF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX41LEN          R1=L'OFF RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD STAFF RECORD AND SUBSETS                                       *         
*      STAFF RECORD (05045)                                           *         
*      LIMTED AGY/CLIENT RECORD (05046)                                         
***********************************************************************         
                                                                                
LOADSTF  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
         XC    SVVALS(SVVALLNQ),SVVALS                                          
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST STAFF RECORD           
         USING TLSTD,R2                                                         
         XC    TLSTKEY,TLSTKEY     CLEAR FILL THE KEY                           
         MVI   TLSTCD,TLSTCDQ                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LSTF10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LSTFX                                                            
         CLC   TLSTCD(TLSTCD+L'TLSTCD-TLSTD),IOKEY                              
         JNE   LSTFX               ALL DONE IF COMPANY HEXCOMP CHANGES          
                                                                                
         GOTO1 AFILTSTF            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LSTF40                                                           
                                                                                
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
                                                                                
         GOTO1 AINITSTF            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXSTFF,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LSTF20   GOTO1 VTOXSTFF,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LSTF40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LSTF40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LSTF40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LSTF30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LSTF30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LSTF20              TOO MANY IOS                                 
         J     LSTFX                                                            
*                                                                               
LSTF40   MVC   IOKEY(L'TLSTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LSTF50                                                           
         GOTO1 AREADHI                                                          
         JE    LSTF50                                                           
         DC    H'0'                                                             
*                                                                               
LSTF50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LSTF10                                                           
*                                                                               
LSTFX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE STAFF RECORD AND SUBSETS                                     *         
*        STAFF RECORD (05045)                                         *         
*        STAFF LIMITED AGENCY/CLIENT RECORD (05046)                             
***********************************************************************         
                                                                                
UPDTSTF  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLSTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
         GOTO1 AFILTSTF                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITSTF                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXSTFF,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
USTF10   GOTO1 VTOXSTFF,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    USTFX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   USTFX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    USTF20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
USTF20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    USTF40                                                           
         TM    TLSTSTAT,X'80'      IS STAFF DELETED?                            
         JO    USTF25                                                           
         MVI   COPYFLAG,X'01'                                                   
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05045'                        
         JE    *+14                                                             
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05046'                        
         JNE   USTF30                                                           
USTF25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     USTF40                                                           
*                                                                               
USTF30   CLI   COPYFLAG,X'01'                                                   
         JNE   USTF10                                                           
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
USTF40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLSTSTAT,X'80'      IS ASSET DELETED?                            
         JO    USTFX                                                            
         J     USTF30                                                           
*                                                                               
USTFX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER ASSET RECORD AT R2                                           *         
***********************************************************************         
                                                                                
FILTSTF  NTR1  BASE=*,LABEL=*                                                   
         USING TLSTD,R2                                                         
         CLI   TLSTCD,TLSTCDQ     STAFF RECORD?                                 
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE ASSET RECORD                                             *         
***********************************************************************         
                                                                                
INITSTF  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX45LEN          R1=L'ASS RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CHECK RECORD AND SUBSETS                                       *         
*      CHECK RECORD (05057)                                           *         
***********************************************************************         
                                                                                
LOADCHK  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CHK RECORD             
         USING TLCKD,R2                                                         
         XC    TLCKKEY,TLCKKEY     CLEAR FILL THE KEY                           
         MVI   TLCKCD,TLCKCDQ      RECORD 5D FOR CHECK                          
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCHK10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LCHKX                                                            
*                                                                               
         CLI   0(R2),TLCKCDQ                                                    
         JNE   LCHKX                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AFILTCHK            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LCHK40                                                           
*                                                                               
         GOTO1 AINITCHK            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXCHKC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS,VT00A88             
*                                  GET NEXT UNCOMMITTED RECORD                  
LCHK20   GOTO1 VTOXCHKC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LCHK40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LCHK40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LCHK40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LCHK30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LCHK30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LCHK20              TOO MANY IOS                                 
         J     LCHKX                                                            
*                                                                               
LCHK40   MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD                          
         GOTO1 ACHKSEQ                                                          
         JE    LCHK50                                                           
         GOTO1 AREADHI                                                          
         JE    LCHK50                                                           
         DC    H'0'                                                             
*                                                                               
LCHK50   GOTO1 VDATAMGR,DMCB,DMRSEQ,CHKDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCHK10                                                           
*                                                                               
LCHKX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CHECK RECORD AND SUBSETS                                     *         
*        CHECK RECORD (05057)                                         *         
***********************************************************************         
                                                                                
UPDTCHK  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCKD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCHK                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCHK            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXCHKC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
UCHK10   GOTO1 VTOXCHKC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UCHKX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UCHKX                                                            
         CLI   SXDTPLFM,0                                                       
         JE    UCHK20                                                           
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UCHK20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UCHK40                                                           
         TM    TLCKSTAT,X'80'      IS INVOICE DELETED?                          
         JO    UCHK25                                                           
         CLC   TOXRETYP-TOXRECD(L'TOXRETYP,RF),=C'05057'                        
         JNE   UCHK35                                                           
         MVI   COPYFLAG,X'01'                                                   
UCHK25   MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UCHK40                                                           
*                                                                               
UCHK30   CLI   COPYFLAG,X'01'                                                   
         JNE   UCHK10                                                           
UCHK35   L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UCHK40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         TM    TLCKSTAT,X'80'      IS IT DELETED?                               
         JO    UCHKX                                                            
         J     UCHK30                                                           
*                                                                               
UCHKX    J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CHECK RECORD AT R2                                           *         
***********************************************************************         
                                                                                
FILTCHK  NTR1  BASE=*,LABEL=*                                                   
         USING TACDD,R2                                                         
         MVI   ELCODE,TACDELQ                                                   
         BRAS  RE,GETEL                                                         
         JNE   NO                                                               
         OC    TACDCHK,TACDCHK     CHECK NUMBER?                                
         JZ    YES                                                              
         CLI   TACDDTE,X'A4'       PRIOR TO 2004?                               
         JL    NO                  DON'T KEEP IT                                
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CHECK RECORD                                             *         
***********************************************************************         
                                                                                
INITCHK  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX57LEN          R1=L'CHK RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT GROUP RECORD (05065)                                             
***********************************************************************         
                                                                                
LOADPGR  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PGR RECORD             
         USING TLPGD,R2                                                         
         XC    TLPGKEY,TLPGKEY     CLEAR FILL THE KEY                           
         MVI   TLPGCD,TLPGCDQ      RECORD 0C FOR OFFICE                         
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPGR10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LPGRX                                                            
*                                                                               
         GOTO1 AFILTPGR            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LPGR40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITPGR            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXPGRC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LPGR20   GOTO1 VTOXPGRC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LPGR40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LPGR40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LPGR40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LPGR30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LPGR30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LPGR20              TOO MANY IOS                                 
         J     LPGRX                                                            
*                                                                               
LPGR40   MVC   IOKEY(L'TLPGKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LPGR50                                                           
         GOTO1 AREADHI                                                          
         JE    LPGR50                                                           
         DC    H'0'                                                             
*                                                                               
LPGR50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPGR10                                                           
*                                                                               
LPGRX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT GROUP RECORD (05065)                                 *         
***********************************************************************         
                                                                                
UPDTPGR  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLPGD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPGR                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPGR                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXPGRC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UPGR10   GOTO1 VTOXPGRC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UPGRX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPGRX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPGR20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UPGR20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UPGR40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UPGR40                                                           
*                                                                               
UPGR30   CLI   COPYFLAG,X'01'                                                   
         JNE   UPGR10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UPGR10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPGR40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UPGR30                                                           
*                                                                               
UPGRX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCT GROUP RECORD AT R2                                   *         
***********************************************************************         
                                                                                
FILTPGR  NTR1  BASE=*,LABEL=*                                                   
         USING TLPGD,R2                                                         
         CLI   TLPGCD,TLPGCDQ                                                   
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRODUCT GROUP RECORD                                     *         
***********************************************************************         
                                                                                
INITPGR  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX65LEN          R1=L'PGR RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PRODUCT GROUP RECORD (05065)                                             
***********************************************************************         
                                                                                
LOADPTY  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST PTY RECORD             
         USING TLPTD,R2                                                         
         XC    TLPTKEY,TLPTKEY     CLEAR FILL THE KEY                           
         MVI   TLPTCD,TLPTCDQ      RECORD 0C FOR OFFICE                         
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LPTY10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LPTYX                                                            
*                                                                               
         GOTO1 AFILTPTY            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LPTY40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITPTY            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXPTYC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LPTY20   GOTO1 VTOXPTYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LPTY40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LPTY40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LPTY40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LPTY30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LPTY30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LPTY20              TOO MANY IOS                                 
         J     LPTYX                                                            
*                                                                               
LPTY40   MVC   IOKEY(L'TLPTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LPTY50                                                           
         GOTO1 AREADHI                                                          
         JE    LPTY50                                                           
         DC    H'0'                                                             
*                                                                               
LPTY50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LPTY10                                                           
*                                                                               
LPTYX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT GROUP RECORD (05065)                                 *         
***********************************************************************         
                                                                                
UPDTPTY  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLPTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTPTY                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITPTY                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXPTYC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UPTY10   GOTO1 VTOXPTYC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UPTYX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UPTYX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UPTY20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UPTY20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UPTY40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UPTY40                                                           
*                                                                               
UPTY30   CLI   COPYFLAG,X'01'                                                   
         JNE   UPTY10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UPTY10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UPTY40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UPTY30                                                           
*                                                                               
UPTYX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCT GROUP RECORD AT R2                                   *         
***********************************************************************         
                                                                                
FILTPTY  NTR1  BASE=*,LABEL=*                                                   
         USING TLPTD,R2                                                         
         CLI   TLPTCD,TLPTCDQ                                                   
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRODUCT GROUP RECORD                                     *         
***********************************************************************         
                                                                                
INITPTY  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX66LEN          R1=L'PTY RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD MARKET RECORDS (05053-05056)                                             
***********************************************************************         
                                                                                
LOADMKT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST MKT RECORD             
         USING TLMTD,R2                                                         
         XC    TLMTKEY,TLMTKEY     CLEAR FILL THE KEY                           
         MVI   TLMTCD,TLMTCDQ                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LMKT10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LMKTX                                                            
*                                                                               
         GOTO1 AFILTMKT            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LMKT40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITMKT            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXMKTC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LMKT20   GOTO1 VTOXMKTC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LMKT40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LMKT40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LMKT40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LMKT30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LMKT30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LMKT20              TOO MANY IOS                                 
         J     LMKTX                                                            
*                                                                               
LMKT40   MVC   IOKEY(L'TLMTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LMKT50                                                           
         GOTO1 AREADHI                                                          
         JE    LMKT50                                                           
         DC    H'0'                                                             
*                                                                               
LMKT50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LMKT10                                                           
*                                                                               
LMKTX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT GROUP RECORD (05065)                                 *         
***********************************************************************         
                                                                                
UPDTMKT  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLMTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTMKT                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITMKT                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXMKTC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UMKT10   GOTO1 VTOXMKTC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UMKTX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UMKTX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UMKT20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UMKT20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UMKT40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UMKT40                                                           
*                                                                               
UMKT30   CLI   COPYFLAG,X'01'                                                   
         JNE   UMKT10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UMKT10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UMKT40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UMKT30                                                           
*                                                                               
UMKTX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER PRODUCT GROUP RECORD AT R2                                   *         
***********************************************************************         
                                                                                
FILTMKT  NTR1  BASE=*,LABEL=*                                                   
         USING TLMTD,R2                                                         
         CLI   TLMTCD,TLMTCDQ                                                   
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE PRODUCT GROUP RECORD                                     *         
***********************************************************************         
                                                                                
INITMKT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX53LEN          R1=L'MKT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD USAGE HISTORY RECORDS (5049-5052)                                        
***********************************************************************         
                                                                                
LOADUSE  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST USE RECORD             
         USING TLUHD,R2                                                         
         XC    TLUHKEY,TLUHKEY     CLEAR FILL THE KEY                           
         MVI   TLUHCD,TLUHCDQ                                                   
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LUSE10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LUSEX                                                            
         MVC   SVUHKEY,0(R2)                                                    
*                                                                               
         GOTO1 AFILTUSE            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LUSE40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITUSE            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXUSEC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LUSE20   GOTO1 VTOXUSEC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LUSE40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LUSE40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LUSE40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LUSE30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LUSE30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LUSE20              TOO MANY IOS                                 
         J     LUSEX                                                            
*                                                                               
LUSE40   MVC   IOKEY(L'TLUHKEY),SVUHKEY READ NEXT RECORD - SEQUENT              
         GOTO1 ACHKSEQ                                                          
         JE    LUSE50                                                           
         GOTO1 AREADHI                                                          
         JE    LUSE50                                                           
         DC    H'0'                                                             
*                                                                               
LUSE50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LUSE10                                                           
*                                                                               
LUSEX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
SVUHKEY  DS    XL32                                                             
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE PRODUCT GROUP RECORD (05065)                                 *         
***********************************************************************         
                                                                                
UPDTUSE  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLMTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTUSE                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITUSE                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXUSEC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UUSE10   GOTO1 VTOXUSEC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UUSEX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UUSEX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UUSE20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UUSE20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UUSE40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UUSE40                                                           
*                                                                               
UUSE30   CLI   COPYFLAG,X'01'                                                   
         JNE   UUSE10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UUSE10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UUSE40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UUSE30                                                           
*                                                                               
UUSEX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER USAGE HISTORY RECORD AT R2                                   *         
***********************************************************************         
                                                                                
FILTUSE  NTR1  BASE=*,LABEL=*                                                   
         USING TLUHD,R2                                                         
         CLI   TLUHCD,TLUHCDQ                                                   
         JNE   NO                                                               
         OC    TLUHCSEQ,TLUHCSEQ                                                
         JNZ   NO                                                               
         J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE USAGE HISTORY RECORD                                     *         
***********************************************************************         
                                                                                
INITUSE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX49LEN          R1=L'USE RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* NETWORK RECORD (05067)                                                        
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADNET  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING NETTABD,R2                                                       
         LA    R2,NETTAB                                                        
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LNET10   CLI   0(R2),EOF                                                        
         BE    LNETX                                                            
*                                                                               
         GOTO1 AINITNET            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX67LEN)                                          
         MVC   TOXRETYP,=C'05067'                                               
*                                                                               
         MVI   TX67CODE-1,MXTRTQ                                                
         MVI   TX67NAME-1,MXTRTQ                                                
*                                                                               
         MVC   TX67CODE,NETCODE                                                 
         MVC   TX67NAME,NETNAME                                                 
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         AP    PK67CNT,=P'1'                                                    
*                                                                               
         AHI   R2,NETTLNQ                                                       
         B     LNET10              TOO MANY IOS                                 
*                                                                               
LNETX    J     YES                                                              
         DROP  R2,R3                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
NETTAB   DC    C'A',CL16'ABC'                                                   
         DC    C'B',CL16'BOUNCE TV'                                             
         DC    C'C',CL16'CBS'                                                   
         DC    C'E',CL16'METV'                                                  
         DC    C'F',CL16'FOX'                                                   
         DC    C'I',CL16'ITN'                                                   
         DC    C'M',CL16'MNT'                                                   
         DC    C'N',CL16'NBC'                                                   
         DC    C'S',CL16'SYNDICATION'                                           
         DC    C'T',CL16'THIS TV'                                               
         DC    C'W',CL16'CW'                                                    
         DC    C'X',CL16'ION/PAX'                                               
         DC    C'Y',CL16'ANTENNA TV'                                            
         DC    AL1(EOF)                                                         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* INITIALIZE RECORD COUNT RECORD (05999)                              *         
***********************************************************************         
                                                                                
INITNET  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX67LEN          R1=L'NET RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* COUNT RECORD (05999)                                                *         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOADCNT  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING RECTBD,R2                                                        
         LA    R2,RECTAB                                                        
         CLC   TYPECODE,=C'INV'                                                 
         BNE   *+8                                                              
         LA    R2,RECTABIN                                                      
*                                                                               
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LCNT10   CLI   0(R2),EOF                                                        
         BE    LCNTX                                                            
*                                                                               
         GOTO1 AINITCNT            INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX99LEN)                                          
         MVC   TOXRETYP,=C'05999'                                               
*                                                                               
         MVI   TX99REC-1,MXTRTQ                                                 
         MVI   TX99CNT-1,MXTRTQ                                                 
*                                                                               
         MVC   TX99REC,RECRECNO                                                 
         SR    RE,RE                                                            
         ICM   RE,3,RECCNTDS                                                    
         LA    RE,WORKD(RE)                                                     
         ZAP   DUB,0(8,RE)                                                      
         CVB   R1,DUB                                                           
*        STCM  R1,15,TX99CNT                                                    
         ST    R1,FULL                                                          
         EDIT  FULL,TX99CNT,0,ALIGN=RIGHT,ZERO=NOBLANK                          
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         AHI   R2,RECTLNQ                                                       
         B     LCNT10              TOO MANY IOS                                 
*                                                                               
LCNTX    J     YES                                                              
         DROP  R2,R3                                                            
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* INITIALIZE RECORD COUNT RECORD (05999)                              *         
***********************************************************************         
                                                                                
INITCNT  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX99LEN          R1=L'CNT RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
*                                                                               
***********************************************************************         
* TABLES                                                              *         
***********************************************************************         
                                                                                
RECTAB   DS    0C                                                               
         DC    C'05000',AL2(PK00CNT-WORKD)                                      
         DC    C'05001',AL2(PK01CNT-WORKD)                                      
         DC    C'05002',AL2(PK02CNT-WORKD)                                      
         DC    C'05003',AL2(PK03CNT-WORKD)                                      
         DC    C'05004',AL2(PK04CNT-WORKD)                                      
         DC    C'05005',AL2(PK05CNT-WORKD)                                      
         DC    C'05006',AL2(PK06CNT-WORKD)                                      
         DC    C'05007',AL2(PK07CNT-WORKD)                                      
         DC    C'05008',AL2(PK08CNT-WORKD)                                      
         DC    C'05009',AL2(PK09CNT-WORKD)                                      
         DC    C'05010',AL2(PK10CNT-WORKD)                                      
         DC    C'05011',AL2(PK11CNT-WORKD)                                      
         DC    C'05012',AL2(PK12CNT-WORKD)                                      
         DC    C'05013',AL2(PK13CNT-WORKD)                                      
         DC    C'05014',AL2(PK14CNT-WORKD)                                      
         DC    C'05015',AL2(PK15CNT-WORKD)                                      
         DC    C'05016',AL2(PK16CNT-WORKD)                                      
         DC    C'05017',AL2(PK17CNT-WORKD)                                      
         DC    C'05018',AL2(PK18CNT-WORKD)                                      
         DC    C'05019',AL2(PK19CNT-WORKD)                                      
         DC    C'05020',AL2(PK20CNT-WORKD)                                      
         DC    C'05021',AL2(PK21CNT-WORKD)                                      
         DC    C'05022',AL2(PK22CNT-WORKD)                                      
         DC    C'05023',AL2(PK23CNT-WORKD)                                      
         DC    C'05024',AL2(PK24CNT-WORKD)                                      
         DC    C'05025',AL2(PK25CNT-WORKD)                                      
         DC    C'05026',AL2(PK26CNT-WORKD)                                      
         DC    C'05027',AL2(PK27CNT-WORKD)                                      
         DC    C'05028',AL2(PK28CNT-WORKD)                                      
         DC    C'05029',AL2(PK29CNT-WORKD)                                      
         DC    C'05030',AL2(PK30CNT-WORKD)                                      
         DC    C'05031',AL2(PK31CNT-WORKD)                                      
         DC    C'05032',AL2(PK32CNT-WORKD)                                      
         DC    C'05033',AL2(PK33CNT-WORKD)                                      
         DC    C'05034',AL2(PK34CNT-WORKD)                                      
         DC    C'05035',AL2(PK35CNT-WORKD)                                      
         DC    C'05036',AL2(PK36CNT-WORKD)                                      
         DC    C'05037',AL2(PK37CNT-WORKD)                                      
         DC    C'05038',AL2(PK38CNT-WORKD)                                      
         DC    C'05039',AL2(PK39CNT-WORKD)                                      
         DC    C'05040',AL2(PK40CNT-WORKD)                                      
         DC    C'05041',AL2(PK41CNT-WORKD)                                      
         DC    C'05042',AL2(PK42CNT-WORKD)                                      
         DC    C'05043',AL2(PK43CNT-WORKD)                                      
         DC    C'05044',AL2(PK44CNT-WORKD)                                      
         DC    C'05045',AL2(PK45CNT-WORKD)                                      
         DC    C'05046',AL2(PK46CNT-WORKD)                                      
         DC    C'05047',AL2(PK47CNT-WORKD)                                      
         DC    C'05048',AL2(PK48CNT-WORKD)                                      
         DC    C'05049',AL2(PK49CNT-WORKD)                                      
         DC    C'05050',AL2(PK50CNT-WORKD)                                      
         DC    C'05051',AL2(PK51CNT-WORKD)                                      
         DC    C'05052',AL2(PK52CNT-WORKD)                                      
         DC    C'05053',AL2(PK53CNT-WORKD)                                      
         DC    C'05054',AL2(PK54CNT-WORKD)                                      
         DC    C'05055',AL2(PK55CNT-WORKD)                                      
         DC    C'05056',AL2(PK56CNT-WORKD)                                      
         DC    C'05057',AL2(PK57CNT-WORKD)                                      
         DC    C'05058',AL2(PK58CNT-WORKD)                                      
         DC    C'05059',AL2(PK59CNT-WORKD)                                      
         DC    C'05060',AL2(PK60CNT-WORKD)                                      
         DC    C'05061',AL2(PK61CNT-WORKD)                                      
         DC    C'05062',AL2(PK62CNT-WORKD)                                      
         DC    C'05063',AL2(PK63CNT-WORKD)                                      
         DC    C'05064',AL2(PK64CNT-WORKD)                                      
         DC    C'05065',AL2(PK65CNT-WORKD)                                      
         DC    C'05066',AL2(PK66CNT-WORKD)                                      
         DC    C'05067',AL2(PK67CNT-WORKD)                                      
         DC    C'05068',AL2(PK68CNT-WORKD)                                      
         DC    C'05069',AL2(PK69CNT-WORKD)                                      
         DC    C'05070',AL2(PK70CNT-WORKD)                                      
         DC    C'05071',AL2(PK71CNT-WORKD)                                      
         DC    C'05072',AL2(PK72CNT-WORKD)                                      
         DC    C'05073',AL2(PK73CNT-WORKD)                                      
         DC    C'05074',AL2(PK74CNT-WORKD)                                      
         DC    C'05075',AL2(PK75CNT-WORKD)                                      
         DC    C'05076',AL2(PK76CNT-WORKD)                                      
         DC    C'05077',AL2(PK77CNT-WORKD)                                      
         DC    C'05078',AL2(PK78CNT-WORKD)                                      
         DC    C'05079',AL2(PK79CNT-WORKD)                                      
         DC    C'05080',AL2(PK80CNT-WORKD)                                      
         DC    C'05081',AL2(PK81CNT-WORKD)                                      
         DC    C'05082',AL2(PK82CNT-WORKD)                                      
         DC    C'05083',AL2(PK83CNT-WORKD)                                      
         DC    C'05084',AL2(PK84CNT-WORKD)                                      
         DC    C'05085',AL2(PK85CNT-WORKD)                                      
         DC    C'05086',AL2(PK86CNT-WORKD)                                      
         DC    AL1(EOF)                                                         
*                                                                               
RECTABIN DS    0C                                                               
         DC    C'05010',AL2(PK10CNT-WORKD)                                      
         DC    C'05015',AL2(PK15CNT-WORKD)                                      
         DC    C'05048',AL2(PK48CNT-WORKD)                                      
         DC    C'05049',AL2(PK49CNT-WORKD)                                      
         DC    AL1(EOF)                                                         
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
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
*                                                                               
CHG      USING TLRCD,R2                                                         
         CLI   RRECTY,3            ADD OF NEW RECORD?                           
         JE    YES                 YES                                          
*                                                                               
         L     R5,DXACPYB          GET COPY RECORD ADDRESS                      
         LA    R4,RECVHDR+L'RECVHDR                                             
CPY      USING TLRCD,R4                                                         
*                                                                               
         CLC   CHG.TLRCLEN,CPY.TLRCLEN                                          
         JNE   YES                 RECORD LENGTH HAS CHANGED                    
         XR    R3,R3                                                            
         ICM   R3,3,CPY.TLRCLEN                                                 
         LR    R5,R3                                                            
         CLCL  R2,R4               COMPARE TWO RECORDS                          
         JNE   YES                                                              
         J     NO                  RECORDS ARE IDENTICAL                        
         DROP  CHG,CPY                                                          
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT TALENT RECORDS IN LOAD MODE                   *         
* R2 = A(TALENT DIRECTORY RECORD BUFFER)                              *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(FORMAT CONVERT ROUTINE)                                      *         
***********************************************************************         
                                                                                
TALLOAD  NTR1  BASE=*,LABEL=*                                                   
         MVC   ALPARMS,0(R1)                                                    
         LM    R3,R5,0(R1)                                                      
         LTR   R5,R5               FILTER ROUTINE?                              
         JZ    TLOA10                                                           
         GOTO1 (R5)                FILTER RECORD                                
         JNE   TLOA40              NOT VALID - GET NEXT                         
*                                                                               
TLOA10   GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
TLOA20   GOTO1 (R4)                INITIALIZE EXTRACT BUFFER                    
*                                  CALL RECORD EXTRACT ROUTINE                  
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),0                                  
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    TLOA40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    TLOA40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   TLOA40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    TLOA30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 ALPACNVX,DMCB,(R7)                                               
*                                                                               
TLOA30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
TLOA40   GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JNE   NO                  TOO MANY IOS                                 
*                                                                               
         MVC   IOKEY(L'TLAYKEY),0(R2) READ NEXT RECORD - SEQUENT                
         CLI   TYPEDEEP,0                                                       
         JNE   TLOA50              YES, NEED TO READ HIGH                       
*                                                                               
         GOTO1 ACHKSEQ             SEE IF READ SEQUENCE BROKEN                  
         JE    TLOA60              NO                                           
         GOTO1 AREADHI                                                          
         JE    TLOA60                                                           
         DC    H'0'                                                             
*                                                                               
TLOA50   LA    RF,IOKEY            READ NEXT RECORD - HIGH                      
         USING TLDRD,RF                                                         
         SR    RE,RE                                                            
         IC    RE,TYPEDEEP         LEVEL TO FILTER AT                           
         BCTR  RE,0                                                             
         LA    R1,TLDRCD                                                        
         AR    R1,RE                                                            
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
TLOA60   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT CONTROL TEMPO RECORDS IN LOAD MODE            *         
* R2 = A(TALENT DIRECTORY RECORD BUFFER)                              *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(EXTRACT RECORD INITIALISATION ROUTINE)                       *         
* P3 = A(RECORD FILTER ROUTINE)                                       *         
* P4 = A(FORMAT CONVERT ROUTINE)                                      *         
***********************************************************************         
                                                                                
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
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),0                                  
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
*                                                                               
CXLOAD50 GOTO1 VDATAMGR,DMCB,(X'00',DMRSEQ),CTFILE,IOKEY,(R2)                   
         J     YES                                                              
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO EXTRACT TALENT RECORDS IN UPDATE MODE                 *         
* R2 = A(TALENT RECORD BUFFER)                                        *         
* P1 = A(EXTRACT ROUTINE)                                             *         
* P2 = A(3 CHAR MNEMONIC)                                             *         
***********************************************************************         
                                                                                
TALUPDT  NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         GOTO1 (R3),DMCB,DXAXREC,(R2),0,(R6),0                                  
         CLI   DMCB+8,FF           DATA NOT COMPLETE - DO NOT WRITE             
         JE    NO                                                               
         TM    DMCB+8,X'80'                                                     
         JO    NO                                                               
*                                                                               
         CLI   DXWRITE,C'Y'                                                     
         JNE   YES                 DO NOT WRITE RECORD                          
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    TALU20              DO NOT CONVERT RECORD                        
*                                                                               
         CLI   DXACTION,C'C'       SPECIAL CODE FOR CHANGES                     
         JNE   TALU10                                                           
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
         GOTO1 (R3),DMCB,ACOPYBUF,(R2),0,(R6)         BUILD COPY REC            
*                                                                               
         L     R0,DXAXREC          R0=A(CHANGE SQL RECORD)                      
         L     RE,ACOPYBUF         RE=A(COPY SQL RECORD)                        
         LH    RF,0(RE)            RF=L'RECORD                                  
*                                                                               
         LH    R1,=AL2(TX00AGY-TOXRECD) DISP TO COMPANY                         
         AR    R0,R1               BUMP TO COMPANY CODE IN BOTH RECS            
         AR    RE,R1                                                            
         SR    RF,R1               REDUCE LENGTH APPROPRIATELY                  
         SH    RF,=AL2(L'TX00X)    DON'T LOOK AT TRAILING BYTES                 
         LR    R1,RF                                                            
         CLCL  R0,RE               IF EXTRACT VERSIONS OF COPY/CHANGE           
         JE    YES                 ARE THE SAME THEN SKIP                       
*                                                                               
TALU10   GOTO1 VTOXCNVX,DMCB,(R7)                                               
         L     RF,DXASQLB                                                       
*                                                                               
TALU20   GOTO1 DXPUT,DMCB,(RF),(R7)                                             
         J     YES                                                              
         DROP  R5                                                               
*                                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD CONTROL RECORD AND SUBSETS                                     *         
*      CONTROL RECORD (05070)                                         *         
***********************************************************************         
                                                                                
LOADCTL  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST CTL RECORD             
         USING TLCTD,R2                                                         
         XC    TLCTKEY,TLCTKEY     CLEAR FILL THE KEY                           
         MVI   TLCTCD,TLCTCDQ      RECORD 3C FOR CONTROL                        
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LCTL10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LCTLX                                                            
         CLC   TLCTCD(TLCTCD+L'TLCTCD-TLCTD),IOKEY                              
         JNE   LCTLX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTCTL            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LCTL40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITCTL            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXCTLC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LCTL20   GOTO1 VTOXCTLC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LCTL40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LCTL40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LCTL40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LCTL30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LCTL30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LCTL20              TOO MANY IOS                                 
         J     LCTLX                                                            
*                                                                               
LCTL40   MVC   IOKEY(L'TLCTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LCTL50                                                           
         GOTO1 AREADHI                                                          
         JE    LCTL50                                                           
         DC    H'0'                                                             
*                                                                               
LCTL50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LCTL10                                                           
*                                                                               
LCTLX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE CONTROL RECORD AND SUBSETS                                   *         
*        CONTROL RECORD (05070)                                       *         
***********************************************************************         
                                                                                
UPDTCTL  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLCTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTCTL                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITCTL                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXCTLC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UCTL10   GOTO1 VTOXCTLC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UCTLX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UCTLX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UCTL20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UCTL20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UCTL40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UCTL40                                                           
*                                                                               
UCTL30   CLI   COPYFLAG,X'01'                                                   
         JNE   UCTL10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UCTL10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UCTL40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UCTL30                                                           
*                                                                               
UCTLX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER CONTROL RECORD AT R2                                         *         
***********************************************************************         
                                                                                
FILTCTL  NTR1  BASE=*,LABEL=*                                                   
         USING TLCTD,R2                                                         
         CLI   TLCTCD,TLCTCDQ      CONTROL RECORD?                              
         JNE   NO                                                               
         OC    TLCTAGY,TLCTAGY     AGENCY?                                      
         JNZ   YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CONTROL RECORD                                           *         
***********************************************************************         
                                                                                
INITCTL  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX70LEN          R1=L'CTL RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD EMPLOYER RECORD AND SUBSETS                                    *         
*      EMPLOYER RECORD (05071)                                        *         
***********************************************************************         
                                                                                
LOADEMP  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST EMP RECORD             
         USING TLEMD,R2                                                         
         XC    TLEMKEY,TLEMKEY     CLEAR FILL THE KEY                           
         MVI   TLEMCD,TLEMCDQ      RECORD 58 FOR EMPLOYER                       
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LEMP10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LEMPX                                                            
         CLC   TLEMCD(TLEMCD+L'TLEMCD-TLEMD),IOKEY                              
         JNE   LEMPX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTEMP            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LEMP40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITEMP            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXEMPC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LEMP20   GOTO1 VTOXEMPC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LEMP40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LEMP40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LEMP40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LEMP30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LEMP30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LEMP20              TOO MANY IOS                                 
         J     LEMPX                                                            
*                                                                               
LEMP40   MVC   IOKEY(L'TLEMKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LEMP50                                                           
         GOTO1 AREADHI                                                          
         JE    LEMP50                                                           
         DC    H'0'                                                             
*                                                                               
LEMP50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LEMP10                                                           
*                                                                               
LEMPX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE EMPLOYER RECORD AND SUBSETS                                  *         
*        EMPLOYER RECORD (05071)                                      *         
***********************************************************************         
                                                                                
UPDTEMP  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLEMD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTEMP                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITEMP                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXEMPC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UEMP10   GOTO1 VTOXEMPC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UEMPX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UEMPX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UEMP20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UEMP20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UEMP40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UEMP40                                                           
*                                                                               
UEMP30   CLI   COPYFLAG,X'01'                                                   
         JNE   UEMP10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UEMP10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UEMP40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UEMP30                                                           
*                                                                               
UEMPX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER EMPLOYER RECORD AT R2                                        *         
***********************************************************************         
                                                                                
FILTEMP  NTR1  BASE=*,LABEL=*                                                   
         USING TLEMD,R2                                                         
         CLI   TLEMCD,TLEMCDQ      EMPLOYER RECORD?                             
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE EMPLOYER RECORD                                          *         
***********************************************************************         
                                                                                
INITEMP  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX71LEN          R1=L'EMP RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD LIEN RECORD (05072)                                            *         
***********************************************************************         
LOADLIE  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST EMP RECORD             
         USING TLLND,R2                                                         
         XC    TLLNKEY,TLLNKEY     CLEAR FILL THE KEY                           
         MVI   TLLNCD,TLLNCDQ      RECORD 74 FOR LIEN                           
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LLIE10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LLIEX                                                            
         CLC   TLLNCD(TLLNCD+L'TLLNCD-TLLND),IOKEY                              
         JNE   LLIEX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTLIE            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LLIE40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITLIE            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXLIEC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LLIE20   GOTO1 VTOXLIEC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LLIE40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LLIE40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LLIE40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LLIE30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LLIE30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LLIE20              TOO MANY IOS                                 
         J     LLIEX                                                            
*                                                                               
LLIE40   MVC   IOKEY(L'TLLNKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LLIE50                                                           
         GOTO1 AREADHI                                                          
         JE    LLIE50                                                           
         DC    H'0'                                                             
*                                                                               
LLIE50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LLIE10                                                           
*                                                                               
LLIEX    J     YES                                                              
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE LIEN RECORD                                                            
***********************************************************************         
UPDTLIE  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLLND,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTLIE                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITLIE                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXLIEC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
ULIE10   GOTO1 VTOXLIEC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    ULIEX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   ULIEX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    ULIE20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
ULIE20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    ULIE40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     ULIE40                                                           
*                                                                               
ULIE30   CLI   COPYFLAG,X'01'                                                   
         JNE   ULIE10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    ULIE10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
ULIE40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     ULIE30                                                           
*                                                                               
ULIEX    J     YES                                                              
         DROP  R2,R5                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER LIEN RECORD                                                            
***********************************************************************         
FILTLIE  NTR1  BASE=*,LABEL=*                                                   
         USING TLLND,R2                                                         
         CLI   TLLNCD,TLLNCDQ      LIEN RECORD?                                 
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INIT LIEN RECORD                                                              
***********************************************************************         
INITLIE  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX72LEN          R1=L'LIEN RECORD (LONGEST)                   
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD DUE COMPANY RECORD (05073)                                     *         
***********************************************************************         
LOADDC   NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST EMP RECORD             
         USING TLDUD,R2                                                         
         XC    TLDUKEY,TLDUKEY     CLEAR FILL THE KEY                           
         MVI   TLDUCD,TLDUCDQ      RECORD 78 FOR DUE COMPANY                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LDC10    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LDCX                                                             
         CLC   TLDUCD(TLDUCD+L'TLDUCD-TLDUD),IOKEY                              
         JNE   LDCX                ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTDC             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LDC40                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITDC             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXDCC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LDC20    GOTO1 VTOXDCC,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LDC40               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LDC40               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LDC40                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LDC30               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LDC30    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LDC20               TOO MANY IOS                                 
         J     LDCX                                                             
*                                                                               
LDC40    MVC   IOKEY(L'TLDUKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LDC50                                                            
         GOTO1 AREADHI                                                          
         JE    LDC50                                                            
         DC    H'0'                                                             
*                                                                               
LDC50    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LDC10                                                            
*                                                                               
LDCX     J     YES                                                              
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE DUE COMPANY RECORD                                                     
***********************************************************************         
UPDTDC   NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLDUD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTDC                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITDC                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXDCC,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UDC10    GOTO1 VTOXDCC,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UDCX                DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UDCX                                                             
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UDC20               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UDC20    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UDC40                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UDC40                                                            
*                                                                               
UDC30    CLI   COPYFLAG,X'01'                                                   
         JNE   UDC10                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UDC10               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UDC40    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UDC30                                                            
*                                                                               
UDCX     J     YES                                                              
         DROP  R2,R5                                                            
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER DUE COMPANY RECORD                                                     
***********************************************************************         
FILTDC   NTR1  BASE=*,LABEL=*                                                   
         USING TLDUD,R2                                                         
         CLI   TLDUCD,TLDUCDQ      DUE COMPANY RECORD?                          
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INIT DUE COMPANY RECORD                                                       
***********************************************************************         
INITDC   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX73LEN          R1=L'DUE COMPANY RECORD (LONGEST)            
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD INTERNET NEW MEDIA (5074)                                      *         
***********************************************************************         
                                                                                
LOAD74   NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLMDKEY,R2                                                       
         XC    TLMDKEY,TLMDKEY     CLEAR                                        
         MVI   TLMDCD,TLMDCDQ      RECORD 44 FOR MEDIA REC                      
         MVI   TLMDTYPE,4          SET FOR N (NEW MEDIA)                        
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD74A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD74X                                                            
*                                                                               
         GOTO1 AFILT74             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD74E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT74             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX74C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD74B    GOTO1 VTOX74C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD74D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD74D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD74D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD74C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD74C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD74B               TOO MANY IOS                                 
         J     LD74X                                                            
*                                                                               
LD74D    MVC   IOKEY(L'TLMDKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD74E                                                            
         GOTO1 AREADHI                                                          
         JE    LD74E                                                            
         DC    H'0'                                                             
*                                                                               
LD74E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD74A                                                            
*                                                                               
LD74X    J     YES                                                              
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE MEDIA RECORD                                                           
***********************************************************************         
                                                                                
UPDT74   NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLMDD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT74                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT74                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX74C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP74A    GOTO1 VTOX74C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP74X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP74X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP74B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP74B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP74D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP74D                                                            
*                                                                               
UP74C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP74A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP74A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP74D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP74C                                                            
*                                                                               
UP74X    J     YES                                                              
         DROP  R2,R5                                                            
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER NEW MEDIA RECORD                                                       
***********************************************************************         
                                                                                
FILT74   NTR1  BASE=*,LABEL=*                                                   
         USING TLMDD,R2                                                         
         CLI   TLMDCD,TLMDCDQ      MEDIA REC?                                   
         JNE   NO                                                               
         CLI   TLMDTYPE,4          IS IT NEW MEDIA                              
         JE    YES                                                              
         CLI   TLMDTYPE,8          IS IT INTERNET                               
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INIT INTERNET NEW MEDIA RECORD                                                
***********************************************************************         
                                                                                
INIT74   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX74LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD COMMERICAL POOL (5076)                                                   
***********************************************************************         
                                                                                
LOAD76   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLOGKEY,R2                                                       
         XC    TLOGKEY,TLOGKEY     CLEAR                                        
         MVI   TLOGCD,TLOGCDQ      RECORD EC FOR CMML GROUOP                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD76A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD76X                                                            
         CLI   TLOGCD,TLOGCDQ                                                   
         JNE   LD76X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT76             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD76E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT76             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX76C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD76B    GOTO1 VTOX76C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD76D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD76D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD76D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD76C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD76C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD76B               TOO MANY IOS                                 
         J     LD76X                                                            
*                                                                               
LD76D    MVC   IOKEY(L'TLOGKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD76E                                                            
         GOTO1 AREADHI                                                          
         JE    LD76E                                                            
         DC    H'0'                                                             
*                                                                               
LD76E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD76A                                                            
*                                                                               
LD76X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE COMMERCIAL POOL                                                        
***********************************************************************         
                                                                                
UPDT76   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLOGD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT76                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT76                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX76C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP76A    GOTO1 VTOX76C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP76X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP76X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP76B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP76B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP76D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP76D                                                            
*                                                                               
UP76C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP76A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP76A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP76D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP76C                                                            
*                                                                               
UP76X    J     YES                                                              
*        DROP  R2,R5                                                            
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR COMMERICAL POOL RECORD                                             
***********************************************************************         
                                                                                
FILT76   NTR1  BASE=*,LABEL=*                                                   
         USING TLOGD,R2                                                         
         CLI   TLOGCD,TLOGCDQ      CMML REC?                                    
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMERCIAL POOL                                                               
***********************************************************************         
                                                                                
INIT76   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX76LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD BILL TO DATA (5077)                                                      
***********************************************************************         
                                                                                
LOAD77   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLATKEY,R2                                                       
         XC    TLATKEY,TLATKEY     CLEAR                                        
         MVI   TLATCD,TLATCDQ      RECORD EC FOR CMML GROUOP                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD77A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD77X                                                            
         CLI   TLATCD,TLATCDQ                                                   
         JNE   LD77X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT77             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD77E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT77             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX77C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD77B    GOTO1 VTOX77C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD77D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD77D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD77D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD77C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD77C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD77B               TOO MANY IOS                                 
         J     LD77X                                                            
*                                                                               
LD77D    MVC   IOKEY(L'TLATKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD77E                                                            
         GOTO1 AREADHI                                                          
         JE    LD77E                                                            
         DC    H'0'                                                             
*                                                                               
LD77E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD77A                                                            
*                                                                               
LD77X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE COMMERCIAL POOL                                                        
***********************************************************************         
                                                                                
UPDT77   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLATD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT77                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT77                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX77C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP77A    GOTO1 VTOX77C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP77X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP77X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP77B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP77B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP77D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP77D                                                            
*                                                                               
UP77C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP77A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP77A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP77D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP77C                                                            
*                                                                               
UP77X    J     YES                                                              
*        DROP  R2,R5                                                            
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR COMMERICAL POOL RECORD                                             
***********************************************************************         
                                                                                
FILT77   NTR1  BASE=*,LABEL=*                                                   
         USING TLATD,R2                                                         
         CLI   TLATCD,TLATCDQ      CMML REC?                                    
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMERCIAL POOL                                                               
***********************************************************************         
                                                                                
INIT77   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX77LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD PUBLISHED MUSIC DATA (5079)                                              
***********************************************************************         
                                                                                
LOAD79   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLMUKEY,R2                                                       
         XC    TLMUKEY,TLMUKEY     CLEAR                                        
         MVI   TLMUCD,TLMUCDQ      RECORD EC FOR CMML GROUOP                    
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD79A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD79X                                                            
         CLI   TLMUCD,TLMUCDQ                                                   
         JNE   LD79X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT79             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD79E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT79             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX79C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD79B    GOTO1 VTOX79C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD79D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD79D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD79D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD79C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD79C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD79B               TOO MANY IOS                                 
         J     LD79X                                                            
*                                                                               
LD79D    MVC   IOKEY(L'TLMUKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD79E                                                            
         GOTO1 AREADHI                                                          
         JE    LD79E                                                            
         DC    H'0'                                                             
*                                                                               
LD79E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD79A                                                            
*                                                                               
LD79X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE COMMERCIAL POOL                                                        
***********************************************************************         
                                                                                
UPDT79   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLMUD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT79                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT79                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX79C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP79A    GOTO1 VTOX79C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP79X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP79X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP79B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP79B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP79D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP79D                                                            
*                                                                               
UP79C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP79A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP79A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP79D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP79C                                                            
*                                                                               
UP79X    J     YES                                                              
*        DROP  R2,R5                                                            
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR COMMERICAL POOL RECORD                                             
***********************************************************************         
                                                                                
FILT79   NTR1  BASE=*,LABEL=*                                                   
         USING TLMUD,R2                                                         
         CLI   TLMUCD,TLMUCDQ      CMML REC?                                    
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* COMMERCIAL POOL                                                               
***********************************************************************         
                                                                                
INIT79   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX79LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD EPISODE DATA (5081)                                                      
***********************************************************************         
                                                                                
LOAD81   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLEPKEY,R2                                                       
         XC    TLEPKEY,TLEPKEY     CLEAR                                        
         MVI   TLEPCD,TLEPCDQ      RECORD EP FOR EPISODE                        
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD81A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD81X                                                            
         CLI   TLEPCD,TLEPCDQ                                                   
         JNE   LD81X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT81             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD81E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT81             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX81C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD81B    GOTO1 VTOX81C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD81D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD81D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD81D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD81C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD81C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD81B               TOO MANY IOS                                 
         J     LD81X                                                            
*                                                                               
LD81D    MVC   IOKEY(L'TLEPKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD81E                                                            
         GOTO1 AREADHI                                                          
         JE    LD81E                                                            
         DC    H'0'                                                             
*                                                                               
LD81E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD81A                                                            
*                                                                               
LD81X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE EPISODE                                                                
***********************************************************************         
                                                                                
UPDT81   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLEPD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT81                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT81                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX81C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP81A    GOTO1 VTOX81C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP81X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP81X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP81B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP81B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP81D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP81D                                                            
*                                                                               
UP81C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP81A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP81A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP81D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP81C                                                            
*                                                                               
UP81X    J     YES                                                              
*        DROP  R2,R5                                                            
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR EPISODE RECORD                                                     
***********************************************************************         
                                                                                
FILT81   NTR1  BASE=*,LABEL=*                                                   
         USING TLEPD,R2                                                         
         CLI   TLEPCD,TLEPCDQ      EPISODE REC?                                 
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EPISODE                                                                       
***********************************************************************         
                                                                                
INIT81   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX81LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD ECAST DATA (5083)                                                        
***********************************************************************         
                                                                                
LOAD83   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLECKEY,R2                                                       
         XC    TLECKEY,TLECKEY     CLEAR                                        
         MVI   TLECCD,TLECCDQ      RECORD EC FOR ECAST                          
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD83A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD83X                                                            
         CLI   TLECCD,TLECCDQ                                                   
         JNE   LD83X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT83             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD83E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT83             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX83C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD83B    GOTO1 VTOX83C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD83D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD83D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD83D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD83C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD83C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD83B               TOO MANY IOS                                 
         J     LD83X                                                            
*                                                                               
LD83D    MVC   IOKEY(L'TLECKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD83E                                                            
         GOTO1 AREADHI                                                          
         JE    LD83E                                                            
         DC    H'0'                                                             
*                                                                               
LD83E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD83A                                                            
*                                                                               
LD83X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE ECAST                                                                  
***********************************************************************         
                                                                                
UPDT83   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLECD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT83                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT83                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX83C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP83A    GOTO1 VTOX83C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP83X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP83X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP83B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP83B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP83D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP83D                                                            
*                                                                               
UP83C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP83A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP83A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP83D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP83C                                                            
*                                                                               
UP83X    J     YES                                                              
*        DROP  R2,R5                                                            
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR ECAST RECORD                                                       
***********************************************************************         
                                                                                
FILT83   NTR1  BASE=*,LABEL=*                                                   
         USING TLECD,R2                                                         
         CLI   TLECCD,TLECCDQ      ECAST REC?                                   
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ECAST                                                                         
***********************************************************************         
                                                                                
INIT83   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX83LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD FIXED CYCLE TRACKING (5084)                                              
***********************************************************************         
                                                                                
LOAD84   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLFTKEY,R2                                                       
         XC    TLFTKEY,TLFTKEY     CLEAR                                        
         MVI   TLFTCD,TLFTCDQ      RECORD FT FOR FIXED CYCLE TRACKING           
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD84A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD84X                                                            
         CLI   TLFTCD,TLFTCDQ                                                   
         JNE   LD84X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT84             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD84E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT84             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX84C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD84B    GOTO1 VTOX84C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD84D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD84D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD84D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD84C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD84C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD84B               TOO MANY IOS                                 
         J     LD84X                                                            
*                                                                               
LD84D    MVC   IOKEY(L'TLFTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD84E                                                            
         GOTO1 AREADHI                                                          
         JE    LD84E                                                            
         DC    H'0'                                                             
*                                                                               
LD84E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD84A                                                            
*                                                                               
LD84X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE FIXED CYCLE TRACKING                                                   
***********************************************************************         
                                                                                
UPDT84   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLFTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT84                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT84                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX84C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP84A    GOTO1 VTOX84C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP84X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP84X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP84B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP84B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP84D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP84D                                                            
*                                                                               
UP84C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP84A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP84A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP84D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP84C                                                            
*                                                                               
UP84X    J     YES                                                              
*        DROP  R2,R5                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR FIXED CYCLE TRACKING RECORD                                        
***********************************************************************         
                                                                                
FILT84   NTR1  BASE=*,LABEL=*                                                   
         USING TLFTD,R2                                                         
         CLI   TLFTCD,TLFTCDQ      FIXED CYCLE TRACKING REC?                    
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FIXED CYCLE TRACKING                                                          
***********************************************************************         
                                                                                
INIT84   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX84LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD USAGE HISTORY (5085)                                                     
***********************************************************************         
                                                                                
LOAD85   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLUHKEY,R2                                                       
         XC    TLUHKEY,TLUHKEY     CLEAR                                        
         MVI   TLUHCD,TLUHCDQ      RECORD FT FOR FIXED CYCLE TRACKING           
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD85A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD85X                                                            
         CLI   TLUHCD,TLUHCDQ                                                   
         JNE   LD85X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT85             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD85E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT85             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX85C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD85B    GOTO1 VTOX85C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD85D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD85D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD85D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD85C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD85C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD85B               TOO MANY IOS                                 
         J     LD85X                                                            
*                                                                               
LD85D    MVC   IOKEY(L'TLFTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD85E                                                            
         GOTO1 AREADHI                                                          
         JE    LD85E                                                            
         DC    H'0'                                                             
*                                                                               
LD85E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD85A                                                            
*                                                                               
LD85X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE USAGE HISTORY                                                          
***********************************************************************         
                                                                                
UPDT85   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLFTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT85                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT85                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX85C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP85A    GOTO1 VTOX85C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP85X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP85X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP85B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP85B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP85D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP85D                                                            
*                                                                               
UP85C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP85A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP85A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP85D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP85C                                                            
*                                                                               
UP85X    J     YES                                                              
*        DROP  R2,R5                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR USAGE HISTORY RECORD                                               
***********************************************************************         
                                                                                
FILT85   NTR1  BASE=*,LABEL=*                                                   
         USING TLUHD,R2                                                         
         CLI   TLUHCD,TLUHCDQ      USAGE HISTORY REC?                           
         JNE   NO                                                               
         OC    TLUHCSEQ,TLUHCSEQ   IS THERE A CAST INPUT SEQ?                   
         JZ    NO                                                               
         CLC   TLUHUSE,=C'CBL'     ONLY WANT CBL, SCB, LCB                      
         JE    YES                                                              
         CLC   TLUHUSE,=C'SCB'                                                  
         JE    YES                                                              
         CLC   TLUHUSE,=C'LCB'                                                  
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USAGE HISTORY                                                                 
***********************************************************************         
                                                                                
INIT85   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX85LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD HISTORY COMMENT (5086)                                                   
***********************************************************************         
                                                                                
LOAD86   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST RECORD                 
         USING TLHCKEY,R2                                                       
         XC    TLHCKEY,TLHCKEY     CLEAR                                        
         MVI   TLHCCD,TLHCCDQ      RECORD HC FOR HISTORY COMMENT                
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LD86A    TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LD86X                                                            
         CLI   TLHCCD,TLHCCDQ                                                   
         JNE   LD86X               ALL DONE IF TYPE CHANGES                     
*                                                                               
         GOTO1 AFILT86             FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LD86E                                                            
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINIT86             INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX86C,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                      
*                                  GET NEXT UNCOMMITTED RECORD                  
LD86B    GOTO1 VTOX86C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LD86D               DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LD86D               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LD86D                                                            
         CLI   SXDTPLFM,0                                                       
         JE    LD86C               PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LD86C    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LD86B               TOO MANY IOS                                 
         J     LD86X                                                            
*                                                                               
LD86D    MVC   IOKEY(L'TLFTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LD86E                                                            
         GOTO1 AREADHI                                                          
         JE    LD86E                                                            
         DC    H'0'                                                             
*                                                                               
LD86E    GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD86A                                                            
*                                                                               
LD86X    J     YES                                                              
*        DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE HISTORY COMMENT                                                        
***********************************************************************         
                                                                                
UPDT86   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLFTD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILT86                                                          
         JNE   YES                                                              
*                                                                               
         GOTO1 AINIT86                                                          
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOX86C,DMCB,DXAXREC,(R2),(1,0),(R6),0                           
*                                  GET NEXT UNCOMMITTED RECORD                  
UP86A    GOTO1 VTOX86C,DMCB,DXAXREC,(R2),(2,0),(R6),0                           
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UP86X               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UP86X                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UP86B               DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UP86B    CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UP86D                                                            
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UP86D                                                            
*                                                                               
UP86C    CLI   COPYFLAG,X'01'                                                   
         JNE   UP86A                                                            
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UP86A               IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UP86D    GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UP86C                                                            
*                                                                               
UP86X    J     YES                                                              
*        DROP  R2,R5                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER FOR USAGE HISTORY RECORD                                               
***********************************************************************         
                                                                                
FILT86   NTR1  BASE=*,LABEL=*                                                   
         USING TLHCD,R2                                                         
         CLI   TLHCCD,TLHCCDQ      HISTORY COMMENT REC?                         
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* USAGE HISTORY                                                                 
***********************************************************************         
                                                                                
INIT86   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX86LEN          R1 MAX RECLEN                                
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CONTRACT TYPE (05078)                                                         
*       ONLY ON LOADS                                                 *         
***********************************************************************         
                                                                                
LOAD78   NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         USING CONTD,R2                                                         
         LA    R2,TACONTS          POINT TO TABLE                               
         USING TOXRECD,R3                                                       
         L     R3,DXAXREC                                                       
*                                                                               
LD78A    CLI   0(R2),EOF                                                        
         BE    LD78X                                                            
         CLI   0(R2),CONTUAS       SKIP THIS CATEGORY                           
         JNE   LD78B                                                            
         LA    R2,L'TACONTS(R2)                                                 
         J     LD78A                                                            
*                                                                               
LD78B    GOTO1 AINIT78             INITIALIZE EXTRACT BUFFER                    
*                                                                               
         XC    TOXRELEN,TOXRELEN   RECORD LENGTH/TYPE/ENDOFREC                  
         MVC   TOXRELEN(2),=Y(TX19LEN)                                          
         MVC   TOXRETYP,=C'05078'                                               
*                                                                               
         MVI   TX78CNTY-1,MXTRTQ                                                
         MVI   TX78CNNM-1,MXTRTQ                                                
         MVI   TX78X,MXTRTQ                                                     
         MVI   TX78X+1,MXTRTQ                                                   
*                                                                               
         MVC   TX78CNTY,CONTEQU                                                 
         MVC   TX78CNNM,CONNAME                                                 
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
*                                                                               
         LA    R2,CONNEXT                                                       
         AP    PK78CNT,=P'1'                                                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LD78A                                                            
*                                                                               
LD78X    J     YES                                                              
*        DROP  R2,R3                                                            
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
       ++INCLUDE TASYSCONT                                                      
         EJECT                                                                  
***********************************************************************         
* INITIALIZE CONTRACT TYPE                                                      
***********************************************************************         
                                                                                
INIT78   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX78LEN                                                       
         GOTO1 AINITALL                                                         
         J     YES                                                              
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* LOAD TIMESHEET DAY RECORD AND SUBSETS                               *         
*      TIMESHEET DAY RECORD (05069)                                   *         
***********************************************************************         
                                                                                
LOADTIM  NTR1  BASE=*,LABEL=*                                                   
         MVC   MAXIOS,DXMAXREC                                                  
*                                                                               
         LA    R2,IOKEY            SET KEY TO READ FIRST TIME RECORD            
         USING TLTMD,R2                                                         
         XC    TLTMKEY,TLTMKEY     CLEAR FILL THE KEY                           
         MVI   TLTMCD,TLTMCDQ      RECORD B8 FOR TIMESHEET                      
         L     R2,DXARECB                                                       
         GOTO1 AREADHI                                                          
         JNE   NO                                                               
*                                                                               
LTIM10   TM    DMCB+8,X'80'        ALL DONE IF EOF                              
         JO    LTIMX                                                            
         CLC   TLTMCD(TLTMCD+L'TLTMCD-TLTMD),IOKEY                              
         JNE   LTIMX               ALL DONE IF COMPANY HEXCOMP CHANGES          
*                                                                               
         GOTO1 AFILTTIM            FILTER RECORD, GET NEXT IF NOT VALID         
         JNE   LTIM40                                                           
*                                                                               
         MVC   TALADDR,TLDRDA-TLDRD(R2)                                         
         GOTO1 AGETIT              GET RECORD                                   
         JNE   NO                  PROBLEM WITH GETREC                          
*                                                                               
         GOTO1 AINITTIM            INITIALIZE EXTRACT BUFFER                    
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXTIMC,DMCB,DXAXREC,(R2),(1,0),(R6),PKCNTS                     
*                                  GET NEXT UNCOMMITTED RECORD                  
LTIM20   GOTO1 VTOXTIMC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         TM    DMCB+8,X'80'                                                     
         JO    LTIM40              DON'T WRITE                                  
         CLI   DMCB+8,FF                                                        
         JE    LTIM40              DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   LTIM40                                                           
         CLI   SXDTPLFM,0                                                       
         JE    LTIM30              PUT UNCONVERTED RECORD ONLY                  
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
LTIM30   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         GOTO1 ADECIOC             DECREMENT IO COUNT                           
         JE    LTIM20              TOO MANY IOS                                 
         J     LTIMX                                                            
*                                                                               
LTIM40   MVC   IOKEY(L'TLCTKEY),0(R2) READ NEXT RECORD - SEQUENT                
         GOTO1 ACHKSEQ                                                          
         JE    LTIM50                                                           
         GOTO1 AREADHI                                                          
         JE    LTIM50                                                           
         DC    H'0'                                                             
*                                                                               
LTIM50   GOTO1 VDATAMGR,DMCB,DMRSEQ,TALDIR,IOKEY,(R2),DMWORK                    
         OC    MAXIOS,MAXIOS       EXIT IF MAXIMUM IOS EXCEEDED                 
         JNZ   LTIM10                                                           
*                                                                               
LTIMX    J     YES                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE TIMESHEET RECORD AND SUBSETS                                 *         
*        TIMESHEET RECORD (05069)                                     *         
***********************************************************************         
                                                                                
UPDTTIM  NTR1  BASE=*,LABEL=*                                                   
         USING RECDS,R5                                                         
         L     R5,DXARECB                                                       
         USING TLTMD,R2                                                         
         LA    R2,RECVHDR+L'RECVHDR                                             
*                                                                               
         GOTO1 AFILTTIM                                                         
         JNE   YES                                                              
*                                                                               
         GOTO1 AINITTIM                                                         
*                                  BUILD EXTRACT RECORDS                        
         GOTO1 VTOXTIMC,DMCB,DXAXREC,(R2),(1,0),(R6),0                          
*                                  GET NEXT UNCOMMITTED RECORD                  
UTIM10   GOTO1 VTOXTIMC,DMCB,DXAXREC,(R2),(2,0),(R6),0                          
         MVC   BYTE,DMCB+8         SAVE RETURN CODE FOR CALL BACK TEST          
         CLI   DMCB+8,FF                                                        
         JE    UTIMX               DATA NOT COMPLETE - NO WRITE                 
         CLI   DXWRITE,C'Y'                                                     
         JNE   UTIMX                                                            
         L     RF,DXAXREC                                                       
         CLI   SXDTPLFM,0                                                       
         JE    UTIM20              DO NOT CONVERT RECORD                        
*                                                                               
         GOTO1 VTOXCNVX,DMCB,(R7)                                               
*                                                                               
         L     RF,DXASQLB                                                       
UTIM20   CLI   TOXREACT-TOXRECD(RF),C'A'       IF ADD SKIP TO PUT               
         JE    UTIM40                                                           
         MVI   COPYFLAG,X'01'                                                   
         MVI   TOXREACT-TOXRECD(RF),C'D'                                        
         J     UTIM40                                                           
*                                                                               
UTIM30   CLI   COPYFLAG,X'01'                                                   
         JNE   UTIM10                                                           
         CLI   DXACTION,C'D'       WAS THIS A DELETED RECORD?                   
         JE    UTIM10              IF SO-DON'T ADD                              
         L     RF,DXASQLB          RF=A(CONVERTED RECORD)                       
         MVI   TOXREACT-TOXRECD(RF),C'A'                                        
         MVI   COPYFLAG,X'00'                                                   
*                                                                               
UTIM40   GOTO1 DXPUT,DMCB,DXASQLB,(R7)                                          
         J     UTIM30                                                           
*                                                                               
UTIMX    J     YES                                                              
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FILTER TIMESHEET RECORD AT R2                                       *         
***********************************************************************         
                                                                                
FILTTIM  NTR1  BASE=*,LABEL=*                                                   
         USING TLTMD,R2                                                         
         CLI   TLTMCD,TLTMCDQ      TIMESHEET RECORD?                            
         JNE   NO                                                               
         CLI   TLTMSTA,0           SKIP WEB AND P+ TIMESHEETS                   
         JE    YES                                                              
         J     NO                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INITIALIZE TIMESHEET DAY RECORD                                     *         
***********************************************************************         
                                                                                
INITTIM  NTR1  BASE=*,LABEL=*                                                   
         LA    R1,TX69LEN          R1=L'TIM RECORD (LONGEST)                    
         GOTO1 AINITALL                                                         
         J     YES                                                              
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
         EJECT                                                                  
         GETEL R2,DATADISP,ELCODE                                               
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
*                                                                               
ATYPTAB  DS    A                   A(TYPTAB) - TYPTABD                          
AENCKEY  DS    A                   A(PASSWORD ENCRYPTION KEY OR 0)              
TALADDR  DS    F                   DISK ADDRESS                                 
IOKEY    DS    CL64                                                             
IOKEYSAV DS    CL42                                                             
TEMPKEY  DS    CL42                                                             
FULL     DS    F                                                                
BYTE     DS    XL1                                                              
BYTE2    DS    XL1                                                              
ERROR    DS    XL1                                                              
RETCODE  DS    XL1                                                              
HEADFLAG DS    XL1                                                              
RECTYPE  DS    XL1                                                              
MAXIOS   DS    XL4                 RECORD IO LIMIT COUNT                        
COPYFLAG DS    XL1                 UPDATE FLAG FOR COPY                         
SVKEY    DS    CL(L'IOKEY)                                                      
*                                                                               
SVVALS   DS    0C                                                               
SVICOM   DS    CL(L'TLCOCOM)                                                    
SVCOKEY  DS    XL42                                                             
SVINHKEY DS    XL42                                                             
SVVALLNQ EQU   *-SVVALS                                                         
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    XL1                                                              
*                                                                               
PRIALPHA DS    CL2                 PRIMARY ALPHA                                
COMPANY  DS    XL1                                                              
VERSION  DS    XL1                                                              
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
ALPARMS  DS    0XL16                                                            
ALPAEXTR DS    A                   EXTRACT ROUTINE                              
ALPAINIT DS    A                   INITIALISATION ROUTINE                       
ALPAFILT DS    A                   FILTER ROUTINE                               
ALPACNVX DS    A                   CONVERT ROUTINE                              
*                                                                               
HIDREC   DS    CL(16)                                                           
*                                                                               
PKCNTS   DS    0C                                                               
PK00CNT  DS    PL8                 05000 REC COUNT (AGENCY)                     
PKCNTLNQ EQU   *-PKCNTS                                                         
PK01CNT  DS    PL8                 05001 REC COUNT (CLIENT)                     
PK02CNT  DS    PL8                 05002 REC COUNT (PRODUCT)                    
PK03CNT  DS    PL8                 05003 REC COUNT (ASSET)                      
PK04CNT  DS    PL8                 05004 REC COUNT (VERSION)                    
PK05CNT  DS    PL8                 05005 REC COUNT (ALIAS)                      
PK06CNT  DS    PL8                 05006 REC COUNT (W-4)                        
PK07CNT  DS    PL8                 05007 REC COUNT (PERFORMER)                  
PK08CNT  DS    PL8                 05008 REC COUNT (FIXED CYCLE)                
PK09CNT  DS    PL8                 05009 REC COUNT (VERSION-PERFORMER)          
PK10CNT  DS    PL8                 05010 REC COUNT (INVOICE)                    
PK11CNT  DS    PL8                 05011 REC COUNT (TRACK)                      
PK12CNT  DS    PL8                 05012 REC COUNT (TRACK-PERFORMER)            
PK13CNT  DS    PL8                 05013 REC COUNT (TRACK-VERSION)              
PK14CNT  DS    PL8                 05014 REC COUNT (TRACK ASSET PERF)           
PK15CNT  DS    PL8                 05015 REC COUNT (PRIMARY INVOICE)            
PK16CNT  DS    PL8                 05016 REC COUNT (MEDIA)                      
PK17CNT  DS    PL8                 05017 REC COUNT (ASSET TYPE)                 
PK18CNT  DS    PL8                 05018 REC COUNT (W4 TYPE)                    
PK19CNT  DS    PL8                 05019 REC COUNT (PERFORMER CATEGORY)         
PK20CNT  DS    PL8                 05020 REC COUNT (USE TYPE)                   
PK21CNT  DS    PL8                 05021 REC COUNT (CLIENT GROUP)               
PK22CNT  DS    PL8                 05022 REC COUNT (USE SUBTYPE)                
PK23CNT  DS    PL8                 05023 REC COUNT (ACTRA TYPE)                 
PK24CNT  DS    PL8                 05024 REC COUNT (AGENCY GROUP)               
PK25CNT  DS    PL8                 05025 REC COUNT (W4 ASSOCIATED CORP)         
PK26CNT  DS    PL8                 05026 REC COUNT (PRINT COMMERCIAL)           
PK27CNT  DS    PL8                 05027 REC COUNT (DEAL)                       
PK28CNT  DS    PL8                 05028 REC COUNT (AREA)                       
PK29CNT  DS    PL8                 05029 REC COUNT (PRINT USE)                  
PK30CNT  DS    PL8                 05030 REC COUNT (GUARANTEE)                  
PK31CNT  DS    PL8                 05031 REC COUNT (GUARANTEE AGY-CLI)          
PK32CNT  DS    PL8                 05032 REC COUNT (GUARANTEE PERIOD)           
PK33CNT  DS    PL8                 05033 REC COUNT (GUARANTEE CONTRACT)         
PK34CNT  DS    PL8                 05034 REC COUNT (GUARANTEE CON YEAR)         
PK35CNT  DS    PL8                 05035 REC COUNT (GUARANTEE COMMENT)          
PK36CNT  DS    PL8                 05036 REC COUNT (GUARANTEE YEAR TRK)         
PK37CNT  DS    PL8                 05037 REC COUNT (VERSION CMNT LINE)          
PK38CNT  DS    PL8                 05038 REC COUNT (AGENT)                      
PK39CNT  DS    PL8                 05039 REC COUNT (GUARANTEE TRACKING)         
PK40CNT  DS    PL8                 05040 REC COUNT (GURANNTEE EXL USE)          
PK41CNT  DS    PL8                 05041 REC COUNT (OFFICE)                     
PK42CNT  DS    PL8                 05042 REC COUNT (OVERSCALE AMOUNT)           
PK43CNT  DS    PL8                 05043 REC COUNT (OVERSCALE PERCENT1)         
PK44CNT  DS    PL8                 05044 REC COUNT (OVERSCALE PERCENT2)         
PK45CNT  DS    PL8                 05045 REC COUNT (STAFF RECORD)               
PK46CNT  DS    PL8                 05046 REC COUNT (STAFF LIMIT)                
PK47CNT  DS    PL8                 05047 REC COUNT (PREVIOUS VERSIONID)         
PK48CNT  DS    PL8                 05048 REC COUNT (NETWORK USE DET)            
PK49CNT  DS    PL8                 05049 REC COUNT (TV MARKET USE DET)          
PK50CNT  DS    PL8                 05050 REC COUNT (RADIO MKT USE DET)          
PK51CNT  DS    PL8                 05051 REC COUNT (CABLE NTWK USE DET)         
PK52CNT  DS    PL8                 05052 REC COUNT (LCL CBLSYS USE DET)         
PK53CNT  DS    PL8                 05053 REC COUNT (TELEVISION MARKET)          
PK54CNT  DS    PL8                 05054 REC COUNT (RADIO MARKET)               
PK55CNT  DS    PL8                 05055 REC COUNT (CABLE NETWORK)              
PK56CNT  DS    PL8                 05056 REC COUNT (LOCAL CABLE SYSTEM)         
PK57CNT  DS    PL8                 05057 REC COUNT (CHECK)                      
PK58CNT  DS    PL8                 05058 REC COUNT (CHECK - FEDERAL)            
PK59CNT  DS    PL8                 05059 REC COUNT (CHECK - STATE)              
PK60CNT  DS    PL8                 05060 REC COUNT (CHECK - CITY)               
PK61CNT  DS    PL8                 05061 REC COUNT (CHECK - CANADIAN)           
PK62CNT  DS    PL8                 05062 REC COUNT (CHECK - LIEN)               
PK63CNT  DS    PL8                 05063 REC COUNT (CHECK - DUE COMP)           
PK64CNT  DS    PL8                 05064 REC COUNT (CHECK - AGENT)              
PK65CNT  DS    PL8                 05065 REC COUNT (PRODUCT GROUP)              
PK66CNT  DS    PL8                 05066 REC COUNT (PRODUCT TYPE)               
PK67CNT  DS    PL8                 05067 REC COUNT (NETWORK)                    
PK68CNT  DS    PL8                 05068 REC COUNT (EST/JOB NUMBER)             
PK69CNT  DS    PL8                 05069 REC COUNT (TIMESHEET)                  
PK70CNT  DS    PL8                 05070 REC COUNT (CONTROL)                    
PK71CNT  DS    PL8                 05071 REC COUNT (EMPLOYER)                   
PK72CNT  DS    PL8                 05072 REC COUNT (LIEN)                       
PK73CNT  DS    PL8                 05073 REC COUNT (DUE COMPANY)                
PK74CNT  DS    PL8                 05074 REC COUNT (MEDIA                       
PK75CNT  DS    PL8                 05075 REC COUNT (INTERNET/NEWMED)            
PK76CNT  DS    PL8                 05076 REC COUNT (COMMERCIAL POOL)            
PK77CNT  DS    PL8                 05077 REC COUNT (BILL TO)                    
PK78CNT  DS    PL8                 05078 REC COUNT (CONTRACT TYPE)              
PK79CNT  DS    PL8                 05079 REC COUNT (PUBLISHED MUSIC)            
PK80CNT  DS    PL8                 05080 REC COUNT (VERSION PUB MUSIC)          
PK81CNT  DS    PL8                 05081 REC COUNT (EPISODE)                    
PK82CNT  DS    PL8                 05082 REC COUNT (PERFORMER-EPISODE)          
PK83CNT  DS    PL8                 05083 REC COUNT (ECAST)                      
PK84CNT  DS    PL8                 05084 REC COUNT (FIXED CYCLE TRK)            
PK85CNT  DS    PL8                 05085 REC COUNT (USAGE HISTORY)              
PK86CNT  DS    PL8                 05086 REC COUNT (HISTORY COMMENT)            
PKCNTQ   EQU   (*-PKCNTS)/PKCNTLNQ                                              
*                                                                               
IOL      DS    F                   GENERAL IO AREA                              
IO       DS    4096X                                                            
*                                                                               
EOF      EQU   X'FF'                                                            
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
VDATAMGR DS    V                                                                
VDMOD000 DS    V                                                                
VDADDS   DS    V                                                                
VLOGIO   DS    V                                                                
VDATCON  DS    V                                                                
VBUFFRIN DS    V                                                                
VTOXCNVX DS    V                                                                
*                                                                               
* TOXROUTS ADDRESSES                                                            
VTOXAGYC DS    V                   AGENCY RECORDS                               
VTOXCLIC DS    V                   CLIENT RECORDS                               
VTOXPRDC DS    V                   PRODUCT RECORDS                              
VTOXASSC DS    V                   ASSET RECORDS                                
VTOXVERC DS    V                   VERSION RECORDS                              
VTOXALIC DS    V                   ALIAS RECORDS                                
VTOXW4RC DS    V                   AGENCY RECORDS                               
VTOXPERC DS    V                   PERFORMER RECORDS                            
VTOXINVC DS    V                   INVOICE RECORDS                              
VTOXCGRC DS    V                   CLIENT GROUP RECORD                          
VTOXAGRC DS    V                   AGENCY GROUP RECORD                          
VTOXAREC DS    V                   AREA                                         
VTOXPUSC DS    V                   PRINT USE                                    
VTOXGUAC DS    V                   GUARANTEE                                    
VTOXGUCC DS    V                   GUARANTEE CONTRACT                           
VTOXGCCC DS    V                   GUARANTEE COMMENT                            
VTOXGCYC DS    V                   GUARANTEE CONTRACT YEAR TRACKING             
VTOXVCLC DS    V                   VERSION COMMENTS                             
VTOXAGTC DS    V                   AGENT                                        
VTOXGTKC DS    V                   GUARANTEE TRACKING                           
VTOXOFFC DS    V                   OFFICE                                       
VTOXSTFF DS    V                   STAFF RECORD                                 
VTOXCHKC DS    V                   CHECK RECORD                                 
VTOXPGRC DS    V                   PRODUCT GROUP                                
VTOXPTYC DS    V                   PRODUCT TYPE                                 
VTOXMKTC DS    V                   MARKET RECORD                                
VTOXUSEC DS    V                   USAGE HISTORY                                
VTOXCTLC DS    V                   CONTROL                                      
VTOXEMPC DS    V                   EMPLOYER                                     
VTOXTIMC DS    V                   TIMESHEET DAY                                
VTOXLIEC DS    V                   LIEN                                         
VTOXDCC  DS    V                   DUE COMPANY                                  
VTOX74C  DS    V                   INTERNET NEWMEDIA                            
VTOX76C  DS    V                   COMMERCIAL POOL                              
VTOX77C  DS    V                   BILL TO                                      
VTOX78C  DS    V                   CONTRACT TYPE                                
VTOX79C  DS    V                   PUBLISHED MUSIC                              
VTOX81C  DS    V                   EPISODE                                      
VTOX83C  DS    V                   ECAST                                        
VTOX84C  DS    V                   FIXED CYCLE TRACKING                         
VTOX85C  DS    V                   USAGE HISTORY                                
VTOX86C  DS    V                   HISTORY COMMENT                              
* END OF TOXROUTS                                                               
*                                                                               
         DS    CL8                 COMMON INTERNAL ROUTINES                     
ATALLOAD DS    A                                                                
ACXLOAD  DS    A                                                                
ATALUPDT DS    A                                                                
ADECIOC  DS    A                                                                
ACHKSEQ  DS    A                                                                
AGETTYP  DS    A                                                                
AGETIT   DS    A                                                                
AREADHI  DS    A                   READ HIGH ON THE TALENT FILE                 
AREADCHI DS    A                   READ HIGH ON THE CONTROL FILE                
ARECCMP  DS    A                   COMPARE CHANGE/COPY RECORD                   
AHIDBUF  DS    A                   HOLDING FEE ID TABLE FOR BUFFERIN            
ACOMFACS DS    A                   ADDRESS OF COMFACS FOR BUFFERIN              
VT00A88  DS    A                   T00A88                                       
*                                                                               
* LOAD ROUTINE ADDRESSES                                                        
         DS    CL8                 LOAD ROUTINES                                
ALOADALL DS    A                   ALL                                          
ALOADAGY DS    A                   AGENCY                                       
ALOADCLI DS    A                   CLIENT                                       
ALOADPRD DS    A                   PRODUCT                                      
ALOADASS DS    A                   ASSET                                        
ALOADVER DS    A                   VERSION                                      
ALOADALI DS    A                   ALIAS                                        
ALOADW4R DS    A                   W-4                                          
ALOADPER DS    A                   PERFORMER                                    
ALOADINV DS    A                   INVOICE                                      
ALOADMED DS    A                   MEDIA                                        
ALOADAST DS    A                   ASSET TYPE                                   
ALOADW4T DS    A                   W4 TYPE                                      
ALOADPEC DS    A                   PERFORMER CATEGORY                           
ALOADUST DS    A                   USE TYPE                                     
ALOADCGR DS    A                   CLIENT GROUP                                 
ALOADUSS DS    A                   USE SUBTYPE                                  
ALOADACT DS    A                   ACTRA TYPE                                   
ALOADAGR DS    A                   AGENCY GROUP                                 
ALOADARE DS    A                   AREA                                         
ALOADPUS DS    A                   PRINT USE                                    
ALOADGUA DS    A                   GUARANTEE                                    
ALOADGUC DS    A                   GUARANTEE CONTRACT                           
ALOADGCC DS    A                   GUARANTEE COMMENT                            
ALOADGCY DS    A                   GUARANTEE CONTRACT YEAR TRACKING             
ALOADAGT DS    A                   AGENT                                        
ALOADGTK DS    A                   GUARANTEE TRACKING                           
ALOADVCL DS    A                   COMMENTS                                     
ALOADOFF DS    A                   OFFICE                                       
ALOADCNT DS    A                   RECORD COUNT RECORD                          
ALOADSTF DS    A                   STAFF                                        
ALOADCHK DS    A                   CHECK                                        
ALOADPGR DS    A                   PRODUCT GROUP                                
ALOADPTY DS    A                   PRODUCT TYPE                                 
ALOADMKT DS    A                   MARKET                                       
ALOADUSE DS    A                   USAGE HISTORY                                
ALOADNET DS    A                   NETWORK                                      
ALOADCTL DS    A                   CONTROL                                      
ALOADEMP DS    A                   EMPLOYER                                     
ALOADTIM DS    A                   TIMESHEET DAY                                
ALOADLIE DS    A                   LIEN                                         
ALOADDC  DS    A                   DUE COMPANY                                  
ALOAD74  DS    A                   MEDIA                                        
ALOAD76  DS    A                   COMMERCIAL POOL                              
ALOAD77  DS    A                   BILL TO                                      
ALOAD78  DS    A                   CONTRACT TYPE                                
ALOAD79  DS    A                   PUBLISHED MUSIC                              
ALOAD81  DS    A                   EPISODE                                      
ALOAD83  DS    A                   ECAST                                        
ALOAD84  DS    A                   FIXED CYCLE TRACKING                         
ALOAD85  DS    A                   USAGE HISTORY                                
ALOAD86  DS    A                   HISTORY COMMENT                              
* END OF LOAD ROUTINE ADDRESSES                                                 
*                                                                               
* UPDATE ROUTINE ADDRESSES                                                      
         DS    CL8                 UPDATE ROUTINES                              
AUPDTALL DS    A                   ALL RECORDS                                  
AUPDTAGY DS    A                   AGENCY                                       
AUPDTCLI DS    A                   CLIENT                                       
AUPDTPRD DS    A                   PRODUCT                                      
AUPDTASS DS    A                   ASSET                                        
AUPDTVER DS    A                   VERSION                                      
AUPDTALI DS    A                   ALIAS                                        
AUPDTW4R DS    A                   W-4                                          
AUPDTPER DS    A                   PERFORMER                                    
AUPDTINV DS    A                   INVOICE                                      
         DS    A                   MEDIA                                        
         DS    A                   ASSET TYPE                                   
         DS    A                   W4 TYPE                                      
         DS    A                   PERFORMER CATEGORY                           
         DS    A                   USE TYPE                                     
AUPDTCGR DS    A                   CLIENT GROUP                                 
         DS    A                   USE SUBTYPE                                  
         DS    A                   ACTRA TYPE                                   
AUPDTAGR DS    A                   AGENCY GROUP                                 
AUPDTARE DS    A                   AREA                                         
AUPDTPUS DS    A                   PRINT USE                                    
AUPDTGUA DS    A                   GUARANTEE                                    
AUPDTGUC DS    A                   GUARANTEE CONTRACT                           
AUPDTGCC DS    A                   GUARANTEE COMMENT                            
AUPDTGCY DS    A                   GUARANTEE CONTRACT YEAR TRACKING             
AUPDTAGT DS    A                   AGENT                                        
AUPDTGTK DS    A                   GUARANTEE TRACKING                           
AUPDTVCL DS    A                   COMMENTS                                     
AUPDTOFF DS    A                   OFFICE                                       
AUPDTSTF DS    A                   STAFF                                        
AUPDTCHK DS    A                   CHECK                                        
AUPDTPGR DS    A                   PRODUCT GROUP                                
AUPDTPTY DS    A                   PRODUCT TYPE                                 
AUPDTMKT DS    A                   MARKET                                       
AUPDTUSE DS    A                   USAGE HISTORY                                
AUPDTCTL DS    A                   CONTROL                                      
AUPDTEMP DS    A                   EMPLOYER                                     
AUPDTTIM DS    A                   TIMESHEET DAY                                
AUPDTLIE DS    A                   LIEN                                         
AUPDTDC  DS    A                   DUE COMPANY                                  
AUPDT74  DS    A                   MEDIA                                        
AUPDT76  DS    A                   COMMERCIAL POOL                              
AUPDT77  DS    A                   BILL TO                                      
         DS    A                   CONTRACT TYPE (LOAD ONLY)                    
AUPDT79  DS    A                   PUBLISHED MUSIC                              
AUPDT81  DS    A                   EPISODE                                      
AUPDT83  DS    A                   ECAST                                        
AUPDT84  DS    A                   FIXED CYCLE TRACKING                         
AUPDT85  DS    A                   USAGE HISTORY                                
AUPDT86  DS    A                   HISTORY COMMENT                              
         DS    A                   RECORD COUNT RECORD                          
* END OF UPDATE ROUTINE ADDRESSES                                               
*                                                                               
* FILTER ROUTINE ADDRESSES                                                      
         DS    CL8                 FILTER ROUTINES                              
AFILTAGY DS    A                   AGENCY                                       
AFILTCLI DS    A                   CLIENT                                       
AFILTPRD DS    A                   PRODUCT                                      
AFILTASS DS    A                   ASSET                                        
AFILTVER DS    A                   VERSION                                      
AFILTALI DS    A                   ALIAS                                        
AFILTW4R DS    A                   W-4                                          
AFILTPER DS    A                   PERFORMER                                    
AFILTINV DS    A                   INVOICE                                      
         DS    A                   MEDIA                                        
         DS    A                   ASSET TYPE                                   
         DS    A                   W4 TYPE                                      
         DS    A                   PERFORMER CATEGORY                           
         DS    A                   USE TYPE                                     
AFILTCGR DS    A                   CLIENT GROUP                                 
         DS    A                   USE SUBTYPE                                  
         DS    A                   ACTRA TYPE                                   
AFILTAGR DS    A                   AGENCY GROUP                                 
AFILTARE DS    A                   AREA                                         
AFILTPUS DS    A                   PRINT USE                                    
AFILTGUA DS    A                   GUARANTEE                                    
AFILTGUC DS    A                   GUARANTEE CONTRACT                           
AFILTGCC DS    A                   GUARANTEE COMMENT                            
AFILTGCY DS    A                   GUARANTEE CONTRACT YEAR TRACKING             
AFILTAGT DS    A                   AGENT                                        
AFILTGTK DS    A                   GUARANTEE TRACKING                           
AFILTVCL DS    A                   VERSION COMMENTS                             
AFILTOFF DS    A                   OFFICE                                       
AFILTSTF DS    A                   STAFF                                        
AFILTCHK DS    A                   CHECK                                        
AFILTPGR DS    A                   PRODUCT GROUP                                
AFILTPTY DS    A                   PRODUCT TYPE                                 
AFILTMKT DS    A                   MARKET                                       
AFILTUSE DS    A                   USAGE HISTORY                                
AFILTCTL DS    A                   CONTROL                                      
AFILTEMP DS    A                   EMPLOYER                                     
AFILTTIM DS    A                   TIMESHEET DAY                                
AFILTLIE DS    A                   LIEN                                         
AFILTDC  DS    A                   DUE COMPANY                                  
AFILT74  DS    A                   MEDIA                                        
AFILT76  DS    A                   COMMERCIAL POOL                              
AFILT77  DS    A                   BILL TO                                      
         DS    A                   CONTRACT TYPE                                
AFILT79  DS    A                   PUBLISHED MUSIC                              
AFILT81  DS    A                   EPISODE                                      
AFILT83  DS    A                   ECAST                                        
AFILT84  DS    A                   FIXED CYCLE TRACKING                         
AFILT85  DS    A                   USAGE HISTORY                                
AFILT86  DS    A                   HISTORY COMMENT                              
         DS    A                   RECORD COUNT RECORD                          
* END OF FILTER ROUTINE ADDRESSES                                               
*                                                                               
* INITIALIZATION ROUTINE ADDRESSES                                              
         DS    CL8                 INITIALISATION ROUTINES                      
AINITALL DS    A                   ADD RECORDS                                  
AINITAGY DS    A                   AGENCY                                       
AINITCLI DS    A                   CLIENT                                       
AINITPRD DS    A                   PRODUCT                                      
AINITASS DS    A                   ASSET                                        
AINITVER DS    A                   VERSION                                      
AINITALI DS    A                   ALIAS                                        
AINITW4R DS    A                   W-4                                          
AINITPER DS    A                   PERFORMER                                    
AINITINV DS    A                   INVOICE                                      
AINITMED DS    A                   MEDIA                                        
AINITAST DS    A                   ASSET TYPE                                   
AINITW4T DS    A                   W4 TYPE                                      
AINITPEC DS    A                   PERFORMER CATEGORY                           
AINITUST DS    A                   USE TYPE                                     
AINITCGR DS    A                   CLIENT GROUP                                 
AINITUSS DS    A                   USE SUBTYPE                                  
AINITACT DS    A                   ACTRA TYPE                                   
AINITAGR DS    A                   AGENCY GROUP                                 
AINITARE DS    A                   AREA                                         
AINITPUS DS    A                   PRINT USE                                    
AINITGUA DS    A                   GUARANTEE                                    
AINITGUC DS    A                   GUARANTEE CONTRACT                           
AINITGCC DS    A                   GUARANTEE COMMENT                            
AINITGCY DS    A                   GUARANTEE CONTRACT YEAR TRACKING             
AINITAGT DS    A                   AGENT                                        
AINITGTK DS    A                   GUARANTEE TRACKING                           
AINITVCL DS    A                   VERSION COMMENTS                             
AINITOFF DS    A                   OFFICE                                       
AINITCNT DS    A                   RECORD COUNT RECORD                          
AINITSTF DS    A                   STAFF                                        
AINITCHK DS    A                   CHECK                                        
AINITPGR DS    A                   PRODUCT GROUP                                
AINITPTY DS    A                   PRODUCT TYPE                                 
AINITMKT DS    A                   MARKET                                       
AINITUSE DS    A                   USAGE HISTORY                                
AINITNET DS    A                   NETWORK                                      
AINITCTL DS    A                   CONTROL                                      
AINITEMP DS    A                   EMPLOYER                                     
AINITTIM DS    A                   TIMESHEET                                    
AINITLIE DS    A                   LIEN                                         
AINITDC  DS    A                   DUE COMPANY                                  
AINIT74  DS    A                   MEDIA                                        
AINIT76  DS    A                   COMMERCIAL POOL                              
AINIT77  DS    A                   BILL TO                                      
AINIT78  DS    A                   CONTRACT TYPE                                
AINIT79  DS    A                   PUBLISHED MUSIC                              
AINIT81  DS    A                   EPISODE                                      
AINIT83  DS    A                   ECAST                                        
AINIT84  DS    A                   FIXED CYCLE TRACKING                         
AINIT85  DS    A                   USAGE HISTORY                                
AINIT86  DS    A                   HISTORY COMMENT                              
* END OF INITIALIZATION ADDRESSES                                               
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
TALDIR   DS    CL7                                                              
TALFIL   DS    CL7                                                              
CHKDIR   DS    CL7                                                              
CHKFIL   DS    CL7                                                              
DMDA     DS    F                                                                
DTFADDR  DS    F                                                                
ACOPYBUF DS    A                                                                
SPACES   DS    CL80                                                             
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECORD NETWORK                                                 
***********************************************************************         
                                                                                
NETTABD  DSECT                                                                  
NETCODE  DS    CL1                 CODE                                         
NETNAME  DS    CL16                NAME                                         
NETTLNQ  EQU   *-NETTABD                                                        
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER RECORD COUNT TABLE                                   *         
***********************************************************************         
                                                                                
RECTBD   DSECT                                                                  
RECRECNO DS    CL5                 RECORD NUMBER                                
RECCNTDS DS    XL2                 DISPLACMENET TO RECORD COUNT FIELD           
RECTLNQ  EQU   *-RECTBD                                                         
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
                                                                                
W4TYD    DSECT                                                                  
W4TEQU   DS    CL1                 W4 TYPE                                      
W4TNAME  DS    CL11                W4 NAME                                      
W4TNEXT  EQU   *-W4TYD                                                          
                                                                                
TGTABLED DSECT                   * SYSTEM TABLES *                              
TGAUNITS DS    V                   TALENT FEDERAL/STATE UNIT CODES              
TGACTYPS DS    V                   CANADIAN COMMERCIAL TYPES                    
TGAMEDS  DS    V                   MEDIA TYPES                                  
TGAUNIS  DS    V                   UNION CODES                                  
TGAYEARS DS    V                   CONTRACT YEARS                               
TGACATS  DS    V                   CATEGORY CODES                               
TGAUPGRS DS    V                   UPGRADE CODES                                
TGAMAJS  DS    V                   MAJOR CODES                                  
TGAUSES  DS    V                   USE CODES                                    
TGACOMT  DS    V                   COMMERCIAL TYPES                             
TGABTYP  DS    V                   BILLING TYPES                                
TGAAPPL  DS    V                   APPLIED CODES                                
TGAIERRS DS    V                   INVOICE ERROR MESSAGES                       
TGASTAFS DS    V                   STAFF CODES                                  
TGACKLK  DS    V                   URGENT CHECK RUN LOCKOUT STATUS              
TGALICS  DS    V                   LICENSER CODES                               
TGADJST  DS    V                   ADJUSTMENT CODES                             
TGACERRS DS    V                   CAST ERROR CODES                             
TGAGRACT DS    V                   $GEN RECACT TABLE                            
TGARRACT DS    V                   $REP RECACT TABLE                            
TGATHRES DS    V                   THRESHOLD TABLE                              
TGALOCS  DS    V                   FGR LOCATION CODES                           
TGAEDTYP DS    V                   EDIT TYPES                                   
                                                                                
* DXDSECTS                                                                      
       ++INCLUDE DXDSECTS                                                       
*                                                                               
* TOXRECD                                                                       
       ++INCLUDE TOXRECD                                                        
*                                                                               
* FAFACTS                                                                       
       ++INCLUDE FAFACTS                                                        
*                                                                               
* DDPERVALD                                                                     
       ++INCLUDE DDPERVALD                                                      
*                                                                               
* DMDTFIS                                                                       
       ++INCLUDE DMDTFIS                                                        
*                                                                               
* DMGREQUS                                                                      
       ++INCLUDE DMGREQUS                                                       
*                                                                               
* TAGENFILE                                                                     
       ++INCLUDE TAGENFILE                                                      
*                                                                               
* CTGENFILE                                                                     
       ++INCLUDE CTGENFILE                                                      
*                                                                               
* TASYSDSECT                                                                    
       ++INCLUDE TASYSDSECT                                                     
*                                                                               
* TASYSEQUS                                                                     
       ++INCLUDE TASYSEQUS                                                      
*                                                                               
* RXUSERD                                                                       
       ++INCLUDE RXUSERD                                                        
*                                                                               
* SEACSFILE                                                                     
       ++INCLUDE SEACSFILE                                                      
*                                                                               
* DDBUFFD                                                                       
       ++INCLUDE DDBUFFD                                                        
*                                                                               
* FACTRYEQUS                                                                    
       ++INCLUDE FACTRYEQUS                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047TOXTRACT  10/30/19'                                      
         END                                                                    
